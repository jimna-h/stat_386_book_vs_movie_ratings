import difflib
import os
import time

import pandas as pd
import requests
from dotenv import load_dotenv

"""General Variables"""
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
INPUT_CSV = os.path.join(SCRIPT_DIR, "data", "raw", "wikidata_pairs.csv")
OUTPUT_CSV = os.path.join(SCRIPT_DIR, "coverage_sample.csv")
SAMPLE_SIZE = 50  # Set to None to process every pair.
MATCH_THRESHOLD = 0.85
REQUEST_INTERVAL = 0.7

USER_AGENT = "STAT386-project/0.1 jbruce1@byu.edu"
HEADERS = {"User-Agent": USER_AGENT}
HARDCOVER_URL = "https://api.hardcover.app/v1/graphql"

INPUT_COLUMNS = [
    "film",
    "book",
    "bookLabel",
    "authorLabel",
    "formLabel",
    "filmLinks",
    "tmdb",
]

OUTPUT_COLUMNS = [
    "title_tmdb",
    "title_OL",
    "title_HC",
    "author_OL",
    "author_HC",
    "form",
    "date_tmdb",
    "date_OL",
    "date_HC",
    "rating_tmdb",
    "votes_tmdb",
    "rating_OL",
    "votes_OL",
    "rating_HC",
    "votes_HC",
    "runtime_tmdb",
    "budget_tmdb",
    "revenue_tmdb",
    "pages_OL",
    "pages_HC",
]


def request_json(method, url, params=None, payload=None, headers=None):
    if headers is None:
        headers = HEADERS

    try:
        response = requests.request(
            method,
            url,
            params=params,
            json=payload,
            headers=headers,
            timeout=30,
        )
        response.raise_for_status()
        time.sleep(REQUEST_INTERVAL)
        return response.json()
    except requests.RequestException as exc:
        print(f"Request failed: {exc}")
        return None


def best_title_match(title, candidates):
    best, best_score = None, 0.0
    for candidate in candidates:
        candidate_title = candidate.get("title") or ""
        score = difflib.SequenceMatcher(
            None, str(title).casefold(), candidate_title.casefold()
        ).ratio()
        if score > best_score:
            best, best_score = candidate, score
    return best, best_score


def tmdb_lookup(tmdb_id, api_key):
    empty = {
        "title_tmdb": None,
        "date_tmdb": None,
        "rating_tmdb": None,
        "votes_tmdb": None,
        "runtime_tmdb": None,
        "budget_tmdb": None,
        "revenue_tmdb": None,
    }
    if pd.isna(tmdb_id):
        return empty

    data = request_json(
        "GET",
        f"https://api.themoviedb.org/3/movie/{int(tmdb_id)}",
        params={"api_key": api_key},
    )
    if not data:
        return empty
    return {
        "title_tmdb": data.get("title"),
        "date_tmdb": data.get("release_date"),
        "rating_tmdb": data.get("vote_average"),
        "votes_tmdb": data.get("vote_count"),
        "runtime_tmdb": data.get("runtime"),
        "budget_tmdb": data.get("budget"),
        "revenue_tmdb": data.get("revenue"),
    }


def openlibrary_lookup(title, author):
    data = request_json(
        "GET",
        "https://openlibrary.org/search.json",
        params={
            "title": title,
            "author": author,
            "fields": "title,author_name,first_publish_year,ratings_average,ratings_count,number_of_pages_median",
            "limit": 5,
        },
    )
    docs = (data or {}).get("docs", [])
    best, best_score = best_title_match(title, docs)

    if best is None or best_score < MATCH_THRESHOLD:
        return {}
    return {
        "title_OL": best.get("title"),
        "author_OL": ", ".join(best.get("author_name") or []),
        "date_OL": best.get("first_publish_year"),
        "rating_OL": best.get("ratings_average"),
        "votes_OL": best.get("ratings_count"),
        "pages_OL": best.get("number_of_pages_median"),
    }


def hardcover_request(query, variables, api_key):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
        **HEADERS,
    }
    payload = request_json(
        "POST",
        HARDCOVER_URL,
        payload={"query": query, "variables": variables},
        headers=headers,
    )
    if payload and payload.get("errors"):
        print(f"Hardcover GraphQL error: {payload['errors']}")
        return None
    return (payload or {}).get("data")


def hardcover_lookup(title, author, api_key):
    search_query = """
        query SearchBooks($q: String!) {
          search(query: $q, query_type: "book", per_page: 10) { ids }
        }
    """
    search_text = f"{title} {author}".strip()
    search_data = hardcover_request(search_query, {"q": search_text}, api_key)
    ids = ((search_data or {}).get("search") or {}).get("ids") or []
    if not ids:
        return {}

    details_query = """
        query BookDetails($ids: [Int!]) {
          books(where: {id: {_in: $ids}}) {
            id
            title
            rating
            ratings_count
            default_cover_edition {
              release_date
              pages
            }
            contributions {
              contribution
              author { name }
            }
          }
        }
    """
    details_data = hardcover_request(details_query, {"ids": ids}, api_key)
    books = ((details_data or {}).get("books") or [])
    best, best_score = best_title_match(title, books)

    if best is None or best_score < MATCH_THRESHOLD:
        return {}
    edition = best.get("default_cover_edition") or {}
    authors = []
    for contribution in best.get("contributions") or []:
        author = contribution.get("author")
        if (
            (contribution.get("contribution") or "").casefold() == "author"
            and author
            and author.get("name")
        ):
            authors.append(author["name"])
    return {
        "title_HC": best.get("title"),
        "author_HC": ", ".join(authors),
        "date_HC": edition.get("release_date"),
        "rating_HC": best.get("rating"),
        "votes_HC": best.get("ratings_count"),
        "pages_HC": edition.get("pages"),
    }


def main():
    load_dotenv(os.path.join(SCRIPT_DIR, ".env"))
    tmdb_api_key = os.environ.get("TMDB_API_KEY")
    hardcover_api_key = os.environ.get("HARDCOVER_API_KEY")
    if not tmdb_api_key:
        raise SystemExit("Add TMDB_API_KEY to the .env file beside this script.")
    if not hardcover_api_key:
        raise SystemExit("Add HARDCOVER_API_KEY to the .env file beside this script.")
    if not os.path.isfile(INPUT_CSV):
        raise SystemExit(
            f"Wikidata CSV not found: {INPUT_CSV}\n"
            "Update INPUT_CSV near the top of this file if it is stored elsewhere."
        )

    df = pd.read_csv(INPUT_CSV)
    missing_columns = sorted(set(INPUT_COLUMNS) - set(df.columns))
    if missing_columns:
        raise SystemExit(f"Input CSV is missing required columns: {missing_columns}")

    df["tmdb"] = pd.to_numeric(df["tmdb"], errors="coerce")
    df = df.drop_duplicates(subset=["film", "book"])
    df = df.sort_values("filmLinks", ascending=False)
    if SAMPLE_SIZE is not None:
        df = df.head(SAMPLE_SIZE)
    df = df[INPUT_COLUMNS].reset_index(drop=True)
    if df.empty:
        raise SystemExit("No input rows to process after filtering and sampling.")

    rows = []
    for index, row in df.iterrows():
        film = tmdb_lookup(row["tmdb"], tmdb_api_key)
        openlibrary = openlibrary_lookup(row["bookLabel"], row["authorLabel"])
        hardcover = hardcover_lookup(
            row["bookLabel"], row["authorLabel"], hardcover_api_key
        )
        rows.append(
            {
                **film,
                **openlibrary,
                **hardcover,
                "form": row["formLabel"],
            }
        )
        print(
            f"[{index + 1}/{len(df)}] {row['bookLabel']} -> "
            f"TMDb: {film['title_tmdb']}"
        )

    output = pd.DataFrame(rows, columns=OUTPUT_COLUMNS)
    output.to_csv(OUTPUT_CSV, index=False)
    print("\nRating coverage:")
    for source, title_column, votes_column in [
        ("TMDb", "title_tmdb", "votes_tmdb"),
        ("Open Library", "title_OL", "votes_OL"),
        ("Hardcover", "title_HC", "votes_HC"),
    ]:
        matched = output[title_column].notna().sum()
        votes = output[votes_column].fillna(0)
        print(f"{source}: {matched} matched, {(votes > 0).sum()} rated, {(votes >= 10).sum()} with 10+ votes")
    print(f"\nSaved {len(output)} rows to {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
