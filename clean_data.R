# Cleaned feature-film analysis dataset. data/coverage_sample.csv stays untouched.
raw <- read.csv("data/coverage_sample.csv", stringsAsFactors = FALSE, check.names = FALSE)

na_if <- function(x, cond) { x[which(cond)] <- NA; x }   # which() ignores NA conditions

# Parse years (date_HC may be a full date or a bare year)
raw$date_OL <- suppressWarnings(as.numeric(raw$date_OL))
hc_year <- suppressWarnings(as.numeric(format(as.Date(raw$date_HC), "%Y")))
raw$date_HC <- ifelse(is.na(hc_year), suppressWarnings(as.numeric(raw$date_HC)), hc_year)

clean <- raw
clean$row_id <- seq_len(nrow(raw))

# Book ratings with no positive votes are invalid; normalize valid scores to 0-1.
for (src in c("OL", "HC")) {
  rating <- paste0("rating_", src)
  votes  <- raw[[paste0("votes_", src)]]
  date   <- paste0("date_", src)
  pages  <- paste0("pages_", src)

  clean[[rating]] <- na_if(raw[[rating]], is.na(votes) | votes <= 0) / 5
  clean[[date]]   <- na_if(raw[[date]], raw[[date]] <= 0)
  clean[[pages]]  <- na_if(raw[[pages]], raw[[pages]] == 0)
}

# Hardcover years outside plausible publication history are invalid.
current_year <- as.integer(format(Sys.Date(), "%Y"))
clean$date_HC <- na_if(clean$date_HC,
  !is.na(clean$date_HC) & (clean$date_HC < 1400 | clean$date_HC > current_year))

# TMDB: zero-vote ratings, placeholder money, and zero runtimes are invalid
clean$rating_tmdb  <- na_if(raw$rating_tmdb, raw$votes_tmdb <= 0) / 10
clean$budget_tmdb  <- na_if(raw$budget_tmdb,  raw$budget_tmdb < 1000)
clean$revenue_tmdb <- na_if(raw$revenue_tmdb, raw$revenue_tmdb < 1000)
clean$runtime_tmdb <- na_if(raw$runtime_tmdb, raw$runtime_tmdb == 0)

# MPAA: trim whitespace, blank -> NA
clean$mpaa_tmdb <- trimws(raw$mpaa_tmdb)
clean$mpaa_tmdb[clean$mpaa_tmdb %in% ""] <- NA

# Form grouping: later rules override earlier ones (lowest to highest priority)
form <- tolower(trimws(raw$form))
clean$form_group <- "other"
for (rule in list(c("novel", "novel"), c("play|drama", "play"), c("narration", "narration"),
                  c("novella", "novella"), c("short story", "short story")))
  clean$form_group[grepl(rule[1], form)] <- rule[2]

# Combined book fields: average numeric attributes, use earliest year, prefer HC text.
combine_average <- function(ol, hc) {
  value <- rowMeans(cbind(ol, hc), na.rm = TRUE)
  value[!is.finite(value)] <- NA_real_
  value
}
combine_earliest_year <- function(ol, hc) {
  value <- pmin(ol, hc, na.rm = TRUE)
  value[!is.finite(value)] <- NA_real_
  value
}
combine_sum <- function(ol, hc) {
  value <- rowSums(cbind(ol, hc), na.rm = TRUE)
  value[is.na(ol) & is.na(hc)] <- NA_real_
  value
}
combine_text <- function(ol, hc) {
  ol <- trimws(ol)
  hc <- trimws(hc)
  ol[is.na(ol) | ol == ""] <- NA_character_
  hc[is.na(hc) | hc == ""] <- NA_character_
  ifelse(!is.na(hc), hc, ol)
}
combine_rating_weighted <- function(rating_OL, rating_HC, votes_OL, votes_HC) {
  valid_OL <- !is.na(rating_OL) & !is.na(votes_OL) & votes_OL > 0
  valid_HC <- !is.na(rating_HC) & !is.na(votes_HC) & votes_HC > 0
  weight_OL <- ifelse(valid_OL, votes_OL, 0)
  weight_HC <- ifelse(valid_HC, votes_HC, 0)
  value_OL <- ifelse(valid_OL, rating_OL, 0)
  value_HC <- ifelse(valid_HC, rating_HC, 0)
  total_votes <- weight_OL + weight_HC
  result <- (value_OL * weight_OL + value_HC * weight_HC) / total_votes
  result[total_votes == 0] <- NA_real_
  result
}

clean$title_book_combined  <- combine_text(clean$title_OL, clean$title_HC)
clean$author_book_combined <- combine_text(clean$author_OL, clean$author_HC)
clean$year_book_combined   <- combine_earliest_year(clean$date_OL, clean$date_HC)
clean$rating_book_combined <- combine_average(clean$rating_OL, clean$rating_HC)
clean$rating_book_combined_weighted <- combine_rating_weighted(
  clean$rating_OL, clean$rating_HC, clean$votes_OL, clean$votes_HC)
clean$votes_book_combined  <- combine_sum(clean$votes_OL, clean$votes_HC)
clean$pages_book_combined  <- combine_average(clean$pages_OL, clean$pages_HC)

# Analysis cohort: rated films with >= 20 TMDB votes and feature-length runtime
eligible <- !is.na(clean$rating_tmdb) & !is.na(raw$votes_tmdb) & raw$votes_tmdb >= 20 &
  !is.na(clean$runtime_tmdb) & clean$runtime_tmdb >= 40

# Hide rows with no title from either book catalog.
has_title_OL <- !is.na(raw$title_OL) & nzchar(trimws(raw$title_OL))
has_title_HC <- !is.na(raw$title_HC) & nzchar(trimws(raw$title_HC))
eligible <- eligible & (has_title_OL | has_title_HC)

analysis <- clean[eligible, ]
write.csv(analysis, "data/coverage_clean.csv", row.names = FALSE, na = "")

cat("Rows:", nrow(analysis), " Columns:", ncol(analysis), "\n")
