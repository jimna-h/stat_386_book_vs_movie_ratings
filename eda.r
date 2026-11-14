dat <- read.csv("data/coverage_clean.csv", stringsAsFactors = FALSE,
                na.strings = c("", "NA"))   # clean_data.R writes NA as empty strings
# Shared visual style for all exploratory plots.
op_global <- par(no.readonly = TRUE)
par(family = "sans", bg = "white", col.axis = "#34495E", col.lab = "#34495E",
    col.main = "#233746", mar = c(4.2, 4.2, 3.0, 1.0),
    mgp = c(2.4, 0.7, 0), tcl = -0.25, bty = "l", las = 1,
    cex.main = 0.95, cex.lab = 0.9, cex.axis = 0.82)
col_ol <- "#397A9E"
col_hc <- "#D17A52"
col_points <- adjustcolor("#34495E", alpha.f = 0.55)
col_trend <- "#2C7A7B"
col_reference <- "#C65D4B"
col_hist <- "#83AFC1"


# ---- Quick sample and cleaned-variable checks ------------------------------
print(nrow(dat))  # every row meets the feature-film cohort criteria
print(summary(dat[c("rating_tmdb", "rating_OL", "rating_HC",
                    "votes_tmdb", "votes_OL", "votes_HC", "runtime_tmdb",
                    "pages_OL", "pages_HC", "date_OL", "date_HC",
                    "budget_tmdb", "revenue_tmdb", "year_book_combined",
                    "rating_book_combined", "rating_book_combined_weighted",
                    "votes_book_combined",
                    "pages_book_combined")]))
print(round(sort(colMeans(is.na(dat)), decreasing = TRUE), 2))

# ---- Book rating coverage (every row has a TMDB rating by construction) ----
print(addmargins(table(OpenLibrary = !is.na(dat$rating_OL),
                       Hardcover   = !is.na(dat$rating_HC))))

# ---- Vote counts: log1p keeps zero votes visible ---------------------------
op <- par(mfrow = c(1, 3))
vote_breaks <- pretty(range(log1p(unlist(dat[c("votes_tmdb", "votes_OL", "votes_HC")])),
                           na.rm = TRUE), n = 25)
vote_hists <- lapply(c("votes_tmdb", "votes_OL", "votes_HC"), function(v)
  hist(log1p(dat[[v]]), breaks = vote_breaks, plot = FALSE))
vote_ylim <- c(0, max(vapply(vote_hists, function(h) max(h$counts), numeric(1))))
for (v in c("votes_tmdb", "votes_OL", "votes_HC")) {
  vote_color <- if (v == "votes_OL") col_ol else if (v == "votes_HC") col_hc else col_hist
  hist(log1p(dat[[v]]), breaks = vote_breaks, xlim = range(vote_breaks),
       ylim = vote_ylim, col = vote_color, border = "white", main = v,
       xlab = "log(1 + votes)")
}
par(op)

# ---- Helper: plot a pair and report linear and rank correlations -----------
plot_relationship <- function(x, y, xlab, ylab, main,
                              xlim = NULL, ylim = NULL, trend = TRUE, point_color = col_points) {
  keep <- is.finite(x) & is.finite(y)   # also excludes -Inf from log10(0)
  plot(x[keep], y[keep], xlab = xlab, ylab = ylab, main = main,
       pch = 16, cex = 0.72, col = point_color, xlim = xlim, ylim = ylim)
  if (sum(keep) > 1 && length(unique(x[keep])) > 1 && length(unique(y[keep])) > 1) {
    if (trend) abline(lm(y[keep] ~ x[keep]), col = col_trend, lwd = 2)
    c(n = sum(keep),
      pearson  = cor(x[keep], y[keep], method = "pearson"),
      spearman = cor(x[keep], y[keep], method = "spearman"))
  } else {
    c(n = sum(keep), pearson = NA_real_, spearman = NA_real_)
  }
}

# ---- Book vs. film ratings (all ratings use a 0-1 scale) -------------------
book_rating_OL <- dat$rating_OL
book_rating_HC <- dat$rating_HC
op <- par(mfrow = c(1, 2))
ol_stats <- plot_relationship(book_rating_OL, dat$rating_tmdb,
  "Open Library rating (0-1)", "TMDB rating (0-1)",
  "Open Library vs. film", xlim = c(0, 1), ylim = c(0, 1), point_color = col_ol)
hc_stats <- plot_relationship(book_rating_HC, dat$rating_tmdb,
  "Hardcover rating (0-1)", "TMDB rating (0-1)",
  "Hardcover vs. film", xlim = c(0, 1), ylim = c(0, 1), point_color = col_hc)
par(op)
print(ol_stats)
print(hc_stats)

# ---- Sensitivity check: book ratings with at least 20 votes ----------------
# These subsets are smaller and skew toward popular titles; report n with each r.
min_votes <- 20
ol_reliable <- !is.na(dat$votes_OL) & dat$votes_OL >= min_votes
hc_reliable <- !is.na(dat$votes_HC) & dat$votes_HC >= min_votes
op <- par(mfrow = c(1, 2))
ol_reliable_stats <- plot_relationship(book_rating_OL[ol_reliable],
  dat$rating_tmdb[ol_reliable], "Open Library rating (0-1)", "TMDB rating (0-1)",
  "Open Library, 20+ votes", xlim = c(0, 1), ylim = c(0, 1), point_color = col_ol)
hc_reliable_stats <- plot_relationship(book_rating_HC[hc_reliable],
  dat$rating_tmdb[hc_reliable], "Hardcover rating (0-1)", "TMDB rating (0-1)",
  "Hardcover, 20+ votes", xlim = c(0, 1), ylim = c(0, 1), point_color = col_hc)
par(op)
print(ol_reliable_stats)
print(hc_reliable_stats)

# ---- Compare the two book rating sources (0-1 scale) -----------------------
print(plot_relationship(dat$rating_OL, dat$rating_HC,
  "Open Library rating", "Hardcover rating", "Open Library vs. Hardcover"))

# ---- Book length vs. runtime (pages on log scale: multi-volume sets) -------
op <- par(mfrow = c(1, 2))
page_xlim <- range(log10(c(dat$pages_OL, dat$pages_HC)), na.rm = TRUE)
runtime_ylim <- range(dat$runtime_tmdb, na.rm = TRUE)
print(plot_relationship(log10(dat$pages_OL), dat$runtime_tmdb,
  "Open Library pages (log10)", "Runtime (minutes)", "Pages vs. runtime",
  xlim = page_xlim, ylim = runtime_ylim, trend = FALSE, point_color = col_ol))
print(plot_relationship(log10(dat$pages_HC), dat$runtime_tmdb,
  "Hardcover pages (log10)", "Runtime (minutes)", "Hardcover pages vs. runtime",
  xlim = page_xlim, ylim = runtime_ylim, trend = FALSE, point_color = col_hc))
par(op)

# ---- Publication year and release gap --------------------------------------
# Open Library / Hardcover dates may be edition dates, so gaps are approximate
# and negative gaps can reflect reissues rather than errors.
movie_year     <- as.integer(format(as.Date(dat$date_tmdb), "%Y"))
release_gap    <- movie_year - dat$date_OL
release_gap_HC <- movie_year - dat$date_HC
op <- par(mfrow = c(1, 2))
release_xlim <- range(c(release_gap, release_gap_HC), na.rm = TRUE)
print(plot_relationship(release_gap, dat$rating_tmdb,
  "Film year minus Open Library year", "TMDB rating",
  "Release gap (Open Library)", xlim = release_xlim, ylim = c(0, 1),
  trend = FALSE, point_color = col_ol))
print(plot_relationship(release_gap_HC, dat$rating_tmdb,
  "Film year minus Hardcover year", "TMDB rating",
  "Release gap (Hardcover)", xlim = release_xlim, ylim = c(0, 1),
  trend = FALSE, point_color = col_hc))
par(op)
print(summary(release_gap))
print(summary(release_gap_HC))

# ---- Financial comparison (nominal dollars; values < $1,000 already NA) ----
print(plot_relationship(log10(dat$budget_tmdb), log10(dat$revenue_tmdb),
  "Budget (log10 nominal dollars)", "Revenue (log10 nominal dollars)",
  "Budget vs. revenue"))

# ---- Rating differences on the same 0-1 scale -----------------------------
# Part of the film-minus-book gap is a scale/rating-culture artifact.
rating_difference_OL <- dat$rating_tmdb - book_rating_OL
rating_difference_HC <- dat$rating_tmdb - book_rating_HC
op <- par(mfrow = c(1, 2))
difference_limits <- range(c(rating_difference_OL, rating_difference_HC), na.rm = TRUE)
difference_breaks <- pretty(difference_limits, n = 20)
difference_hists <- list(
  hist(rating_difference_OL, breaks = difference_breaks, plot = FALSE),
  hist(rating_difference_HC, breaks = difference_breaks, plot = FALSE))
difference_ylim <- c(0, max(vapply(difference_hists, function(h) max(h$counts), numeric(1))))
hist(rating_difference_OL[is.finite(rating_difference_OL)], breaks = difference_breaks,
     xlim = difference_limits, ylim = difference_ylim, col = col_ol, border = "white",
     xlab = "TMDB minus Open Library", main = "Film - book (Open Library)")
abline(v = 0, col = col_reference, lwd = 2, lty = 2)
hist(rating_difference_HC[is.finite(rating_difference_HC)], breaks = difference_breaks,
     xlim = difference_limits, ylim = difference_ylim, col = col_hc, border = "white",
     xlab = "TMDB minus Hardcover", main = "Film - book (Hardcover)")
abline(v = 0, col = col_reference, lwd = 2, lty = 2)
par(op)
print(summary(rating_difference_OL))
print(summary(rating_difference_HC))

# ---- Form and MPAA comparisons ---------------------------------------------
# Labels are built separately for each panel so n matches the data plotted.
form_levels <- c("narration", "novel", "novella", "other", "play", "short story")
dat$form_group <- factor(dat$form_group, levels = form_levels)
group_names <- function(d) {
  n <- table(d$form_group)
  paste0(names(n), "\n(n=", as.integer(n), ")")
}
rev_dat <- dat[!is.na(dat$revenue_tmdb), ]
print(table(dat$form_group))
op <- par(mfrow = c(1, 2))
boxplot(rating_tmdb ~ form_group, data = dat, names = group_names(dat),
        las = 2, outline = FALSE, col = c("#52796F", "#84A98C", "#A3B18A", "#CAD2C5", "#6C757D", "#ADB5BD"),
        xlab = "Book form", ylab = "TMDB rating",
        main = "Film rating by book form")
boxplot(log10(revenue_tmdb) ~ form_group, data = rev_dat, names = group_names(rev_dat),
        las = 2, outline = FALSE, col = c("#52796F", "#84A98C", "#A3B18A", "#CAD2C5", "#6C757D", "#ADB5BD"),
        xlab = "Book form", ylab = "log10 revenue",
        main = "Revenue by book form")
par(op)
print(table(dat$mpaa_tmdb, useNA = "ifany"))
boxplot(rating_tmdb ~ mpaa_tmdb, data = dat, las = 2,
        outline = FALSE, xlab = "MPAA rating", ylab = "TMDB rating",
        main = "Film rating by MPAA")

# ---- Coverage by decade (revenue is nominal, not inflation-adjusted) -------
# Decades with fewer than min_n films are dropped because medians are unstable.
dat$film_decade <- floor(movie_year / 10) * 10
min_n <- 20
decade_summary <- function(d, var) {
  d <- d[!is.na(d[[var]]) & !is.na(d$film_decade), ]
  out <- aggregate(d[[var]], by = list(film_decade = d$film_decade),
                   FUN = function(z) c(median = median(z), n = length(z)))
  out <- data.frame(film_decade = out$film_decade,
                    median = out$x[, "median"], n = out$x[, "n"])
  out[out$n >= min_n, ]
}
decade_rating  <- decade_summary(dat, "rating_tmdb")
decade_revenue <- decade_summary(dat, "revenue_tmdb")
print(decade_rating)
print(decade_revenue)
op <- par(mfrow = c(1, 2))
decade_xlim <- range(c(decade_rating$film_decade, decade_revenue$film_decade), na.rm = TRUE)
plot(decade_rating$film_decade, decade_rating$median, type = "b",
     xlim = decade_xlim, xlab = "Film decade", ylab = "Median rating",
     main = paste0("Rating by decade (n >= ", min_n, ")"))
plot(decade_revenue$film_decade, decade_revenue$median, type = "b",
     xlim = decade_xlim, xlab = "Film decade", ylab = "Median nominal revenue",
     main = paste0("Revenue by decade (n >= ", min_n, ")"))
par(op)



# ---- Common joint missingness patterns -------------------------------------
# rating_OL, rating_HC, pages_OL, pages_HC, date_OL, date_HC
book_fields <- c("rating_OL", "rating_HC", "pages_OL", "pages_HC", "date_OL", "date_HC")
book_present <- !is.na(dat[book_fields])
pattern_key <- apply(book_present, 1, function(row) paste0(as.integer(row), collapse = ""))
pattern_counts <- head(sort(table(pattern_key), decreasing = TRUE), 10)
op <- par(mar = c(7, 4, 3, 1))
barplot(pattern_counts, las = 2, cex.names = 0.75, ylab = "Records",
        main = "Top missingness patterns")
mtext(paste("Digits:", paste(book_fields, collapse = ", ")), side = 1, line = 5.5, cex = 0.7)
par(op)
# Restore the caller's graphics settings.
par(op_global)
