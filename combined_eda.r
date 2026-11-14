# Exploratory analysis using combined Open Library / Hardcover book fields.
dat <- read.csv("data/coverage_clean.csv", stringsAsFactors = FALSE,
                na.strings = c("", "NA"))

# Shared visual style.
op_global <- par(no.readonly = TRUE)
par(family = "sans", bg = "white", col.axis = "#34495E", col.lab = "#34495E",
    col.main = "#233746", mar = c(4.2, 4.2, 3.0, 1.0),
    mgp = c(2.4, 0.7, 0), tcl = -0.25, bty = "l", las = 1,
    cex.main = 0.95, cex.lab = 0.9, cex.axis = 0.82)
col_book <- "#6A5D9E"
col_film <- "#4C8196"
col_points <- adjustcolor("#34495E", alpha.f = 0.55)
col_trend <- "#2C7A7B"
col_reference <- "#C65D4B"
col_hist <- "#83AFC1"

# Quick sample and combined-variable checks.
print(nrow(dat))
print(summary(dat[c("rating_tmdb", "rating_book_combined",
                    "rating_book_combined_weighted",
                    "votes_tmdb", "votes_book_combined", "runtime_tmdb",
                    "pages_book_combined", "year_book_combined",
                    "budget_tmdb", "revenue_tmdb")] ))
print(round(sort(colMeans(is.na(dat)), decreasing = TRUE), 2))

# Book / film rating coverage in the filtered cohort.
print(addmargins(table(book_rating = !is.na(dat$rating_book_combined),
                       film_rating = !is.na(dat$rating_tmdb))))

# Vote counts are strongly skewed; log1p keeps zero votes visible.
op <- par(mfrow = c(1, 2))
vote_breaks <- pretty(range(log1p(c(dat$votes_tmdb, dat$votes_book_combined)),
                            na.rm = TRUE), n = 25)
film_vote_hist <- hist(log1p(dat$votes_tmdb), breaks = vote_breaks, plot = FALSE)
book_vote_hist <- hist(log1p(dat$votes_book_combined), breaks = vote_breaks, plot = FALSE)
vote_ylim <- c(0, max(film_vote_hist$counts, book_vote_hist$counts))
hist(log1p(dat$votes_tmdb), breaks = vote_breaks, xlim = range(vote_breaks),
     ylim = vote_ylim, col = col_film, border = "white",
     main = "TMDB votes", xlab = "log(1 + votes)")
hist(log1p(dat$votes_book_combined), breaks = vote_breaks, xlim = range(vote_breaks),
     ylim = vote_ylim, col = col_book, border = "white",
     main = "Combined book votes", xlab = "log(1 + votes)")
par(op)

# Plot a pair and report linear and rank correlations.
plot_relationship <- function(x, y, xlab, ylab, main,
                              xlim = NULL, ylim = NULL, trend = TRUE,
                              point_color = col_points) {
  keep <- is.finite(x) & is.finite(y)
  plot(x[keep], y[keep], xlab = xlab, ylab = ylab, main = main,
       pch = 16, cex = 0.72, col = point_color, xlim = xlim, ylim = ylim)
  if (sum(keep) > 1 && length(unique(x[keep])) > 1 && length(unique(y[keep])) > 1) {
    if (trend) abline(lm(y[keep] ~ x[keep]), col = col_trend, lwd = 2)
    c(n = sum(keep), pearson = cor(x[keep], y[keep]),
      spearman = cor(x[keep], y[keep], method = "spearman"))
  } else {
    c(n = sum(keep), pearson = NA_real_, spearman = NA_real_)
  }
}

# Combined book and film ratings are already on a common 0-1 scale.
book_rating <- dat$rating_book_combined
book_rating_weighted <- dat$rating_book_combined_weighted
op <- par(mfrow = c(1, 2))
combined_rating_stats <- plot_relationship(book_rating, dat$rating_tmdb,
  "Combined book rating (0-1)", "TMDB rating (0-1)",
  "Simple average vs. film", xlim = c(0, 1), ylim = c(0, 1),
  point_color = col_book)
weighted_rating_stats <- plot_relationship(book_rating_weighted, dat$rating_tmdb,
  "Vote-weighted book rating (0-1)", "TMDB rating (0-1)",
  "Vote-weighted average vs. film", xlim = c(0, 1), ylim = c(0, 1),
  point_color = col_book)
par(op)
print(combined_rating_stats)
print(weighted_rating_stats)

# Sensitivity check among combined book ratings with at least 20 votes.
min_votes <- 20
reliable_book <- !is.na(dat$votes_book_combined) & dat$votes_book_combined >= min_votes
reliable_rating_stats <- plot_relationship(book_rating[reliable_book],
  dat$rating_tmdb[reliable_book], "Combined book rating (0-1)", "TMDB rating (0-1)",
  "Combined book, 20+ votes", xlim = c(0, 1), ylim = c(0, 1),
  point_color = col_book)
print(reliable_rating_stats)

# Combined pages vs. film runtime.
plot_relationship(dat$pages_book_combined, dat$runtime_tmdb,
  "Combined book pages", "Runtime (minutes)", "Combined pages vs. runtime",
  trend = FALSE, point_color = col_book)

# Approximate release gap uses the earliest available OL / HC publication year.
movie_year <- as.integer(format(as.Date(dat$date_tmdb), "%Y"))
release_gap <- movie_year - dat$year_book_combined
release_gap_stats <- plot_relationship(release_gap, dat$rating_tmdb,
  "Film year minus combined book year", "TMDB rating",
  "Release gap vs. film rating", trend = FALSE, point_color = col_book)
print(release_gap_stats)
print(summary(release_gap))

# Financial values are nominal; log scale is used for the wide range.
financial_stats <- plot_relationship(log10(dat$budget_tmdb), log10(dat$revenue_tmdb),
  "Budget (log10 nominal dollars)", "Revenue (log10 nominal dollars)",
  "Budget vs. revenue")
print(financial_stats)

# Film-minus-book rating difference.
rating_difference <- dat$rating_tmdb - book_rating
op <- par(mfrow = c(1, 1))
hist(rating_difference[is.finite(rating_difference)], breaks = 20,
     col = col_book, border = "white", xlab = "TMDB minus combined book rating",
     main = "Film - combined book rating")
abline(v = 0, col = col_reference, lwd = 2, lty = 2)
print(summary(rating_difference))

# Form and MPAA comparisons.
form_levels <- c("narration", "novel", "novella", "other", "play", "short story")
dat$form_group <- factor(dat$form_group, levels = form_levels)
group_names <- function(d) {
  n <- table(d$form_group)
  paste0(names(n), "\n(n=", as.integer(n), ")")
}
rev_dat <- dat[!is.na(dat$revenue_tmdb), ]
print(table(dat$form_group))
op <- par(mfrow = c(1, 2))
form_colors <- c("#52796F", "#84A98C", "#A3B18A", "#CAD2C5", "#6C757D", "#ADB5BD")
boxplot(rating_tmdb ~ form_group, data = dat, names = group_names(dat),
        las = 2, outline = FALSE, col = form_colors,
        xlab = "Book form", ylab = "TMDB rating", main = "Film rating by book form")
boxplot(log10(revenue_tmdb) ~ form_group, data = rev_dat, names = group_names(rev_dat),
        las = 2, outline = FALSE, col = form_colors,
        xlab = "Book form", ylab = "log10 revenue", main = "Revenue by book form")
par(op)
print(table(dat$mpaa_tmdb, useNA = "ifany"))
boxplot(rating_tmdb ~ mpaa_tmdb, data = dat, las = 2, outline = FALSE,
        xlab = "MPAA rating", ylab = "TMDB rating", main = "Film rating by MPAA")

# Rating and nominal revenue by decade; suppress groups with fewer than 20 films.
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
decade_rating <- decade_summary(dat, "rating_tmdb")
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

# Missingness patterns for combined book fields.
book_fields <- c("title_book_combined", "author_book_combined",
                 "year_book_combined", "rating_book_combined",
                 "votes_book_combined", "pages_book_combined")
book_present <- !is.na(dat[book_fields])
pattern_key <- apply(book_present, 1, function(row) paste0(as.integer(row), collapse = ""))
pattern_counts <- head(sort(table(pattern_key), decreasing = TRUE), 10)
op <- par(mar = c(7, 4, 3, 1))
barplot(pattern_counts, las = 2, cex.names = 0.75, ylab = "Records",
        main = "Combined-book missingness patterns")
mtext(paste("Digits:", paste(book_fields, collapse = ", ")),
      side = 1, line = 5.5, cex = 0.7)
par(op)

# Restore caller graphics settings.
par(op_global)
