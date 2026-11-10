dat <- read.csv("coverage_sample.csv", nrows = 2000, stringsAsFactors = FALSE)

cat("Rows in this EDA sample:", nrow(dat), "\n")

eda_vars <- c(
  "rating_tmdb", "rating_OL", "rating_HC", "votes_tmdb",
  "runtime_tmdb", "pages_OL", "pages_HC", "date_OL",
  "budget_tmdb", "revenue_tmdb"
)

cat("\nSummary of key variables:\n")
print(summary(dat[eda_vars]))

cat("\nMissing values by column (proportion):\n")
print(round(sort(colMeans(is.na(dat)), decreasing = TRUE), 2))

cat("\nMost common book forms:\n")
form_counts <- sort(table(dat$form), decreasing = TRUE)
print(head(form_counts, 10))

cat("\nZero values in budget, revenue, and book publication year:\n")
print(sapply(dat[c("budget_tmdb", "revenue_tmdb", "date_OL")],
             function(x) sum(x == 0, na.rm = TRUE)))

plot_relationship <- function(x, y, xlab, ylab, main,
                              xlim = NULL, ylim = NULL) {
  keep <- complete.cases(x, y)
  cat("\n", main, "- complete pairs:", sum(keep), sep = "")

  plot(x[keep], y[keep], xlab = xlab, ylab = ylab, main = main,
       pch = 19, col = "gray40", xlim = xlim, ylim = ylim)

  if (sum(keep) > 1 && length(unique(x[keep])) > 1) {
    abline(lm(y[keep] ~ x[keep]), col = "steelblue", lwd = 2)
    cat("; Pearson correlation:",
        round(cor(x[keep], y[keep]), 2), "\n")
  } else {
    cat("\n")
  }
}

# Convert book ratings from a 0-5 scale to the TMDB 0-10 scale.
book_rating_OL <- dat$rating_OL * 2
book_rating_HC <- dat$rating_HC * 2

par(mfrow = c(2, 3))

plot_relationship(
  book_rating_OL, dat$rating_tmdb,
  "Open Library rating (rescaled to 0-10)", "TMDB movie rating",
  "Book vs. film ratings (Open Library)", xlim = c(0, 10), ylim = c(0, 10)
)
abline(0, 1, col = "firebrick", lty = 2)

plot_relationship(
  book_rating_HC, dat$rating_tmdb,
  "Hardcover rating (rescaled to 0-10)", "TMDB movie rating",
  "Book vs. film ratings (Hardcover)", xlim = c(0, 10), ylim = c(0, 10)
)
abline(0, 1, col = "firebrick", lty = 2)

plot_relationship(
  dat$pages_OL, dat$runtime_tmdb,
  "Open Library pages", "Movie runtime (minutes)",
  "Book length vs. film runtime"
)

movie_year <- as.numeric(format(as.Date(dat$date_tmdb), "%Y"))
release_gap <- movie_year - ifelse(dat$date_OL > 0, dat$date_OL, NA)
plot_relationship(
  release_gap, dat$rating_tmdb,
  "Film year minus book publication year", "TMDB movie rating",
  "Release gap vs. film rating"
)
abline(h = mean(dat$rating_tmdb, na.rm = TRUE), col = "firebrick", lty = 2)

positive_budget <- ifelse(dat$budget_tmdb > 0, log10(dat$budget_tmdb), NA)
positive_revenue <- ifelse(dat$revenue_tmdb > 0, log10(dat$revenue_tmdb), NA)
plot_relationship(
  positive_budget, positive_revenue,
  "Movie budget (log10)", "Movie revenue (log10)",
  "Budget vs. box office revenue"
)
abline(0, 1, col = "firebrick", lty = 2)

rating_difference <- dat$rating_tmdb - book_rating_OL
rating_difference <- rating_difference[is.finite(rating_difference)]
hist(rating_difference, breaks = 20,
     xlab = "TMDB rating minus Open Library rating (both 0-10)",
     main = "Film vs. book rating difference", col = "gray80")
abline(v = 0, col = "firebrick", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

cat("\nRating difference summary (TMDB minus rescaled Open Library):\n")
print(summary(dat$rating_tmdb - book_rating_OL))
cat("\nFinancial amounts are nominal and are not adjusted for inflation.\n")
cat("These plots are exploratory; associations do not establish causation.\n")
