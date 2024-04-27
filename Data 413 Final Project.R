library(gtrendsR)
# Define the keyword (soccer) and Google properties (e.g., web, news, images, youtube)
keywords <- c("Israel genocide", "Israel terrorism")
gprop <- c("web")

# Set other parameters
states <- c(
  "US-AL", "US-AK", "US-AZ", "US-AR", "US-CA", "US-CO", "US-CT", "US-DE", "US-FL", "US-GA",
  "US-HI", "US-ID", "US-IL", "US-IN", "US-IA", "US-KS", "US-KY", "US-LA", "US-ME", "US-MD",
  "US-MA", "US-MI", "US-MN", "US-MS", "US-MO", "US-MT", "US-NE", "US-NV", "US-NH", "US-NJ",
  "US-NM", "US-NY", "US-NC", "US-ND", "US-OH", "US-OK", "US-OR", "US-PA", "US-RI", "US-SC",
  "US-SD", "US-TN", "US-TX", "US-UT", "US-VT", "US-VA", "US-WA", "US-WV", "US-WI", "US-WY"
)
time <- "today 12-m"  # Last five years
hl <- "en-US"  # Language
tz <- 0  # Timezone

# Perform the query
get_trends_state <- function(keyword) {
  trends_state <- gtrends(keyword = keyword,
                    geo = states,
                    time = time,
                    gprop = gprop,
                    hl = hl,
                    tz = tz)
  return(trends_state)
}
print(get_trends_state)
# View the results
trends_data <- lapply(keywords, get_trends)


interest_over_time <- lapply(trends_data, function(trends) trends$interest_over_time)



combined_interest <- do.call(rbind, interest_over_time)



print(head(combined_interest))
