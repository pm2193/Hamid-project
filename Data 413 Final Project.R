library(gtrendsR)
# Define the keyword (soccer) and Google properties (e.g., web, news, images, youtube)
keywords <- c("Israel genocide", "Israel terrorism")
gprop <- c("web")

# Set other parameters
geo <- "US"  # All regions
time <- "today 12-m"  # Last five years
hl <- "en-US"  # Language
tz <- 0  # Timezone

# Perform the query
get_trends <- function(keyword) {
  trends <- gtrends(keyword = keyword,
                    geo = geo,
                    time = time,
                    gprop = gprop,
                    hl = hl,
                    tz = tz)
  return(trends)
}

# View the results
trends_data <- lapply(keywords, get_trends)


interest_over_time <- lapply(trends_data, function(trends) trends$interest_over_time)



combined_interest <- do.call(rbind, interest_over_time)



print(head(combined_interest))
