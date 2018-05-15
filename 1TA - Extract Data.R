library("rvest")

url <- "https://www.tripadvisor.com/Airline_Review-d8729174-Reviews-Turkish-Airlines"
url <- "https://www.tripadvisor.com/Airline_Review-d8729174-Reviews-or20-Turkish-Airlines#REVIEWS"

df_total = data.frame()

for (i in seq(0, 20050, 10))
{
  if (i == 0) {
    url <- "https://www.tripadvisor.com/Airline_Review-d8729069-Reviews-Emirates"
  }
  
  else  {
    url <- paste(
      "https://www.tripadvisor.com/Airline_Review-d8729069-Reviews-or",i,"-Emirates#REVIEWS", 
      sep = "")
  }
  
  reviews <- url %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  rating <- reviews %>%
    html_node(".rating .rating_s_fill") %>%
    html_attr("alt") %>%
    gsub(" of 5 stars", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%b %d, %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  
  df <- data.frame(id, quote, rating, date, review, stringsAsFactors = FALSE)
  df_total <- rbind(df_total, df)
}

# Save an object to a file
saveRDS(df_total, file = "tripadvisor_emirates200500.rds")

trip_turkishairlines <- readRDS(file = "tripadvisor_turkishairlines6846.rds") %>% View()
trip_emirates <- readRDS(file = "tripadvisor_emirates20058.rds")

