# -----------------------------------------------------
# PART 1
# This script is for downloading data from the workshop.
# 0) Good practices, clean variables and libraries
# 1) Web scraping to access the page
# 2) Save the data in a structure
# 3) Export the data to a .csv file
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Good practices, clean variables and libraries
# -----------------------------------------------------

# Clean environment and libraries
rm(list = ls())

require(pacman)
p_load(rvest, dplyr, tidyr, readr, httr, jsonlite)

# Access the page
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
pagina <- read_html(url)

# Extract links within the <ul> at the end of the page (relative)
links <- pagina %>%
  html_nodes("ul li a") %>%
  html_attr("href")

# Convert relative links to absolute 
links <- ifelse(grepl("^http", links), links, paste0(url, links))

# Create empty final dataset
base_final <- data.frame()



subpag <- links[2]
response <- GET(subpag)
json_data <- content(response, as = "text", encoding = "UTF-8")
standings <- fromJSON(json_data)

standings_df <- as.data.frame(standings)




subpag <- html_node(subpag, "table.table") %>%
  html_table(fill=TRUE)



# Loop through each link and extract data
for (link in links) {
  # Read the page of each link
  subpagina <- read_html(link)

  # Extract the table (adjust the selector according to the structure of each page)
  tabla <- subpagina %>%
    html_node("container-fluid") %>%
    html_table()

  # Add the data to the final dataset
  base_final <- bind_rows(base_final, tabla)
}

# ...existing code...
