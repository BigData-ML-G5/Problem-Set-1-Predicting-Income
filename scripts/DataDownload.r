# -----------------------------------------------------
# PART 1
# This script is for downloading data from the workshop.
# 0) Clean variables and Libraries
# 1) Web scraping to access the page
# 2) Save the data in a structure
# 3) Clean data and check variables
# 4) Export the data to a .csv file
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Good practices, clean variables and libraries
# -----------------------------------------------------

# Clean variables and Libraries
rm(list = ls())

require(pacman)
p_load(rvest, dplyr, tidyr, readr, httr, jsonlite, boot)

# -----------------------------------------------------
# 1) Web scraping to access the page
# -----------------------------------------------------



# Access the page
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
pagina <- read_html(url)

# Extract links within the <ul> at the end of the page (relative)
links <- pagina %>%
  html_nodes("ul li a") %>%
  html_attr("href")

# Convert relative links to absolute 
links <- ifelse(grepl("^http", links), links, paste0(url, links))

# -----------------------------------------------------
# 2) Save the data in a structure
# -----------------------------------------------------
# While doing the web scrapping, we found the table in each link is dinamic,
# thus, checking into the structure of the html, we found it creates the table
# with the data through a request to another we page. Therefore we will 
# directly extract the data from this original web

# Create empty final dataset
db <- data.frame()

link_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
link_end <- ".html"
numbers <- c(1:10)

upload_data <- function(numero) {
  # Build the URL
  test <- paste0(link_base, numero, link_end)
  # Read the subpage
  subpage_link <- read_html(test)
  # Extract the table
  table <- subpage_link %>%
    html_node("table") %>%
    html_table()
  # Add the data into final db
  db <<- rbind(db, table)
}

lapply(numbers, upload_data)


# -----------------------------------------------------
# 3) Clean data and check variables
# -----------------------------------------------------





# -----------------------------------------------------
# 4) Export the data to a .csv file
# -----------------------------------------------------






