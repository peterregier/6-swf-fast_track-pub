###############################################################################
# Downloading National Stream Internet Hydrography Network for SSN Analysis
# Nagel et al., 2017
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_RSelenium.R"

# Local Import-Export

raw_data <- "./raw_data"
processed_data <- "./processed_data"


# Loading/installing required libraries
librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman,
                 rvest,
                 data.table,
                 utils, 
                 readr,
                 xml2, 
                 methods,
                 R.utils,
                 sp,
                 sf,
                 leaflet)


text = html_text(html_nodes(site, 'p,h1,h2,h3')) # comma separate

# Set path to downloads folder
downloads_folder <- if (Sys.getenv("OS") == "Windows_NT") {
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads")
} else {
  file.path(Sys.getenv("HOME"), "Downloads")
}

# Opening a Selenium client-server object with specific download preferences
# Set the download preferences (to allow multiple file downloads without pop ups)
chrome_options <- list(
  chromeOptions = list(
    prefs = list(
      "download.default_directory" = "~/Downloads",
      "download.prompt_for_download" = FALSE,
      "download.directory_upgrade" = TRUE,
      "download.overwrite" = TRUE,
      "profile.default_content_settings.popups" = 0,
      "profile.content_settings.exceptions.automatic_downloads.*.setting" = 1,
      "safebrowsing.enabled" = TRUE
    )
  )
)

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)


# Open a client browser for webscrapping
remDr <- rs_driver_object$client

# To start downloading data, we first need to specify the url we want to navigate to
target_url <- "https://www.fs.usda.gov/rm/boise/AWAE/projects/NationalStreamInternet/NSI_network.html"

# Navigate to your target url
remDr$navigate(target_url)

# Wait for the page to load
Sys.sleep(5) 

# Saving the html as a plain text file in the metadata folder
site = read_html(target_url)
text_content = html_text(html_nodes(site, 'p,h1,h2,h3')) # comma separate

# Save the text content as a text file
txt_file_path <- file.path(getwd(), "metadata", "target_page.txt")
writeLines(text_content, txt_file_path)

# Explore the page and find css selector

css_selector <- "#Content-outer > div > div.NorWeST_1 > table:nth-child(2) > tbody > tr:nth-child(2) > td > table"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = css_selector)$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) %>% 
  select(-Var.3)

# Splitting the second column into two columns
split_column <- strsplit(files_table$Stream.Line.and.Prediction.Point.Shapefiles..zip.format., "\n")

# Creating two new columns
files_table$streamline_Shapefile <- sapply(split_column, function(x) x[1])
files_table$predictpoints_Shapefile <- sapply(split_column, function(x) x[2])

# Dropping the original second column
files_table$Stream.Line.and.Prediction.Point.Shapefiles..zip.format. <- NULL

# Selecting the desired processing unit (NHD Plus V2: from 01-Northeast to 18-California)
# according to their position on files_table

my_selection <- c(20)  # Example vector of selections

my_files_table <- files_table[my_selection,]

# Table selector
table_selector <- "#Content-outer > div > div.NorWeST_1 > table:nth-child(2) > tbody > tr:nth-child(2) > td > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)


for (my_row in seq_along(my_selection)) {
  selection <- my_selection[my_row]
  row <- table_rows[[selection]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#Content-outer > div > div.NorWeST_1 > table:nth-child(2) > tbody > tr:nth-child(2) > td > table > tbody > tr:nth-child(", selection + 1, ") > td:nth-child(2) > p:nth-child(1) > a")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

retrieve_data <- function(downloads_folder) {
  # Get the current time
  current_time <- Sys.time()
  
  # Calculate the time threshold for file selection (2 hours ago)
  time_threshold <- current_time - 60 * 60 * 2  # 60 seconds * 60 minutes * 2 = 2 hours
  
  # Retrieve the zip files downloaded within the last 2 hours
  downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, pattern = ".zip$", recursive = TRUE)
  recent_files <- file.info(downloaded_files)$mtime > time_threshold
  
  # Filter the downloaded files to keep only the recent ones
  downloaded_files <- downloaded_files[recent_files]
  
  temp_dir <- tempdir()
  
  extracted_files <- vector("list", length(downloaded_files))
  
  for (i in seq_along(downloaded_files)) {
    file_path <- downloaded_files[i]
    
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Create a new directory with the file name
    new_dir <- file.path(temp_dir, file_name)
    dir.create(new_dir)
    
    # Unzip the file into the new directory
    unzip(file_path, exdir = new_dir)
    
    # Get the extracted file paths
    extracted_files[[i]] <- list.files(path = new_dir, full.names = TRUE)
    
    # Move the file to the temporary directory
    file.rename(file_path, file.path(temp_dir, basename(file_path)))
    
    # Print the file names within the folder
    cat("Files in", file_name, "folder:\n")
    cat(paste0(extracted_files[[i]], "\n"), sep = "")
    cat("\n")
  }
  
  return(extracted_files)
}

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

data <- retrieve_data(paste(downloads_folder))

pnw_shapefiles <- st_read(paste(data[[1]][4]))

pnw_data <- pnw_shapefiles %>% 
  mutate(HUC4 = substr(REACHCODE,1,4)) %>% 
  filter(HUC4 == "1703" | HUC4 == "1709")
  
st_write(pnw_data,paste(raw_data,"nsi_network_ywrb.shp",sep = '/'),
         append = FALSE)



