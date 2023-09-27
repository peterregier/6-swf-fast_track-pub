###############################################################################
# Downloading data Son et al 2022 Data from ESS-DIVE
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_rselenium.R"

# Local Import-Export

raw_data <- "./raw_data"
processed_data <- "./processed_data"
metadata <- "./metadata"

# Loading/installing required libraries
librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman,
                 rvest,
                 data.table,
                 utils)

# Reference comid's
comid_reference <- read_csv("../enhanced_nhdplus_21/raw_data/230711_reference_comids.csv",
                            show_col_types = FALSE) 

# Set the path to the downloads folder
downloads_folder <- if (Sys.getenv("OS")=="Windows_NT"){
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads")
} else{file.path(Sys.getenv("HOME"), "Downloads")}


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

# Open RSelenium Server
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)


# Open a client browser for webscrapping
remDr <- rs_driver_object$client

################################################################################
# ESS-DIVE MOdel Inputs, Outputs, Scripts - Spatial variation microbial resp.
# Son et al., 2022 (ver. 3.0 Jan-2021)
################################################################################


# To start downloading data, we first need to specify the url we want to navigate to
target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1962818"

# Go to the web page
remDr$navigate(target_url)

# Wait for the page to load
Sys.sleep(5) 


# Find the expand button and click on it (if exists)
expand_button <- tryCatch(
  remDr$findElement(using = "css selector", value = "#table-container > div > table > tfoot"),
  error = function(e) NULL
)

if (!is.null(expand_button)) {
  expand_button$clickElement()
  Sys.sleep(5)
}

# Explore the page and find css selector
css_selector <- "#table-container > div > table"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = css_selector)$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) 

colnames(files_table) <- c("position",
                           "name",
                           "info",
                           "file_type",
                           "size",
                           "n_downloads",
                           "action_download")
files_table <- files_table%>% 
  slice(-1) %>% 
  filter(is.na(name)==FALSE) %>% 
  mutate(child = rownames(.))

# Looking at files table, identify the row(s) that contain the files you want to
# download

my_selection <- c(1:3,5)

my_files_table <- files_table[my_selection,]

# Downloading data
# table selector
table_selector <- "#table-container > div > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (i in 1:length(new_table_rows)){
  
  row <- new_table_rows[[i]]
  
  download_pattern <- my_files_table[i,"file_name"]
  
  download_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(",my_files_table$child[i],") > td.download-btn.btn-container > a")
  
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
  
}

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

retrieve_data <- function(downloads_folder) {
  # Get the current time
  current_time <- Sys.time()
  
  # Calculate the time threshold for file selection (2 hours ago)
  time_threshold <- current_time - 60 * 60 * 2  # 60 seconds * 60 minutes * 2 = 2 hours
  
  # Retrieve all files downloaded within the last 2 hours
  downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, recursive = TRUE)
  recent_files <- file.info(downloaded_files)$mtime > time_threshold
  
  # Filter the downloaded files to keep only the recent ones
  downloaded_files <- downloaded_files[recent_files]
  
  temp_dir <- tempdir()
  
  extracted_files <- vector("list", length(downloaded_files))
  
  all_files <- vector("list", length(downloaded_files))
  
  for (i in seq_along(downloaded_files)) {
    file_path <- downloaded_files[i]
    
    # Check if the file is a zip file
    is_zip <- tools::file_ext(file_path) == "zip"
    
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    if (is_zip) {
      # Create a new directory with the file name
      new_dir <- file.path(temp_dir, file_name)
      dir.create(new_dir)
      
      # Unzip the file into the new directory
      unzip(file_path, exdir = new_dir)
      
      # Get the extracted file paths
      extracted_files[[i]] <- list.files(path = new_dir, full.names = TRUE)
      
      # Move the zip file to the temporary directory
      file.rename(file_path, file.path(temp_dir, basename(file_path)))
      
      # Print the file names within the folder
      cat("Files in", file_name, "folder:\n")
      cat(paste0(extracted_files[[i]], "\n"), sep = "")
      cat("\n")
    } else {
      # Move the non-zip file to the temporary directory
      file.rename(file_path, file.path(temp_dir, basename(file_path)))
    }
    
    # Add the file path to the list of all files
    all_files[[i]] <- file.path(temp_dir, basename(file_path))
    
    # Print the file path
    cat("File:", file.path(temp_dir, basename(file_path)), "\n")
  }
  
  list(all_files = unlist(all_files), extracted_files = unlist(extracted_files))
}

file_paths <- retrieve_data(paste(downloads_folder))

# Saving metadata files

# Define the destination directory
destination_folder <- file.path("metadata")

# Move the CSV files
csv_files <- file_paths$all_files[grep("\\.csv$", file_paths$all_files)]
file.rename(csv_files, file.path(destination_folder, basename(csv_files)))

# Move the XML files
xml_files <- file_paths$all_files[grep("\\.xml$", file_paths$all_files)]
file.rename(xml_files, file.path(destination_folder, basename(xml_files)))

# zip files
unzip_list <- list.files(paste(file_paths[2]), full.names = TRUE)
print(unzip_list)

# Selecting csv files to move to the subfolder model_data

# zip files
unzip_list <- list.files(paste(file_paths[2]), full.names = TRUE)
print(unzip_list)

# Index of the elements to extract
subset_indices <- c(9:16, 53:54)

# Extract subset of elements from the list
csv_extract <- unzip_list[subset_indices]

# Convert subset_elements to a vector
csv_files <- unlist(csv_extract)
print(csv_files)

# Merge with comid_reference and move merged files to raw_data folder
for (i in 1:length(csv_files)) {
  # Read the CSV file
  csv_data <- read.csv(csv_files[i])
  
  # Identify columns containing "comid" (case-insensitive)
  comid_cols <- grep("(?i)comid", colnames(csv_data), perl = TRUE, ignore.case = TRUE)
  
  # Loop through identified columns and rename to "comid"
  for (col in comid_cols) {
    colnames(csv_data)[col] <- "comid"
  }
  
  # Merge with comid_reference by comid (case-insensitive)
  merged_data <- merge(csv_data, comid_reference, by = "comid", ignore.case = TRUE)
  
  # Save the merged data as a new CSV file
  merged_file <- file.path(raw_data, paste0("merged_", basename(csv_files[i])))
  write.csv(merged_data, merged_file, row.names = FALSE)
  
  # Move the merged file to the raw_data folder
  file.rename(merged_file, file.path(raw_data, basename(merged_file)))
}

# Happy coding!

# Reproducibility is the key to open, equitable, and accessible science. By the
# people, for the people!



