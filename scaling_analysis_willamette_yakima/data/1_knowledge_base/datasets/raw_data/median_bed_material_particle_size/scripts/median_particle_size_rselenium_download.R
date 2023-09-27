###############################################################################
# Downloading A national map of riverine median bed-material particle size over CONUS
# Li et al., 2023 
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_RSelenium.R"

# Local Import-Export

raw_data <- "raw_data"
processed_data <- "processed_data"

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
                 foreign,
                 sf)

# Reference comid's
comid_reference <- read_csv("../enhanced_nhdplus_21/raw_data/reference_comids_tocomids.csv",
                            show_col_types = FALSE) 

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
target_url <- "https://zenodo.org/record/4921987"

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

css_selector <- "#collapseTwo > table"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = css_selector)$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) 

colnames(files_table) <- c("dataset",
                           "size",
                           "links")


files_table <- files_table %>% 
  mutate(file_name = sub("\\s.*", "", .$dataset),
         # file_extension = sub("^.*\\.", "", file_name),
         child = rownames(.),
         size_unit = sub("[^[:alpha:]]+", "", size),
         size_MB = if_else(size_unit=="KB",parse_number(size)/1000,parse_number(size)))


my_selection <- c(2:3)

my_files_table <- files_table[my_selection,]

# table selector
table_selector <- "#collapseTwo > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (my_row in 1:length(new_table_rows)) {
  row <- new_table_rows[[my_row]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#collapseTwo > table > tbody > tr:nth-child(", my_files_table$child[my_row], ") > td:nth-child(3) > span > a")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

################################################################################
# Extracting Files
################################################################################
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
  
  for (i in seq_along(downloaded_files)) {
    file_path <- downloaded_files[i]
    
    # Check if the file is a zip file
    is_zip <- tools::file_ext(file_path) == "zip"
    
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    if (is_zip) {
      tryCatch({
        # Create a new directory with the file name
        new_dir <- file.path(temp_dir, file_name)
        dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
        
        # Unzip the file into the new directory
        unzip(file_path, exdir = new_dir)
        
        # Get the extracted file paths
        extracted_files_list <- list.files(path = new_dir, full.names = TRUE, recursive = TRUE)
        
        # Print the file names within the folder
        cat("Files in", file_name, "folder:\n")
        cat(paste0(extracted_files_list, "\n"), sep = "")
        cat("\n")
        
        # Move the zip file to the temporary directory
        file.rename(file_path, file.path(temp_dir, basename(file_path)))
        
        # Store the extracted file paths with their original extensions
        extracted_files[[i]] <- extracted_files_list
      }, error = function(e) {
        cat("Failed to unzip", file_name, "\n")
        cat("File:", file_path, "\n")
        message(e)
      })
    } else {
      # Move the non-zip file to the temporary directory
      file.rename(file_path, file.path(temp_dir, basename(file_path)))
      
      # Add the file path to the list of non-zip files
      extracted_files[[i]] <- file.path(temp_dir, basename(file_path))
    }
    
    # Print the file path
    cat("File:", file.path(temp_dir, basename(file_path)), "\n")
  }
  
  list(extracted_files = unlist(extracted_files), temp_dir = temp_dir)
}

# Access the extracted file paths using result$extracted_files
result <- retrieve_data(paste(downloads_folder))
extracted_files <- result$extracted_files
extracted_files

################################################################################
# Processing & Saving Files

process_files <- function(file_paths, temp_dir) {
  destination_folder1 <- "metadata"
  destination_folder2 <- "raw_data"
  
  data_list <- list()  # List to store individual txt data
  
  for (file_path in file_paths) {
    extension <- tools::file_ext(file_path)
    
    if (extension == "png") {
      new_file_path <- file.path(destination_folder1, basename(file_path))
      file.rename(file_path, new_file_path)
    } else {
      # Read the dbf files
      basin_data <- read.dbf(file_path)
      
      # Perform any necessary data processing/manipulation
      basin_data <- basin_data %>% 
        rename(comid = COMID,
               stream_order = StreamOrde,
               wshd_area_km2 = TotDASqKM,
               d50_mm = D50.mm.)
      
      pnw_basin_data <- merge(comid_reference,
                              basin_data,
                              by = "comid",
                              all.x = TRUE) 
      project_name <- basename(getwd())
      csv_file_path <- file.path(destination_folder2, paste0(project_name, ".csv"))
      write.csv(pnw_basin_data, file = csv_file_path, row.names = FALSE)
    }
  }
}

temp_dir <- tempfile()

outputs <- process_files(extracted_files,temp_dir)