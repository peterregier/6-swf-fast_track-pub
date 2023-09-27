###############################################################################
# Data Wrangling Son et al 2022 Spatial Variability Metabolism
###############################################################################

# Local Import-Export

raw_data <- "raw"
processed_data <- "processed"

# Loading/installing required libraries
librarian::shelf(tidyverse,
                 utils)

# Downloading files into a temporary directory

download_table <- read_csv(paste(raw_data,"table_ess_dive_downloads_son_etal_22.csv", sep = '/'),
                           show_col_types = FALSE)

# Download folder
downloads_folder <- if (Sys.getenv("OS") == "Windows_NT") {
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads")
} else {
  file.path(Sys.getenv("HOME"), "Downloads")
}

# mod_inp <- tempfile(pattern = 'model_inputs',
#                     tmpdir = tempdir(),
#                     fileext = '.zip')
my_data_selection = 9
my_download_url = download_table$links[my_data_selection]
file_name <- download_table$name[my_data_selection]
mod_inp <- paste(downloads_folder,file_name,sep = "/")

download.file(url = my_download_url, 
              mod_inp)

# Find the most recent file in the downloads folder
downloaded_files <- list.files(downloads_folder, full.names = TRUE)
most_recent_file <- downloaded_files[which.max(file.info(downloaded_files)$mtime)]

my_data <- unzip("C:/Users/guer310/Downloads/Sinuousity_CONUS.zip" )

zip_command <- paste("unzip", shQuote(most_recent_file), "-d", shQuote(downloads_folder))
system(zip_command)







# Extract the files from the most recent zip file
extracted_files <- unzip(most_recent_file, list = TRUE)
file_names <- extracted_files$Name

# Print the list of file names
print(file_names)

# Getting file names within most_recent_file
input_list <- unzip(most_recent_file, list = TRUE)



download.file(url = my_download_url, 
              mod_inp,
              timeout = max(800, getOption("timeout")))

# Get list of files in zipped file
input_list <- unzip(mod_inp, list = TRUE)$Name



download.file(url = my_download_url, 
              paste(downloads_folder,file,sep = '/'))
















# Model inputs
my_data_selection = 9
my_download_url <- download_table$links[my_data_selection]
file <- download_table$name[my_data_selection]
download.file(url = my_download_url, 
              paste(downloads_folder,file,sep = '/'))



# Set the path to the temporary directory
temp_dir <- tempdir()

# Get the name of the file
file_name <- basename(most_recent_file)

# Create the full path to the file in the temporary directory
temp_file_path <- file.path(temp_dir, file_name)

# Move the file to the temporary directory
file.rename(most_recent_file, temp_file_path)