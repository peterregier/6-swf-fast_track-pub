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


# Model inputs
my_data_selection = 9
my_download_url <- download_table$links[my_data_selection]

mod_inp <- tempfile(pattern = 'model_inputs',
                    tmpdir = tempdir(),
                    fileext = '.zip')
download.file(url = my_download_url, 
              mod_inp,
              timeout = max(800, getOption("timeout")))

# Get list of files in zipped file
input_list <- unzip(mod_inp, list = TRUE)$Name

#Extracting specific shapefiles for YRB and WRB to use COMIDs to filter remaining
#files pertaining to the entire Columbia River Basin (CRB)

#Creating a folder within our raw data folder to store shape files for the Yakima 
#River Basin (YRB) and Willamette River Basin (WRB)

dir.create(paste(raw_data,"shape_files",sep = "/"))

extract_shapes <- c( "model_inputs/nhd_CR_stream_sub8.dbf",
                     "model_inputs/nhd_CR_stream_sub8.prj",
                     "model_inputs/nhd_CR_stream_sub8.shp",
                     "model_inputs/nhd_CR_stream_sub8.shx",
                     "model_inputs/nhd_CR_stream_sub9.dbf",
                     "model_inputs/nhd_CR_stream_sub9.prj",      
                     "model_inputs/nhd_CR_stream_sub9.shp" ,
                     "model_inputs/nhd_CR_stream_sub9.shx")

unzip(mod_inp, files = extract_shapes, 
      exdir = file.path(raw_data,"shape_files"))

# Extract specific files from zipped file to temporary directory

extract_files <- c("model_inputs/nexss_inputs.csv", 
                   "model_inputs/nhd_CR_stream_annual_DOC.csv",
                   "model_inputs/nhd_CR_stream_annual_DO.csv",
                   "model_inputs/nhd_CR_stream_no3.csv",
                   "model_inputs/nhd_CR_stream_nlcd_2001.csv" ,
                   "model_inputs/nhd_CR_stream_orders.csv")

temp_dir <- tempdir()

# Loop over extract_files vector and extract each file to temp_dir
for (file in extract_files) {
  file_path <- file.path(temp_dir, file)
  unzip(mod_inp, files = file, exdir = temp_dir)
}

# create a list to store the data frames
data_list <- list()

# loop through the files and read them into R
for (file in extract_files) {
  file_path <- file.path(temp_dir, file)
  data <- read.csv(file_path, header = TRUE)
  data_list[[file]] <- data
}

# access the data frames using their file names as keys in the list
ordr_dat <- data_list[["model_inputs/nhd_CR_stream_orders.csv"]]
nlcd_dat <- data_list[["model_inputs/nhd_CR_stream_nlcd_2001.csv"]]
nexs_dat <- data_list[["model_inputs/nexss_inputs.csv"]]
pdoc_dat <- data_list[["model_inputs/nhd_CR_stream_annual_DOC.csv"]]
pdox_dat <- data_list[["model_inputs/nhd_CR_stream_annual_DO.csv"]]
pno3_dat <- data_list[["model_inputs/nhd_CR_stream_no3.csv"]]

# Merging all data sets into a single one

# create a list of the datasets to merge
datasets <- list(ordr_dat, nlcd_dat, nexs_dat, pdoc_dat, pdox_dat, pno3_dat)

# Define the new column name
new_col_name <- "comid"

datasets <- lapply(seq_along(datasets), function(i) {
  colnames(datasets[[i]])[1] <- new_col_name
  datasets[[i]]
})

list2env(setNames(datasets, c("ordr_dat", "nlcd_dat", "nexs_dat", "pdoc_dat", "pdox_dat", "pno3_dat")), envir = .GlobalEnv)

# merge the datasets by comid
merged_dat <- datasets[[1]]
for (i in 2:length(datasets)) {
  merged_dat <- merge(merged_dat, datasets[[i]], by = "comid", all.x = TRUE)
}









