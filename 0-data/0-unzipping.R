# Unzipping contents of the ICPSR data to be useful

# ---- start --------------------------------------------------------------

library("tidyverse")

# Create a directory for the data
local_dir    <- "0-data/ICPSR"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# Extract the file names and only keep the rda
zip_files <- unzip("0-data/raw/ICPSR_35206-V4.zip", list = TRUE)

zip_tsv <- zip_files[grepl(".tsv", zip_files$Name),]

unzip("0-data/raw/ICPSR_35206-V4.zip", files = zip_tsv$Name,
      exdir = data_source, junkpaths = T)

# Codebooks
codebooks <- paste0(local_dir, "/codebooks")
if (!file.exists(codebooks)) dir.create(codebooks, recursive = T)

zip_code <- zip_files[grepl("Codebook", zip_files$Name),]

unzip("0-data/raw/ICPSR_35206-V4.zip", files = zip_code$Name,
      exdir = codebooks, junkpaths = T)
