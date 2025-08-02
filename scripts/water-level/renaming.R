rm(list=ls())

library(tidyverse)

main_dir <- "/Users/dhardy/Library/CloudStorage/Dropbox/Sapelo_NSF/water_level_survey/data/hobo-data-by-site/test"

# sub_dirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

## append subfolder names to files in subfolders
# for (folder_path in sub_dirs) {
#   # Extract the folder name
#   folder_name <- basename(folder_path)
#   
#   # List files within the current folder
#   files_in_folder <- list.files(folder_path, full.names = TRUE)
#   
#   # Loop through files and rename them
#   for (old_file_path in files_in_folder) {
#     # Extract file name and extension
#     old_file_name_ext <- basename(old_file_path)
#     file_extension <- tools::file_ext(old_file_name_ext)
#     file_name_without_ext <- tools::file_path_sans_ext(old_file_name_ext)
#     
#     # Construct the new file name using the folder name
#     new_file_name <- paste0(folder_name, "_", file_name_without_ext, ".", file_extension)
#     new_file_path <- file.path(folder_path, new_file_name)
#     
#     # Rename the file
#     file.rename(from = old_file_path, to = new_file_path)
#   }
# }

## rename files with new site name
# List files within the current folder
files_in_folder <- list.files(main_dir, full.names = TRUE) 

# Rename the file
file.rename(from = old_file_name, to = new_file_name)
