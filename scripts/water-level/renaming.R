rm(list=ls())

library(tidyverse)

main_dir <- "/Users/dhardy/Library/CloudStorage/Dropbox/Sapelo_NSF/water_level_survey/data/hobo-data-by-site/test"

sub_dirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

## append subfolder names to files in subfolders
for (folder_path in sub_dirs) {
  # Extract the folder name
  folder_name <- basename(folder_path)

  # List files within the current folder
  files_in_folder <- list.files(folder_path, full.names = TRUE)

  # Loop through files and rename them
  for (old_file_path in files_in_folder) {
    # Extract file name and extension
    old_file_name_ext <- basename(old_file_path)
    file_extension <- tools::file_ext(old_file_name_ext)
    file_name_without_ext <- tools::file_path_sans_ext(old_file_name_ext)

    # Extract the last four characters of the base name
    # Ensure the base name is long enough to extract 4 characters
    if (nchar(file_name_without_ext) >= 4) {
      new_base_name <- substr(file_name_without_ext, nchar(file_name_without_ext) - 3, nchar(file_name_without_ext))
    } else {
      # If the base name is shorter than 4 characters, use the whole base name
      new_base_name <- file_name_without_ext 
    }
    
    # Construct the new file name
    # new_name <- paste0(new_base_name, ".", extension)
    
    # Construct the new file name using the folder name
    new_file_name <- paste0(new_base_name, "-", folder_name, ".", file_extension)
    new_file_path <- file.path(folder_path, new_file_name)

    # Rename the file
    file.rename(from = old_file_path, to = new_file_path)
  }
}

# Create some dummy files for demonstration
file.create("example_file_1234.txt")
file.create("another_long_name_abcd.csv")

# Get a list of files to rename
old_names <- list.files(pattern = "\\.txt$|\\.csv$")

# Loop through each file to rename
# for (old_name in old_names) {
#   # Get the base name (without extension)
#   base_name <- tools::file_path_sans_ext(old_name)
#   
#   # Get the extension
#   extension <- tools::file_ext(old_name)
#   
#   # Extract the last four characters of the base name
#   # Ensure the base name is long enough to extract 4 characters
#   if (nchar(base_name) >= 4) {
#     new_base_name <- substr(base_name, nchar(base_name) - 3, nchar(base_name))
#   } else {
#     # If the base name is shorter than 4 characters, use the whole base name
#     new_base_name <- base_name 
#   }
#   
#   # Construct the new file name
#   new_name <- paste0(new_base_name, ".", extension)
#   
#   # Rename the file
#   file.rename(old_name, new_name)
#   
#   cat(paste("Renamed '", old_name, "' to '", new_name, "'\n"))
}
