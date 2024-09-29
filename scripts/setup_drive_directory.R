# Mount Google Drive using Python (run this in an R cell)
system('python3 -c "from google.colab import drive; drive.mount(\'/content/drive\')"')

# Check if the Google Drive is mounted successfully
if (dir.exists("/content/drive/My Drive")) {
  # Define the function as a character string
  function_code <- "
  setup_drive_directory <- function(email, main_folder = 'GEOG279_F24', sub_folder = 'assignment1') {
    # Set gargle option to automatically cache OAuth credentials
    options(gargle_oauth_cache = TRUE)
    
    # Authenticate Google Drive with the provided email
    drive_auth(email = email)
    
    # Create the main folder in Google Drive (if it doesn't already exist)
    drive_mkdir(main_folder)
    
    # Create the sub-folder within the main folder
    drive_mkdir(file.path(main_folder, sub_folder))
    
    # Construct the path to the sub-folder
    drive_path <- file.path('/content/drive/My Drive', main_folder, sub_folder)
    
    # Create the directory if it does not exist
    if (!dir.exists(drive_path)) {
      dir.create(drive_path, recursive = TRUE)
    }
    
    # Set the working directory
    setwd(drive_path)
    
    # Confirm the working directory
    message('Working directory set to: ', getwd())
  }
  "
  
  # Path to save the R script
  script_path <- "/content/drive/My Drive/setup_drive_directory.R"
  
  # Save the function to an R script file in your Google Drive
  writeLines(function_code, script_path)
  
  message("Script saved to: ", script_path)
} else {
  message("Google Drive is not mounted. Please check the mounting process.")
}