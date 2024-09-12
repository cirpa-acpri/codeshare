# File Path Function    » Fraser Hay, Conestoga College - fhay{}conestogac.on.ca
# -----------------------------------------------------------------------
# This function is intended to allow smarter file pathing. OneDrive by default installs to your USERPROFILE folder, eg. "C:/Users/Fhay".
# This means hard-coding file paths into R scripts breaks on a per-user basis since each user has different paths to a script.
# fp() resolves this by using your USERPROFILE home path as the basic building block for the path itself. 

# Partial file paths are allowed, but unless you're working in your base USERPROFILE directory, you should specify a 'share.basepath' 
# to your desired (shared) working directory. (eg. {OneDrive Institution}/{Shared Folder})

fp = function(path, share.basepath = NULL) {
  # Locate the current USERPROFILE directory
  user_profile = Sys.getenv("USERPROFILE") |> gsub("\\\\", "/", x = _) 
  if (grepl("^C:/Users/", path)) {
    # If this is a full file path, simply substitute the first three folders for the USERPROFILE.
    sub("^([^/]+/){3}", paste0(user_profile, "/"), path)
  } else {
    # Otherwise, this is a partial file path. Use the 'share.basepath' if available.
    paste0(user_profile, "/", ifelse(is.null(share.basepath), "", paste0(share.basepath, "/")), path)
  }
}

# One-liner: useful for pasting into scrips.
fp = function(path, share.basepath = "Conestoga College/Institutional Research") { user_profile <- gsub("\\\\", "/", Sys.getenv("USERPROFILE")); if (grepl("^C:/Users/", path)) { sub("^([^/]+/){3}", paste0(user_profile, "/"), path) } else { paste0(user_profile, "/", ifelse(is.null(share.basepath), "", paste0(share.basepath, "/")), path) } }

# # Testing
# fp("C:/Users/test/OneDrive Folder/~Scripts/~Python/Python Excel Dataframe Paster.py")
# fp("~Scripts/~Python/Python Excel Dataframe Paster.py", share.basepath = "Work/OneDrive Folder")

