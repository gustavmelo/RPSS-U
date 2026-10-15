############# Packages ################################################

{
  if (!require("tidyverse")) install.packages("tidyverse", dependencies=TRUE)
  if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
  if (!require("stringi")) install.packages("stringi", dependencies=TRUE)
}

#### Read data ##########################################################

root_dir  <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)][1]), winslash = "/"))
}
ninja_dir <- file.path(root_dir, "00_Data", "Raw", "Wind_Series")
data_dir  <- file.path(root_dir, "00_Data", "Processed")

# List all .xlsx files in the raw data directory
files <- list.files(ninja_dir, pattern = "\\.xlsx$", full.names = TRUE)
file_names <- tools::file_path_sans_ext(basename(files))

# Reads one raw file and extracts the wind speed column
read_xlsx_file <- function(file_path) {
  xlsx_data <- read_excel(file.path(file_path))
  return(xlsx_data[, "Wind_speed"])
}

# Applies the function to every file and builds a list of vectors
data_list <- setNames(lapply(files, read_xlsx_file), file_names)

# Builds a data frame with the date column
start_date <- as.Date("1984-01-01")
end_date <- as.Date("2023-12-01")
df_final <- data.frame(
  date = seq.Date(from = start_date, to = end_date, by = "month")
)

# Extracts year and month from the date column
df_final <- df_final %>%
  mutate(
    year = year(date),
    month = month(date)
  )

data_df <- as.data.frame(data_list)
colnames(data_df) <- names(data_list)

df_final <- cbind(df_final, data_df)

# Drops the date column, keeps year/month
df_final <- df_final %>%
  select(-date)

colnames(df_final) <- colnames(df_final) %>%
  stri_trans_general("Latin-ASCII") %>%  # Removes accents
  tolower() %>%                          # Converts to lowercase
  gsub(" ", "", .)                        # Removes spaces


# Reads the municipality code table and extracts the Code/Municipality columns
codes <- read.csv(file.path(data_dir, "SDG_Criteria.csv"), header = TRUE)
codes <- codes[,c(1,2)]

# Normalizes the municipality names the same way the column names were normalized
codes$Municipality <- codes$Municipality %>%
  stri_trans_general("Latin-ASCII") %>%  # Removes accents
  tolower() %>%                          # Converts to lowercase
  gsub(" ", "", .)                        # Removes spaces


# Builds a Municipality -> Code mapping vector
name_map <- setNames(codes$Code, codes$Municipality)

# Replaces df_final column names (3 to 169) with the municipality code where a match exists
colnames(df_final)[3:169] <- ifelse(
  colnames(df_final)[3:169] %in% names(name_map),
  name_map[colnames(df_final)[3:169]],
  colnames(df_final)[3:169]
)


# Saves the renamed data frame as a CSV
write.csv(df_final, file.path(data_dir, "TimeSeries.csv"), row.names = FALSE)
