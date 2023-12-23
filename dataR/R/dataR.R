library(dplyr)
# Check the size of files -------------------------------------------------
#' Get the size of individual files
#'
#' This function takes a vector of file paths and returns a data frame with the
#' filename and size of each file. File sizes can be reported in different units
#' (bytes, kilobytes, megabytes, gigabytes).
#'
#' @param file_paths A vector of file paths.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @return A data frame with filename and size columns.
#' @export
#'
#' @examples
#' \dontrun{
#' file_size(c("file1.txt", "file2.txt"), units = 'kb')
#' }
#'
#' @import dplyr
file_size <- function(file_paths, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- c(bytes = 1, kb = 1024, mb = 1024^2, gb = 1024^3)

  files_to_list <- purrr::map(file_paths,
                              ~data.frame(filename = .x,
                                          size = file.info(.x)$size))
  df <- do.call(rbind, files_to_list) %>%
    mutate(size = size / unit_coefficients[[units]])
  return(df)
}

#' Get the total size of multiple files
#'
#' This function takes a vector of file paths and returns the total size of all
#' files combined. File sizes can be reported in different units
#' (bytes, kilobytes, megabytes, gigabytes).
#'
#' @param file_paths A vector of file paths.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @return The total size of all files combined.
#' @export
#'
#' @examples
#' \dontrun{
#' files_size(c("file1.txt", "file2.txt"), units = 'mb')
#' }
files_size <- function(file_paths, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- c(bytes = 1, kb = 1024, mb = 1024^2, gb = 1024^3)

  size <- sum(file.info(file_paths)$size)

  return(size / unit_coefficients[[units]])
}


# Delete files ------------------------------------------------------------

#' Delete Stata Temporary Files
#'
#' This function deletes temporary Stata files from the temporary directory.
#'
#' The function identifies Stata temporary files based on their naming convention.
#' It then deletes the identified files and prints the number of files deleted
#' along with the cleared disk space.
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' delete_stata_temps()
#' }
#'
#' @seealso [Stata temporary files FAQ](https://www.stata.com/statalist/archive/2004-01/msg00542.html)
delete_stata_temps <- function() {
  # Get the temporary folder
  temp_folder <- paste(
    rev(rev(str_split_1(tempdir(), pattern = '/|\\\\'))[-1L]), collapse = '/')

  # Identify Stata temporary files
  temps <- list.files(temp_folder, pattern = 'ST(D|G|H|i|J|Q|_|W).*tmp', full.names = TRUE)

  # Delete Stata temporary files
  deleted_size <- files_size(temps, units = 'gb')
  file.remove(temps)

  # Print the result
  cat(paste('Deleted', length(temps), 'files clearing', deleted_size, 'GB!\n'))
}

#' Eases the cleaning of folder by creating an excel with  details about
#' the files in the folders that you want to clean. Once create the user must
#' change/delete the zeros in the delete column for the files that must be
#' deleted.
#'
#' This function takes a vector of folder paths, retrieves details about files
#' in those folders, and writes the information to an Excel file.
#' The details include file names, last update timestamps, file sizes,
#' proportional sizes to the folder where it is stored, and add a column for potential deletion.
#'
#' @param folders A vector of folder paths.
#' @param file_path The path where the Excel file will be saved.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' folder_details(c("/path/to/folder1", "/path/to/folder2"),
#'                 file_path = "details.xlsx", units = 'kb')
#' }
folder_details <- function(folders, file_path = NULL, units = 'bytes') {
  unit_coefficients <- 1024 ^ c(bytes = 0, kb = 1, mb = 2, gb = 3)
  units <- tolower(units)

  if (!units %in% names(unit_coefficients) ) {
    stop('Invalid value for units. Valid values: "bytes", "kb", "mb", "gb"')
  }
  if (is.null(file_path)) {
    file_path <- sprintf('%s/files.xlsx')
  }
  df <- NULL
  for (folder in folders) {
    files <- list.files(folder, recursive = T)
    full_files <- list.files(folder,
                             recursive = T, full.names = T)

    df0 <- dplyr::tibble(
      folder, files, last_update = file.info(full_files)$mtime,
      size = file.info(full_files)$size / unit_coefficients[[units]]) %>%
      arrange(desc(size)) %>%
      mutate(prop_size = size / sum(size, na.rm = T), delete = 0)
    df <- rbind(df, df0)
  }
  names(df)[3L] <- sprintf('%s_%s', names(df)[3L], units)
  writexl::write_xlsx(df, file_path)
}

#' Delete files based on information stored in an Excel file created by the
#' function `dataR::folder_details()`
#'
#' This function reads an Excel file containing details about files in folders,
#' filters files based on the "delete" column, and deletes the selected files.
#' The "delete" column is expected to contain binary values (0 or 1) indicating
#' whether the corresponding file should be deleted.
#'
#' @param file_path The path to the Excel file containing file details.
#' @param save The value in the delete column that avoid the deletion of the file.
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' delete_files_from_excel("details.xlsx", save = 0)
#' }
delete_files_from_excel <- function(file_path, save = 0) {
  df <- read_excel(file_path) %>% filter(delete != save) %>%
    mutate(full_file_paths = paste(folder, files, sep = '/'))

  file.remove(df$full_file_paths)
}
