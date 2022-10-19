# Function for importing solute units template

#'
#' @param template_name the name of the file
#' @param folder_name the name of the folder the template is in 
#' @return new_template
#'
#' @examples
#' \dontrun{
#' # Generate solute units template
#' import_solute_units_template(folder_name = "metajam_example", template_name = "Stream_Data_Template.xlsx")
#' }

import_solute_units_template <- function(folder_name, template_name) {
  
  new_template_name <- readxl::read_excel(here::here(folder_name, template_name), sheet = "Solute Units")
  
  solute_units_Template <- as.data.frame(dplyr::select(new_template_name, Measurement, Unit))
  
  return(solute_units_Template)
}

# Function to pull river data from EDI portals

#'
#' @param my_data_url (link) EDI link to dataset
#' @param desired_path_to_data where would you like the data to go
#'
#' @return mydata$data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull EDI data from a portal 
#'data_from_EDI <- function(my_data_url,
#'                          desired_path_to_data
#'                        )
#'                   
#'                    }
data_from_EDI <- function(my_data_url,
                          desired_path_to_data
                        ) {
  
  dir.create(desired_path_to_data, showWarnings = FALSE)
  
  downloaded_data <- metajam::download_d1_data(data_url = my_data_url, path = desired_path_to_data)
  
  my_data <- metajam::read_d1_files(downloaded_data, na = c("u", "NP", "DNS", "EQCL", "-888.88", "-888", "-999", "-888.888", "-888.8888"))

  new_df <- my_data$data
  
  return(new_df)
}

# Function to clean up river data pulled from EDI portal

#'
#' @param my_dataframe name of the df that needs cleaning. Should be equal to my_data$data
#'
#' @return cleaned up df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull EDI data from a portal 
#'EDI_river_data_cleanup <- function(my_dataframe)
#'                   
#'                    }
EDI_river_data_cleanup  <- function(my_dataframe
) {
  
  clean_df <- tibble::add_column(my_dataframe, 'Site/Stream Name' = 'knb-lter-hbr.4.15', .before = "Year")
  
  clean_df <- tibble::add_column(clean_df, LTER = 'HBR', .before = "Year")
  
  clean_df <- dplyr::rename(clean_df, 'Sampling Date' = 'Year_Month')
  
  names(clean_df)[7:31] <- substring(names(clean_df)[7:31],7,15)
  
  dplyr::rename(clean_df, 'Spec Cond' = 'SpecCond', 'Si' = 'SiO2')
  
  return(clean_df)
}

# Function to read in river data template

#'
#' @param river_data_folder the folder where the river data template excel workbook is
#' @param river_data_template the name of the excel workbook that you want to use as a template
#' @return river_template
#'
#' @examples
#' \dontrun{
#' # Generate river data template
#' import_river_data_template(river_data_folder = "metajam_example", river_data_template = "Raw stream data template.xlsx")
#' }

import_river_data_template <- function(river_data_folder, river_data_template) {
  
  river_template <- readxl::read_excel(here::here(river_data_folder,river_data_template), col_types = "text") 
  
  dplyr::mutate(river_template, `Sampling Date` = as.Date(`Sampling Date`))

  return(river_template)
}

# Function to make a table that matches the template
#' @param correct_names a string of the correct names
#' @param data_name the dataframe you want to match the river_template
#' @return fuzzy match
#'
#' @examples
#' \dontrun{
#' # Make a table that matches the template
#' match_river_data_template( river_data_template = "Raw stream data template.xlsx")
#' }

match_river_data_template <- function(river_data_folder, river_data_template) {
filter(attributeName %in% c(correct_names))

solute_units_table_new  %>% 
  rename(Measurement = attributeName, Unit = unit) 

}
