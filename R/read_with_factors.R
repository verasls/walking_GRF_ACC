read_with_factors <- function(file) {
  # Read a csv file using the read_csv() function from readr package with the
  # BMI_cat variable as factor
  #
  # Args:
  #   file: path to file
  #
  # Returns:
  #   The csv file as a data frame (tibble) with BMI_cat variable as factor with
  #   the correct levels
  
  require(readr)
  
  df <- read_csv(
    file,
    col_types = cols(BMI_cat = col_factor(levels = c(
      "normal weight", "overweight", "class I obesity", "class II obesity", "class III obesity"
    )
    ))
  )
  
  return(df)
}