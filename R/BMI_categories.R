BMI_categories <- function(df) {
  # Classify BMI categories
  #
  # Args:
  #   df: data frame containing a column with BMI values
  #
  # Returns:
  #   The data frame with a column containing the BMI classification
  
  df$BMI_cat <- NA
  
  for (i in 1:nrow(df)) {
    if (df$BMI[i] >= 18.5 & df$BMI[i] < 25) {
      df$BMI_cat[i] <- "normal weight"
    } else {
      if (df$BMI[i] >= 25 & df$BMI[i] < 30) {
        df$BMI_cat[i] <- "overweight"
      } else {
        if (df$BMI[i] >= 30 & df$BMI[i] < 35) {
          df$BMI_cat[i] <- "class I obesity"
        } else {
          if (df$BMI[i] >= 35 & df$BMI[i] < 40) {
            df$BMI_cat[i] <- "class II obesity"
          } else {
            if (df$BMI[i] >= 40) {
              df$BMI_cat[i] <- "class III obesity"
            }
          }
        }
      }
    }
  }
  
  df$BMI_cat <- factor(
    df$BMI_cat, 
    levels = c(
      "normal weight", "overweight", "class I obesity", 
      "class II obesity", "class III obesity"
      )
    )
  
  return(df)
}