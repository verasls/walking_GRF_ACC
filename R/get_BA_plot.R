get_BA_plot <- function(df, actual_var, predicted_var) {
  # Builds a Bland-Altman plot using ggplot2 package
  #
  # Args:
  #   df: data frame containing the data used to build the plot
  #   actual_var: dependent variable name (as character)
  #   predicted_var: predicted variable name (as character)
  #
  # Returns:
  #   A Bland-Altman plot (as ggplot object)
  
  require(ggplot2)
  
  df[, ncol(df) + 1] <-  df[, actual_var] - df[, predicted_var]
  df[, ncol(df) + 1] <- (df[, actual_var] + df[, predicted_var]) / 2
  
  names(df)[ncol(df) - 1] <- "diff"
  names(df)[ncol(df)]     <- "mean"
  
  BA_plot <- ggplot(data = df) +
    geom_point(mapping = aes(x = mean, y = diff)) +
    geom_hline(yintercept = mean(df$diff)) +
    geom_hline(
      yintercept = mean(df$diff) + 1.96 * sd(df$diff),
      linetype = "dotted"
    ) +
    geom_hline(
      yintercept = mean(df$diff) - 1.96 * sd(df$diff),
      linetype = "dotted"
    )  
  
  return(BA_plot)
}