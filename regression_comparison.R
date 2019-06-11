# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
source(here("R", "get_BA_plot.R"))
source(here("R", "accuracy_indices.R"))

# Prepare data frame ------------------------------------------------------

non_obese_df <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv") %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, pVACC_g, pVGRF_N, pVGRF_N_predicted) %>% 
  filter(BMI < 30) # filters for normal weight and overweight

obese_df <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv") %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, pVACC_g, pVGRF_N, pVGRF_N_predicted) %>% 
  filter(BMI >= 30) # filters for class II-II obesity

whole_sample_df <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv") %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, pVACC_g, pVGRF_N, pVGRF_N_predicted)

# Apply Neugebauer 2014 equation ------------------------------------------

non_obese_df$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(non_obese_df)) {
  non_obese_df$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * non_obese_df$pVACC_g[i]) + (0.014 * non_obese_df$body_mass[i]))
}

obese_df$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(obese_df)) {
  obese_df$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * obese_df$pVACC_g[i]) + (0.014 * obese_df$body_mass[i]))
}

whole_sample_df$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(whole_sample_df)) {
  whole_sample_df$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * whole_sample_df$pVACC_g[i]) + (0.014 * whole_sample_df$body_mass[i]))
}

# Bland-Altman plots ------------------------------------------------------

non_obese_our_BA_plot  <- get_BA_plot(non_obese_df, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_BA_plot <-  get_BA_plot(non_obese_df, "pVGRF_N", "pVGRF_N_Neugebauer")

obese_our_BA_plot  <- get_BA_plot(obese_df, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_BA_plot <-  get_BA_plot(obese_df, "pVGRF_N", "pVGRF_N_Neugebauer")

whole_sample_our_BA_plot  <- get_BA_plot(whole_sample_df, "pVGRF_N", "pVGRF_N_predicted")

whole_sample_Neug_BA_plot <-  get_BA_plot(whole_sample_df, "pVGRF_N", "pVGRF_N_Neugebauer")

# Check whether bias is statisticaly different than 0
# For non-obese sample
# Our equation
non_obese_df$our_diff <- non_obese_df$pVGRF_N - non_obese_df$pVGRF_N_predicted
non_obese_df$our_mean <- (non_obese_df$pVGRF_N + non_obese_df$pVGRF_N_predicted) / 2
t.test(non_obese_df$our_diff, mu = 0)
# Neugebauer's equation
non_obese_df$Neug_diff <- non_obese_df$pVGRF_N - non_obese_df$pVGRF_N_Neugebauer
non_obese_df$Neug_mean <- (non_obese_df$pVGRF_N + non_obese_df$pVGRF_N_Neugebauer) / 2
t.test(non_obese_df$Neug_diff, mu = 0)

# For obese sample
# Our equation
obese_df$our_diff <- obese_df$pVGRF_N - obese_df$pVGRF_N_predicted
obese_df$our_mean <- (obese_df$pVGRF_N + obese_df$pVGRF_N_predicted) / 2
t.test(obese_df$our_diff, mu = 0)
# Neugebauer's equation
obese_df$Neug_diff <- obese_df$pVGRF_N - obese_df$pVGRF_N_Neugebauer
obese_df$Neug_mean <- (obese_df$pVGRF_N + obese_df$pVGRF_N_Neugebauer) / 2
t.test(obese_df$Neug_diff, mu = 0)

# For whole sample
# Our equation
whole_sample_df$our_diff <- whole_sample_df$pVGRF_N - whole_sample_df$pVGRF_N_predicted
whole_sample_df$our_mean <- (whole_sample_df$pVGRF_N + whole_sample_df$pVGRF_N_predicted) / 2
t.test(whole_sample_df$our_diff, mu = 0)
# Neugebauer's equation
whole_sample_df$Neug_diff <- whole_sample_df$pVGRF_N - whole_sample_df$pVGRF_N_Neugebauer
whole_sample_df$Neug_mean <- (whole_sample_df$pVGRF_N + whole_sample_df$pVGRF_N_Neugebauer) / 2
t.test(whole_sample_df$Neug_diff, mu = 0)

### Linear regressions to identify proportional bias
# For non-obese sample
# Our equation
non_obese_our_BA_plot_LR <- lm(our_diff ~ our_mean, data = non_obese_df)
summary(non_obese_our_BA_plot_LR)
# Neugebauer's equation
non_obese_Neug_BA_plot_LR <- lm(Neug_diff ~ Neug_mean, data = non_obese_df)
summary(non_obese_Neug_BA_plot_LR)

# For obese sample
# Our equation
obese_our_BA_plot_LR <- lm(our_diff ~ our_mean, data = obese_df)
summary(obese_our_BA_plot_LR)
# Neugebauer's equation
obese_Neug_BA_plot_LR <- lm(Neug_diff ~ Neug_mean, data = obese_df)
summary(obese_Neug_BA_plot_LR)

# For whole sample
# Our equation
whole_sample_our_BA_plot_LR <- lm(our_diff ~ our_mean, data = whole_sample_df)
summary(whole_sample_our_BA_plot_LR)
# Neugebauer's equation
whole_sample_Neug_BA_plot_LR <- lm(Neug_diff ~ Neug_mean, data = whole_sample_df)
summary(whole_sample_Neug_BA_plot_LR)

# Indices of accuracy -----------------------------------------------------

non_obese_our_accuracy  <- accuracy_indices(non_obese_df, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_accuracy <- accuracy_indices(non_obese_df, "pVGRF_N", "pVGRF_N_Neugebauer")

obese_our_accuracy  <- accuracy_indices(obese_df, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_accuracy <- accuracy_indices(obese_df, "pVGRF_N", "pVGRF_N_Neugebauer")

whole_sample_our_accuracy  <- accuracy_indices(whole_sample_df, "pVGRF_N", "pVGRF_N_predicted")

whole_sample_Neug_accuracy <- accuracy_indices(whole_sample_df, "pVGRF_N", "pVGRF_N_Neugebauer")