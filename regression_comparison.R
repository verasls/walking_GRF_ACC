source("analysis.R")

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

non_obese_our_BA_plot  <- get_BA_plot(non_obese_data, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_BA_plot <-  get_BA_plot(non_obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

obese_our_BA_plot  <- get_BA_plot(obese_data, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_BA_plot <-  get_BA_plot(obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

whole_sample_our_BA_plot  <- get_BA_plot(whole_sample_data, "pVGRF_N", "pVGRF_N_predicted")

whole_sample_Neug_BA_plot <-  get_BA_plot(whole_sample_data, "pVGRF_N", "pVGRF_N_Neugebauer")

# Indices of accuracy -----------------------------------------------------

non_obese_our_accuracy  <- accuracy_indices(non_obese_data, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_accuracy <- accuracy_indices(non_obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

obese_our_accuracy  <- accuracy_indices(obese_data, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_accuracy <- accuracy_indices(obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

whole_sample_our_accuracy  <- accuracy_indices(whole_sample_data, "pVGRF_N", "pVGRF_N_predicted")

whole_sample_Neug_accuracy <- accuracy_indices(whole_sample_data, "pVGRF_N", "pVGRF_N_Neugebauer")