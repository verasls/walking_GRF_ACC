source("analysis.R")

# Prepare data frame ------------------------------------------------------

obese_data <- LOOCV_hip_vert_LMM %>% 
  select(ID, speed, body_mass, BMI, pVACC_g, pVGRF_N, pVGRF_N_predicted) %>% 
  filter(BMI >= 30) # filters for class II-II obesity

non_obese_data <- LOOCV_hip_vert_LMM %>% 
  select(ID, speed, body_mass, BMI, pVACC_g, pVGRF_N, pVGRF_N_predicted) %>% 
  filter(BMI < 30) # filters for normal weight and overweight

# Apply Neugebauer 2014 equation ------------------------------------------

obese_data$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(obese_data)) {
  obese_data$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * obese_data$pVACC_g[i]) + (0.014 * obese_data$body_mass[i]))
}

non_obese_data$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(non_obese_data)) {
  non_obese_data$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * non_obese_data$pVACC_g[i]) + (0.014 * non_obese_data$body_mass[i]))
}

# Bland-Altman plots ------------------------------------------------------

obese_our_BA_plot  <- get_BA_plot(obese_data, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_BA_plot <-  get_BA_plot(obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

non_obese_our_BA_plot  <- get_BA_plot(non_obese_data, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_BA_plot <-  get_BA_plot(non_obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

# Indices of accuracy -----------------------------------------------------

obese_our_accuracy  <- accuracy_indices(obese_data, "pVGRF_N", "pVGRF_N_predicted")

obese_Neug_accuracy <- accuracy_indices(obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")

non_obese_our_accuracy  <- accuracy_indices(non_obese_data, "pVGRF_N", "pVGRF_N_predicted")

non_obese_Neug_accuracy <- accuracy_indices(non_obese_data, "pVGRF_N", "pVGRF_N_Neugebauer")