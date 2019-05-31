source("analysis.R")

# Prepare data frame ------------------------------------------------------

hip_vert_df <- LOOCV_hip_vert %>% 
  select(ID, speed, body_mass, BMI, pVACC_g, pVGRF_N, pVGRF_N_predicted) %>% 
  filter(BMI >= 35) # filters for class II-II obesity

# Apply Neugebauer 2014 equation ------------------------------------------

hip_vert_df$pVGRF_N_Neugebauer <- NA
for (i in 1:nrow(hip_vert_df)) {
  hip_vert_df$pVGRF_N_Neugebauer[i] <-  
    exp(5.247 + (0.271 * hip_vert_df$pVACC_g[i]) + (0.014 * hip_vert_df$body_mass[i]))
}

# Bland-Altman plots ------------------------------------------------------

our_BA_plot  <- get_BA_plot(hip_vert_df, "pVGRF_N", "pVGRF_N_predicted")

Neug_BA_plot <-  get_BA_plot(hip_vert_df, "pVGRF_N", "pVGRF_N_Neugebauer")

# Indices of accuracy -----------------------------------------------------

our_accuracy  <- accuracy_indices(hip_vert_df, "pVGRF_N", "pVGRF_N_predicted")

Neug_accuracy <- accuracy_indices(hip_vert_df, "pVGRF_N", "pVGRF_N_Neugebauer")