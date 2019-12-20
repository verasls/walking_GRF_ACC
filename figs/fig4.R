# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

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

# Non-obese sample --------------------------------------------------------

# Our equation
non_obese_df$our_diff <- non_obese_df$pVGRF_N - non_obese_df$pVGRF_N_predicted
non_obese_df$our_mean <- (non_obese_df$pVGRF_N + non_obese_df$pVGRF_N_predicted) / 2
non_obese_our_BA_plot <- ggplot(data = non_obese_df) +
  geom_point(mapping = aes(x = our_mean, y = our_diff)) +
  geom_hline(yintercept = mean(non_obese_df$our_diff)) +
  geom_hline(
    yintercept = mean(non_obese_df$our_diff) + 1.96 * sd(non_obese_df$our_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(non_obese_df$our_diff) - 1.96 * sd(non_obese_df$our_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-500, 520), expand = c(0, 0),
    breaks = seq(-500, 500, 250)
    ) +
  scale_x_continuous(
    limits = c(0, 3020), expand = c(0, 0),
    breaks = seq(0, 3000, 500)
    ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Normal weight and overweight",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Neugebauer's equation
non_obese_df$Neug_diff <- non_obese_df$pVGRF_N - non_obese_df$pVGRF_N_Neugebauer
non_obese_df$Neug_mean <- (non_obese_df$pVGRF_N + non_obese_df$pVGRF_N_Neugebauer) / 2
non_obese_Neug_BA_plot <- ggplot(data = non_obese_df) +
  geom_point(mapping = aes(x = Neug_mean, y = Neug_diff)) +
  geom_hline(yintercept = mean(non_obese_df$Neug_diff)) +
  geom_hline(
    yintercept = mean(non_obese_df$Neug_diff) + 1.96 * sd(non_obese_df$Neug_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(non_obese_df$Neug_diff) - 1.96 * sd(non_obese_df$Neug_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-500, 520), expand = c(0, 0),
    breaks = seq(-500, 500, 250)
  ) +
  scale_x_continuous(
    limits = c(0, 3020), expand = c(0, 0),
    breaks = seq(0, 3000, 500)
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Normal weight and overweight",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Obese sample ------------------------------------------------------------

# Our equation
obese_df$our_diff <- obese_df$pVGRF_N - obese_df$pVGRF_N_predicted
obese_df$our_mean <- (obese_df$pVGRF_N + obese_df$pVGRF_N_predicted) / 2
obese_our_BA_plot <- ggplot(data = obese_df) +
  geom_point(mapping = aes(x = our_mean, y = our_diff)) +
  geom_hline(yintercept = mean(obese_df$our_diff)) +
  geom_hline(
    yintercept = mean(obese_df$our_diff) + 1.96 * sd(obese_df$our_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(obese_df$our_diff) - 1.96 * sd(obese_df$our_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-500, 520), expand = c(0, 0),
    breaks = seq(-500, 500, 250)
  ) +
  scale_x_continuous(
    limits = c(0, 3020), expand = c(0, 0),
    breaks = seq(0, 3000, 500)
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Class I to III obesity",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Neugebauer's equation
obese_df$Neug_diff <- obese_df$pVGRF_N - obese_df$pVGRF_N_Neugebauer
obese_df$Neug_mean <- (obese_df$pVGRF_N + obese_df$pVGRF_N_Neugebauer) / 2
obese_Neug_BA_plot <- ggplot(data = obese_df) +
  geom_point(mapping = aes(x = Neug_mean, y = Neug_diff)) +
  geom_hline(yintercept = mean(obese_df$Neug_diff)) +
  geom_hline(
    yintercept = mean(obese_df$Neug_diff) + 1.96 * sd(obese_df$Neug_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(obese_df$Neug_diff) - 1.96 * sd(obese_df$Neug_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-500, 520), expand = c(0, 0),
    breaks = seq(-500, 500, 250)
  ) +
  scale_x_continuous(
    limits = c(0, 3020), expand = c(0, 0),
    breaks = seq(0, 3000, 500)
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Class I to III obesity",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Whole sample ------------------------------------------------------------

# Our equation
whole_sample_df$our_diff <- whole_sample_df$pVGRF_N - whole_sample_df$pVGRF_N_predicted
whole_sample_df$our_mean <- (whole_sample_df$pVGRF_N + whole_sample_df$pVGRF_N_predicted) / 2
whole_sample_our_BA_plot <- ggplot(data = whole_sample_df) +
  geom_point(mapping = aes(x = our_mean, y = our_diff)) +
  geom_hline(yintercept = mean(whole_sample_df$our_diff)) +
  geom_hline(
    yintercept = mean(whole_sample_df$our_diff) + 1.96 * sd(whole_sample_df$our_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(whole_sample_df$our_diff) - 1.96 * sd(whole_sample_df$our_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-500, 520), expand = c(0, 0),
    breaks = seq(-500, 500, 250)
  ) +
  scale_x_continuous(
    limits = c(0, 3020), expand = c(0, 0),
    breaks = seq(0, 3000, 500)
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Whole sample",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Neugebauer's equation
whole_sample_df$Neug_diff <- whole_sample_df$pVGRF_N - whole_sample_df$pVGRF_N_Neugebauer
whole_sample_df$Neug_mean <- (whole_sample_df$pVGRF_N + whole_sample_df$pVGRF_N_Neugebauer) / 2
whole_sample_Neug_BA_plot <- ggplot(data = whole_sample_df) +
  geom_point(mapping = aes(x = Neug_mean, y = Neug_diff)) +
  geom_hline(yintercept = mean(whole_sample_df$Neug_diff)) +
  geom_hline(
    yintercept = mean(whole_sample_df$Neug_diff) + 1.96 * sd(whole_sample_df$Neug_diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(whole_sample_df$Neug_diff) - 1.96 * sd(whole_sample_df$Neug_diff),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-500, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 3000), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Whole sample",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

BA_plot_grid <- plot_grid(
  non_obese_our_BA_plot, non_obese_Neug_BA_plot,
  obese_our_BA_plot, obese_Neug_BA_plot,
  whole_sample_our_BA_plot, whole_sample_Neug_BA_plot,
  labels = c("A", "B", "", "", "", ""),
  align  = "h", vjust = 1, label_size = 16,
  ncol   = 2, nrow = 3
)

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig4.tiff",
#   plot = BA_plot_grid, width = 50, height = 35, dpi = 600, units = "cm"
# )