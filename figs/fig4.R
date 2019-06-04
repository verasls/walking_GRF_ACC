# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(cowplot)
source(here("R", "BMI_categories.R"))

# Read and prepare data ---------------------------------------------------

# For resultant ground reaction force
LOOCV_ankle_res <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_res.csv")
LOOCV_back_res  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res.csv")
LOOCV_hip_res   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res.csv")

# For vertical ground reaction force
LOOCV_ankle_vert <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_vert.csv")
LOOCV_back_vert  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert.csv")
LOOCV_hip_vert   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv")

# Resultant (actual X predicted pRGRF) ------------------------------------

# Ankle
ankle_pRGRF_plot <- ggplot(data = LOOCV_ankle_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Ankle",
    x = "Actual pRGRF (N)",
    y = "Predicted pRGRF (N)"
  )

# Back
back_pRGRF_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Low Back",
    x = "Actual pRGRF (N)",
    y = "Predicted pRGRF (N)"
  )

# Hip
hip_pRGRF_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "Actual pRGRF (N)",
    y = "Predicted pRGRF (N)"
  )

# Vertical (actual X predicted pVGRF) -------------------------------------

# Ankle
ankle_pVGRF_plot <- ggplot(data = LOOCV_ankle_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Ankle",
    x = "Actual pVGRF (N)",
    y = "Predicted pVGRF (N)"
  )

# Back
back_pVGRF_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Lower Back",
    x = "Actual pVGRF (N)",
    y = "Predicted pVGRF (N)"
  )

# Hip
hip_pVGRF_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  geom_abline() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "Actual pVGRF (N)",
    y = "Predicted pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

GRF_plot_grid <- plot_grid(
  ankle_pRGRF_plot, ankle_pVGRF_plot,
  back_pRGRF_plot, back_pVGRF_plot,
  hip_pRGRF_plot, hip_pVGRF_plot,
  labels = c("A", "B", "", "", "", ""),
  align = "vh", vjust = 1, label_size = 16,
  ncol = 2, nrow = 3
)

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig4.tiff",
#   plot = GRF_plot_grid, width = 35, height = 35, dpi = 600, units = "cm"
# )