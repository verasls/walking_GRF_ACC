# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(cowplot)
source(here("R", "BMI_categories.R"))

# Read and prepare data ---------------------------------------------------

# For resultant ground reaction force
LOOCV_ankle_res <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_res.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

LOOCV_back_res  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

LOOCV_hip_res   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

# For vertical ground reaction force
LOOCV_ankle_vert <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_vert.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

LOOCV_back_vert  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

LOOCV_hip_vert   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv") %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

# Actual pRGRF vs pRACC ---------------------------------------------------

# Ankle
ankle_pRGRF_pRACC_plot <- ggplot(data = LOOCV_ankle_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 7), expand = c(0, 0), breaks = seq(0, 7, 1)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Ankle",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Back
back_pRGRF_pRACC_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Lower Back",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Hip
hip_pRGRF_pRACC_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Actual pVGRF vs pVACC ---------------------------------------------------

# Ankle
ankle_pVGRF_pVACC_plot <- ggplot(data = LOOCV_ankle_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 7), expand = c(0, 0), breaks = seq(0, 7, 1)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Ankle",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Back
back_pVGRF_pVACC_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Lower Back",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Hip
hip_pVGRF_pVACC_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

GRF_ACC_plot_grid_1 <- plot_grid(
  ankle_pRGRF_pRACC_plot + theme(legend.position = "none"),
  ankle_pVGRF_pVACC_plot + theme(legend.position = "none"),
  back_pRGRF_pRACC_plot + theme(legend.position = "none"), 
  back_pVGRF_pVACC_plot + theme(legend.position = "none"), 
  hip_pRGRF_pRACC_plot + theme(legend.position = "none"), 
  hip_pVGRF_pVACC_plot + theme(legend.position = "none"), 
  labels = c("A", "B", "", "", "", ""),
  align = "h", vjust = 1, label_size = 16,
  ncol = 2, nrow = 3
)

legend <- get_legend(ankle_pRGRF_pRACC_plot)

GRF_ACC_plot_grid <- plot_grid(GRF_ACC_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.1))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig3.tiff",
#   plot = GRF_ACC_plot_grid, width = 50, height = 35, dpi = 600, units = "cm"
# )