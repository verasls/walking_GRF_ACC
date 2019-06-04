# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(cowplot)
source(here("R", "BMI_categories.R"))

# Read and prepare data ---------------------------------------------------

# For resultant ground reaction force
LOOCV_ankle_res <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_res.csv") %>% 
  select(ID, speed, ankle_actual = pRGRF_N, ankle_predicted = pRGRF_N_predicted)
LOOCV_back_res  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res.csv") %>% 
  select(ID, speed, back_actual = pRGRF_N, back_predicted = pRGRF_N_predicted)
LOOCV_hip_res   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res.csv") %>% 
  select(ID, speed, hip_actual = pRGRF_N, hip_predicted = pRGRF_N_predicted)

# For vertical ground reaction force
LOOCV_ankle_vert <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_vert.csv") %>% 
  select(ID, speed, ankle_actual = pVGRF_N, ankle_predicted = pVGRF_N_predicted)
LOOCV_back_vert  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert.csv") %>% 
  select(ID, speed, back_actual = pVGRF_N, back_predicted = pVGRF_N_predicted)
LOOCV_hip_vert   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv") %>% 
  select(ID, speed, hip_actual = pVGRF_N, hip_predicted = pVGRF_N_predicted)

# Merge resultant data
resultant <- LOOCV_ankle_res %>% 
  left_join(LOOCV_back_res, by = c("ID", "speed")) %>% 
  left_join(LOOCV_hip_res, by = c("ID", "speed")) %>% 
  mutate(pRGRF_N = ((ankle_actual + back_actual + hip_actual) / 3)) %>% 
  gather(
    ankle_predicted, back_predicted, hip_predicted,
    key = "acc_placement", value = "pRGRF_N_predicted"
    ) %>% 
  select(ID, speed, pRGRF_N, pRGRF_N_predicted, acc_placement) %>% 
  na.omit()

# Merge vertical data
vertical <- LOOCV_ankle_vert %>% 
  left_join(LOOCV_back_vert, by = c("ID", "speed")) %>% 
  left_join(LOOCV_hip_vert, by = c("ID", "speed")) %>% 
  mutate(pVGRF_N = ((ankle_actual + back_actual + hip_actual) / 3)) %>% 
  gather(
    ankle_predicted, back_predicted, hip_predicted,
    key = "acc_placement", value = "pVGRF_N_predicted"
  ) %>% 
  select(ID, speed, pVGRF_N, pVGRF_N_predicted, acc_placement) %>% 
  na.omit()

# Resultant (actual X predicted pRGRF) ------------------------------------

pRGRF_plot <- ggplot(data = resultant) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted, shape = acc_placement)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_shape_manual(
    values = c(15, 16, 17), 
    labels = c("Ankle", "Lower Back", "Hip")
  ) +
  geom_abline() +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
    ) +
  labs(
    x = "Actual pRGRF (N)",
    y = "Predicted pRGRF (N)"
  )

# Vertical (actual X predicted pVGRF) -------------------------------------

pVGRF_plot <- ggplot(data = vertical) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted, shape = acc_placement)) +
  scale_y_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2550), breaks = seq(0, 2550, 500), expand = c(0, 0)) +
  scale_shape_manual(
    values = c(15, 16, 17),
    labels = c("Ankle", "Lower Back", "Hip")
  ) +
  geom_abline() +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    x = "Actual pVGRF (N)",
    y = "Predicted pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

GRF_plot_grid_1 <- plot_grid(
  pRGRF_plot + theme(legend.position = "none"),
  pVGRF_plot + theme(legend.position = "none"),
  labels = c("A", "B"),
  align = "vh", vjust = 1, label_size = 16,
  ncol = 2, nrow = 1
)

legend <- get_legend(pRGRF_plot)

GRF_plot_grid <- plot_grid(GRF_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.1))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig4.1.tiff",
#   plot = GRF_plot_grid, width = 50, height = 25, dpi = 600, units = "cm"
# )