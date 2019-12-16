# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(cowplot)
source(here("R", "read_with_factors.R"))

# Read and prepare data ---------------------------------------------------

# For resultant ground reaction force
LOOCV_back_res  <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res.csv")
LOOCV_hip_res   <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res.csv")

# For vertical ground reaction force
LOOCV_back_vert  <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert.csv")
LOOCV_hip_vert   <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv")

# Lower back resultant ----------------------------------------------------

LOOCV_back_res$diff <- LOOCV_back_res$pRGRF_N - LOOCV_back_res$pRGRF_N_predicted
LOOCV_back_res$mean <- (LOOCV_back_res$pRGRF_N + LOOCV_back_res$pRGRF_N_predicted) / 2
back_res_BA_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = mean, y = diff, colour = BMI_cat, shape = BMI_cat)) +
  geom_hline(yintercept = mean(LOOCV_back_res$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_res$diff) + 1.96 * sd(LOOCV_back_res$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_res$diff) - 1.96 * sd(LOOCV_back_res$diff),
    linetype = "dotted"
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(-500, 520), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2525), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Lower Back",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Difference of Actual and Predicted pRGRF (N)"
  )

# Hip resultant ----------------------------------------------------------

LOOCV_hip_res$diff <- LOOCV_hip_res$pRGRF_N - LOOCV_hip_res$pRGRF_N_predicted
LOOCV_hip_res$mean <- (LOOCV_hip_res$pRGRF_N + LOOCV_hip_res$pRGRF_N_predicted) / 2
hip_res_BA_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = mean, y = diff, colour = BMI_cat, shape = BMI_cat)) +
  geom_hline(yintercept = mean(LOOCV_hip_res$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_res$diff) + 1.96 * sd(LOOCV_hip_res$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_res$diff) - 1.96 * sd(LOOCV_hip_res$diff),
    linetype = "dotted"
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(-500, 520), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2525), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Difference of Actual and Predicted pRGRF (N)"
  )

# Lower back vertical -----------------------------------------------------

LOOCV_back_vert$diff <- LOOCV_back_vert$pVGRF_N - LOOCV_back_vert$pVGRF_N_predicted
LOOCV_back_vert$mean <- (LOOCV_back_vert$pVGRF_N + LOOCV_back_vert$pVGRF_N_predicted) / 2
back_vert_BA_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = mean, y = diff, colour = BMI_cat, shape = BMI_cat)) +
  geom_hline(yintercept = mean(LOOCV_back_vert$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_vert$diff) + 1.96 * sd(LOOCV_back_vert$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_vert$diff) - 1.96 * sd(LOOCV_back_vert$diff),
    linetype = "dotted"
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(-500, 520), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2525), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Lower Back",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Hip vertical ----------------------------------------------------------

LOOCV_hip_vert$diff <- LOOCV_hip_vert$pVGRF_N - LOOCV_hip_vert$pVGRF_N_predicted
LOOCV_hip_vert$mean <- (LOOCV_hip_vert$pVGRF_N + LOOCV_hip_vert$pVGRF_N_predicted) / 2
hip_vert_BA_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = mean, y = diff, colour = BMI_cat, shape = BMI_cat)) +
  geom_hline(yintercept = mean(LOOCV_hip_vert$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_vert$diff) + 1.96 * sd(LOOCV_hip_vert$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_vert$diff) - 1.96 * sd(LOOCV_hip_vert$diff),
    linetype = "dotted"
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(-500, 520), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2525), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "Mean of Actual and Predicted pVGRF (N)",
    y = "Difference of Actual and Predicted pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

BA_plot_grid_1 <- plot_grid(
  back_res_BA_plot   + theme(legend.position = "none"),
  back_vert_BA_plot  + theme(legend.position = "none"),
  hip_res_BA_plot    + theme(legend.position = "none"),
  hip_vert_BA_plot   + theme(legend.position = "none"),
  labels = c("A", "B", "", ""),
  align  = "h", vjust = 1, label_size = 16,
  ncol   = 2, nrow = 2
)

legend <- get_legend(back_res_BA_plot)

BA_plot_grid <- plot_grid(BA_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.1))


# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig2.tiff",
#   plot = BA_plot_grid, width = 50, height = 25, dpi = 600, units = "cm"
# )