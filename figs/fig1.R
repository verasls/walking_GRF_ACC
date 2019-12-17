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

# For resultant loading rate
LOOCV_back_res_LR  <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res_LR.csv")
LOOCV_hip_res_LR   <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res_LR.csv")

# For vertical loading rate
LOOCV_back_vert_LR  <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert_LR.csv")
LOOCV_hip_vert_LR   <- read_with_factors("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert_LR.csv")

# Actual pRGRF vs pRACC ---------------------------------------------------

# Lower back
back_pRGRF_pRACC_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pRACC_g, y = pRGRF_N, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1, 2.5), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Lower Back",
    x = "pRACC (g)",
    y = "pRGRF (N)"
  )

# Hip
hip_pRGRF_pRACC_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pRACC_g, y = pRGRF_N, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "pRACC (g)",
    y = "pRGRF (N)"
  )

# Actual pVGRF vs pVACC ---------------------------------------------------

# Lower back
back_pVGRF_pVACC_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pVACC_g, y = pVGRF_N, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1, 2.5), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Lower Back",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Hip
hip_pVGRF_pVACC_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pVACC_g, y = pVGRF_N, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1, 3), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "Hip",
    x = "pVACC (g)",
    y = "pVGRF (N)"
  )

# Actual pRLR vs pRATR ----------------------------------------------------

# Lower back
back_pRLR_pRATR_plot <- ggplot(data = LOOCV_back_res_LR) +
  geom_point(mapping = aes(x = pRATR_gs, y = pRLR_Ns, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pRATR_gs, y = pRLR_Ns, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(
    limits = c(0, 30000), expand = c(0, 0),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Lower Back",
    x = quote("pRATR"~(g%.%s^1)),
    y = quote("pRLR"~(N%.%s^1))
  )

# Hip
hip_pRLR_pRATR_plot <- ggplot(data = LOOCV_hip_res_LR) +
  geom_point(mapping = aes(x = pRATR_gs, y = pRLR_Ns, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pRATR_gs, y = pRLR_Ns, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(
    limits = c(0, 30000), expand = c(0, 0),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Hip",
    x = quote("pRATR"~(g%.%s^1)),
    y = quote("pRLR"~(N%.%s^1))
  )

# Actual pVLR vs pVATR ----------------------------------------------------

# Lower back
back_pVLR_pVATR_plot <- ggplot(data = LOOCV_back_vert_LR) +
  geom_point(mapping = aes(x = pVATR_gs, y = pVLR_Ns, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pVATR_gs, y = pVLR_Ns, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(
    limits = c(0, 30000), expand = c(0, 0),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Lower Back",
    x = quote("pVATR"~(g%.%s^1)),
    y = quote("pVLR"~(N%.%s^1))
  )

# Hip
hip_pVLR_pVATR_plot <- ggplot(data = LOOCV_hip_vert_LR) +
  geom_point(mapping = aes(x = pVATR_gs, y = pVLR_Ns, colour = BMI_cat, shape = BMI_cat)) +
  geom_smooth(
    mapping = aes(x = pVATR_gs, y = pVLR_Ns, colour = BMI_cat),
    method = "lm",
    se = FALSE
  ) +
  scale_colour_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
  scale_y_continuous(
    limits = c(0, 30000), expand = c(0, 0),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(
    title = "Hip",
    x = quote("pVATR"~(g%.%s^1)),
    y = quote("pVLR"~(N%.%s^1))
  )

# Plot grid ---------------------------------------------------------------

GRF_ACC_plot_grid_1 <- plot_grid(
  back_pRGRF_pRACC_plot + theme(legend.position = "none"), 
  back_pVGRF_pVACC_plot + theme(legend.position = "none"), 
  hip_pRGRF_pRACC_plot + theme(legend.position = "none"), 
  hip_pVGRF_pVACC_plot + theme(legend.position = "none"),
  back_pRLR_pRATR_plot + theme(legend.position = "none"),
  back_pVLR_pVATR_plot + theme(legend.position = "none"),
  hip_pRLR_pRATR_plot + theme(legend.position = "none"),
  hip_pVLR_pVATR_plot + theme(legend.position = "none"),
  labels = c("A", "B", "", "", "C", "D", "", ""),
  align = "h", vjust = 1, label_size = 16,
  ncol = 2, nrow = 4
)

legend <- get_legend(back_pRGRF_pRACC_plot)

GRF_ACC_plot_grid <- plot_grid(GRF_ACC_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.1))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig1.tiff",
#   plot = GRF_ACC_plot_grid, width = 50, height = 50, dpi = 600, units = "cm"
# )