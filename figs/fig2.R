# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Read files --------------------------------------------------------------

vertical  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/vert_ANOVA_df.csv")
resultant <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/res_ANOVA_df.csv")


# General config for the plots --------------------------------------------

# Change group level names for the legend
# (only for one of the plots, due to shared legend)
vertical$group <- as.factor(vertical$group)
levels(vertical$group)[1] <- "Actual peak GRF"
levels(vertical$group)[2] <- "Peak GRF predicted by ankle accelerometer"
levels(vertical$group)[3] <- "Peak GRF predicted by back accelerometer  "
levels(vertical$group)[4] <- "Peak GRF predicted by hip accelerometer"

# Set legend colours
cbbPalette <- c("#000000", "#D55E00", "#0072B2", "#009E73", "#F0E442")

# pRGRF plot --------------------------------------------------------------

pRGRF_plot <- ggplot(data = resultant, aes(x = speed, y = pRGRF, colour = group)) + 
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_colour_manual(values = cbbPalette) +
  scale_y_continuous(breaks = seq(from = 800, to = 1400, by = 100)) +
  expand_limits(y = c(800, 1400)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "A)",
    x = bquote("Speed" ~ (km^. ~ h^-1)),
    y = "pRGRF (N)"
  )

# pVGRF plot --------------------------------------------------------------

pVGRF_plot <- ggplot(data = vertical, aes(x = speed, y = pVGRF, colour = group)) + 
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_colour_manual(values = cbbPalette) +
  scale_y_continuous(breaks = seq(from = 800, to = 1400, by = 100)) +
  expand_limits(y = c(800, 1400)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "B)",
    x = bquote("Speed" ~ (km^. ~ h^-1)),
    y = "pVGRF (N)"
  )

# Plot grid ---------------------------------------------------------------

GRF_plot_grid_1 <- plot_grid(
  pRGRF_plot + theme(legend.position = "none"),
  pVGRF_plot + theme(legend.position = "none"), 
  ncol = 2, nrow = 1
)

legend <- get_legend(pVGRF_plot)

GRF_plot_grid <- plot_grid(GRF_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.2))

# Uncomment lines below to save plot
ggsave(
  filename = "figs/fig2.pdf",
  plot = GRF_plot_grid, width = 30, height = 15, dpi = 300, units = "cm"
)