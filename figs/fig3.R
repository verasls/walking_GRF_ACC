# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Read files --------------------------------------------------------------

resultant_GRF <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/res_ANOVA_df.csv")
vertical_GRF  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/vert_ANOVA_df.csv")
resultant_LR  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/res_ANOVA_LR_df.csv")
vertical_LR   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/vert_ANOVA_LR_df.csv")

# General config for the plots --------------------------------------------

# Recode group factor
resultant_GRF <- resultant_GRF %>% 
  filter(group != "ankle")
resultant_GRF$group <- as.factor(resultant_GRF$group)
resultant_GRF$group <- recode(
  resultant_GRF$group,
  "actual" = "Actual peak GRF",
  "back" = "Peak GRF predicted by lower back accelerometer",
  "hip" = "Peak GRF predicted by hip accelerometer"
)

vertical_GRF <- vertical_GRF %>% 
  filter(group != "ankle")
vertical_GRF$group <- as.factor(vertical_GRF$group)
vertical_GRF$group <- recode(
  vertical_GRF$group,
  "actual" = "Actual peak GRF",
  "back" = "Peak GRF predicted by lower back accelerometer",
  "hip" = "Peak GRF predicted by hip accelerometer"
)

resultant_LR$group <- as.factor(resultant_LR$group)
resultant_LR$group <- recode(
  resultant_LR$group,
  "actual" = "Actual peak LR",
  "back" = "Peak LR predicted by lower back accelerometer",
  "hip" = "Peak LR predicted by hip accelerometer"
)

vertical_LR$group <- as.factor(vertical_LR$group)
vertical_LR$group <- recode(
  vertical_LR$group,
  "actual" = "Actual peak LR",
  "back" = "Peak LR predicted by lower back accelerometer",
  "hip" = "Peak LR predicted by hip accelerometer"
)

# pRGRF plot --------------------------------------------------------------

pRGRF_plot <- ggplot(data = resultant_GRF, aes(x = speed, y = pRGRF, group = group)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(from = 800, to = 1400, by = 100)) +
  expand_limits(y = c(800, 1400)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "a",
    x = quote("Speed"~(km%.%h^-1)),
    y = "pRGRF (N)"
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 1060, yend = 1060) +
  annotate("segment", x = 1.7, xend = 1.7, y = 1050, yend = 1070) +
  annotate("segment", x = 2.3, xend = 2.3, y = 1050, yend = 1070) +
  annotate("text", x = 2, y = 1090, label = expression(paste(italic("p"), "= 0.79"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 1130, yend = 1130) +
  annotate("segment", x = 2.7, xend = 2.7, y = 1120, yend = 1140) +
  annotate("segment", x = 3.3, xend = 3.3, y = 1120, yend = 1140) +
  annotate("text", x = 3, y = 1160, label = expression(paste(italic("p"), "= 0.58"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 1200, yend = 1200) +
  annotate("segment", x = 3.7, xend = 3.7, y = 1190, yend = 1210) +
  annotate("segment", x = 4.3, xend = 4.3, y = 1190, yend = 1210) +
  annotate("text", x = 4, y = 1230, label = expression(paste(italic("p"), "= 0.74"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 1280, yend = 1280) +
  annotate("segment", x = 4.7, xend = 4.7, y = 1270, yend = 1290) +
  annotate("segment", x = 5.3, xend = 5.3, y = 1270, yend = 1290) +
  annotate("text", x = 5, y = 1310, label = expression(paste(italic("p"), "= 0.86"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 1360, yend = 1360) +
  annotate("segment", x = 5.7, xend = 5.7, y = 1350, yend = 1370) +
  annotate("segment", x = 6.3, xend = 6.3, y = 1350, yend = 1370) +
  annotate("text", x = 6, y = 1390, label = expression(paste(italic("p"), "= 0.76")))

# pVGRF plot --------------------------------------------------------------

pVGRF_plot <- ggplot(data = vertical_GRF, aes(x = speed, y = pVGRF, group = group)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(from = 800, to = 1400, by = 100)) +
  expand_limits(y = c(800, 1400)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    title = "b",
    x = quote("Speed"~(km%.%h^-1)),
    y = "pVGRF (N)"
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 1050, yend = 1050) +
  annotate("segment", x = 1.7, xend = 1.7, y = 1040, yend = 1060) +
  annotate("segment", x = 2.3, xend = 2.3, y = 1040, yend = 1060) +
  annotate("text", x = 2, y = 1080, label = expression(paste(italic("p"), "= 0.56"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 1110, yend = 1110) +
  annotate("segment", x = 2.7, xend = 2.7, y = 1100, yend = 1120) +
  annotate("segment", x = 3.3, xend = 3.3, y = 1100, yend = 1120) +
  annotate("text", x = 3, y = 1140, label = expression(paste(italic("p"), "= 0.22"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 1190, yend = 1190) +
  annotate("segment", x = 3.7, xend = 3.7, y = 1180, yend = 1200) +
  annotate("segment", x = 4.3, xend = 4.3, y = 1180, yend = 1200) +
  annotate("text", x = 4, y = 1220, label = expression(paste(italic("p"), "= 0.20"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 1255, yend = 1255) +
  annotate("segment", x = 4.7, xend = 4.7, y = 1245, yend = 1265) +
  annotate("segment", x = 5.3, xend = 5.3, y = 1245, yend = 1265) +
  annotate("text", x = 5, y = 1285, label = expression(paste(italic("p"), "= 0.94"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 1310, yend = 1310) +
  annotate("segment", x = 5.7, xend = 5.7, y = 1300, yend = 1320) +
  annotate("segment", x = 6.3, xend = 6.3, y = 1300, yend = 1320) +
  annotate("text", x = 6, y = 1340, label = expression(paste(italic("p"), "= 0.79")))

# pRLR plot ---------------------------------------------------------------

pRLR_plot <- ggplot(data = resultant_LR, aes(x = speed, y = pRLR, group = group)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(from = 4000, to = 18000, by = 2000)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "c",
    x = quote("Speed"~(km%.%h^-1)),
    y = quote("pRLR"~(N%.%s^-1))
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 6300, yend = 6300) +
  annotate("segment", x = 1.7, xend = 1.7, y = 6050, yend = 6550) +
  annotate("segment", x = 2.3, xend = 2.3, y = 6050, yend = 6550) +
  annotate("text", x = 2, y = 7100, label = expression(paste(italic("p"), "< 0.01"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 8300, yend = 8300) +
  annotate("segment", x = 2.7, xend = 2.7, y = 8050, yend = 8550) +
  annotate("segment", x = 3.3, xend = 3.3, y = 8050, yend = 8550) +
  annotate("text", x = 3, y = 9100, label = expression(paste(italic("p"), "= 0.38"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 10800, yend = 10800) +
  annotate("segment", x = 3.7, xend = 3.7, y = 10550, yend = 11050) +
  annotate("segment", x = 4.3, xend = 4.3, y = 10550, yend = 11050) +
  annotate("text", x = 4, y = 11600, label = expression(paste(italic("p"), "= 0.13"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 13000, yend = 13000) +
  annotate("segment", x = 4.7, xend = 4.7, y = 12750, yend = 13250) +
  annotate("segment", x = 5.3, xend = 5.3, y = 12750, yend = 13250) +
  annotate("text", x = 5, y = 13800, label = expression(paste(italic("p"), "= 0.79"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 17000, yend = 17000) +
  annotate("segment", x = 5.7, xend = 5.7, y = 16750, yend = 17250) +
  annotate("segment", x = 6.3, xend = 6.3, y = 16750, yend = 17250) +
  annotate("text", x = 6, y = 17800, label = expression(paste(italic("p"), "= 0.26")))

# pVLR plot ---------------------------------------------------------------

pVLR_plot <- ggplot(data = vertical_LR, aes(x = speed, y = pVLR, group = group)) +
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(from = 4000, to = 18000, by = 2000)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "d",
    x = quote("Speed"~(km%.%h^-1)),
    y = quote("pVLR"~(N%.%s^-1))
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 6300, yend = 6300) +
  annotate("segment", x = 1.7, xend = 1.7, y = 6050, yend = 6550) +
  annotate("segment", x = 2.3, xend = 2.3, y = 6050, yend = 6550) +
  annotate("text", x = 2, y = 7100, label = expression(paste(italic("p"), "< 0.01"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 8300, yend = 8300) +
  annotate("segment", x = 2.7, xend = 2.7, y = 8050, yend = 8550) +
  annotate("segment", x = 3.3, xend = 3.3, y = 8050, yend = 8550) +
  annotate("text", x = 3, y = 9100, label = expression(paste(italic("p"), "= 0.30"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 10800, yend = 10800) +
  annotate("segment", x = 3.7, xend = 3.7, y = 10550, yend = 11050) +
  annotate("segment", x = 4.3, xend = 4.3, y = 10550, yend = 11050) +
  annotate("text", x = 4, y = 11600, label = expression(paste(italic("p"), "= 0.57"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 13000, yend = 13000) +
  annotate("segment", x = 4.7, xend = 4.7, y = 12750, yend = 13250) +
  annotate("segment", x = 5.3, xend = 5.3, y = 12750, yend = 13250) +
  annotate("text", x = 5, y = 13800, label = expression(paste(italic("p"), "= 0.78"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 17000, yend = 17000) +
  annotate("segment", x = 5.7, xend = 5.7, y = 16750, yend = 17250) +
  annotate("segment", x = 6.3, xend = 6.3, y = 16750, yend = 17250) +
  annotate("text", x = 6, y = 17800, label = expression(paste(italic("p"), "= 0.23")))

# Plot grid ---------------------------------------------------------------

# GRF plot grid
GRF_plot_grid_1 <- plot_grid(
  pRGRF_plot + theme(legend.position = "none"),
  pVGRF_plot + theme(legend.position = "none"), 
  ncol = 2, nrow = 1
)

legend_GRF <- get_legend(pRGRF_plot)

GRF_plot_grid <- plot_grid(GRF_plot_grid_1, legend_GRF, ncol = 1, rel_heights = c(1, 0.2))

# LR plot grid
LR_plot_grid_1 <- plot_grid(
  pRLR_plot + theme(legend.position = "none"),
  pVLR_plot + theme(legend.position = "none"), 
  ncol = 2, nrow = 1
)

legend_LR <- get_legend(pRLR_plot)

LR_plot_grid <- plot_grid(LR_plot_grid_1, legend_LR, ncol = 1, rel_heights = c(1, 0.2))

# Combine plots
plot_grid <- plot_grid(
  GRF_plot_grid, LR_plot_grid,
  ncol = 1, nrow = 2
)

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig3.tiff",
#   plot = plot_grid, width = 30, height = 30, dpi = 300, units = "cm"
# )