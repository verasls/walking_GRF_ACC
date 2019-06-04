# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Read files --------------------------------------------------------------

resultant <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/res_ANOVA_df.csv")
vertical  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/vert_ANOVA_df.csv")

# General config for the plots --------------------------------------------

# Change group level names for the legend
# (only for one of the plots, due to shared legend)
resultant$group <- as.factor(resultant$group)
levels(resultant$group)[1] <- "Actual peak GRF"
levels(resultant$group)[2] <- "Peak GRF predicted by ankle accelerometer"
levels(resultant$group)[3] <- "Peak GRF predicted by back accelerometer  "
levels(resultant$group)[4] <- "Peak GRF predicted by hip accelerometer"

# pRGRF plot --------------------------------------------------------------

pRGRF_plot <- ggplot(data = resultant, aes(x = speed, y = pRGRF, group = group)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
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
    title = "A)",
    x = bquote("Speed" ~ (km^. ~ h^-1)),
    y = "pRGRF (N)"
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 1060, yend = 1060) +
  annotate("segment", x = 1.7, xend = 1.7, y = 1050, yend = 1070) +
  annotate("segment", x = 2.3, xend = 2.3, y = 1050, yend = 1070) +
  annotate("text", x = 2, y = 1090, label = expression(paste(italic("p"), "= 0.42"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 1130, yend = 1130) +
  annotate("segment", x = 2.7, xend = 2.7, y = 1120, yend = 1140) +
  annotate("segment", x = 3.3, xend = 3.3, y = 1120, yend = 1140) +
  annotate("text", x = 3, y = 1160, label = expression(paste(italic("p"), "= 0.19"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 1200, yend = 1200) +
  annotate("segment", x = 3.7, xend = 3.7, y = 1190, yend = 1210) +
  annotate("segment", x = 4.3, xend = 4.3, y = 1190, yend = 1210) +
  annotate("text", x = 4, y = 1230, label = expression(paste(italic("p"), "= 0.91"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 1280, yend = 1280) +
  annotate("segment", x = 4.7, xend = 4.7, y = 1270, yend = 1290) +
  annotate("segment", x = 5.3, xend = 5.3, y = 1270, yend = 1290) +
  annotate("text", x = 5, y = 1310, label = expression(paste(italic("p"), "= 0.53"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 1360, yend = 1360) +
  annotate("segment", x = 5.7, xend = 5.7, y = 1350, yend = 1370) +
  annotate("segment", x = 6.3, xend = 6.3, y = 1350, yend = 1370) +
  annotate("text", x = 6, y = 1390, label = expression(paste(italic("p"), "= 0.58")))

# pVGRF plot --------------------------------------------------------------

pVGRF_plot <- ggplot(data = vertical, aes(x = speed, y = pVGRF, group = group)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(0.5), aes(shape = group)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5), aes(linetype = group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(from = 800, to = 1400, by = 100)) +
  expand_limits(y = c(800, 1400)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "B)",
    x = bquote("Speed" ~ (km^. ~ h^-1)),
    y = "pVGRF (N)"
  ) +
  annotate("segment", x = 1.7, xend = 2.3, y = 1050, yend = 1050) +
  annotate("segment", x = 1.7, xend = 1.7, y = 1040, yend = 1060) +
  annotate("segment", x = 2.3, xend = 2.3, y = 1040, yend = 1060) +
  annotate("text", x = 2, y = 1080, label = expression(paste(italic("p"), "= 0.96"))) +
  annotate("segment", x = 2.7, xend = 3.3, y = 1110, yend = 1110) +
  annotate("segment", x = 2.7, xend = 2.7, y = 1100, yend = 1120) +
  annotate("segment", x = 3.3, xend = 3.3, y = 1100, yend = 1120) +
  annotate("text", x = 3, y = 1140, label = expression(paste(italic("p"), "= 0.88"))) +
  annotate("segment", x = 3.7, xend = 4.3, y = 1190, yend = 1190) +
  annotate("segment", x = 3.7, xend = 3.7, y = 1180, yend = 1200) +
  annotate("segment", x = 4.3, xend = 4.3, y = 1180, yend = 1200) +
  annotate("text", x = 4, y = 1220, label = expression(paste(italic("p"), "= 0.30"))) +
  annotate("segment", x = 4.7, xend = 5.3, y = 1255, yend = 1255) +
  annotate("segment", x = 4.7, xend = 4.7, y = 1245, yend = 1265) +
  annotate("segment", x = 5.3, xend = 5.3, y = 1245, yend = 1265) +
  annotate("text", x = 5, y = 1285, label = expression(paste(italic("p"), "= 0.26"))) +
  annotate("segment", x = 5.7, xend = 6.3, y = 1310, yend = 1310) +
  annotate("segment", x = 5.7, xend = 5.7, y = 1300, yend = 1320) +
  annotate("segment", x = 6.3, xend = 6.3, y = 1300, yend = 1320) +
  annotate("text", x = 6, y = 1340, label = expression(paste(italic("p"), "= 0.90")))

# Plot grid ---------------------------------------------------------------

GRF_plot_grid_1 <- plot_grid(
  pRGRF_plot + theme(legend.position = "none"),
  pVGRF_plot + theme(legend.position = "none"), 
  ncol = 2, nrow = 1
)

legend <- get_legend(pRGRF_plot)

GRF_plot_grid <- plot_grid(GRF_plot_grid_1, legend, ncol = 1, rel_heights = c(1, 0.2))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig2.tiff",
#   plot = GRF_plot_grid, width = 30, height = 15, dpi = 600, units = "cm"
# )