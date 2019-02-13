# Load packages and functions ---------------------------------------------

library(tidyverse)
library(here)
library(nlme)
library(piecewiseSEM)
source(here("R", "cross_validate_mixed_model.R"))
source(here("R", "accuracy_indices.R"))

# Prepare data ------------------------------------------------------------

ankle <- read_csv(here("data", "ankle_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

back <- read_csv(here("data", "back_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6) %>% 
  mutate(BMI = body_mass / (height_m ^ 2))

hip <- read_csv(here("data", "hip_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

# Linear mixed models -----------------------------------------------------

# For vertical peak ground reaction force
ankle_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle
)
r2_ankle_vert_LMM <- rsquared(ankle_vert_LMM)

back_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back
)
r2_back_vert_LMM <- rsquared(back_vert_LMM)

hip_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip
)
r2_hip_vert_LMM <- rsquared(hip_vert_LMM)

# For resultant peak ground reaction force
ankle_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle
)
r2_ankle_res_LMM <- rsquared(ankle_res_LMM)

back_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back
)
r2_back_res_LMM <- rsquared(back_res_LMM)

hip_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip
)
r2_hip_res_LMM <- rsquared(hip_res_LMM)

# Leave-one-out cross-validation ------------------------------------------

# For vertical peak ground reaction force
fix_eff    <- pVGRF_N ~ pVACC_g + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_vert_LMM <- do.call(rbind, (lapply(unique(ankle$ID), cross_validate_mixed_model, df = ankle)))
# Back
LOOCV_back_vert_LMM <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))
# Hip
LOOCV_hip_vert_LMM <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

# For resultant peak ground reaction force
fix_eff    <- pRGRF_N ~ pRACC_g + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_res_LMM <- do.call(rbind, (lapply(unique(ankle$ID), cross_validate_mixed_model, df = ankle)))
# Back
LOOCV_back_res_LMM <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))
# Hip
LOOCV_hip_res_LMM <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

# Bland-Altman plots ------------------------------------------------------

# For vertical peak ground reaction force
# Ankle
LOOCV_ankle_vert_LMM$diff <- LOOCV_ankle_vert_LMM$pVGRF_N - LOOCV_ankle_vert_LMM$pVGRF_N_predicted
LOOCV_ankle_vert_LMM$mean <- (LOOCV_ankle_vert_LMM$pVGRF_N + LOOCV_ankle_vert_LMM$pVGRF_N_predicted) / 2
ankle_vert_BA_plot <- ggplot(data = LOOCV_ankle_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_vert_LMM$diff) + 1.96 * sd(LOOCV_ankle_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_vert_LMM$diff) - 1.96 * sd(LOOCV_ankle_vert_LMM$diff),
    linetype = "dotted"
  )

# Back
LOOCV_back_vert_LMM$diff <- LOOCV_back_vert_LMM$pVGRF_N - LOOCV_back_vert_LMM$pVGRF_N_predicted
LOOCV_back_vert_LMM$mean <- (LOOCV_back_vert_LMM$pVGRF_N + LOOCV_back_vert_LMM$pVGRF_N_predicted) / 2
back_vert_BA_plot <- ggplot(data = LOOCV_back_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_vert_LMM$diff) + 1.96 * sd(LOOCV_back_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_vert_LMM$diff) - 1.96 * sd(LOOCV_back_vert_LMM$diff),
    linetype = "dotted"
  )

# Hip
LOOCV_hip_vert_LMM$diff <- LOOCV_hip_vert_LMM$pVGRF_N - LOOCV_hip_vert_LMM$pVGRF_N_predicted
LOOCV_hip_vert_LMM$mean <- (LOOCV_hip_vert_LMM$pVGRF_N + LOOCV_hip_vert_LMM$pVGRF_N_predicted) / 2
hip_vert_BA_plot <- ggplot(data = LOOCV_hip_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_vert_LMM$diff) + 1.96 * sd(LOOCV_hip_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_vert_LMM$diff) - 1.96 * sd(LOOCV_hip_vert_LMM$diff),
    linetype = "dotted"
  )

# For resultant peak ground reaction force
# Ankle
LOOCV_ankle_res_LMM$diff <- LOOCV_ankle_res_LMM$pRGRF_N - LOOCV_ankle_res_LMM$pRGRF_N_predicted
LOOCV_ankle_res_LMM$mean <- (LOOCV_ankle_res_LMM$pRGRF_N + LOOCV_ankle_res_LMM$pRGRF_N_predicted) / 2
ankle_res_BA_plot <- ggplot(data = LOOCV_ankle_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_res_LMM$diff) + 1.96 * sd(LOOCV_ankle_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_res_LMM$diff) - 1.96 * sd(LOOCV_ankle_res_LMM$diff),
    linetype = "dotted"
  )

# Back
LOOCV_back_res_LMM$diff <- LOOCV_back_res_LMM$pRGRF_N - LOOCV_back_res_LMM$pRGRF_N_predicted
LOOCV_back_res_LMM$mean <- (LOOCV_back_res_LMM$pRGRF_N + LOOCV_back_res_LMM$pRGRF_N_predicted) / 2
back_res_BA_plot <- ggplot(data = LOOCV_back_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_res_LMM$diff) + 1.96 * sd(LOOCV_back_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_res_LMM$diff) - 1.96 * sd(LOOCV_back_res_LMM$diff),
    linetype = "dotted"
  )

# Hip
LOOCV_hip_res_LMM$diff <- LOOCV_hip_res_LMM$pRGRF_N - LOOCV_hip_res_LMM$pRGRF_N_predicted
LOOCV_hip_res_LMM$mean <- (LOOCV_hip_res_LMM$pRGRF_N + LOOCV_hip_res_LMM$pRGRF_N_predicted) / 2
hip_res_BA_plot <- ggplot(data = LOOCV_hip_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_res_LMM$diff) + 1.96 * sd(LOOCV_hip_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_res_LMM$diff) - 1.96 * sd(LOOCV_hip_res_LMM$diff),
    linetype = "dotted"
  )

# Indices of accuracy -----------------------------------------------------

# For vertical peak ground reaction force
# Ankle
ankle_vert_accuracy <- accuracy_indices(LOOCV_ankle_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Back
back_vert_accuracy <- accuracy_indices(LOOCV_back_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Hip
hip_vert_accuracy <- accuracy_indices(LOOCV_hip_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")

# For resultant peak ground reaction force
# Ankle
ankle_res_accuracy <- accuracy_indices(LOOCV_ankle_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Back
back_res_accuracy <- accuracy_indices(LOOCV_back_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Hip
hip_res_accuracy <- accuracy_indices(LOOCV_hip_res_LMM, "pRGRF_N", "pRGRF_N_predicted")