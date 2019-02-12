# Load packages and functions ---------------------------------------------

library(tidyverse)
library(here)
library(nlme)
library(piecewiseSEM)
source(here("R", "cross_validate_mixed_model.R"))
source(here("R", "accuracy_indices.R"))

# Prepare data ------------------------------------------------------------

ankle_pri <- read_csv(here("data", "ankle_pri.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

ankle_sec <- read_csv(here("data", "ankle_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

back_pri <- read_csv(here("data", "back_pri.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

back_sec <- read_csv(here("data", "back_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6) %>% 
  mutate(BMI = body_mass / (height_m ^ 2))

hip_pri <- read_csv(here("data", "hip_pri.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

hip_sec <- read_csv(here("data", "hip_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6)

# Linear mixed models -----------------------------------------------------

# For vertical peak ground reaction force
ankle_pri_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle_pri
)
r2_ankle_pri_vert_LMM <- rsquared(ankle_pri_vert_LMM)

ankle_sec_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle_sec
)
r2_ankle_sec_vert_LMM <- rsquared(ankle_sec_vert_LMM)

back_pri_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_pri
)
r2_back_pri_vert_LMM <- rsquared(back_pri_vert_LMM)

back_sec_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_sec
)
r2_back_sec_vert_LMM <- rsquared(back_sec_vert_LMM)

hip_pri_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_pri
)
r2_hip_pri_vert_LMM <- rsquared(hip_pri_vert_LMM)

hip_sec_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_sec
)
r2_hip_sec_vert_LMM <- rsquared(hip_sec_vert_LMM)

# For resultant peak ground reaction force
ankle_pri_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle_pri
)
r2_ankle_pri_res_LMM <- rsquared(ankle_pri_res_LMM)

ankle_sec_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle_sec
)
r2_ankle_sec_res_LMM <- rsquared(ankle_sec_res_LMM)

back_pri_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_pri
)
r2_back_pri_res_LMM <- rsquared(back_pri_res_LMM)

back_sec_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_sec
)
r2_back_sec_res_LMM <- rsquared(back_sec_res_LMM)

hip_pri_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_pri
)
r2_hip_pri_res_LMM <- rsquared(hip_pri_res_LMM)

hip_sec_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_sec
)
r2_hip_sec_res_LMM <- rsquared(hip_sec_res_LMM)

# Leave-one-out cross-validation ------------------------------------------

# For vertical peak ground reaction force
fix_eff    <- pVGRF_N ~ pVACC_g + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_pri_vert_LMM <- do.call(rbind, (lapply(unique(ankle_pri$ID), cross_validate_mixed_model, df = ankle_pri)))
LOOCV_ankle_sec_vert_LMM <- do.call(rbind, (lapply(unique(ankle_sec$ID), cross_validate_mixed_model, df = ankle_sec)))
# Back
LOOCV_back_pri_vert_LMM <- do.call(rbind, (lapply(unique(back_pri$ID), cross_validate_mixed_model, df = back_pri)))
LOOCV_back_sec_vert_LMM <- do.call(rbind, (lapply(unique(back_sec$ID), cross_validate_mixed_model, df = back_sec)))
# Hip
LOOCV_hip_pri_vert_LMM <- do.call(rbind, (lapply(unique(hip_pri$ID), cross_validate_mixed_model, df = hip_pri)))
LOOCV_hip_sec_vert_LMM <- do.call(rbind, (lapply(unique(hip_sec$ID), cross_validate_mixed_model, df = hip_sec)))

# For resultant peak ground reaction force
fix_eff    <- pRGRF_N ~ pRACC_g + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_pri_res_LMM <- do.call(rbind, (lapply(unique(ankle_pri$ID), cross_validate_mixed_model, df = ankle_pri)))
LOOCV_ankle_sec_res_LMM <- do.call(rbind, (lapply(unique(ankle_sec$ID), cross_validate_mixed_model, df = ankle_sec)))
# Back
LOOCV_back_pri_res_LMM <- do.call(rbind, (lapply(unique(back_pri$ID), cross_validate_mixed_model, df = back_pri)))
LOOCV_back_sec_res_LMM <- do.call(rbind, (lapply(unique(back_sec$ID), cross_validate_mixed_model, df = back_sec)))
# Hip
LOOCV_hip_pri_res_LMM <- do.call(rbind, (lapply(unique(hip_pri$ID), cross_validate_mixed_model, df = hip_pri)))
LOOCV_hip_sec_res_LMM <- do.call(rbind, (lapply(unique(hip_sec$ID), cross_validate_mixed_model, df = hip_sec)))

# Bland-Altman plots ------------------------------------------------------

# For vertical peak ground reaction force
# Ankle_pri
LOOCV_ankle_pri_vert_LMM$diff <- LOOCV_ankle_pri_vert_LMM$pVGRF_N - LOOCV_ankle_pri_vert_LMM$pVGRF_N_predicted
LOOCV_ankle_pri_vert_LMM$mean <- (LOOCV_ankle_pri_vert_LMM$pVGRF_N + LOOCV_ankle_pri_vert_LMM$pVGRF_N_predicted) / 2
ankle_pri_vert_BA_plot <- ggplot(data = LOOCV_ankle_pri_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_pri_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_pri_vert_LMM$diff) + 1.96 * sd(LOOCV_ankle_pri_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_pri_vert_LMM$diff) - 1.96 * sd(LOOCV_ankle_pri_vert_LMM$diff),
    linetype = "dotted"
  )

# Ankle_sec
LOOCV_ankle_sec_vert_LMM$diff <- LOOCV_ankle_sec_vert_LMM$pVGRF_N - LOOCV_ankle_sec_vert_LMM$pVGRF_N_predicted
LOOCV_ankle_sec_vert_LMM$mean <- (LOOCV_ankle_sec_vert_LMM$pVGRF_N + LOOCV_ankle_sec_vert_LMM$pVGRF_N_predicted) / 2
ankle_sec_vert_BA_plot <- ggplot(data = LOOCV_ankle_sec_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_sec_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_sec_vert_LMM$diff) + 1.96 * sd(LOOCV_ankle_sec_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_sec_vert_LMM$diff) - 1.96 * sd(LOOCV_ankle_sec_vert_LMM$diff),
    linetype = "dotted"
  )

# Back_pri
LOOCV_back_pri_vert_LMM$diff <- LOOCV_back_pri_vert_LMM$pVGRF_N - LOOCV_back_pri_vert_LMM$pVGRF_N_predicted
LOOCV_back_pri_vert_LMM$mean <- (LOOCV_back_pri_vert_LMM$pVGRF_N + LOOCV_back_pri_vert_LMM$pVGRF_N_predicted) / 2
back_pri_vert_BA_plot <- ggplot(data = LOOCV_back_pri_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_pri_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_pri_vert_LMM$diff) + 1.96 * sd(LOOCV_back_pri_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_pri_vert_LMM$diff) - 1.96 * sd(LOOCV_back_pri_vert_LMM$diff),
    linetype = "dotted"
  )

# Back_sec
LOOCV_back_sec_vert_LMM$diff <- LOOCV_back_sec_vert_LMM$pVGRF_N - LOOCV_back_sec_vert_LMM$pVGRF_N_predicted
LOOCV_back_sec_vert_LMM$mean <- (LOOCV_back_sec_vert_LMM$pVGRF_N + LOOCV_back_sec_vert_LMM$pVGRF_N_predicted) / 2
back_sec_vert_BA_plot <- ggplot(data = LOOCV_back_sec_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_sec_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_sec_vert_LMM$diff) + 1.96 * sd(LOOCV_back_sec_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_sec_vert_LMM$diff) - 1.96 * sd(LOOCV_back_sec_vert_LMM$diff),
    linetype = "dotted"
  )

# Hip_pri
LOOCV_hip_pri_vert_LMM$diff <- LOOCV_hip_pri_vert_LMM$pVGRF_N - LOOCV_hip_pri_vert_LMM$pVGRF_N_predicted
LOOCV_hip_pri_vert_LMM$mean <- (LOOCV_hip_pri_vert_LMM$pVGRF_N + LOOCV_hip_pri_vert_LMM$pVGRF_N_predicted) / 2
hip_pri_vert_BA_plot <- ggplot(data = LOOCV_hip_pri_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_pri_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_pri_vert_LMM$diff) + 1.96 * sd(LOOCV_hip_pri_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_pri_vert_LMM$diff) - 1.96 * sd(LOOCV_hip_pri_vert_LMM$diff),
    linetype = "dotted"
  )

# Hip_sec
LOOCV_hip_sec_vert_LMM$diff <- LOOCV_hip_sec_vert_LMM$pVGRF_N - LOOCV_hip_sec_vert_LMM$pVGRF_N_predicted
LOOCV_hip_sec_vert_LMM$mean <- (LOOCV_hip_sec_vert_LMM$pVGRF_N + LOOCV_hip_sec_vert_LMM$pVGRF_N_predicted) / 2
hip_sec_vert_BA_plot <- ggplot(data = LOOCV_hip_sec_vert_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_sec_vert_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_sec_vert_LMM$diff) + 1.96 * sd(LOOCV_hip_sec_vert_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_sec_vert_LMM$diff) - 1.96 * sd(LOOCV_hip_sec_vert_LMM$diff),
    linetype = "dotted"
  )

# For resultant peak ground reaction force
# Ankle_pri
LOOCV_ankle_pri_res_LMM$diff <- LOOCV_ankle_pri_res_LMM$pRGRF_N - LOOCV_ankle_pri_res_LMM$pRGRF_N_predicted
LOOCV_ankle_pri_res_LMM$mean <- (LOOCV_ankle_pri_res_LMM$pRGRF_N + LOOCV_ankle_pri_res_LMM$pRGRF_N_predicted) / 2
ankle_pri_res_BA_plot <- ggplot(data = LOOCV_ankle_pri_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_pri_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_pri_res_LMM$diff) + 1.96 * sd(LOOCV_ankle_pri_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_pri_res_LMM$diff) - 1.96 * sd(LOOCV_ankle_pri_res_LMM$diff),
    linetype = "dotted"
  )

# Ankle_sec
LOOCV_ankle_sec_res_LMM$diff <- LOOCV_ankle_sec_res_LMM$pRGRF_N - LOOCV_ankle_sec_res_LMM$pRGRF_N_predicted
LOOCV_ankle_sec_res_LMM$mean <- (LOOCV_ankle_sec_res_LMM$pRGRF_N + LOOCV_ankle_sec_res_LMM$pRGRF_N_predicted) / 2
ankle_sec_res_BA_plot <- ggplot(data = LOOCV_ankle_sec_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_ankle_sec_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_sec_res_LMM$diff) + 1.96 * sd(LOOCV_ankle_sec_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_ankle_sec_res_LMM$diff) - 1.96 * sd(LOOCV_ankle_sec_res_LMM$diff),
    linetype = "dotted"
  )

# Back_pri
LOOCV_back_pri_res_LMM$diff <- LOOCV_back_pri_res_LMM$pRGRF_N - LOOCV_back_pri_res_LMM$pRGRF_N_predicted
LOOCV_back_pri_res_LMM$mean <- (LOOCV_back_pri_res_LMM$pRGRF_N + LOOCV_back_pri_res_LMM$pRGRF_N_predicted) / 2
back_pri_res_BA_plot <- ggplot(data = LOOCV_back_pri_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_pri_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_pri_res_LMM$diff) + 1.96 * sd(LOOCV_back_pri_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_pri_res_LMM$diff) - 1.96 * sd(LOOCV_back_pri_res_LMM$diff),
    linetype = "dotted"
  )

# Back_sec
LOOCV_back_sec_res_LMM$diff <- LOOCV_back_sec_res_LMM$pRGRF_N - LOOCV_back_sec_res_LMM$pRGRF_N_predicted
LOOCV_back_sec_res_LMM$mean <- (LOOCV_back_sec_res_LMM$pRGRF_N + LOOCV_back_sec_res_LMM$pRGRF_N_predicted) / 2
back_sec_res_BA_plot <- ggplot(data = LOOCV_back_sec_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_sec_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_sec_res_LMM$diff) + 1.96 * sd(LOOCV_back_sec_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_sec_res_LMM$diff) - 1.96 * sd(LOOCV_back_sec_res_LMM$diff),
    linetype = "dotted"
  )

# Hip_pri
LOOCV_hip_pri_res_LMM$diff <- LOOCV_hip_pri_res_LMM$pRGRF_N - LOOCV_hip_pri_res_LMM$pRGRF_N_predicted
LOOCV_hip_pri_res_LMM$mean <- (LOOCV_hip_pri_res_LMM$pRGRF_N + LOOCV_hip_pri_res_LMM$pRGRF_N_predicted) / 2
hip_pri_res_BA_plot <- ggplot(data = LOOCV_hip_pri_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_pri_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_pri_res_LMM$diff) + 1.96 * sd(LOOCV_hip_pri_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_pri_res_LMM$diff) - 1.96 * sd(LOOCV_hip_pri_res_LMM$diff),
    linetype = "dotted"
  )

# Hip_sec
LOOCV_hip_sec_res_LMM$diff <- LOOCV_hip_sec_res_LMM$pRGRF_N - LOOCV_hip_sec_res_LMM$pRGRF_N_predicted
LOOCV_hip_sec_res_LMM$mean <- (LOOCV_hip_sec_res_LMM$pRGRF_N + LOOCV_hip_sec_res_LMM$pRGRF_N_predicted) / 2
hip_sec_res_BA_plot <- ggplot(data = LOOCV_hip_sec_res_LMM) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_sec_res_LMM$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_sec_res_LMM$diff) + 1.96 * sd(LOOCV_hip_sec_res_LMM$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_sec_res_LMM$diff) - 1.96 * sd(LOOCV_hip_sec_res_LMM$diff),
    linetype = "dotted"
  )

# Indices of accuracy -----------------------------------------------------

# For vertical peak ground reaction force
# Ankle
ankle_pri_vert_accuracy <- accuracy_indices(LOOCV_ankle_pri_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
ankle_sec_vert_accuracy <- accuracy_indices(LOOCV_ankle_sec_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Back
back_pri_vert_accuracy <- accuracy_indices(LOOCV_back_pri_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
back_sec_vert_accuracy <- accuracy_indices(LOOCV_back_sec_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Hip
hip_pri_vert_accuracy <- accuracy_indices(LOOCV_hip_pri_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
hip_sec_vert_accuracy <- accuracy_indices(LOOCV_hip_sec_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")

# For resultant peak ground reaction force
# Ankle
ankle_pri_res_accuracy <- accuracy_indices(LOOCV_ankle_pri_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
ankle_sec_res_accuracy <- accuracy_indices(LOOCV_ankle_sec_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Back
back_pri_res_accuracy <- accuracy_indices(LOOCV_back_pri_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
back_sec_res_accuracy <- accuracy_indices(LOOCV_back_sec_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Hip
hip_pri_res_accuracy <- accuracy_indices(LOOCV_hip_pri_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
hip_sec_res_accuracy <- accuracy_indices(LOOCV_hip_sec_res_LMM, "pRGRF_N", "pRGRF_N_predicted")