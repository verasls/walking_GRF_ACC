# Load packages and functions ---------------------------------------------

library(tidyverse)
library(here)
library(nlme)
library(piecewiseSEM)
library(ez)
source(here("R", "cross_validate_mixed_model.R"))
source(here("R", "accuracy_indices.R"))
source(here("R", "get_BA_plot.R"))

# 1. Prepare data ---------------------------------------------------------

ankle <- read_csv(here("data", "ankle_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6) %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

back <- read_csv(here("data", "back_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6) %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

hip <- read_csv(here("data", "hip_sec.csv")) %>% 
  select(-c(body_weight, pVGRF_BW, pRGRF_BW, pVACC_ms2, pVACC_ms2, pRACC_ms2)) %>% 
  filter(speed <= 6) %>% 
  BMI_categories() %>% 
  select(ID, speed, body_mass, height, BMI, BMI_cat, everything())

# 2. Sample descriptives --------------------------------------------------

# Demographics
samp_desc <- read_csv(here("data", "sample_demographics.csv")) %>% 
  filter(ID != 35) # ID 35 only with primary acc data (not used in these analysis)
sample_descriptives <- summarise(
  .data = samp_desc,
  age_mean       = round(mean(age), digits = 1),
  age_sd         = round(sd(age), digits = 1),
  body_mass_mean = round(mean(body_mass), digits = 1),
  body_mass_sd   = round(sd(body_mass), digits = 1),
  height_mean    = round(mean(height), digits = 1),
  height_sd      = round(sd(height), digits = 1),
  BMI_mean       = round(mean(BMI), digits = 1),
  BMI_sd         = round(sd(BMI), digits = 1)
)
sex <- table(samp_desc$sex)

# 3. Linear mixed models --------------------------------------------------

# For resultant peak ground reaction force
ankle_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + I(pRACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle
)
r2_ankle_res_LMM <- rsquared(ankle_res_LMM)

back_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + I(pRACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back
)
r2_back_res_LMM <- rsquared(back_res_LMM)

hip_res_LMM <- lme(
  fixed = pRGRF_N ~ pRACC_g + I(pRACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip
)
r2_hip_res_LMM <- rsquared(hip_res_LMM)

# For vertical peak ground reaction force
ankle_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + I(pVACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = ankle
)
r2_ankle_vert_LMM <- rsquared(ankle_vert_LMM)

back_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + I(pVACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back
)
r2_back_vert_LMM <- rsquared(back_vert_LMM)

hip_vert_LMM <- lme(
  fixed = pVGRF_N ~ pVACC_g + I(pVACC_g^2) + body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip
)
r2_hip_vert_LMM <- rsquared(hip_vert_LMM)

# 4. Leave-one-out cross-validation ---------------------------------------

# For resultant peak ground reaction force
fix_eff    <- pRGRF_N ~ pRACC_g + I(pRACC_g^2) + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_res_LMM <- do.call(rbind, (lapply(unique(ankle$ID), cross_validate_mixed_model, df = ankle)))
# Back
LOOCV_back_res_LMM <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))
# Hip
LOOCV_hip_res_LMM <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

# For vertical peak ground reaction force
fix_eff    <- pVGRF_N ~ pVACC_g + I(pVACC_g^2) + body_mass
rand_eff   <- ~ 1 | ID
# Ankle
LOOCV_ankle_vert_LMM <- do.call(rbind, (lapply(unique(ankle$ID), cross_validate_mixed_model, df = ankle)))
# Back
LOOCV_back_vert_LMM <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))
# Hip
LOOCV_hip_vert_LMM <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

# 5. Bland-Altman plots ---------------------------------------------------

# For resultant peak ground reaction force
# Ankle
ankle_res_BA_plot <- get_BA_plot(LOOCV_ankle_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Back
back_res_BA_plot <- get_BA_plot(LOOCV_back_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Hip
hip_res_BA_plot <- get_BA_plot(LOOCV_hip_res_LMM, "pRGRF_N", "pRGRF_N_predicted")

# For vertical peak ground reaction force
# Ankle
ankle_vert_BA_plot <- get_BA_plot(LOOCV_ankle_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Back
back_vert_BA_plot <- get_BA_plot(LOOCV_back_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Hip
hip_vert_BA_plot <- get_BA_plot(LOOCV_hip_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")

### Linear regressions to identify proportional bias
# For resultant peak ground reaction force
# Ankle
LOOCV_ankle_res_LMM$diff <- LOOCV_ankle_res_LMM$pRGRF_N - LOOCV_ankle_res_LMM$pRGRF_N_predicted
LOOCV_ankle_res_LMM$mean <- (LOOCV_ankle_res_LMM$pRGRF_N + LOOCV_ankle_res_LMM$pRGRF_N_predicted) / 2
ankle_res_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_ankle_res_LMM)
summary(ankle_res_BA_plot_LR)
# Back
LOOCV_back_res_LMM$diff <- LOOCV_back_res_LMM$pRGRF_N - LOOCV_back_res_LMM$pRGRF_N_predicted
LOOCV_back_res_LMM$mean <- (LOOCV_back_res_LMM$pRGRF_N + LOOCV_back_res_LMM$pRGRF_N_predicted) / 2
back_res_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_res_LMM)
summary(back_res_BA_plot_LR)
# Hip
LOOCV_hip_res_LMM$diff <- LOOCV_hip_res_LMM$pRGRF_N - LOOCV_hip_res_LMM$pRGRF_N_predicted
LOOCV_hip_res_LMM$mean <- (LOOCV_hip_res_LMM$pRGRF_N + LOOCV_hip_res_LMM$pRGRF_N_predicted) / 2
hip_res_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_res_LMM)
summary(hip_res_BA_plot_LR)

# For vertical peak ground reaction force
# Ankle
LOOCV_ankle_vert_LMM$diff <- LOOCV_ankle_vert_LMM$pVGRF_N - LOOCV_ankle_vert_LMM$pVGRF_N_predicted
LOOCV_ankle_vert_LMM$mean <- (LOOCV_ankle_vert_LMM$pVGRF_N + LOOCV_ankle_vert_LMM$pVGRF_N_predicted) / 2
ankle_vert_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_ankle_vert_LMM)
summary(ankle_vert_BA_plot_LR)
# Back
LOOCV_back_vert_LMM$diff <- LOOCV_back_vert_LMM$pVGRF_N - LOOCV_back_vert_LMM$pVGRF_N_predicted
LOOCV_back_vert_LMM$mean <- (LOOCV_back_vert_LMM$pVGRF_N + LOOCV_back_vert_LMM$pVGRF_N_predicted) / 2
back_vert_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_vert_LMM)
summary(back_vert_BA_plot_LR)
# Hip
LOOCV_hip_vert_LMM$diff <- LOOCV_hip_vert_LMM$pVGRF_N - LOOCV_hip_vert_LMM$pVGRF_N_predicted
LOOCV_hip_vert_LMM$mean <- (LOOCV_hip_vert_LMM$pVGRF_N + LOOCV_hip_vert_LMM$pVGRF_N_predicted) / 2
hip_vert_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_vert_LMM)
summary(hip_vert_BA_plot_LR)

# Check whether bias is statisticaly different than 0
# For resultant peak ground reaction force
# Ankle
t.test(LOOCV_ankle_res_LMM$diff, mu = 0)
# Back
t.test(LOOCV_back_res_LMM$diff, mu = 0)
# Hip
t.test(LOOCV_hip_res_LMM$diff, mu = 0)

# For vertical peak ground reaction force
# Ankle
t.test(LOOCV_ankle_vert_LMM$diff, mu = 0)
# Back
t.test(LOOCV_back_vert_LMM$diff, mu = 0)
# Hip
t.test(LOOCV_hip_vert_LMM$diff, mu = 0)

# 6. Indices of accuracy --------------------------------------------------

# For resultant peak ground reaction force
# Ankle
ankle_res_accuracy <- accuracy_indices(LOOCV_ankle_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Back
back_res_accuracy <- accuracy_indices(LOOCV_back_res_LMM, "pRGRF_N", "pRGRF_N_predicted")
# Hip
hip_res_accuracy <- accuracy_indices(LOOCV_hip_res_LMM, "pRGRF_N", "pRGRF_N_predicted")

# For vertical peak ground reaction force
# Ankle
ankle_vert_accuracy <- accuracy_indices(LOOCV_ankle_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Back
back_vert_accuracy <- accuracy_indices(LOOCV_back_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")
# Hip
hip_vert_accuracy <- accuracy_indices(LOOCV_hip_vert_LMM, "pVGRF_N", "pVGRF_N_predicted")

# 7. ANOVA ----------------------------------------------------------------
# ** 7.1. Resultant GRF ---------------------------------------------------
# **** 7.1.1. Building data frame -----------------------------------------

## Predicted pRGRF
ankle_res_pred <- LOOCV_ankle_res_LMM %>% 
  select(ID, speed, pRGRF_N_predicted) %>% 
  spread(key = speed, value = pRGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = ankle
  )

back_res_pred <- LOOCV_back_res_LMM %>% 
  select(ID, speed, pRGRF_N_predicted) %>% 
  spread(key = speed, value = pRGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back
  )

hip_res_pred <- LOOCV_hip_res_LMM %>% 
  select(ID, speed, pRGRF_N_predicted) %>% 
  spread(key = speed, value = pRGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip
  )

res_pred_df <- ankle_res_pred %>% 
  full_join(back_res_pred, by = c("ID", "speed")) %>% 
  full_join(hip_res_pred, by = c("ID", "speed")) %>% 
  na.omit()

## Actual pRGRF
ankle_res_actual <- LOOCV_ankle_res_LMM %>% 
  select(ID, speed, pRGRF_N) %>% 
  spread(key = speed, value = pRGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_ankle
  )

back_res_actual <- LOOCV_back_res_LMM %>% 
  select(ID, speed, pRGRF_N) %>% 
  spread(key = speed, value = pRGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_back
  )

hip_res_actual <- LOOCV_hip_res_LMM %>% 
  select(ID, speed, pRGRF_N) %>% 
  spread(key = speed, value = pRGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_hip
  )

res_actual_df <- ankle_res_actual %>% 
  full_join(back_res_actual, by = c("ID", "speed")) %>% 
  full_join(hip_res_actual, by = c("ID", "speed")) %>% 
  na.omit()

res_actual_df <- res_actual_df %>% 
  mutate(actual = (actual_ankle + actual_back + actual_hip) / 3) %>% 
  select(ID, speed, actual) 

## Merge predicted and actual data frames
res_ANOVA_df <- res_pred_df %>% 
  full_join(res_actual_df, by = c("ID", "speed")) %>% 
  gather(
    ankle, back, hip, actual,
    key = "group",
    value = "pRGRF"
  )
res_ANOVA_df$ID    <- as.factor(res_ANOVA_df$ID)
res_ANOVA_df$speed <- as.factor(res_ANOVA_df$speed)
res_ANOVA_df$group <- as.factor(res_ANOVA_df$group)

# **** 7.1.2. ANOVA -------------------------------------------------------

res_ANOVA <- ezANOVA(
  data     = res_ANOVA_df,
  dv       = pRGRF,
  wid      = ID,
  within   = .(speed, group),
  detailed = TRUE,
  type     = 3
)

# Post hoc (Holm)
res_ANOVA_df$speed_group <- interaction(res_ANOVA_df$speed, res_ANOVA_df$group)

res_posthoc <- pairwise.t.test(res_ANOVA_df$pRGRF, res_ANOVA_df$speed_group, paired = TRUE, p.adjust.method = "holm")

# By speed
# 2 km/h
res_ANOVA_s2 <- ezANOVA(
  data     = res_ANOVA_df %>% filter(speed == 2),
  dv       = pRGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 3 km/h
res_ANOVA_s3 <- ezANOVA(
  data     = res_ANOVA_df %>% filter(speed == 3),
  dv       = pRGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 4 km/h
res_ANOVA_s4 <- ezANOVA(
  data     = res_ANOVA_df %>% filter(speed == 4),
  dv       = pRGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 5 km/h
res_ANOVA_s5 <- ezANOVA(
  data     = res_ANOVA_df %>% filter(speed == 5),
  dv       = pRGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 6 km/h
res_ANOVA_s6 <- ezANOVA(
  data     = res_ANOVA_df %>% filter(speed == 6),
  dv       = pRGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# ** 7.2. Vertical GRF ----------------------------------------------------
# **** 7.2.1. Building data frame -----------------------------------------

## Predicted pVGRF
ankle_vert_pred <- LOOCV_ankle_vert_LMM %>% 
  select(ID, speed, pVGRF_N_predicted) %>% 
  spread(key = speed, value = pVGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = ankle
  )

back_vert_pred <- LOOCV_back_vert_LMM %>% 
  select(ID, speed, pVGRF_N_predicted) %>% 
  spread(key = speed, value = pVGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back
  )

hip_vert_pred <- LOOCV_hip_vert_LMM %>% 
  select(ID, speed, pVGRF_N_predicted) %>% 
  spread(key = speed, value = pVGRF_N_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip
  )

vert_pred_df <- ankle_vert_pred %>% 
  full_join(back_vert_pred, by = c("ID", "speed")) %>% 
  full_join(hip_vert_pred, by = c("ID", "speed")) %>% 
  na.omit()

## Actual pVGRF
ankle_vert_actual <- LOOCV_ankle_vert_LMM %>% 
  select(ID, speed, pVGRF_N) %>% 
  spread(key = speed, value = pVGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_ankle
  )

back_vert_actual <- LOOCV_back_vert_LMM %>% 
  select(ID, speed, pVGRF_N) %>% 
  spread(key = speed, value = pVGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_back
  )

hip_vert_actual <- LOOCV_hip_vert_LMM %>% 
  select(ID, speed, pVGRF_N) %>% 
  spread(key = speed, value = pVGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_hip
  )

vert_actual_df <- ankle_vert_actual %>% 
  full_join(back_vert_actual, by = c("ID", "speed")) %>% 
  full_join(hip_vert_actual, by = c("ID", "speed")) %>% 
  na.omit()

vert_actual_df <- vert_actual_df %>% 
  mutate(actual = (actual_ankle + actual_back + actual_hip) / 3) %>% 
  select(ID, speed, actual) 

## Merge predicted and actual data frames
vert_ANOVA_df <- vert_pred_df %>% 
  full_join(vert_actual_df, by = c("ID", "speed")) %>% 
  gather(
    ankle, back, hip, actual,
    key = "group",
    value = "pVGRF"
  )
vert_ANOVA_df$ID    <- as.factor(vert_ANOVA_df$ID)
vert_ANOVA_df$speed <- as.factor(vert_ANOVA_df$speed)
vert_ANOVA_df$group <- as.factor(vert_ANOVA_df$group)

# **** 7.2.2. ANOVA -------------------------------------------------------

vert_ANOVA <- ezANOVA(
  data     = vert_ANOVA_df,
  dv       = pVGRF,
  wid      = ID,
  within   = .(speed, group),
  detailed = TRUE,
  type     = 3
)

# Post hoc (Holm)
vert_ANOVA_df$speed_group <- interaction(vert_ANOVA_df$speed, vert_ANOVA_df$group)

vert_posthoc <- pairwise.t.test(vert_ANOVA_df$pVGRF, vert_ANOVA_df$speed_group, paired = TRUE, p.adjust.method = "holm")

# By speed
# 2 km/h
vert_ANOVA_s2 <- ezANOVA(
  data     = vert_ANOVA_df %>% filter(speed == 2),
  dv       = pVGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 3 km/h
vert_ANOVA_s3 <- ezANOVA(
  data     = vert_ANOVA_df %>% filter(speed == 3),
  dv       = pVGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 4 km/h
vert_ANOVA_s4 <- ezANOVA(
  data     = vert_ANOVA_df %>% filter(speed == 4),
  dv       = pVGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 5 km/h
vert_ANOVA_s5 <- ezANOVA(
  data     = vert_ANOVA_df %>% filter(speed == 5),
  dv       = pVGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 6 km/h
vert_ANOVA_s6 <- ezANOVA(
  data     = vert_ANOVA_df %>% filter(speed == 6),
  dv       = pVGRF,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)