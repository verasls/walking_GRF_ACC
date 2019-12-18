# Load packages and functions ---------------------------------------------

library(tidyverse)
library(here)
library(nlme)
library(piecewiseSEM)
library(ez)
source(here("R", "BMI_categories.R"))
source(here("R", "cross_validate_mixed_model.R"))
source(here("R", "accuracy_indices.R"))
source(here("R", "get_BA_plot.R"))

# 1. Prepare data ---------------------------------------------------------

anthopometric <- read_csv(here("data", "back_sec.csv")) %>% 
  select(ID, speed, height, BMI, sex, age)

back_LR <- read_csv(here("data", "max_rates_back_IMU.csv")) %>% 
  left_join(anthopometric, by = c("ID", "speed")) %>% 
  BMI_categories() %>% 
  select(
    ID, speed, body_mass, height, BMI, BMI_cat, sex, age, pVLR_Ns, pVATR_gs, pRLR_Ns, pRATR_gs
  )

hip_LR <- read_csv(here("data", "max_rates_hip_IMU.csv")) %>% 
  left_join(anthopometric, by = c("ID", "speed")) %>% 
  BMI_categories() %>% 
  select(
    ID, speed, body_mass, height, BMI, BMI_cat, sex, age, pVLR_Ns, pVATR_gs, pRLR_Ns, pRATR_gs
  )

# Explore -----------------------------------------------------------------

ggplot(data = back_LR) +
  geom_point(mapping = aes(x = pRATR_gs, y = pRLR_Ns, colour = BMI_cat))

ggplot(data = back_LR) +
  geom_point(mapping = aes(x = pVATR_gs, y = pVLR_Ns, colour = BMI_cat))
  

back_LR$speed <- as.factor(back_LR$speed)
ggplot(data = back_LR) +
  geom_point(mapping = aes(x = speed, y = pRLR_Ns, colour = BMI_cat))

back_LR %>% 
  filter(speed == 2) %>% 
  arrange(desc(pRLR_Ns))

# 2. Linear mixed models --------------------------------------------------

# For resultant peak loading rate
back_res_LR_LMM <- lme(
  fixed = pRLR_Ns ~ pRATR_gs + I(pRATR_gs^2) + body_mass + pRATR_gs:body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_LR,
  na.action = na.omit
)
rsquared(back_res_LR_LMM)

hip_res_LR_LMM <- lme(
  fixed = pRLR_Ns ~ pRATR_gs + I(pRATR_gs^2) + body_mass + pRATR_gs:body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_LR,
  na.action = na.omit
)
rsquared(hip_res_LR_LMM)

# For vertical peak loading rate
back_vert_LR_LMM <- lme(
  fixed = pVLR_Ns ~ pVATR_gs + I(pVATR_gs^2) + body_mass + pVATR_gs:body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back_LR,
  na.action = na.omit
)
rsquared(back_vert_LR_LMM)

hip_vert_LR_LMM <- lme(
  fixed = pVLR_Ns ~ pVATR_gs + I(pVATR_gs^2) + body_mass + pVATR_gs:body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip_LR,
  na.action = na.omit
)
rsquared(hip_vert_LR_LMM)

# 3. Leave-one-out cross validation ---------------------------------------

# For resultant peak loading rate
fix_eff    <- pRLR_Ns ~ pRATR_gs + I(pRATR_gs^2) + body_mass + pRATR_gs:body_mass
rand_eff   <- ~ 1 | ID
# Back
LOOCV_back_res_LR_LMM <- do.call(rbind, (lapply(unique(back_LR$ID), cross_validate_mixed_model, df = back_LR)))
# Hip
LOOCV_hip_res_LR_LMM <- do.call(rbind, (lapply(unique(hip_LR$ID), cross_validate_mixed_model, df = hip_LR)))

# For vertical peak loading rate
fix_eff    <- pVLR_Ns ~ pVATR_gs + I(pVATR_gs^2) + body_mass + pVATR_gs:body_mass
rand_eff   <- ~ 1 | ID
# Back
LOOCV_back_vert_LR_LMM <- do.call(rbind, (lapply(unique(back_LR$ID), cross_validate_mixed_model, df = back_LR)))
# Hip
LOOCV_hip_vert_LR_LMM <- do.call(rbind, (lapply(unique(hip_LR$ID), cross_validate_mixed_model, df = hip_LR)))

# Writing LOOCV data
LOOCV_back_res_LR <- LOOCV_back_res_LR_LMM %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, sex, age, pRLR_Ns, pRATR_gs, pRLR_Ns_predicted)
LOOCV_hip_res_LR <- LOOCV_hip_res_LR_LMM %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, sex, age, pRLR_Ns, pRATR_gs, pRLR_Ns_predicted)
LOOCV_back_vert_LR <- LOOCV_back_vert_LR_LMM %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, sex, age, pVLR_Ns, pVATR_gs, pVLR_Ns_predicted)
LOOCV_hip_vert_LR <- LOOCV_hip_vert_LR_LMM %>% 
  select(ID, speed, body_mass, BMI, BMI_cat, sex, age, pVLR_Ns, pVATR_gs, pVLR_Ns_predicted)

write_csv(LOOCV_back_res_LR, "~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res_LR.csv")
write_csv(LOOCV_hip_res_LR, "~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res_LR.csv")
write_csv(LOOCV_back_vert_LR, "~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert_LR.csv")
write_csv(LOOCV_hip_vert_LR, "~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert_LR.csv")

# 4. Bland-Altman plots ---------------------------------------------------

# For resultant peak loading rate
# Back
back_res_LR_BA_plot <- get_BA_plot(LOOCV_back_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")
# Back
hip_res_LR_BA_plot <- get_BA_plot(LOOCV_hip_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")

# For vertical peak loading rate
# Back
back_vert_LR_BA_plot <- get_BA_plot(LOOCV_back_vert_LR_LMM, "pVLR_Ns", "pVLR_Ns_predicted")
# Back
hip_vert_LR_BA_plot <- get_BA_plot(LOOCV_hip_vert_LR_LMM, "pVLR_Ns", "pVLR_Ns_predicted")

# Check whether bias is statisticaly different than 0
# For resultant peak loading rate
# Back
LOOCV_back_res_LR_LMM$diff <- LOOCV_back_res_LR_LMM$pRLR_Ns - LOOCV_back_res_LR_LMM$pRLR_Ns_predicted
LOOCV_back_res_LR_LMM$mean <- (LOOCV_back_res_LR_LMM$pRLR_Ns + LOOCV_back_res_LR_LMM$pRLR_Ns_predicted) / 2
t.test(LOOCV_back_res_LR_LMM$diff, mu = 0)
# Hip
LOOCV_hip_res_LR_LMM$diff <- LOOCV_hip_res_LR_LMM$pRLR_Ns - LOOCV_hip_res_LR_LMM$pRLR_Ns_predicted
LOOCV_hip_res_LR_LMM$mean <- (LOOCV_hip_res_LR_LMM$pRLR_Ns + LOOCV_hip_res_LR_LMM$pRLR_Ns_predicted) / 2
t.test(LOOCV_hip_res_LR_LMM$diff, mu = 0)

# For vertical peak loading rate
# Back
LOOCV_back_vert_LR_LMM$diff <- LOOCV_back_vert_LR_LMM$pVLR_Ns - LOOCV_back_vert_LR_LMM$pVLR_Ns_predicted
LOOCV_back_vert_LR_LMM$mean <- (LOOCV_back_vert_LR_LMM$pVLR_Ns + LOOCV_back_vert_LR_LMM$pVLR_Ns_predicted) / 2
t.test(LOOCV_back_vert_LR_LMM$diff, mu = 0)
# Hip
LOOCV_hip_vert_LR_LMM$diff <- LOOCV_hip_vert_LR_LMM$pVLR_Ns - LOOCV_hip_vert_LR_LMM$pVLR_Ns_predicted
LOOCV_hip_vert_LR_LMM$mean <- (LOOCV_hip_vert_LR_LMM$pVLR_Ns + LOOCV_hip_vert_LR_LMM$pVLR_Ns_predicted) / 2
t.test(LOOCV_hip_vert_LR_LMM$diff, mu = 0)

### Linear regressions to identify proportional bias
# For resultant peak loading rate
# Back
back_res_LR_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_res_LR_LMM)
summary(back_res_LR_BA_plot_LR)
# Hip
hip_res_LR_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_res_LR_LMM)
summary(hip_res_LR_BA_plot_LR)

# For vertical peak loading rate
# Back
back_vert_LR_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_vert_LR_LMM)
summary(back_vert_LR_BA_plot_LR)
# Hip
hip_vert_LR_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_vert_LR_LMM)
summary(hip_vert_LR_BA_plot_LR)

# 5. Indices of accuracy --------------------------------------------------

# For resultant peak loading rate
# Back
back_res_LR_accuracy <- accuracy_indices(LOOCV_back_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")
# Hip
hip_res_LR_accuracy <- accuracy_indices(LOOCV_hip_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")

# For vertical peak loading rate
# Back
back_vert_LR_accuracy <- accuracy_indices(LOOCV_back_vert_LR_LMM, "pVLR_Ns", "pVLR_Ns_predicted")
# Back
hip_vert_LR_accuracy <- accuracy_indices(LOOCV_hip_vert_LR_LMM, "pVLR_Ns", "pVLR_Ns_predicted")

# 6. ANOVA ----------------------------------------------------------------
# ** 6.1. Resultant LR ----------------------------------------------------
# **** 6.1.1. Build data frame --------------------------------------------

## Predicted pRLR
back_res_LR_pred <- LOOCV_back_res_LR %>% 
  select(ID, speed, pRLR_Ns_predicted) %>% 
  spread(key = speed, value = pRLR_Ns_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back
  )

hip_res_LR_pred <- LOOCV_hip_res_LR %>%
  select(ID, speed, pRLR_Ns_predicted) %>% 
  spread(key = speed, value = pRLR_Ns_predicted) %>%
  na.omit() %>%
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip
  )

res_pred_LR_df <- back_res_LR_pred %>% 
  full_join(hip_res_LR_pred, by = c("ID", "speed")) %>% 
  na.omit()

## Actual pRLR
back_res_actual_LR <- LOOCV_back_res_LR %>% 
  select(ID, speed, pRLR_Ns) %>% 
  spread(key = speed, value = pRLR_Ns) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_back
  )

hip_res_actual_LR <- LOOCV_hip_res_LR %>% 
  select(ID, speed, pRLR_Ns) %>% 
  spread(key = speed, value = pRLR_Ns) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_hip
  )

res_actual_LR_df <- back_res_actual_LR %>% 
  full_join(hip_res_actual_LR, by = c("ID", "speed")) %>% 
  na.omit()

res_actual_LR_df <- res_actual_LR_df %>% 
  mutate(actual = (actual_back + actual_hip) / 2) %>% 
  select(ID, speed, actual) 

## Merge predicted and actual data frames
res_ANOVA_LR_df <- res_pred_LR_df %>% 
  full_join(res_actual_LR_df, by = c("ID", "speed")) %>% 
  gather(
    back, hip, actual,
    key = "group",
    value = "pRLR"
  )
res_ANOVA_LR_df$ID    <- as.factor(res_ANOVA_LR_df$ID)
res_ANOVA_LR_df$speed <- as.factor(res_ANOVA_LR_df$speed)
res_ANOVA_LR_df$group <- as.factor(res_ANOVA_LR_df$group)

# Write data frame
write_csv(res_ANOVA_LR_df, "~/Dropbox/Projects/walking_GRF_ACC/res_ANOVA_LR_df.csv")

# **** 6.1.2. ANOVA -------------------------------------------------------

res_ANOVA_LR <- ezANOVA(
  data     = res_ANOVA_LR_df,
  dv       = pRLR,
  wid      = ID,
  within   = .(speed, group),
  detailed = TRUE,
  type     = 3
)

# Post hoc (Holm)
res_ANOVA_LR_df$speed_group <- interaction(res_ANOVA_LR_df$speed, res_ANOVA_LR_df$group)

res_posthoc_LR <- pairwise.t.test(res_ANOVA_LR_df$pRLR, res_ANOVA_LR_df$speed_group, paired = TRUE, p.adjust.method = "holm")

# By speed
# 2 km/h
res_ANOVA_LR_s2 <- ezANOVA(
  data     = res_ANOVA_LR_df %>% filter(speed == 2),
  dv       = pRLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)
s2_res <- res_ANOVA_LR_df %>% filter(speed == 2)
pairwise.t.test(s2_res$pRLR, s2_res$group, paired = TRUE, p.adjust.method = "holm")

# 3 km/h
res_ANOVA_LR_s3 <- ezANOVA(
  data     = res_ANOVA_LR_df %>% filter(speed == 3),
  dv       = pRLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 4 km/h
res_ANOVA_LR_s4 <- ezANOVA(
  data     = res_ANOVA_LR_df %>% filter(speed == 4),
  dv       = pRLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 5 km/h
res_ANOVA_LR_s5 <- ezANOVA(
  data     = res_ANOVA_LR_df %>% filter(speed == 5),
  dv       = pRLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 6 km/h
res_ANOVA_LR_s6 <- ezANOVA(
  data     = res_ANOVA_LR_df %>% filter(speed == 6),
  dv       = pRLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# ** 6.2. Vertical LR -----------------------------------------------------
# **** 6.2.1. Build data frame --------------------------------------------

## Predicted pVLR
back_vert_LR_pred <- LOOCV_back_vert_LR %>% 
  select(ID, speed, pVLR_Ns_predicted) %>% 
  spread(key = speed, value = pVLR_Ns_predicted) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back
  )

hip_vert_LR_pred <- LOOCV_hip_vert_LR %>%
  select(ID, speed, pVLR_Ns_predicted) %>% 
  spread(key = speed, value = pVLR_Ns_predicted) %>%
  na.omit() %>%
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip
  )

vert_pred_LR_df <- back_vert_LR_pred %>% 
  full_join(hip_vert_LR_pred, by = c("ID", "speed")) %>% 
  na.omit()

## Actual pVLR
back_vert_actual_LR <- LOOCV_back_vert_LR %>% 
  select(ID, speed, pVLR_Ns) %>% 
  spread(key = speed, value = pVLR_Ns) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_back
  )

hip_vert_actual_LR <- LOOCV_hip_vert_LR %>% 
  select(ID, speed, pVLR_Ns) %>% 
  spread(key = speed, value = pVLR_Ns) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = actual_hip
  )

vert_actual_LR_df <- back_vert_actual_LR %>% 
  full_join(hip_vert_actual_LR, by = c("ID", "speed")) %>% 
  na.omit()

vert_actual_LR_df <- vert_actual_LR_df %>% 
  mutate(actual = (actual_back + actual_hip) / 2) %>% 
  select(ID, speed, actual) 

## Merge predicted and actual data frames
vert_ANOVA_LR_df <- vert_pred_LR_df %>% 
  full_join(vert_actual_LR_df, by = c("ID", "speed")) %>% 
  gather(
    back, hip, actual,
    key = "group",
    value = "pVLR"
  )
vert_ANOVA_LR_df$ID    <- as.factor(vert_ANOVA_LR_df$ID)
vert_ANOVA_LR_df$speed <- as.factor(vert_ANOVA_LR_df$speed)
vert_ANOVA_LR_df$group <- as.factor(vert_ANOVA_LR_df$group)

# Write data frame
write_csv(vert_ANOVA_LR_df, "~/Dropbox/Projects/walking_GRF_ACC/vert_ANOVA_LR_df.csv")

# **** 6.2.2. ANOVA -------------------------------------------------------

vert_ANOVA_LR <- ezANOVA(
  data     = vert_ANOVA_LR_df,
  dv       = pVLR,
  wid      = ID,
  within   = .(speed, group),
  detailed = TRUE,
  type     = 3
)

# Post hoc (Holm)
vert_ANOVA_LR_df$speed_group <- interaction(vert_ANOVA_LR_df$speed, vert_ANOVA_LR_df$group)

vert_posthoc_LR <- pairwise.t.test(vert_ANOVA_LR_df$pVLR, vert_ANOVA_LR_df$speed_group, paired = TRUE, p.adjust.method = "holm")

# By speed
# 2 km/h
vert_ANOVA_LR_s2 <- ezANOVA(
  data     = vert_ANOVA_LR_df %>% filter(speed == 2),
  dv       = pVLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)
s2_vert <- vert_ANOVA_LR_df %>% filter(speed == 2)
pairwise.t.test(s2_vert$pVLR, s2_vert$group, paired = TRUE, p.adjust.method = "holm")

# 3 km/h
vert_ANOVA_LR_s3 <- ezANOVA(
  data     = vert_ANOVA_LR_df %>% filter(speed == 3),
  dv       = pVLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 4 km/h
vert_ANOVA_LR_s4 <- ezANOVA(
  data     = vert_ANOVA_LR_df %>% filter(speed == 4),
  dv       = pVLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 5 km/h
vert_ANOVA_LR_s5 <- ezANOVA(
  data     = vert_ANOVA_LR_df %>% filter(speed == 5),
  dv       = pVLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)

# 6 km/h
vert_ANOVA_LR_s6 <- ezANOVA(
  data     = vert_ANOVA_LR_df %>% filter(speed == 6),
  dv       = pVLR,
  wid      = ID,
  within   = group,
  detailed = TRUE,
  type     = 3
)