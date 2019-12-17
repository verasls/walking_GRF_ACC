# Load packages and functions ---------------------------------------------

library(tidyverse)
library(here)
library(nlme)
library(piecewiseSEM)
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
  data = back_LR
)
rsquared(back_res_LR_LMM)

# 3. Leave-one-out cross validation ---------------------------------------

# For resultant peak loading rate
fix_eff    <- pRLR_Ns ~ pRATR_gs + I(pRATR_gs^2) + body_mass + pRATR_gs:body_mass
rand_eff   <- ~ 1 | ID
# Back
LOOCV_back_res_LR_LMM <- do.call(rbind, (lapply(unique(back_LR$ID), cross_validate_mixed_model, df = back_LR)))

# 4. Bland-Altman plots ---------------------------------------------------

# For resultant peak loading rate
# Back
back_res_LR_BA_plot <- get_BA_plot(LOOCV_back_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")

# 5. Indices of accuracy --------------------------------------------------

# For resultant peak loading rate
# Back
back_res_LR_accuracy <- accuracy_indices(LOOCV_back_res_LR_LMM, "pRLR_Ns", "pRLR_Ns_predicted")