# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
source(here("R", "BMI_categories.R"))


# Read and prepare data ---------------------------------------------------

# For vertical ground reaction force
LOOCV_ankle_vert <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_vert.csv")
LOOCV_back_vert  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_vert.csv")
LOOCV_hip_vert   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_vert.csv")
# For resultant ground reaction force
LOOCV_ankle_res <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_ankle_res.csv")
LOOCV_back_res  <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_back_res.csv")
LOOCV_hip_res   <- read_csv("~/Dropbox/Projects/walking_GRF_ACC/LOOCV_hip_res.csv")

# Classify BMI

# Apply this step at the beggining of analysis.R to avoid repetition
# Probably the report will need some adjustments

LOOCV_ankle_vert <- BMI_categories(LOOCV_ankle_vert)
LOOCV_back_vert  <- BMI_categories(LOOCV_back_vert)
LOOCV_hip_vert   <- BMI_categories(LOOCV_hip_vert)
LOOCV_ankle_res  <- BMI_categories(LOOCV_ankle_res)
LOOCV_back_res   <- BMI_categories(LOOCV_back_res)
LOOCV_hip_res    <- BMI_categories(LOOCV_hip_res)

# Actual pVGRF vs pVACC ---------------------------------------------------

# Ankle
ankle_pVGRF_pVACC__plot <- ggplot(data = LOOCV_ankle_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat))

# Back
back_pVGRF_pVACC_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat))

# Hip
hip_pVGRF_pVACC_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N, shape = BMI_cat))

# Actual pRGRF vs pRACC ---------------------------------------------------

# Ankle
ankle_pRGRF_pRACC_plot <- ggplot(data = LOOCV_ankle_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat))

# Back
back_pRGRF_pRACC_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat))

# Hip
hip_pRGRF_pRACC_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N, shape = BMI_cat))

# Predicted pVGRF vs pVACC ---------------------------------------------------

# Ankle
ankle_pred_pVGRF_pVACC_plot <- ggplot(data = LOOCV_ankle_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N_predicted, shape = BMI_cat))

# Back
back_pred_pVGRF_pVACC_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N_predicted, shape = BMI_cat))

# Hip
hip_pred_pVGRF_pVACC_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVACC_g, y = pVGRF_N_predicted, shape = BMI_cat))

# Predicted pRGRF vs pRACC ---------------------------------------------------

# Ankle
ankle_pred_pRGRF_pRACC_plot <- ggplot(data = LOOCV_ankle_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N_predicted, shape = BMI_cat))

# Back
back_pred_pRGRF_pRACC_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N_predicted, shape = BMI_cat))

# Hip
hip_pred_pRGRF_pRACC_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRACC_g, y = pRGRF_N_predicted, shape = BMI_cat))

# Actual vs Predicted pGRF ------------------------------------------------

## Ankle
# Vertical
ankle_pVGRF_plot <- ggplot(data = LOOCV_ankle_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted))

# Resultant
ankle_pRGRF_plot <- ggplot(data = LOOCV_ankle_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted))

## Back
# Vertical
back_pVGRF_plot <- ggplot(data = LOOCV_back_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted))

# Resultant
back_pRGRF_plot <- ggplot(data = LOOCV_back_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted))

## Hip
# Vertical
hip_pVGRF_plot <- ggplot(data = LOOCV_hip_vert) +
  geom_point(mapping = aes(x = pVGRF_N, y = pVGRF_N_predicted))

# Resultant
hip_pRGRF_plot <- ggplot(data = LOOCV_hip_res) +
  geom_point(mapping = aes(x = pRGRF_N, y = pRGRF_N_predicted))


# Merge data frames of the 3 placements and try a plot including all placements
# differentiated by shape