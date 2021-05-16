source("libraries.R")
source(here("r", "clean_prediction.R"))

### PART 1: Cleaning prediction output matrix example ----------
# settings for clean_prediction function - do not change -----------

state    <- "Tamil Nadu"
obs_days <- 387
t_pred   <- 150

# load SEIRfansy.predict() output prediction matrix -----------

d <- read_rds(here("data", "prediction_TN.rds"))
dim(d)
d[1:10, 1:10]

# run clean_prediction function - MAY TAKE A ~2 MINUTES ----------
# see `clean_prediction.R` script in `r` subfolder for code

# file.edit(here("r", "clean_prediction.R"))
d_clean <- d %>% clean_prediction(state = state, obs_days = obs_days, t_pred = t_pred)

d_clean

### PART 2: Generate table of predictions ----------
file.edit(here("r", "make_table.R"))
