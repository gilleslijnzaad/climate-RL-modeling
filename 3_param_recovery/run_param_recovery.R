# SETUP
rm(list = ls())
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
mod_dir <- paste0(main_dir, "0_models/")
mod_name <- "2_LRN_discr_approx_stat"
sim <- new.env()
source(paste0(mod_dir, mod_name, ".R"), local = sim) # access functions using sim$fun()
sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils)
plot <- new.env()
source(paste0(util_dir, "plot_utils.R"), local = plot)
fitting <- new.env()
source(paste0(util_dir, "fit_utils.R"), local = fitting)
util <- new.env()
source(paste0(util_dir, "utils.R"), local = util)

# PARAM SETTINGS
params <- list(
  n_part = 50,
  n_trials = 30,
  LR_conf_group = 0.8,
  LR_disconf_group = 0.2,
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2,
  margin_group = 2
)

# RUN SIM
sim_dat <- sim$run(params)

# SAVE DATA
dat_dir <- paste0(current_dir, "1_run/")
dat_path <- paste0(dat_dir, "sim_dat_1.json")
free_params_pp <- c("LR_conf", "LR_disconf", "inv_temp", "initQF", "initQU")
sim_utils$save_sim_dat(params, sim_dat, dat_path, free_params_pp)

# OPTIONAL: ADD PARAMS TO DATA
# add initQF[] and initQU[] to json data
initQFs <- sim_dat$Q_F[which(sim_dat$trial == 1)]
initQUs <- sim_dat$Q_U[which(sim_dat$trial == 1)]
json_dat <- rjson::fromJSON(file = dat_path)
json_dat[["initQF"]] <- initQFs
json_dat[["initQU"]] <- initQUs
cmdstanr::write_stan_json(json_dat, file = dat_path)

# PLOT DATA
plot$sim_plots(sim_dat, params)