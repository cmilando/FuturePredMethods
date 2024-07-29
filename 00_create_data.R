# -----------------------------------------------------------------------------
# 
# Create the synthetic data 
#
# -----------------------------------------------------------------------------
library(eesim)

set.seed(123)
N_REPS = 500

sim_data <- create_sims(n_reps = N_REPS, n = 365 * 7,
                           ##
                           central = 20, sd = 7,
                           exposure_type = "continuous",
                           exposure_trend = "cos1",
                           exposure_amp = -.3,
                           ##
                           average_outcome = 50,
                           outcome_trend = "cos1",
                           outcome_amp = 0.1,
                           rr = 1.05,
                           start.date = "1996-01-01")

lag_data <- create_sims(n_reps = N_REPS, n = 365 * 7,
                        ##
                        central = 20, sd = 7,
                        exposure_type = "continuous",
                        exposure_trend = "cos1",
                        exposure_amp = -.3,
                        ##
                        average_outcome = 25,
                        outcome_trend = "cos1",
                        outcome_amp = 0.1,
                        rr = 1.05,
                        start.date = "1996-01-01")


sim_data[[1]]

# now choose one value from each one
obs_l <- lapply(1:(365*7), function(i) {

  draw = sample(1:N_REPS, 1)
  x <- sim_data[[draw]][i, ]
  y <- lag_data[[draw]][max(i, i - 1), ]
  x$outcome <- x$outcome*0.2 + 75 + 
    y$outcome*0.2 +
    rnorm(nrow(x), mean = 0, sd = 5)
  x
})

obs <- do.call(rbind,obs_l)

colnames(obs)[2] <- 'tmean'





