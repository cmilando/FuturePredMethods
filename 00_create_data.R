# 
# Create the synthetic data 
#

library(eesim)
library(dlnm)
library(splines)
source('crossbasis_local.R')

set.seed(123)
N_REPS = 500

sim_chicago <- create_sims(n_reps = N_REPS, n = 365 * 7, central = 20, sd = 7,
                           exposure_type = "continuous", 
                           exposure_trend = "cos1",
                           exposure_amp = -.6, 
                           average_outcome = 50,
                           outcome_trend = "cos1", 
                           outcome_amp = 0.5, 
                           rr = 1.02, 
                           start.date = "1996-01-01")
# now choose one value from each one
obs_l <- lapply(1:(365*7), function(i) {
  
  draw = sample(1:N_REPS, 1)
  sim_chicago[[draw]][i, ]
})

obs <- do.call(rbind,obs_l)
head(obs)
colnames(obs)[2] <- 'tmean'

## ----
# code to test that it works
# head(obs)
# 
# plot(y = obs$x, x = obs$date)
# plot(y = obs$outcome, x = obs$date)
# 
# obs$date <- as.Date(obs$date, format="%Y-%m-%d")
# 
# argvar <- list(fun="ns", knots = quantile(obs$x,c(10,75,90)/100, na.rm=T))
# 
# maxlag <- 21
# 
# arglag <- list(fun="ns", knots=logknots(maxlag, nk=3))
# 
# # *******
# round_by = 1
# 
# Tmean_buffer = 5
# 
# x_for_basis <- seq(round(min(obs$x) - Tmean_buffer, round_by), 
#                    round(max(obs$x) + Tmean_buffer, round_by), by = 0.1)
# 
# cb <- crossbasis_local(x = x_for_basis, 
#                        x_to_match = obs$x, round_by = 1,
#                        lag =  maxlag, argvar = argvar, arglag = arglag) 
# 
# cb_mat <- as.data.frame(as.matrix(cb))
# 
# names(cb_mat) <- paste0("cb",names(cb_mat))
# 
# obs2 <- cbind(obs, cb_mat)
# head(obs2)
# 
# formula_str = paste('outcome ~ ns(date,df=round(8*length(date)/365.25)) +',
#                     paste(names(cb_mat), collapse = " + "))
# 
# f1 <- as.formula(formula_str)
# 
# 
# m <- glm(f1, data=obs2, family=quasipoisson)
# 
# plot(crosspred(cb, m), 'overall')
# 
