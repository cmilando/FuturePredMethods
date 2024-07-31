# -----------------------------------------------------------------------------
# 
# Plot 4: bootstrap
# This step is not the problem
# -----------------------------------------------------------------------------

get_diff_bs <- function(dt, yr, month,
                        present_estimates, present_se,
                        future_estimates, future_se, future_scen) {
  
  n_bootstrap <- 250
  
  differences <- matrix(NA, nrow = n_bootstrap, ncol = length(future_estimates))
  
  for (i in 1:n_bootstrap) {
    
    # will give warnings because of maxlag
    
    # Sample from the present distribution for each time point
    present_sample <- rnorm(length(present_estimates) - (maxlag), 
                            mean = present_estimates[(maxlag+1):length(present_estimates)], 
                            sd = present_se[(maxlag+1):length(present_estimates)])
    
    # Sample from the combined future distribution for each time point
    future_sample <- rnorm(length(future_estimates) - (maxlag), 
                           mean = future_estimates[(maxlag+1):length(future_estimates)], 
                           sd = future_se[(maxlag+1):length(future_estimates)])
    
    # Calculate the differences
    differences[i, ] <- c(rep(NA, maxlag), future_sample - present_sample)
  }
  dim(differences)
  
  eCI_lower <- apply(differences, 2, quantile, probs = 0.025, na.rm = T)
  eEst <- apply(differences, 2, quantile, probs = 0.5, na.rm = T)
  eCI_upper <- apply(differences, 2, quantile, probs = 0.975, na.rm = T)
  
  diff_df <- data.frame(date = dt, 
                        year = yr,
                        month = month,
                        scen = future_scen,
                        eCI_lower,
                        eEst,
                        eCI_upper)
  
  differences_df <- data.frame(t(differences))
  diff_df <- cbind(diff_df, differences_df)
  
  return(diff_df)
  
}

present_estimates <- obs_comb_pres$exp_est
present_se <- (obs_comb_pres$exp_ub - obs_comb_pres$exp_lb) / (2 * z)

##
future_estimates <- obs_comb1$exp_est
future_se <- (obs_comb1$exp_ub - obs_comb1$exp_lb) / (2 * z)
future_scen <- '245_A - Present E[Y] (eCI 95%)'

diff1 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)
##
future_estimates <- obs_comb2$exp_est
future_se <- (obs_comb2$exp_ub - obs_comb2$exp_lb) / (2 * z)
future_scen <- '245_B - Present E[Y] (eCI 95%)'

diff2 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)
##
future_estimates <- obs_comb3$exp_est
future_se <- (obs_comb3$exp_ub - obs_comb3$exp_lb) / (2 * z)
future_scen <- '245_C - Present E[Y] (eCI 95%)'

diff3 <- get_diff_bs(obs_comb_pres$date, obs_comb_pres$year, obs_comb_pres$month,
                     present_estimates, present_se, 
                     future_estimates, future_se, future_scen)

comb_diff <- rbind(diff1, diff2, diff3)

p3b <- ggplot() +
  theme_pubr() + 
  xlab(NULL) + ylab("Change in Daily ED visits per 100k") +
  geom_ribbon(aes(x = date, ymin = eCI_lower, ymax = eCI_upper, fill = scen, group = scen), alpha = 0.5,
              data = comb_diff %>% filter(year == 1996, month %in% 6:8)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Greens")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Greens")) +
  geom_line(aes(x = date, y = eEst,  col= scen, group = scen),  
            data = comb_diff %>% filter(year == 1996, month %in% 6:8)) +
  coord_cartesian(clip = 'off') +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 12.25, fontface = 'bold',family = ff,
           label = 'd. Bootstrap differences by model,\nusing DLNM Est and SE', hjust = 0) +
  geom_hline(yintercept = 0, linetype = '41') +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())

p3b
