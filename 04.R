# -----------------------------------------------------------------------------
# 
# Plot 5: bootstrap total diffs
#
# -----------------------------------------------------------------------------

# ok now bootstrap
library(pbapply)
library(future)
library(future.apply)
plan(multisession)
n_bootstrap <- 250

# make a comb_mat to sample from
# this should be length(dt) x 3000
# 8 is the number of non-rep columns
cdiff2 <- cbind(diff1, diff2[, 8:(250-1)], diff3[, 8:(250-1)])
cdiff2_mat <- as.matrix(cdiff2[, 8:ncol(cdiff2)])

# Initialize matrix to store the differences
differences <- matrix(NA, nrow = n_bootstrap, ncol = length(present_estimates))

# Sample from the combined future distribution for each time point
sample1 <- function(x) sample(x, 1)

# Generate bootstrap samples and calculate differences
differences <- pbsapply(1:n_bootstrap, function(i) apply(cdiff2_mat, 1, sample1),
                        cl = 'future', future.seed=TRUE)

# Calculate empirical confidence intervals
eCI_lower <- apply(differences, 1, quantile, probs = 0.025, na.rm = T)
eEst <- apply(differences, 1, quantile, probs = 0.5, na.rm = T)
eCI_upper <- apply(differences, 1, quantile, probs = 0.975, na.rm = T)

diff_df <- data.frame(date = obs_comb_pres$date, 
                      year = obs_comb_pres$year,
                      month = obs_comb_pres$month,
                      eCI_lower,
                      eEst,
                      eCI_upper)

diff_df$scen = '245_* (95% eCI)'

p4b <- ggplot(diff_df %>% filter(year == 1996, month %in% 6:8)) +
  theme_pubr() + 
  geom_ribbon(aes(x = date, ymin = eCI_lower, ymax = eCI_upper, fill = scen), 
              alpha = 0.5) +
  scale_fill_manual(values = c('lightgreen')) +
  scale_color_manual(values = c('forestgreen')) +
  geom_line(aes(x = date, y = eEst, color = scen), show.legend = T) +
  xlab(NULL) + ylab("Change in Daily ED visits per 100k") +
  scale_y_continuous(breaks = c(0, 4, 8, 12)) +
  geom_hline(yintercept = 0, linetype = '41') +
  coord_cartesian(clip = 'off') +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 12.25, fontface = 'bold',family = ff,
           label = 'e. Bootstrap the combined differences\n ', hjust = 0) +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())
