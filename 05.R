# -----------------------------------------------------------------------------
# 
# Plot 6: bootstrapp diff
#
# -----------------------------------------------------------------------------

# SO THE END GOAL IS 
# MONTHLY EST + CONF INTERVALS BY REGION AND SSP

# WITHIN A REGION AND SSP
# (1) ADD UP ALL TIMESERIES, 
#     hmmmm can't do this because its not a number
#     >> this is where you could do the delta method thing?
#     >> the problem here is that this isn't weighted by population
#     >> maybe not a problem because they are all over 10,000
# (3) So, now you have a time-series for the whole region,
#     so averagea across days to get year-month avgs
# (2) 
# (3)

# this is an estimate of the risk difference

# put in terms of a % difference from baseline

# 
all_df <- diff_df
all_df$eSE <- (all_df$eCI_upper - all_df$eCI_lower) / (2 * z)

MONTHS_FAC = c(1:12)
names(MONTHS_FAC) = 1:12

all_df$months_fct <- MONTHS_FAC[all_df$month]

all_df$region = 'US'

all_df <- all_df %>%
  mutate(strata1 = paste0(region, ".", months_fct, ".", scen))

scale_calc <- all_df %>%
  filter(month == all_df$month[1]) %>%
  group_by(strata1) %>% tally()

scale_calc$n/31 # n YEARS

SCALING_FCT = 1 / 7

# SPLIT
sub1 <- split(all_df, f = all_df$strata1)

avg1 <- future_lapply(sub1, function(x) {
  #
  #x = sub1[[1]]
  
  # first, remeove any na
  x <- x[!is.na(x$eEst),]
  
  NBOOT <- 250
  mat_ts <- matrix(nrow = nrow(x), ncol = NBOOT)
  
  for(i in 1:nrow(x)) {
    mat_ts[i, ] <- rnorm(NBOOT, mean = x$eEst[i], sd = x$eSE[i])
  }
  
  row_sums = as.matrix(apply(mat_ts, 2, sum) * SCALING_FCT)
  
  # Calculate empirical confidence intervals
  eCI_lower <- apply(row_sums, 2, quantile, probs = 0.025, na.rm = T)
  eEst <- apply(row_sums, 2, quantile, probs = 0.5, na.rm = T)
  eCI_upper <- apply(row_sums, 2, quantile, probs = 0.975, na.rm = T)
  
  diff_df <- data.frame(strata = x$strata1[1],
                        eCI_lower,
                        eEst,
                        eCI_upper)
  diff_df
}, future.seed = T)


avg1_df <- do.call(rbind, avg1)
head(avg1_df)
#saveRDS(avg1_df, 'avg1_df.RDS')

xx <- do.call(rbind, strsplit(avg1_df$strata, ".", fixed = T))

avg1_df <- cbind(avg1_df, xx)

colnames(avg1_df)[5:7] <- c('region','month', 'ssp')
head(avg1_df)

avg1_df$month_int <- as.integer(avg1_df$month)
head(avg1_df)

## NB: in the actual code, baseline rate is calculated from data
## and varies by month 
# baselinerate <- 100

p5 <- ggplot(avg1_df) + theme_classic2() +
  geom_hline(yintercept = 0, linetype = '41') +
  geom_ribbon(aes(x = factor(month, levels = paste0(1:12), ordered = T), 
                  ymin = eCI_lower, ymax = eCI_upper,
                  fill = ssp, group = ssp), alpha = 0.25) +
  scale_x_discrete(labels = c("Jan", "", "", "Apr","", "",
                                "July","", "", "Oct","", "")) +
  coord_cartesian(clip = "off", ylim = c(0, 80)) +
  geom_line(aes(x = factor(month, levels = paste0(1:12), ordered = T), y = eEst, color = ssp, group = ssp)) +
  geom_point(aes(x = factor(month, levels = paste0(1:12), ordered = T), y = eEst, color = ssp, group = ssp),
             shape = 21) +
  scale_color_manual(values = viridis::magma(10)[5]) +
  scale_fill_manual(values = viridis::magma(10)[5]) +
  xlab(NULL) +
  ylab("Monthly Sum of ED Visits per 100k") +
  annotate(geom = 'text',
           x = 1,
           y = 80, fontface = 'bold',
           family = ff,
           label = 'f. Bootstrap the net change and\n eCI by month',
           hjust = 0)  +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())
  