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

# Ok now delta method 4 times to get IRD and % diff from expected 
# Ok now delta method 4 times to get IRD and % diff from expected 
library(tidyverse)
library(future)
library(future.apply)
plan(multisession)
options(future.globals.maxSize = 1200*1024^2)

all_df <- diff_df

# SSP = '245'
# 
# this_df <- all_df %>% filter(scen == SSP)
all_df <- all_df %>%
  mutate(strata1 = paste0(month, ".", scen))
head(all_df)

# first, get the monthly mean
sub1 <- split(all_df, f = all_df$strata1)

avg1 <- future_lapply(sub1, function(x) {
  
  # 
  # x = sub1[[1]]
  
  # first, remeove any na
  x <- x[!is.na(x$eEst),]
  
  # then
  n_x <- nrow(x)
  gradient <- rep(1/n_x, times = n_x)
  
  # get variances
  ci.level = 0.95
  z <- qnorm(1 - (1 - ci.level)/2)
  se_x <- (x$eCI_upper - x$eCI_lower) / (2 * z)
  var_x <- se_x^2
  cov_x <- diag(var_x, nrow = n_x, ncol = n_x)
  
  # OPG
  combined_variance <- t(gradient) %*% cov_x %*% gradient
  
  # Standard error
  combined_se <- sqrt(combined_variance)
  
  # Print the combined standard error
  combined_se
  comb_mean = mean(x$eEst)
  comb_mean
  
  y <- data.frame(x = x$strata1[1],
                  y  = comb_mean,
                  ymin = comb_mean - z * combined_se,
                  ymax = comb_mean + z * combined_se)
  
  y
  
  # # 
  # ggplot(x[sample(1:nrow(x), size = 100),]) +
  #   geom_pointrange(aes(x = date, y = eEst, ymin = eCI_lower, ymax = eCI_upper)) +
  #   geom_pointrange(data = y,
  #                   mapping = aes(x = x, y =y, ymin = ymin, ymax = ymax),
  #                   color = 'red')
  
})

avg1_df <- do.call(rbind, avg1)
head(avg1_df)
#saveRDS(avg1_df, 'avg1_df.RDS')

xx <- do.call(rbind, strsplit(avg1_df$x, ".", fixed = T))

avg1_df <- cbind(avg1_df, xx)

colnames(avg1_df)[5:6] <- c('month', 'ssp')
head(avg1_df)

avg1_df$month_int <- as.integer(avg1_df$month)
head(avg1_df)

library(ggpubr)
library(lemon)
library(extrafont)
font_import()
loadfonts()

#popsize
popsize <- 100

p5 <- ggplot(avg1_df) + theme_classic2() +
  geom_hline(yintercept = 0, linetype = '41') +
  geom_ribbon(aes(x = as.integer(month), 
                  ymin = ymin/popsize * 100, ymax = ymax/popsize * 100,
                  fill = ssp, group = ssp), alpha = 0.25) +
  scale_x_continuous(labels = c("Jan", "Apr",
                              "July", "Oct"), breaks = c(1, 4, 7, 10)) +
  coord_cartesian(clip = "off", ylim = c(0, 3)) +
  geom_line(aes(x = as.integer(month), y = y/popsize * 100, color = ssp, group = ssp)) +
  geom_point(aes(x = as.integer(month), y = y/popsize * 100, color = ssp, group = ssp),
             shape = 21) +
  scale_color_manual(values = viridis::magma(10)[5]) +
  scale_fill_manual(values = viridis::magma(10)[5]) +
  #facet_rep_wrap(~reg_names, repeat.tick.labels = 'x') +
  xlab(NULL) +
  ylab("% Change in Daily ED Visit Rate (%)") +
  annotate(geom = 'text', 
           x = 1,
           y = 3, fontface = 'bold',
           family = ff,
           label = 'f. Use Delta method to estimate\n% change and SE by month', 
           hjust = 0)  +
  theme(legend.position.inside = c(0.15, 0.7),
        legend.position = 'inside',
        legend.title = element_blank())
  

p5


