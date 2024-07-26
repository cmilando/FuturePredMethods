### 
# ok so now, predict I gues

# lets look at the timeseries
ci.level = 0.95
z <- qnorm(1 - (1 - ci.level)/2)
pred_m <- predict(object = m, newdata = obs2, se.fit = T)
exp_est <- exp(pred_m$fit)
exp_ub <- exp(pred_m$fit + z * pred_m$se.fit)
exp_lb <- exp(pred_m$fit - z * pred_m$se.fit)

fit_df <- data.frame(exp_est, exp_ub, exp_lb)
print(head(fit_df[maxlag:(maxlag+100),]))

## right, because of cb, it starts 21 days below, so add 21 0s
# fit_df <- rbind(data.frame(exp_est = rep(NA, maxlag), 
#                            exp_ub= rep(NA, maxlag), 
#                            exp_lb= rep(NA, maxlag)), fit_df)

print(sum(exp_est, na.rm = T))

obs_comb_pres <- cbind(obs2, fit_df)
obs_comb_pres$scen = 'Present E[Y] (95% CI)'
obs_comb_pres$year = year(obs_comb_pres$date)
obs_comb_pres$month = month(obs_comb_pres$date)

col1 = 'lightblue'
col2 = 'blue'

YLIM <- c(150, 230)

library(tidyverse)
library(ggpubr)

p1 <- ggplot(obs_comb_pres %>% filter(year == 1996, month %in% 6:8)) +
  theme_pubr() + #coord_cartesian(ylim = YLIM) +
  scale_fill_manual(values = 'lightblue') +
  scale_color_manual(values = 'blue') +
  geom_line(aes(x = date, y = outcome), color = grey(0.15), linetype = '11') +
  geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub, fill = scen),  
              alpha = 0.5) +
  geom_line(aes(x = date, y = exp_est, color = scen), show.legend = T) +
  xlab(NULL) + ylab("Daily ED visits per 100k") +
  theme(legend.position.inside = c(0.15, 0.75),
        legend.position = 'inside',
        legend.title = element_blank()) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 125, fontface = 'bold',
           label = 'b. Use model to estimate time-series of ED visits in the present', hjust = 0,
           family = ff)

p1
ggsave("p1.png", width = 10/3, height = 5/2, dpi = 600)

library(patchwork)
p0 + p1
