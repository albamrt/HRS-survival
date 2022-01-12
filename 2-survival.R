
# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('ggplot2', 'kableExtra', 'prodlim', 'survival', 'cmprsk')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

###############################################################################
###############################################################################
### Table 3.3: Absolute and relative frequency of the two events 
### (death and liver transplant) and censored observations.
###############################################################################
###############################################################################

tab <- data_basal %>%
  group_by(event) %>%
  summarise(n = n()) %>%
  mutate('Percent' = round((n / sum(n))*100, 2))

kable(as.data.frame(tab), 
      col.names = c("Event", "n", "%"), 
      booktabs = T,  
      caption = "Absolute and relative frequency of the two events (death and liver transplant) and censored observations.") %>% kable_styling(latex_options = "HOLD_position")

###############################################################################
###############################################################################
### Figure 3.4: Comparison of the Kaplan-Meier and cumulative incidence function estimates.
###############################################################################
###############################################################################

KM_fit_death <- survfit(Surv(time = time, event = LT2 == 1) ~ 1, data = data_basal)
KM_fit_trans <- survfit(Surv(time = time, event = LT2 == 2) ~ 1, data = data_basal)
KM_fit_both <- survfit(Surv(time = time, event = LT2 > 0) ~ 1, data = data_basal)

cif <- with(data_basal, cuminc(ftime = time, fstatus = event, cencode = 'Censored'))
cif_both <- with(mutate(data_basal, 
                        event_both = ifelse(event %in% c('Dead', 'Transplant'), 'DeadOrTransplant', 'Censored')),
                 cuminc(ftime = time, fstatus = event_both, 
                        cencode = 'Censored'))

plot.data <- rbind(data.frame(time = KM_fit_death$time, est = 1 - KM_fit_death$surv, 
                              event = 'KM estimation of the cumulative hazard of death (1 - KM)',
                              which.est = '1 - KM', which.event = 'Death'),
                   data.frame(time = KM_fit_trans$time, est = 1 - KM_fit_trans$surv, 
                              event = 'KM estimation of the cumulative hazard of transplant (1 - KM)',
                              which.est = '1 - KM', which.event = 'Transplant'),
                   data.frame(time = KM_fit_both$time, est = 1 - KM_fit_both$surv, 
                              event = 'KM estimation of the cumulative hazard of both (1 - KM)',
                              which.est = '1 - KM', which.event = 'Death or transplant'),
                   data.frame(time = cif$`1 Dead`['time'], est = cif$`1 Dead`['est'], 
                              event = 'Cumulative incidence of death',
                              which.est = 'CIF', which.event = 'Death'),
                   data.frame(time = cif$`1 Transplant`['time'], est = cif$`1 Transplant`['est'],
                              event = 'Cumulative incidence of liver transplant',
                              which.est = 'CIF', which.event = 'Transplant'),
                   data.frame(time = cif_both$`1 DeadOrTransplant`['time'], 
                              est = cif_both$`1 DeadOrTransplant`['est'],
                              event = 'Cumulative incidence of liver transplant',
                              which.est = 'CIF', which.event = 'Death or transplant'))

plot.data$event <- factor(plot.data$event, 
                          levels = c('KM estimation of the cumulative hazard of death (1 - KM)',
                                     'KM estimation of the cumulative hazard of transplant (1 - KM)',
                                     'KM estimation of the cumulative hazard of both (1 - KM)',
                                     'Cumulative incidence of death',
                                     'Cumulative incidence of liver transplant',
                                     'Cumulative incidence of both'))

plot.data$which.event <- factor(plot.data$which.event, levels = c('Death', 'Transplant', 'Death or transplant'))

ggplot(plot.data, aes(time, est)) +
  geom_step(aes(linetype = which.est), size = 1) +
  labs(y = "", x = "Time") + 
  ylim(0, 1) +
  theme_minimal() + 
  theme(legend.title = element_blank(), legend.position=c(.1, 0.85)) +
  scale_color_grey() + 
  facet_wrap(vars(which.event))
