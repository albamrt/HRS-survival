
# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('kableExtra', 'survival', 'riskRegression', 'gridExtra')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

###############################################################################
###############################################################################
### Table 3.6: Time-dependent Cox cause-specific model regression with
### time-varying covariates for different endopoints.
###############################################################################
###############################################################################

# variables to be used:
retained <- vars_of_interest[!(vars_of_interest %in% c("lym", "lympc", "mono", "monopc", "neut", "imneut", "neutpc", "chol", "crp", "pao2","paco2"))]

# Cause-specific model for death:
competing_death_mod <- coxph(as.formula(paste0("Surv(start_time, end_time, event_numeric == 1) ~ ",
                                               paste(retained, collapse = '+'))),  
                             data =  data)

# Cause-specific model for transplant:
competing_transplant_mod <- coxph(as.formula(paste0("Surv(start_time, end_time, event_numeric == 2) ~ ",
                                                    paste(retained, collapse = '+'))),  data =  data)

# Cause-specific model for death or transplant:
competing_any_mod <- coxph(as.formula(paste0("Surv(start_time, end_time, event_numeric > 0) ~ ",
                                             paste(retained, collapse = '+'))),  data =  data)

# cbind the three models together:
timevar_compete_cs <- cbind(summary(competing_death_mod)$conf.int[,-2],
                            summary(competing_transplant_mod)$conf.int[,-2],
                            summary(competing_any_mod)$conf.int[,-2])
colnames(timevar_compete_cs)[colnames(timevar_compete_cs) %in% 'lower .95'] <- '2.5%'
colnames(timevar_compete_cs)[colnames(timevar_compete_cs) %in% 'upper .95'] <- '97.5%'
rownames(timevar_compete_cs) <- sapply(rownames(timevar_compete_cs), function(x) varlabels[paste(x)]) 
colnames(timevar_compete_cs) <- gsub('%', '\\\\%', colnames(timevar_compete_cs))

# print table:
kable(timevar_compete_cs, booktabs = T, digits = 2, escape = FALSE,
      caption = "Time-dependent Cox cause-specific model regression with time-varying covariates for different endpoints.") %>%
  add_header_above(c("", "Death" = 3, "Transplant" = 3, "Death or transplant" = 3)) %>%
  kable_styling(font_size = 7)


###############################################################################
###############################################################################
### Table 3.7: Time-dependent Fine and Gray's regression with time-varying 
### covariates for different endpoints.
###############################################################################
###############################################################################

# for each endpoint a dataset has to be built with the function 'finegray'

## finegray breaks data by all points where there is censorship:
 sort(unique(data_basal$time[data_basal$event == 'Censored'])) # this are the fgstart and fgstop intervals
## when for some intervals there are no focal events (deaths/transplants) the intervals are removed from the dataframe

# Fine-Gray model for death:
death_data <- finegray(Surv(time = start_time, time2 = end_time, 
                            event = relevel(event, ref = 'Censored')) ~ ., 
                       data = data, etype = 'Dead', id = idpatient, timefix = FALSE)
fg_death_fit <- coxph(as.formula(paste0("Surv(fgstart, fgstop, fgstatus) ~ ",
                                        paste(names(competing_death_mod$coefficients), collapse = '+'))),
                      weight = fgwt, data = death_data)

# Fine-Gray model for transplant:
transplant_data <- finegray(Surv(time = start_time, time2 = end_time,
                                 event = relevel(event, ref = 'Censored')) ~ ., 
                            data = data, etype = 'Transplant', id = idpatient, timefix = FALSE)
fg_transplant_fit <- coxph(as.formula(paste0("Surv(fgstart, fgstop, fgstatus) ~ ",
                                             paste(names(competing_death_mod$coefficients), collapse = '+'))),
                           weight = fgwt, data = transplant_data)

# Fine-Gray model for death and transplant:
data_both <- data
data_both$event <- as.factor(ifelse(data_both$event == 'Censored', 'Censored', 'Event'))
death_transplant_data <- finegray(Surv(time = start_time, time2 = end_time, event = relevel(event, ref = 'Censored')) ~ ., 
                                  data = data_both, etype = 'Event', id = idpatient, timefix = FALSE)
fg_both_fit <- coxph(as.formula(paste0("Surv(fgstart, fgstop, fgstatus) ~ ",
                                       paste(names(competing_death_mod$coefficients), collapse = '+'))),
                     weight = fgwt, data = death_transplant_data)

# cbind the three models:
compete_fg <- cbind(summary(fg_death_fit)$conf.int[,-2],
                    summary(fg_transplant_fit)$conf.int[,-2],
                    summary(fg_both_fit)$conf.int[,-2])
colnames(compete_fg)[colnames(compete_fg) %in% 'lower .95'] <- '2.5%'
colnames(compete_fg)[colnames(compete_fg) %in% 'upper .95'] <- '97.5%'
rownames(compete_fg) <- sapply(rownames(compete_fg), function(x) varlabels[paste(x)]) 
colnames(compete_fg) <- gsub('%', '\\\\%', colnames(compete_fg))

# print table:
kable(compete_fg, booktabs = T, digits = 2, escape = FALSE,
      caption = "Time-dependent Fine and Gray's regression with time-varying covariates for different endpoints.") %>%
  add_header_above(c("", "Death" = 3, "Transplant" = 3, "Death or transplant" = 3)) %>%
  kable_styling(font_size = 7)


###############################################################################
###############################################################################
### Figure 3.5: Predicted cumulative baseline cause-specific (left) and 
### subdistribution (right) hazards for death stratified by response to 
### treatment (defined as a time-varying covariate).
###############################################################################
###############################################################################

comp.death.plot <- coxph(Surv(start_time, end_time, event_numeric == 1) ~ strata(response),  
                         data =  data)

comp.death.subdistr.plot <- coxph(Surv(fgstart, fgstop, fgstatus) ~ strata(response),
                                  weight = fgwt, data = death_data)


p1 <- basehaz(comp.death.plot) %>%
  mutate(Response = ifelse(strata == 'response=1', 'Response', 'No response')) %>%
  ggplot(aes(x = time, y = hazard, col = Response)) +
  geom_step() + 
  scale_color_grey() +
  labs(x = 'Time (days)', y = 'Cumulative cause-specific hazard', col = '') +
  theme(legend.position=c(0.85, 0.2))

p2 <- basehaz(comp.death.subdistr.plot) %>%
  mutate(Response = ifelse(strata == 'response=1', 'Response', 'No response')) %>%
  ggplot(aes(x = time, y = hazard, col = Response)) +
  geom_step() + 
  scale_color_grey() +
  labs(x = 'Time (days)', y = 'Cumulative subdistribution hazard', col = '') +
  theme(legend.position=c(0.85, 0.2))

grid.arrange(p1, p2, ncol = 2)
