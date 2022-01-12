# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('kableExtra', 'survival', 'mstate')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

tmat <- transMat(x = list(c(2, 3, 4), c(1, 3, 4), c(), c()), 
                 names = c('Non-responding', 'Responding', 'Transplant', 'Dead'))
tmat
#paths(tmat_competing)

# long format, each row will correspond to a transition for which a patient is at risk:
data <- data %>% 
  group_by(idpatient) %>%
  arrange(idpatient, visitdt) %>% # df must be ordered appropriately
  mutate(
    from = ifelse(creat>= 1.5, 'Non-responding', 'Responding'),
    to_event = ifelse(event == 'Dead', 'Dead',
                      ifelse(event == 'Transplant', 'Transplant',
                             data.table::shift(from, -1))))
data$to_event[is.na(data$to_event)] <- data$from[is.na(data$to_event)]

# create multi-state dataset:
data_multistate <- rbind(
  # repeat each row 4 times, one for each state,
  # so that all events can be represented for each time interval:
  mutate(data, to = 'Non-responding'),
  mutate(data, to = 'Responding'),
  mutate(data, to = 'Dead'), 
  mutate(data, to = 'Transplant')) %>% 
  arrange(idpatient, visitdt) %>% # df must be ordered appropriately
  group_by(idpatient) %>% # create new grouping variable
  mutate(
    status = ifelse((to == to_event) & (from != event), 1, 0))

# remove rows with from == to (always censored):
data_multistate <- data_multistate %>%
  filter(from != to)

###############################################################################
###############################################################################
### Figure 3.7: Aalen-Johansen estimations of transition probabilities to
### death and transplant, from responding and non-responding to treatment.
###############################################################################
###############################################################################

# variables to be used:
retained <- vars_of_interest[!(vars_of_interest %in% c("lym", "lympc", "mono", "monopc", "neut", "imneut", "neutpc", "chol", "crp", "pao2","paco2"))]

# add variable 'trans' with an if for each transition (one for each from-to combination):
data_multistate <- data_multistate %>%
  mutate(trans = case_when(
    from == 'Non-responding'  & to == 'Responding'    ~ 1,
    from == 'Non-responding'  & to == 'Transplant'    ~ 2,
    from == 'Non-responding'  & to == 'Dead'    ~ 3,
    from == 'Responding'  & to == 'Non-responding'    ~ 4,
    from == 'Responding'  & to == 'Transplant'    ~ 5,
    from == 'Responding'  & to == 'Dead'    ~ 6))

# Calculating the cumulative hazard for each transition:
cumhaz = coxph(Surv(start_time, end_time, status) ~ strata(trans), data = data_multistate, method = "breslow")

# simulate data:
simdatfit = msfit(cumhaz, trans = tmat)
simdatprob = probtrans(simdatfit, predt = 0, method = "aalen")

# prepare data to plot:
plot.data.trans <- data.frame(time = rep(simdatprob[[1]]$time, times = 4),
                              prob = c(simdatprob[[1]]$pstate3, simdatprob[[1]]$pstate4,
                                       simdatprob[[2]]$pstate3, simdatprob[[2]]$pstate4),
                              from = c(rep('Non-response', length(simdatprob[[1]]$pstate3)),
                                       rep('Response', length(simdatprob[[1]]$pstate3)),
                                       rep('Non-response', length(simdatprob[[1]]$pstate3)),
                                       rep('Response', length(simdatprob[[1]]$pstate3))),
                              outcome = c(rep('Death', length(simdatprob[[1]]$pstate3)),
                                          rep('Death', length(simdatprob[[1]]$pstate3)),
                                          rep('Transplant', length(simdatprob[[1]]$pstate3)),
                                          rep('Transplant', length(simdatprob[[1]]$pstate3))))
# plot:
ggplot(data = plot.data.trans, aes(x = time, y = prob, col = from)) +
  geom_line() + 
  facet_wrap(vars(outcome)) +
  scale_color_grey() +
  labs(x = 'Time (days)', y = 'Transition probability', col = '') +
  theme(legend.position=c(0.85, 0.2))


###############################################################################
###############################################################################
### Table 3.9: Multi-state Cox models for transition probabilities from 
### non-responding and responding states respectively to death.
###############################################################################
###############################################################################

# variables to be used:
retained_r <- retained[!(retained %in% 'response')]

# non-responding to death:
multi_death_nr <- data_multistate[data_multistate$to == 'Dead' & data_multistate$from == 'Non-responding',]
c1_death_nr <- coxph(
  as.formula(paste0("Surv(start_time, end_time, status) ~ ",
                    paste(retained_r, collapse = '+'))),
  data = multi_death_nr, method = "breslow")
c1_death_nr

# responding to death:
multi_death_r <- data_multistate[data_multistate$to == 'Dead' & data_multistate$from == 'Responding',]
c1_death_r <- coxph(
  as.formula(paste0("Surv(start_time, end_time, status) ~ ",
                    paste(retained_r, collapse = '+'))),
  data = multi_death_r, method = "breslow")
c1_death_r

# build table:
multistate_death <- cbind(summary(c1_death_nr)$conf.int[,-2],
                          summary(c1_death_r)$conf.int[,-2])

# add variable labels:
rownames(multistate_death) <- sapply(rownames(multistate_death), function(x) varlabels[paste(x)]) 
colnames(multistate_death)[colnames(multistate_death) %in% 'lower .95'] <- '2.5%'
colnames(multistate_death)[colnames(multistate_death) %in% 'upper .95'] <- '97.5%'
colnames(multistate_death) <- gsub('%', '\\\\%', colnames(multistate_death))

# print table:
kable(multistate_death, booktabs = T, digits = 2, escape = FALSE,
      caption = "Multi-state Cox models for transition probabilities from non-responding and responding states respectively to death.") %>%
  add_header_above(c("", "Non-responding to death" = 3, "Responding to death" = 3)) %>%
  kable_styling(font_size = 7)


###############################################################################
###############################################################################
### Table 3.10: Multi-state Cox models for transition probabilities from 
### non-responding and responding states respectively to transplant.
###############################################################################
###############################################################################

# non-responding to transplant:
multi_trans_nr <- data_multistate[data_multistate$to == 'Transplant' & data_multistate$from == 'Non-responding',]
c1_trans_nr <- coxph(
  as.formula(paste0("Surv(start_time, end_time, status) ~ ",
                    paste(retained_r, collapse = '+'))),
  data = multi_trans_nr, method = "breslow")
c1_trans_nr

# responding to transplant:
multi_trans_r <- data_multistate[data_multistate$to == 'Transplant' & data_multistate$from == 'Responding',]
c1_trans_r <- coxph(
  as.formula(paste0("Surv(start_time, end_time, status) ~ ",
                    paste(retained_r, collapse = '+'))),
  data = multi_trans_r, method = "breslow")
c1_trans_r

# build table:
multistate_trans <- cbind(summary(c1_trans_nr)$conf.int[,-2],
                          summary(c1_trans_r)$conf.int[,-2])
# add variable labels:
rownames(multistate_trans) <- sapply(rownames(multistate_trans), function(x) varlabels[paste(x)]) 
colnames(multistate_trans)[colnames(multistate_trans) %in% 'lower .95'] <- '2.5%'
colnames(multistate_trans)[colnames(multistate_trans) %in% 'upper .95'] <- '97.5%'
colnames(multistate_trans) <- gsub('%', '\\\\%', colnames(multistate_trans))

# print table:
kable(multistate_trans, booktabs = T, digits = 2, escape = FALSE,
      caption = "Multi-state Cox models for transition probabilities from non-responding and responding states respectively to transplant") %>%
  add_header_above(c("", "Non-responding to transplant" = 3, "Responding to transplant" = 3)) %>%
  kable_styling(font_size = 7)


###############################################################################
###############################################################################
### Table 3.11: Multi-state Cox models for transition probabilities from
### non-responding state to responding one.
###############################################################################
###############################################################################

# non-responding to responding:
multi_resp_nr <- data_multistate[data_multistate$to == 'Responding' & data_multistate$from == 'Non-responding',]
c1_resp_nr <- coxph(
  as.formula(paste0("Surv(start_time, end_time, status) ~ ",
                    paste(retained_r, collapse = '+'))),
  data = multi_resp_nr, method = "breslow")
c1_resp_nr

# build table:
multistate_r <- cbind(summary(c1_resp_nr)$conf.int[,-2])

# add variable labels:
rownames(multistate_r) <- sapply(rownames(multistate_r), function(x) varlabels[paste(x)]) 
colnames(multistate_r)[colnames(multistate_r) %in% 'lower .95'] <- '2.5%'
colnames(multistate_r)[colnames(multistate_r) %in% 'upper .95'] <- '97.5%'
colnames(multistate_r) <- gsub('%', '\\\\%', colnames(multistate_r))

# print table:
kable(multistate_r, booktabs = T, digits = 2, escape = FALSE,
      caption = "Multi-state Cox models for transition probabilities from non-responding state to responding one.") %>%
  add_header_above(c("", "Non-responding to responding" = 3)) %>%
  kable_styling(font_size = 7)

