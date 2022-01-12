
# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('kableExtra', 'survival', 'dynpred', 'gridExtra')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))


###############################################################################
###############################################################################
### Figure 3.6: Stacked barplot of the distribution of responders and 
### non-responders through the landmarks set.
###############################################################################
###############################################################################

# variables to be used:
retained <- vars_of_interest[!(vars_of_interest %in% c("lym", "lympc", "mono", "monopc", "neut", "imneut", "neutpc", "chol", "crp", "pao2","paco2"))]

# create landmark data set:
LMdata <- NULL
LMs <- seq(5, 100, by = 5)
data$time <- difftime(data$LTfree_dt, data$diagnostic_visitdt)

# fixed variables:
fixed <- c('sex', 'age', 'etciralc', 'etcircryp', 'etcirhepb', 'etcirhepc', 'etcirnafld', 'etciroth', 'etunknown')

# time-varying variables:
varying <- vars_of_interest[!(vars_of_interest %in% fixed)]

for (LM in LMs) {
  LMdataLM <- cutLM(data = data, 
                    outcome = list(time = "time", status = "event_numeric"),
                    LM = LM, horizon = LM + 100, 
                    covs = list(fixed = fixed, varying = varying),
                    format = "long",id = "idpatient", rtime = "start_time")
  LMdata <- rbind(LMdata,LMdataLM)
}

# plot:
LMdata %>%
  mutate(response = factor(response, exclude = NULL, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  ggplot(aes(fill = response, x = as.factor(LM))) + 
  geom_bar() + 
  labs(x = "Landmark", y = '', fill = "Response to treatment") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  theme(legend.position=c(.75,.75))

###############################################################################
###############################################################################
### Table 3.8: Landmark super model Cox regressions.
###############################################################################
###############################################################################

# first super-model for death:
LMdata$Tstart <- LMdata$LM
LMsupercox0_death <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric == 1) ~ ", 
                                             paste(retained, collapse = '+'), '+ strata(LM) + cluster(idpatient)')),
                           data = LMdata, method = "breslow")

# first super-model for transplant:
LMsupercox0_trans <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric == 2) ~ ", 
                                             paste(retained, collapse = '+'), '+ strata(LM) + cluster(idpatient)')),
                           data = LMdata, method = "breslow")

# first super-model for death and transplant:
LMsupercox0_both <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric > 0) ~ ", 
                                            paste(retained, collapse = '+'), '+ strata(LM) + cluster(idpatient)')),
                          data = LMdata, method = "breslow")


# functions to be used in second super-model:
g1 <- function(t) (t)
g2 <- function(t) (t)^2

# apply functions to landmark variables:
LMdata$LM1 <- g1(LMdata$LM)
LMdata$LM2 <- g2(LMdata$LM)

# second super-model for death:
LMsupercox2_death <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric == 1) ~ ",
                                             paste(retained, collapse = '+'), 
                                             '+ LM1 + LM2 + cluster(idpatient) ')),
                           data = LMdata, method = "breslow")

# second super-model for transplant:
LMsupercox2_trans <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric == 2) ~ ",
                                             paste(retained, collapse = '+'), 
                                             '+ LM1 + LM2 + cluster(idpatient) ')),
                           data = LMdata, method = "breslow")

# second super-model for death or transplant:
LMsupercox2_both <- coxph(as.formula(paste0("Surv(Tstart, time, event_numeric > 0) ~ ",
                                            paste(retained, collapse = '+'), 
                                            '+ LM1 + LM2 + cluster(idpatient) ')),
                          data = LMdata, method = "breslow")

# cbind all the landmark models together:
LMsupercox <- rbind(cbind(summary(LMsupercox0_death)$conf.int[,-2],
                          summary(LMsupercox0_trans)$conf.int[,-2],
                          summary(LMsupercox0_both)$conf.int[,-2]),
                    cbind(summary(LMsupercox2_death)$conf.int[,-2],
                          summary(LMsupercox2_trans)$conf.int[,-2],
                          summary(LMsupercox2_both)$conf.int[,-2]))

# add variable labels:
varlabels <- c(varlabels,
               'LM1' = '$s$',
               'LM2' = '$s^2$'
)
rownames(LMsupercox) <- sapply(rownames(LMsupercox), function(x) varlabels[paste(x)]) 
colnames(LMsupercox)[colnames(LMsupercox) %in% 'lower .95'] <- '2.5%'
colnames(LMsupercox)[colnames(LMsupercox) %in% 'upper .95'] <- '97.5%'
colnames(LMsupercox) <- gsub('%', '\\\\%', colnames(LMsupercox))

# print table:
kable(LMsupercox, booktabs = T, digits = 2, escape = FALSE,
      caption = "Landmark super model Cox regressions.") %>%
  pack_rows(index = c('Super model 1' = nrow(summary(LMsupercox0_death)$coefficients),
                      #'Super model 2' = nrow(summary(LMsupercox1)$coefficients),
                      'Super model 2' = nrow(summary(LMsupercox2_death)$coefficients))) %>%
  add_header_above(c("", "Death" = 3, "Transplant" = 3, "Death or transplant" = 3)) %>%
  kable_styling(font_size = 7)

