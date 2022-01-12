
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
### Table 4.1: Hazard ratios for all the variables in all the models fitted 
### considering death as the endpoint. Statistically significant (at a 0.05
### level) terms in bold.
###############################################################################
###############################################################################

# function to format those coefficients with associated p-values<0.05 in bold (for latex):
bold_table <- function(dataFrame, coefs, pvals){
  vect <- paste0(ifelse(dataFrame[,pvals] < 0.05, '\\textbf{', ''),
                 round(dataFrame[,coefs], 2), 
                 ifelse(dataFrame[,pvals] < 0.05, '}', ''))
  
  return(data.frame(coefs = vect, row.names = rownames(dataFrame)))
}

# variables to be used:
retained <- vars_of_interest_basal[!(vars_of_interest_basal %in% c("lym", "lympc", "mono", "monopc", "neut", "imneut", "neutpc", "chol", "crp", "pao2","paco2"))]

# source previous codes to load models:
source('2-1-competing-risks-basal-covariates.R')
source('2-2-competing-risks-time-varying-covariates.R')
source('2-3-competing-risks-landmarks.R')
source('2-4-multi-state-model.R')


# format competing risks model with basal covariates:
basal <- merge(bold_table(summary(fg_death)$coef, 'exp(coef)', 'p-value'),
               bold_table(summary(cs_death)$coef, 'exp(coef)', 'Pr(>|z|)'),
               by = 0) %>% column_to_rownames(var = "Row.names")
colnames(basal) <- c('Sub- \n distribution', 'Cause- \n specific')

# format competing risks model with time-varying covariates:
timevar <- merge(bold_table(summary(fg_death_fit)$coef, 'exp(coef)', 'Pr(>|z|)'),
                 bold_table(summary(competing_death_mod)$coef, 'exp(coef)', 'Pr(>|z|)'),
                 by = 0) %>% column_to_rownames(var = "Row.names")
colnames(timevar) <- c('Sub- \n distribution', 'Cause- \n specific')

# format competing risks model with landmarking:
landmark <- merge(bold_table(summary(LMsupercox0_death)$coef, 'exp(coef)', 'Pr(>|z|)'),
                  bold_table(summary(LMsupercox2_death)$coef, 'exp(coef)', 'Pr(>|z|)'),
                  by = 0) %>% column_to_rownames(var = "Row.names")
colnames(landmark) <- c('Super- \n model 1', 'Super- \n model 2')

# format multi-state model:
multistate <- merge(bold_table(summary(c1_death_nr)$coef, 'exp(coef)', 'Pr(>|z|)'),
                    bold_table(summary(c1_death_r)$coef, 'exp(coef)', 'Pr(>|z|)'),
                    by = 0) %>% column_to_rownames(var = "Row.names")
colnames(multistate) <- c('From \n non-responding', 'From \n responding')

# cbind all models:
all_death <- merge(merge(basal, timevar, by = 0, no.dups = FALSE, all = TRUE)  %>% 
                     column_to_rownames(var = "Row.names"),
                   merge(landmark, multistate, by = 0, no.dups= FALSE, all = TRUE) %>%
                     column_to_rownames(var = "Row.names"),
                   by = 0, no.dups= FALSE, all = TRUE) %>%
  column_to_rownames(var = "Row.names")
colnames(all_death) <- gsub('.x', '', names(all_death))
colnames(all_death) <- gsub('.y', '', names(all_death))
colnames(all_death) <- linebreak(colnames(all_death))
rownames(all_death) <- sapply(rownames(all_death), function(x) varlabels[paste(x)]) 
options(knitr.kable.NA = '')

# print table:
kable(all_death, booktabs = T, escape = FALSE,
      caption = 'Hazard ratios for all the variables in all the models fitted considering death as the endpoint. Statistically significant (at a 0.05 level) terms in bold.') %>%
  add_header_above(c("",
                     "Basal variables" = 2, 
                     "Time-dependent variables" = 2, 
                     "Landmark models" = 2,
                     "Multi-state models" = 2))  %>%
  kableExtra::landscape() %>%
  kable_styling(font_size = 9)
