
# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('kableExtra', 'survival', 'riskRegression')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

###############################################################################
###############################################################################
### Table 3.4: Fine and Gray model for time-fixed (basal) variables for different endpoints.
###############################################################################
###############################################################################

# variables to be used:
retained <- vars_of_interest_basal[!(vars_of_interest_basal %in% c("lym", "lympc", "mono", "monopc", "neut", "imneut", "neutpc", "chol", "crp", "pao2","paco2"))]

# Fine-Gray's model for death:
fg_death <- FGR(as.formula(paste0("Hist(time, LT2) ~ ", paste(retained, collapse = "+"))), 
                data = data_basal, cause = 1, y = TRUE)

# Fine-Gray's model for transplant:
fg_transplant <- FGR(as.formula(paste0("Hist(time, LT2) ~ ", paste(retained, collapse = "+"))), 
                     data = data_basal, cause = 2, y = TRUE)

# Fine-Gray's model for death or transplant:
fg_both <- FGR(as.formula(paste0("Hist(time, as.numeric(LT2 > 0)) ~ ", 
                                 paste(retained, collapse = "+"))), 
               data = data_basal, cause = 1, y = TRUE)

# cbind the three models together:
fg <- cbind(summary(fg_death)$conf.int[,-2],
            summary(fg_transplant)$conf.int[,-2],
            summary(fg_both)$conf.int[,-2])
rownames(fg) <- sapply(rownames(fg), function(x) varlabels[paste(x)]) 
colnames(fg) <- gsub('%', '\\\\%', colnames(fg))

# print table:
kable(fg, booktabs = T, digits = 2, escape = FALSE,
      caption = 'Fine and Gray model for time-fixed (basal) variables for different endpoints.') %>%
  add_header_above(c("", "Death" = 3, "Transplant" = 3, "Death or transplant" = 3)) %>%
  kable_styling(font_size = 7)

###############################################################################
###############################################################################
### Table 3.5: Cause-specific model for time-fixed (basal) variables for different endpoints.
###############################################################################
###############################################################################

# Cause-specific model for death:
cs_death <- coxph(as.formula(paste0("Surv(time, LT2 == 1) ~ ", paste(retained, collapse = "+"))),
                  data = data_basal)

# Cause-specific model for transplant:
cs_transplant <- coxph(as.formula(paste0("Surv(time, LT2 == 2) ~ ", paste(retained, collapse = "+"))),
                       data = data_basal)

# Cause-specific model for death or transplant:
cs_both <- coxph(as.formula(paste0("Surv(time, LT2 > 0) ~ ", paste(retained, collapse = "+"))),
                 data = data_basal)

# cbind the three models together:
cs <- cbind(summary(cs_death)$conf.int[,-2],
            summary(cs_transplant)$conf.int[,-2],
            summary(cs_both)$conf.int[,-2])
rownames(cs) <- sapply(rownames(cs), function(x) varlabels[paste(x)])
colnames(cs)[colnames(cs) %in% 'lower .95'] <- '2.5%'
colnames(cs)[colnames(cs) %in% 'upper .95'] <- '97.5%'
colnames(cs) <- gsub('%', '\\\\%', colnames(cs))

# print table:
kable(cs, booktabs = T, digits = 2, escape = FALSE,
      caption = ' Cause-specific model for time-fixed (basal) variables for different endpoints.') %>%
  add_header_above(c("", "Death" = 3, "Transplant" = 3, "Death or transplant" = 3)) %>%
  kable_styling(font_size = 7)
