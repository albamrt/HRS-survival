
# list of packages to be used:
list.of.packages <- c('readxl', 'tidyverse', 'vctrs')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

# set directory:
dir <- 'U:'
dir <- '//10.34.75.200/alba.morato'

# read data:
data <- read_excel(paste0(dir, "/tfm/0-select-data/data/predict-aclara-longitudinal-no-filter.xlsx"))

# check NAs by variable:
sapply(data, function(x) sum(is.na(x))/(nrow(data)))

# imputation of NAs with values of same subject in previous interval:
data <- data %>%
  group_by(idpatient) %>%
  mutate_at(c("map", "hr", "hgb", "wbc", "lym", "lympc", "mono", "monopc", "neut", "imneut", 
              "inr", "alb", "bili", "creat", "na", "chol", "crp", "pao2", "paco2", 
              "asci", "he", "gblee", "binf", 
              "tr_alb", "tr_vasop", 
              "meld", "child", "clifcof", "aclfgr", "aclfyn", "clifcad", "clifcaclf",
              "liversc", "renalsc", "cerebsc", "coagsc", "cardiosc", "respsc",
              "liverfail", "renalfail", "cerebfail", "coagfail", "cardiofail", "respfail"), 
            function(x) vec_fill_missing(x, direction = c("down"))) %>%
  # if HR = -99 -> NA
  mutate(hr = ifelse(hr==-99, NA, hr))

# re-check % of NAs by variable:
sapply(data, function(x) sum(is.na(x))/(nrow(data)))

# adjust variables:
data <- data %>%
  mutate(
    # Etiology of cirrhosis - Other: fill with other cirrhosis only included in ACLARA:
    etciroth = ifelse(!is.na(etciroth) & etciroth == 1 | 
                        !is.na(etcirhepd) & etcirhepd == 1 | 
                        !is.na(etcirbili) & etcirbili == 1 | 
                        !is.na(etcirscle) & etcirscle == 1 |
                        !is.na(etcirah) & etcirah == 1 | 
                        !is.na(etcirwil) & etcirwil == 1 | 
                        !is.na(etcirhemo) & etcirhemo == 1, 1, 0),
    # Neutrophil proportion:
    neutpc = (neut/wbc)*100, 
    # Response to treatment
    response = ifelse(creat < 1.5, 1, 0),
    # time of death, start and end time of the period:
    time = as.numeric(difftime(LTfree_dt, diagnostic_visitdt, units = 'days')),
    start_time = as.numeric(difftime(visitdt, diagnostic_visitdt, units = 'days'))) %>%
  arrange(idpatient, visitdt) %>% # df must be ordered appropriately
  group_by(idpatient) %>% # create new grouping variable
  mutate(post_date = data.table::shift(visitdt, n = -1, fill = NA)) %>%
  # remove variables not needed anymore:
  select(-c(etcirhepd, etcirbili, etcirscle, etcirah, etcirwil, etcirhemo))

data$post_date[is.na(data$post_date)] <- data$LTfree_dt[is.na(data$post_date)]
data$end_time = difftime(data$post_date, data$diagnostic_visitdt, units = 'days')

# remove visits that happen the same day as the final event:
data <- data[data$visitdt != data$LTfree_dt,]

# add event (0 for all but last interval):
data <- data %>%
  mutate(event_numeric = ifelse(post_date == LTfree_dt, LT2, 0),
         event = as.factor(case_when(
           event_numeric == 0 ~ 'Censored',
           event_numeric == 1 ~ 'Dead',
           event_numeric == 2 ~ 'Transplant')))

# variable labels (to be used in tables):
varlabels <- c(idpatient = 'Patient identifier',
               cohort = 'Cohort',
               response = 'Response to treatment (creat < 1.5 mg/dL)',
               tr_alb = 'Treatment with albumin',
               tr_vasop = 'Treatment with vasopressors',
               age = 'Age (yr)',
               sex = 'Female sex', 
               asci = 'Ascites', 
               he = 'Hepatic encephalopathy', 
               gblee = 'Gastrointestinal bleeding',
               aclfyn = 'Presence of ACLF',
               etciralc = 'Etiology of cirrhosis - Alcohol',
               etcirhepc = 'Etiology of cirrhosis - Hepatitis C virus',
               etcirhepb = 'Etiology of cirrhosis - Hepatitis B virus',
               etcirnafld = 'Etiology of cirrhosis - NAFLD/NASH',
               etcircryp = 'Etiology of cirrhosis - Cryptogenic',
               etciroth = 'Etiology of cirrhosis - Other',
               etunknown = 'Etiology of cirrhosis - Unknown',
               binf = 'Bacterial infection',
               map = 'MAP (mmHg)',
               hr = 'Heart rate (bpm)',
               hgb = 'Hemoglobin (g/dL)',
               wbc = "Leucocyte ($\\times10^9$/L)",
               lym = "Lymphocytes ($\\times10^9$/L)",
               lympc = "Lymphocytes (\\%)",
               mono = "Monocytes ($\\times10^9$/L)",
               monopc = "Monocytes (\\%)",
               neut = "Neutrophils ($\\times10^9$/L)",
               neutpc = "Neutrophils (\\%)",
               imneut = "Immature neutrophils (\\%)",
               inr = "INR",
               alb = "Albumin (g/dL)",
               bili = "Total Bilirubin (mg/dL)",
               creat = "Creatinine (mg/dL)",
               na = "Sodium (mEq/L)",
               chol = "Total Cholesterol (mg/dL)",
               crp = "CRP (mg/L)",
               pao2 = "PaO2 (mmHg)",
               paco2 = "PaCO2 (mmHg)")

# variable labels (to be used in plots):
varlabels_plots <- c(idpatient = 'Patient identifier',
                     cohort = 'Cohort',
                     response = 'Response to treatment',
                     tr_alb = 'Treatment \n with albumin',
                     tr_vasop = 'Treatment \n with vasopressors',
                     age = 'Age (yr)',
                     sex = 'Female sex', 
                     asci = 'Ascites', 
                     he = 'Hepatic encephalopathy', 
                     gblee = 'Gastrointestinal bleeding',
                     etciralc = 'Etiology of \n cirrhosis - Alcohol',
                     etcirhepc = 'Etiology of \n cirrhosis - Hepatitis C virus',
                     etcirhepb = 'Etiology of \n cirrhosis - Hepatitis B virus',
                     etcirnafld = 'Etiology of \n cirrhosis - NAFLD/NASH',
                     etcircryp = 'Etiology of \n cirrhosis - Cryptogenic',
                     etciroth = 'Etiology of \n cirrhosis - Other',
                     etunknown = 'Etiology of \n cirrhosis - Unknown',
                     binf = 'Bacterial infection',
                     map = 'MAP (mmHg)',
                     hr = 'Heart rate (bpm)',
                     hgb = 'Hemoglobin (g/dL)',
                     wbc = paste0("Leucocyte (x10^9/L)"),
                     lym = "Lymphocytes (x10^9/L)",
                     lympc = "Lymphocytes (%)",
                     mono = "Monocytes (x10^9/L)",
                     monopc = "Monocytes (%)",
                     neut = "Neutrophils (x10^9/L)",
                     neutpc = "Neutrophils (%)",
                     imneut = "Immature neutrophils (%)",
                     inr = "INR",
                     alb = "Albumin (g/dL)",
                     bili = "Total Bilirubin (mg/dL)",
                     creat = "Creatinine (mg/dL)",
                     na = "Sodium (mEq/L)",
                     chol = "Total Cholesterol (mg/dL)",
                     crp = "CRP (mg/L)",
                     pao2 = "PaO2 (mmHg)",
                     paco2 = "PaCO2 (mmHg)")

# data from the first visit:
data_basal <- data %>%
  group_by(idpatient) %>%
  arrange(idpatient, visitdt) %>%
  slice_head() %>%
  ungroup()

data_basal <- data_basal %>%
  select(-c(event_numeric)) %>%
  mutate(event = as.factor(case_when(
    LT2 == 0 ~ 'Censored',
    LT2 == 1 ~ 'Dead',
    LT2 == 2 ~ 'Transplant')))

# Define variables to be included in descriptive and survival models:
vars_of_interest <- c(
  'age', 'sex',
  'response',
  'tr_alb', 'tr_vasop',
  'etciralc', 'etcirhepb', 'etcirhepc', 'etcirnafld', 'etcircryp', 'etciroth',
  'map', 'hr', 'hgb', 'wbc', 'inr', 'alb', 'bili', 'creat', 'na', 
  'lym', 'lympc', 'mono', 'monopc', 'neut', 'imneut', 'neutpc',
  'chol', 'crp', 'pao2','paco2', 
  'asci', 'he', 'gblee', 'binf'
)

# for the first visit remove the variable 'response':
vars_of_interest_basal <- vars_of_interest[!(vars_of_interest %in% 'response')]
