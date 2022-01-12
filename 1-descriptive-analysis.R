
# run 0-data.R script to load and prepare data:
source('0-data.R')

# packages:
list.of.packages <- c('ggplot2', 'kableExtra', 'prodlim')

# if not installed, install:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load:
invisible(lapply(list.of.packages, library, character.only = TRUE))

# set theme for ggplot:
theme_set(theme_minimal())

###############################################################################
###############################################################################
### Figure 3.1: Number of subjects for the two possible outcomes: death and transplant.
###############################################################################
###############################################################################

with(data_basal, plot(Hist(time, event, cens.code = "Censored"), arrowLabelStyle = "count"))

###############################################################################
###############################################################################
### Table 3.1: Descriptive analysis of continuous variables.
###############################################################################
###############################################################################

# set continuous variables:
continuous <- c('age', 
                'map', 'hr', 'wbc', 'creat', 'bili', 'inr', 'alb', 'na', 'crp', 'chol', 'hgb', 'lym',
                'mono', 'monopc', 'neut', 'neutpc', 'imneut', 'lympc', 'pao2', 'paco2')
continuous <- continuous[continuous %in% vars_of_interest_basal]

# build table with descriptives for the selected continuous variables:
basal_descr_cont <- data_basal %>%
  select(all_of(continuous)) %>%
  gather(key = variable, value = value) %>% 
  mutate(variable = as.factor(variable)) %>%
  group_by(variable) %>%
  summarise(
    '% missing' = (sum(is.na(value)) / n()) *100,
    Mean = mean(value, na.rm = T),
    'Sd' = sd(value, na.rm = T),
    Min = min(value,  na.rm = T),
    '1st Q' = quantile(value, 0.25, na.rm = T),
    Median = median(value,  na.rm = T),
    '3rd Q' = quantile(value, 0.75, na.rm = T),
    Max = max(value,  na.rm = T)
  )
basal_descr_cont[,1] <- sapply(basal_descr_cont[,1], function(x) varlabels[paste(x)]) 

# print table:
colnames(basal_descr_cont) <- gsub('%', '\\\\%', c('', colnames(basal_descr_cont)[-1]))
kable(as.data.frame(basal_descr_cont),
      digits = 2, 
      booktabs = T,
      escape = FALSE,
      caption = "Descriptive analysis of continuous variables.") %>%
  kable_styling(latex_options = "HOLD_position")

###############################################################################
###############################################################################
### Table 3.2: Descriptive analysis of categorical variables.
###############################################################################
###############################################################################

# set categorical variables:
categorical <- c('sex', 'tr_alb', 'tr_vasop',
                 'etciralc', 'etcirhepb', 'etcirhepc', 'etcirnafld', 'etcircryp', 'etciroth',
                 'asci', 'he', 'gblee', 'binf')
categorical <- categorical[categorical %in% vars_of_interest_basal]

# build table with descriptives for the selected categorical variables:
basal_descr_cat <- data_basal %>%
  select(all_of(categorical)) %>%
  gather(key = variable, value = value) %>% 
  mutate(variable = factor(variable, levels = categorical)) %>%
  group_by(variable) %>%
  summarise(
    '% yes' = sum(value, na.rm = T)/ n() *100,
    '% missing' = (sum(is.na(value)) / n()) *100
  )
basal_descr_cat[,1] <- sapply(basal_descr_cat[,1], function(x) varlabels[paste(x)]) 

# print table:
colnames(basal_descr_cat) <-  gsub('%', '\\\\%', c('', colnames(basal_descr_cat)[-1]))
kable(as.data.frame(basal_descr_cat),
      digits = 2, 
      booktabs = T,
      escape = FALSE,
      caption = "Descriptive analysis of categorical variables.") %>%
  kable_styling(latex_options = "HOLD_position")


###############################################################################
###############################################################################
### Figure 3.2: Values of continuous variables in every visit for each subject.
### In the top right the total percentage of missing data is indicated.
###############################################################################
###############################################################################

# build table with NAs:
cont_long_NAs <- data %>%
  ungroup() %>%
  select(all_of(continuous[!continuous %in% 'age'])) %>%
  summarise_each(funs(100*mean(is.na(.)))) %>%
  gather(variable, na_percent, factor_key = TRUE) %>%
  mutate(na_text = paste0(round(na_percent, 2), '%'))
cont_long_NAs$variable <- sapply(cont_long_NAs$variable, function(x) varlabels_plots[paste(x)]) 

# build dataset with data to be plotted selecting the start of each interval with its corresponding values:
plot.cont.long <- data %>%
  gather(variable, measurement, all_of(continuous[!continuous %in% 'age']), factor_key = TRUE) %>%
  arrange(idpatient, time) %>%
  select(start_time, variable, measurement)

# add a last point for the end time of the last visit of every subject:
aux <- data %>% 
  group_by(idpatient) %>% 
  arrange(idpatient, visitdt) %>% 
  slice_tail() %>%
  gather(variable, measurement, all_of(continuous[!continuous %in% 'age']), factor_key = TRUE) %>%
  arrange(idpatient, time) %>%
  select(time, variable, measurement) %>%
  rename(start_time = time)

plot.cont.long <- rbind(plot.cont.long, aux)
plot.cont.long$variable <- sapply(plot.cont.long$variable, function(x) varlabels_plots[paste(x)]) 

# plot:
plot.cont.long %>%
  ggplot( aes(x = start_time, y = measurement, group = idpatient)) +
  geom_step(na.rm = TRUE) +
  geom_point(size = 0.5) +
  labs(x ='Time (days)', y = '') +
  geom_text(data = cont_long_NAs, aes(label=na_text, fill = NULL), 
            x = Inf, y = Inf, hjust = 2, vjust = 2,
            inherit.aes = FALSE) +
  facet_wrap(vars(variable), scales = "free")


###############################################################################
###############################################################################
### Figure 3.3: Absolute frequences of categorical variables through time.
### In the top right the total percentage of missing data is indicated.
###############################################################################
###############################################################################

# build table with NAs:
cat_long_NAs <- data %>%
  ungroup() %>%
  select(all_of(categorical)) %>%
  summarise(across(everything(), ~ c(na_percent = 100*mean(is.na(.x))))) %>%
  gather(variable, na_percent, factor_key = TRUE) %>%
  mutate(na_text = paste0(round(na_percent, 2), '%'))
cat_long_NAs$variable <- sapply(cat_long_NAs$variable, function(x) varlabels_plots[paste(x)]) 
cat_long_NAs$variable <- gsub('-', '\n', cat_long_NAs$variable)

# build dataset with data to be plotted:
plot.cat.long <- data %>%
  ungroup() %>%
  select(all_of(c(categorical, 'start_time', 'end_time'))) %>%
  mutate(row_id = seq(1:nrow(data))) %>%
  merge(.,
        data %>%  
          ungroup() %>%
          mutate(row_id = seq(1:nrow(data))) %>%
          rowwise() %>%
          do(data.frame(row_id = .$row_id, day=seq(.$start_time,.$end_time-1, by = 1))),
        by = 'row_id') %>%
  gather(variable, measurement, all_of(categorical), factor_key = TRUE) 
plot.cat.long$measurement_cat = factor(plot.cat.long$measurement, levels = c(0, 1), labels = c('No', 'Yes'))
plot.cat.long$variable <- sapply(plot.cat.long$variable, function(x) varlabels_plots[paste(x)]) 
plot.cat.long$variable <- gsub('-', '\n', plot.cat.long$variable)

plot.cat.long.prop <- plot.cat.long %>%
  group_by(day, variable) %>%
  dplyr::summarise(avg = mean(measurement, na.rm=T)*100)

# plot:
coeff <- 2
plot.cat.long %>%
  ggplot(aes(day)) +
  geom_histogram(aes(fill = measurement_cat), position = "stack", binwidth = 1) +
  geom_line(data = plot.cat.long.prop, aes(y = avg*coeff), col = 'orange') + 
  labs(x = 'Time (days)', y = '', fill = '') +
  geom_text(data = cat_long_NAs, aes(label=na_text, fill = NULL), 
            x = Inf, y = Inf, hjust = 2, vjust = 2,
            inherit.aes = FALSE) +
  facet_wrap(vars(variable), scales = "free") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(
    # Features of the first axis
    name = "Absolute frequence",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Percentage of 'Yes'")
  ) +
  
  theme(
    axis.title.y.right = element_text(color = 'orange')
  )
