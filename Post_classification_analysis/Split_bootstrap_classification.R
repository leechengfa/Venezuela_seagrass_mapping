### Venezuela split bootstrapping validation
### Lee, Chengfa Benjamin

#-------
# Library
#-------

library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(patchwork) 

#-------
# Directory
#-------

folder <- vector("list", 0)
folder$dir_data <- "/Data/CS/"
folder$folder <- list.files(folder$dir_data, full.names = TRUE)
folder$dir_acolite <- "/Data/ACOLITE/"
folder$acolite <- list.files(folder$dir_acolite, full.names = TRUE)

#-------
# Import data
#-------

import_file <- function(file_name) {
  csv <- read.csv(file_name) %>%
    select(-c(".geo","system.index"))
  
  file_name_extless <- gsub('.csv', '', file_name) %>%
    gsub(folder$dir_data, '', .) %>%
    gsub("CS_Validation_5class_Rrs_", '', .) %>%
    gsub("_p10", '', .)
  csv$CS_product <- ifelse(grepl("cdf", file_name_extless), "cdf", ifelse(grepl("cs", file_name_extless), "cs", "FA"))
  csv$threshold <- ifelse(csv$CS_product != "FA", 
                          ifelse(csv$CS_product != 'cdf', substr(file_name_extless, 3, 4) %>% as.integer(), substr(file_name_extless, 7, 8) %>% as.integer()), 0)
  
  csv$start_year <- lapply(csv$name, function(string) {return(strsplit(string, "_") %>% unlist() %>% "["(match('p10', .) -2) %>% as.integer())}) %>% unlist()
  csv$end_year <- lapply(csv$name, function(string) {return(strsplit(string, "_") %>% unlist() %>% "["(match('p10', .) -1) %>% as.integer())}) %>% unlist()
  csv$time_period <- 1 + csv$end_year-csv$start_year
  csv$time_range <- paste(csv$start_year, csv$end_year, sep = "_")
  
  output <- csv # %>%
  
  return(output)
}

csv_CS <- lapply(folder$folder, import_file) %>% 
  do.call('rbind', .)

import_acolite <- function(file_name) {
  csv <- read.csv(file_name) %>%
    select(-c(".geo","system.index"))
  
  ## rename column names to match columns
  # manual, single approach to ensure that column name order wouldn't affect the renaming
  names(csv)[names(csv) == 'Overall.accuracy'] <- 'OA'
  names(csv)[names(csv) == 'Producer.s.accuracy'] <- 'PA'
  names(csv)[names(csv) == 'User.s.accuracy'] <- 'UA'
  names(csv)[names(csv) == 'Variable.importance'] <- 'feature_selected_list'
  names(csv)[names(csv) == 'TD_size'] <- 'td_size'
  names(csv)[names(csv) == 'VD_size'] <- 'vd_size'
  names(csv)[names(csv) == 'VD.error.matrix'] <- 'vd_error_matrix'
  
  csv$CS_product <- 'ACOLITE'
  csv$threshold <- 0
  csv$name <- file_name %>% 
    str_split('Bootstrap/') %>% 
    unlist %>%
    "["(2)
  
  csv$start_year <- 2023
  csv$end_year <- 2023
  csv$time_period <- 1 + csv$end_year-csv$start_year
  csv$time_range <- paste(csv$start_year, csv$end_year, sep = "_")
  
  output <- csv 
  
  return(output)
}

csv_acolite <- lapply(folder$acolite, import_acolite) %>% 
  do.call('rbind', .)

csv <- rbind(csv_CS, csv_acolite)

#-------
# F1 Score
#-------

calculate_f1_scores <- function(error_matrix) {
  no_classes <- str_count(error_matrix, '\\[') - 1 # remove the extra bracket for the second axis
  
  matrix_EM <- error_matrix %>%
    gsub('\\[', 'c(', .) %>%
    gsub('\\]', ')', .) %>%
    parse(text = .) %>%
    eval() %>%
    matrix(nrow = no_classes)
  
  diagonal <- diag(matrix_EM)
  rowsum <- rowSums(matrix_EM)
  colsum <- colSums(matrix_EM)
  
  # proofed against PA and UA results
  precision <- diagonal/colsum
  recall <- diagonal/rowsum
  
  f1 <- (2*precision*recall)/(precision+recall)
  f1 <- f1[2:length(f1)] # remove dummy class zero from GEE indexing issue
  
  return(f1)
}

csv$F1 <- lapply(csv$vd_error_matrix, calculate_f1_scores)
csv$F1_seagrass <- lapply(csv$F1, FUN = function(x) {x[2]}) %>% unlist()
csv$F1_mean <- lapply(csv$F1, FUN = function(x) {mean(x, na.rm = TRUE)}) %>% unlist()

csv$CS_product <- factor(csv$CS_product, levels = c('FA', 'cs', 'cdf', 'ACOLITE'), labels = c('Original approach', 'Cloud Score+ (CS)', 'Cloud Score+ (CDF)', 'ACOLITE'))
csv$AOI <- factor(csv$AOI, levels = c('coastal', 'openWater'), labels = c('Coastal', 'Open Water'))
csv$time_range <- factor(csv$time_range, levels = c('2017_2017', '2018_2018', '2019_2019', '2020_2020', '2021_2021', '2022_2022', '2023_2023', '2022_2023', '2017_2022', '2017_2023'), labels = c('2017', '2018', '2019', '2020', '2021', '2022', '2023', '2022-2023', '2017-2022', '2017-2023'))

#-------
# Validation plot
#-------

text_sizes <- theme(text = element_text(size = 25), axis.text = element_text(size = 25), plot.title = element_text(size = 45), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
point_size <- guides(color = guide_legend(override.aes = list(size=3)))
facet_settings_FA <- theme(panel.background = element_blank(), panel.border = element_rect(fill = "transparent", colour = 'black'), strip.background.x = element_rect(fill = '#DEDB92', colour = 'black'), strip.background.y = element_rect(fill = '#DEDB92', colour = 'black'))
facet_settings_CS <- theme(panel.background = element_blank(), panel.border = element_rect(fill = "transparent", colour = 'black'), strip.background.x = element_rect(fill = '#DEBB92', colour = 'black'), strip.background.y = element_rect(fill = '#DEBB92', colour = 'black'))
facet_settings_ACOLITE <- theme(panel.border = element_rect(fill = "transparent", colour = 'black'), strip.background.x = element_rect(fill = '#8DB2D5', colour = 'black'), strip.background.y = element_rect(fill = '#8DB2D5', colour = 'black'))
facet_settings_comparison <- theme(panel.background = element_blank(), panel.border = element_rect(fill = "transparent", colour = 'black'), strip.background.x = element_rect(fill = '#c7ffbf', colour = 'black'), strip.background.y = element_rect(fill = '#c7ffbf', colour = 'black'))
colour_scheme <- c('#a2ddc2','#8DD5B5','#76B398','#60917B','#4F7061','#334D42','#1f2e28','#cc0066')


## need to code for average OA and standard error
csv_mean <- csv %>%
  group_by(AOI, CS_product, time_range, threshold) %>%
  summarise(mean_OA = mean(OA), sd_OA = sd(OA), se_OA = sd(OA)/sqrt(20), lowerCI_OA = quantile(OA, probs = 0.25), upperCI_OA = quantile(OA, probs = 0.975), # order of the new column name matters for the pivot_longer's names_sep
            mean_PA = mean(PA), sd_PA = sd(PA), se_PA = sd(PA)/sqrt(20), lowerCI_PA = quantile(PA, probs = 0.25), upperCI_PA = quantile(PA, probs = 0.975),
            mean_UA = mean(UA), sd_UA = sd(UA), se_UA = sd(UA)/sqrt(20), lowerCI_UA = quantile(UA, probs = 0.25), upperCI_UA = quantile(UA, probs = 0.975), 
            mean_F1sg = mean(F1_seagrass), sd_F1sg = sd(F1_seagrass), se_F1sg = sd(F1_seagrass)/sqrt(20), lowerCI_F1sg = quantile(F1_seagrass, probs = 0.25), upperCI_F1sg = quantile(F1_seagrass, probs = 0.975), 
            mean_TD = mean(td_size), sd_TD = sd(td_size),
            mean_VD = mean(vd_size), sd_VD = sd(vd_size),
  ) %>%
  pivot_longer(cols = -c(AOI, CS_product, time_range, threshold), names_to = c('.value', 'group'), names_sep = "_")
# csv_mean$order <- ifelse(csv_mean$group == 'OA', 1,
#                          ifelse(csv_mean$group == 'PA', 2,
#                                 ifelse(csv_mean$group == 'UA', 3,
#                                        ifelse(csv_mean$group == 'F1sg', 4,
#                                               ifelse(csv_mean$group == 'TD', 5,
#                                                      ifelse(csv_mean$group == 'VD', 6,
#                                                             NA))))))
csv_mean$group <- factor(csv_mean$group, levels= c('F1sg','PA','UA','OA','TD','VD'), labels= c('F1 Score (Seagrass)','PA','UA','Overall Accuracy','TD','VD')) ## supersedes $order column
csv_mean$lowerse <- csv_mean$mean-1.96*csv_mean$se
csv_mean$upperse <- csv_mean$mean+1.96*csv_mean$se

### NEW VERSION ###
p_mean_accuracies_FA <- ggplot(csv_mean %>% filter(CS_product == "Original approach") %>% filter(group == "Overall Accuracy" | group == "F1 Score (Seagrass)"), aes(x = AOI, y = mean, colour = time_range)) +
  geom_point(position = position_dodge(.25), size = 2.2) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se), width = .2, linewidth = 1.1, position = position_dodge(.25)) +
  ylim(0.2, 1) + ylab('Accuracies') +
  scale_colour_manual(name = "Temporal Interval", values = colour_scheme) +
  facet_grid(group ~ .) +
  text_sizes + point_size + facet_settings_FA +
  ggtitle('Full Archive Composite Time Interval Optimisation')

p_mean_accuracies_CS <- ggplot(csv_mean %>% filter(CS_product == "Cloud Score+ (CS)") %>% filter(group == "Overall Accuracy" | group == "F1 Score (Seagrass)"), aes(x = AOI, y = mean, colour = factor(threshold))) +
  geom_point(position = position_dodge(.5), size = 2.2) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se), width = .5, linewidth = 1, position = position_dodge(.5)) +
  ylim(0, 1) + ylab('Accuracies') +
  # scale_colour_brewer(name = "Threshold", palette = "BuPu") + # can't see lightest colour
  scale_colour_manual(name = "Threshold", values = RColorBrewer::brewer.pal(7, "Blues")[2:7]) +
  facet_grid(group ~ time_range) +
  text_sizes + point_size + facet_settings_CS +
  ggtitle('CS Composite Threshold Optimisation')

p_mean_accuracies_CDF <- ggplot(csv_mean %>% filter(CS_product == "Cloud Score+ (CDF)") %>% filter(group == "Overall Accuracy" | group == "F1 Score (Seagrass)"), aes(x = AOI, y = mean, colour = factor(threshold))) +
  geom_point(position = position_dodge(.5), size = 2.2) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se), width = .5, linewidth = 1, position = position_dodge(.5)) +
  ylim(0, 1) + ylab('Accuracies') +
  # scale_colour_brewer(name = "Threshold", palette = "BuPu") + # can't see lightest colour
  scale_colour_manual(name = "Threshold", values = RColorBrewer::brewer.pal(7, "Blues")[2:7]) +
  facet_grid(group ~ time_range) +
  text_sizes + point_size + facet_settings_CS +
  ggtitle('CDF Composite Threshold Optimisation')

## ===
## post-hoc temporal interval selection
csv_temporal <- csv_mean %>%
  filter((group == "Overall Accuracy" | group == "F1 Score (Seagrass)")) %>% # can be combined with the latter, but cleaner to separate
  filter(
    (CS_product == 'Cloud Score+ (CS)' & time_range == '2022' & threshold == 60) |
    (CS_product == 'Cloud Score+ (CS)' & time_range == '2023' & threshold == 55) |
    (CS_product == 'Cloud Score+ (CS)' & time_range == '2022-2023' & threshold == 70) |
    (CS_product == 'Cloud Score+ (CDF)' & time_range == '2022' & threshold == 80) |
    (CS_product == 'Cloud Score+ (CDF)' & time_range == '2023' & threshold == 75) |
    (CS_product == 'Cloud Score+ (CDF)' & time_range == '2022-2023' & threshold == 75)
  )

p_temporal_accuracies <- ggplot(csv_temporal, aes(x = AOI, y = mean, colour = time_range)) +
  geom_point(position = position_dodge(.25), size = 2.2) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se), width = .2, linewidth = 1.1, position = position_dodge(.25)) +
  ylim(0, 1) + ylab('Accuracies') +
  scale_colour_manual(name = "Temporal Interval", values = colour_scheme[c(4,6,8)]) +
  facet_grid(group ~ CS_product) +
  text_sizes + point_size + facet_settings_CS +
  ggtitle('CS and CDF Composites Time Interval Optimisation')

## ===
## post-hoc cross-product comparison
csv_crosscompare <- csv_mean %>%
  filter((group == "Overall Accuracy" | group == "F1 Score (Seagrass)")) %>% # can be combined with the latter, but cleaner to separate
  filter(
    (CS_product == 'Original approach' & time_range == '2017-2023') |
    (CS_product == 'Cloud Score+ (CS)' & time_range == '2022' & threshold == 60) |
    (CS_product == 'Cloud Score+ (CDF)' & time_range == '2022-2023' & threshold == 75) |
    (CS_product == 'ACOLITE')
  )

p_crosscompare <- ggplot(csv_crosscompare, aes(x = AOI, y = mean, colour = CS_product)) +
  geom_point(position = position_dodge(.25), size = 2.2) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax = mean+1.96*se), width = .2, linewidth = 1.1, position = position_dodge(.25)) +
  ylim(0, 1) + ylab('Accuracies') +
  scale_colour_manual(name = "Product", values = c('#DEDB92','#DEBB92', '#c4843b','#8DB2D5')) +
  facet_grid(group ~ .) +
  text_sizes + point_size + facet_settings_comparison +
  ggtitle('Cross-Approach Comparison')

## ===
## Scatterplot of F1 to OA
p_scatter_OA_F1_combined <- ggplot(csv, aes(x = OA, y = F1_seagrass, shape = AOI, colour = AOI)) +
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se=TRUE, colour='black', formula=y~x) +
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x = c(0.6, 0.93), label.y = c(0.97,0.97), size = 8) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = c(0.6, 0.93), label.y = c(1,1), size = 8) +
  ylim(0,1) + # xlim(0.7,1) +
  text_sizes + 
  point_size +
  facet_settings_comparison + 
  xlab("Overall Accuracy") + ylab("F1 Score (Seagrass)") +
  ggtitle('Combined Scatterplot of Product F1 (seagrass) to Overall accuracy by AOI')

p_scatter_OA_F1_FA <- ggplot(csv %>% filter(CS_product == "Original approach"), aes(x = OA, y = F1_seagrass, shape = AOI, colour = AOI)) +
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se=TRUE, colour='black', formula=y~x) +
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x = c(0.6, 0.93), label.y = c(0.97,0.97), size = 8) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = c(0.6, 0.93), label.y = c(1,1), size = 8) +
  ylim(0,1) + # xlim(0.7,1) +
  text_sizes + 
  point_size +
  facet_settings_comparison + 
  xlab("Overall Accuracy") + ylab("F1 Score (Seagrass)") +
  ggtitle('FA Scatterplot of Product F1 (seagrass) to Overall accuracy by AOI')

p_scatter_OA_F1_CS <- ggplot(csv %>% filter(CS_product == "Cloud Score+ (CS)"), aes(x = OA, y = F1_seagrass, shape = AOI, colour = AOI)) +
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se=TRUE, colour='black', formula=y~x) +
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x = c(0.6, 0.93), label.y = c(0.97,0.97), size = 8) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = c(0.6, 0.93), label.y = c(1,1), size = 8) +
  ylim(0,1) + # xlim(0.7,1) +
  text_sizes + 
  point_size +
  facet_settings_comparison + 
  xlab("Overall Accuracy") + ylab("F1 Score (Seagrass)") +
  ggtitle('CS Scatterplot of Product F1 (seagrass) to Overall accuracy by AOI')

p_scatter_OA_F1_CDF <- ggplot(csv %>% filter(CS_product == "Cloud Score+ (CDF)"), aes(x = OA, y = F1_seagrass, shape = AOI, colour = AOI)) +
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se=TRUE, colour='black', formula=y~x) +
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x = c(0.6, 0.93), label.y = c(0.97,0.97), size = 8) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = c(0.6, 0.93), label.y = c(1,1), size = 8) +
  ylim(0,1) + # xlim(0.7,1) +
  text_sizes + 
  point_size +
  facet_settings_comparison + 
  xlab("Overall Accuracy") + ylab("F1 Score (Seagrass)") +
  ggtitle('CDF Scatterplot of Product F1 (seagrass) to Overall accuracy by AOI')

p_scatter_OA_F1_ACOLITE <- ggplot(csv %>% filter(CS_product == "ACOLITE"), aes(x = OA, y = F1_seagrass, shape = AOI, colour = AOI)) +
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se=TRUE, colour='black', formula=y~x) +
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x = c(0.6, 0.93), label.y = c(0.97,0.97), size = 8) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = c(0.6, 0.93), label.y = c(1,1), size = 8) +
  ylim(0,1) + # xlim(0.7,1) +
  text_sizes + 
  point_size +
  facet_settings_comparison + 
  xlab("Overall Accuracy") + ylab("F1 Score (Seagrass)") +
  ggtitle('ACOLITE Scatterplot of Product F1 (seagrass) to Overall accuracy by AOI')

