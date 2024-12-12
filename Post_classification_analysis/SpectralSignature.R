### Venezuela Spectral Profile analysis
### Lee, Chengfa Benjamin

#-------
# Library
#-------

library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

#-------
# Directory
#-------

folder <- vector("list", 0)
folder$dir_data <- "/Data/Spectral_FA"
folder$data <- list.files(folder$dir_data) 

#-------
# Import data
#-------

import_file <- function(dir_subfolder, file_name) {
  csv <- read.csv(paste0(dir_subfolder,'/', file_name)) %>%
    select(-c(".geo","system.index")) 
  csv$label <- factor(csv$label, levels = c("sand","deepwater","seagrass","seagrass_Carolina","turbid","coral"), 
                      labels = c('Sand','Deep Water','Seagrass','Seagrass (Reference)','Turbid','Coral (Reference)'))
  
  csv$OWT <- strsplit(file_name, '_') %>% 
    unlist() %>% 
    "["(5) %>%
    gsub(pattern = '.csv', replacement = '', x = .)
  
  output <- csv %>%
    pivot_longer(names(csv %>% select(-c('class','label','gebco','slope', 'bathy_edu','slope2','OWT'))), names_to = "Band")
  output$Band <- factor(output$Band, levels = c(names(csv %>% select(-c('class','label')))))
  
  return(output)
}

csv_coast <- import_file(folder$subfolder, folder$data[1])
csv_openWater <- import_file(folder$subfolder, folder$data[2])
csv <- rbind(csv_coast, csv_openWater)
  csv$OWT <- factor(csv$OWT, levels = c('Coastal','openWater'), labels = c('Coastal','Open Water'))

#-------
# Spectral plot
#-------

background_settings <- theme(panel.background = element_blank(), panel.border = element_rect(fill = 'transparent', colour = 'black'), panel.grid.major = element_line(colour = 'grey99'),
                             strip.background.x = element_rect(fill = '#ffdde2', colour = 'black'), strip.background.y = element_rect(fill = '#ffdde2', colour = 'black'),
                             legend.position = 'bottom')
text_sizes <- theme(text = element_text(size = 25), axis.text = element_text(size = 25), plot.title = element_text(size = 45), legend.text = element_text(size = 25), legend.title = element_text(size = 30))

## mean & median line
csv_stat <- csv %>%
  group_by(label, Band, OWT) %>%
  summarise(mean_value = mean(value), median_value = median(value), sd_value = sd(value), lower_se = mean(value)-1.96*sd(value)/sqrt(length(n)), upper_se = mean(value)+1.96*sd(value)/sqrt(length(n)), quartile_first = quantile(value, c(0.25)), quartile_third = quantile(value, c(0.75)), value = value, gebco = gebco, slope = slope, class = class, OWT = OWT, label = label) %>%
  ungroup()

ymax <- 1 #max(L1C$value)
ymin <- 0 #min(L1C$value)

scale_fill_values <- c("red","blue","green2",'green4',"yellow3","coral")

## spectral profile (combined)
p_ref_label_mean <- ggplot(csv_stat , aes(x = Band, y = mean_value, fill = label)) +
  geom_boxplot(aes(y = value)) +
  scale_fill_manual("", values = scale_fill_values) +
  ylab("Values") +
  ggtitle("Class Spectral Signatures with respect to the Seven-Year Composite") +
  facet_wrap(~ OWT) +
  background_settings +
  text_sizes
