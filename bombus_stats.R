## Script to perform various statistical analysis.
## These may be incorporated into Shiny apps for 
## students to statistically analyze the data.

library(tidyverse)

# ANOVA of proboscis lengths ----------------------------------------------
#

proboscis_lengths <- read_csv("data/proboscis_lengths.csv") %>% 
  gather(key = species, 
         value = length)

proboscis_aov <- aov(length ~ species, data = proboscis_lengths)

summary(proboscis_aov)

## Alternative form

proboscis_lm <- lm(length ~ species, data = proboscis_lengths)
proboscis_aov <- aov(proboscis_lm)
proboscis_tukey <- TukeyHSD(proboscis_aov)

# Linear regression of proboscis and corolla lengths ----------------------
#

pc_length <- read_csv("data/proboscis_corolla_lengths.csv")

pc_model <- lm(corolla ~ proboscis, data = pc_length)
pc_aov <- aov(pc_model)


summary(pc_model)

summary(pc_aov)


