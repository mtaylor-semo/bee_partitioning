## Read and plot proboscis length vs corolla length
## Data scanned from Figure 5 of Pyke et al. 1982
## Data produced via WebPlatDigitizer


library(tidyverse)
library(ggthemes)

prob_raw <- read_csv("proboscis_corolla_lengths.csv")
prob_raw %>% 
  ggplot(aes(x = proboscis,
             y = corolla)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              col = "gray50") +
  theme_few() +
  labs(x = "Mean proboscis length (mm)",
       y = "Mean corolla length (mm)")

## To get regression formula shown in figure 5.
## Comes very close to actual value.
bee_lm <- lm(corolla ~ proboscis, data = prob_raw)

summary(bee_lm)
