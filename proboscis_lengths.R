## Code to imulate proboscis lengths for six species of Bombus.
## Means and standard deviations are from Table 10 of Macior 1974 (page 59).
## Macior mesured 50 individuals for most species and castes, so
## I set all to 50 for purpose of the exercise.

## I generate data for queen and worker castes but I currently
## save only the queen data. Workers have shorter proboscis on
## average but qualitative results are the same.

library(tidyverse)
#library(ggthemes)

frigidus_queen <- rnorm(mean = 7.27, sd = 0.37, n = 50)
frigidus_worker <- rnorm(mean = 5.73, sd = 0.40, n = 50)
frigidus_queen <- round(frigidus_queen, digits = 1)

bifarius_queen <- rnorm(mean = 8.38, sd = 0.40, n = 50)
bifarius_worker <- rnorm(mean = 5.75, sd = 0.37, n = 50)
bifarius_queen <- round(bifarius_queen, digits = 1)

sylvicola_queen <- rnorm(mean = 8.50, sd = 0.48, n = 50)
sylvicola_worker <- rnorm(mean = 5.79, sd = 0.58, n = 50)
sylvicola_queen <- round(sylvicola_queen, digits = 1)

flavifrons_queen <- rnorm(mean = 10.23, sd = 0.71, n = 50)
flavifrons_worker <- rnorm(mean = 7.81, sd = 0.80, n = 50)
flavifrons_queen <- round(flavifrons_queen, digits = 1)

# Name change per Williams et al 2015
# Pyke 1982 used B. kirbyellus, which was later considered a 
# synonym of B. balteatus. Pyke et al. 2012 used B. balteatus.
kirbiellus_queen <- rnorm(mean = 12.11, sd = 0.40, n = 50)
kirbiellus_worker <- rnorm(mean = 9.36, sd = 0.62, n = 50)
kirbiellus_queen <- round(kirbiellus_queen, digits = 1)


appositus_queen <- rnorm(mean = 12.81, sd = 0.38, n = 50)
appositus_worker <- rnorm(mean = 10.48, sd = 0.95, n = 50)
appositus_queen <- round(appositus_queen, digits = 1)


# Check the distribution of queens so that they
# are approximately normal, w/o obvious outliers.
ggplot() +
  geom_histogram(aes(x = frigidus_queen), bg = "darkgreen", alpha = 0.5) +
  geom_histogram(aes(x = bifarius_queen), bg = "purple", alpha = 0.5) +
  geom_histogram(aes(x = kirbiellus_queen), bg = "darkorange", alpha = 0.5) +
  geom_histogram(aes(x = appositus_queen), bg = "red", alpha = 0.5) +
  geom_histogram(aes(x = sylvicola_queen), bg = "blue", alpha = 0.5) +
  geom_histogram(aes(x = flavifrons_queen), bg = "gray30", alpha = 0.5)

# Same for workers
ggplot() +
  geom_histogram(aes(x = frigidus_worker), bg = "darkgreen", alpha = 0.5) +
  geom_histogram(aes(x = bifarius_worker), bg = "purple", alpha = 0.5) +
  geom_histogram(aes(x = kirbyellus_worker), bg = "darkorange", alpha = 0.5) +
  geom_histogram(aes(x = appositus_worker), bg = "red", alpha = 0.5) +
  geom_histogram(aes(x = sylvicola_worker), bg = "blue", alpha = 0.5) +
  geom_histogram(aes(x = flavifrons_worker), bg = "gray30", alpha = 0.5)


# Make tibble of queens for class use
bees_df <- tibble(appositus = appositus_queen, 
                  bifarius = bifarius_queen, 
                  flavifrons = flavifrons_queen,
                  frigidus = frigidus_queen, 
                  kirbiellus = kirbiellus_queen, 
                  sylvicola = sylvicola_queen)

# Save the file.
write_csv(bees_df, "bombus_queens.csv")


## Not used right now.

# Simualting coralla depths shown in Table 1 of 
# Pleasants 1980. Ecology 61: 1446-1459.
bifarius_corolla <- rnorm(mean = 5.38, sd = 0.1, n = 20)
kirbyellus_corolla <- rnorm(mean = 14.18, sd = 0.1, n = 20)
appositus_corolla <- rnorm(mean = 14.11, sd = 0.1, n = 20)


bee_queens <- c(mean(bifarius_queen), mean(kirbyellus_queen), mean(appositus_queen))
flower_corollas <- c(mean(bifarius_corolla), mean(kirbyellus_corolla), mean(appositus_corolla))

bf <- tibble(bee_queens, flower_corollas)
bf
ggplot() +
  geom_point(aes(x = bee_queens, y = flower_corollas))
