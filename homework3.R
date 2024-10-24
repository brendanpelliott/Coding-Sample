#load packages
library(dbplyr)
library(tidyverse)
library(ggplot2)
library(bunching)

#Question 1.1

#load data
all_llma_nc <- read.csv("all_llma_nc.csv")
conforming_limits <- read.csv("conformingloanlimits.csv")

#Question 1.2
#Subset to desired loans
desired_loans <- subset(all_llma_nc, product_type == 10 &
                          original_term %in% c(180, 360) &
                          loan_type == 1 & loan_purpose %in% c(1,2,3,4,5)
                        &
                          year %in% c(2000, 2001, 2002, 2003, 20004, 2005,
                                      2006, 2007))
#Question 1.3

# merge limits with loans
loans_with_limits <- merge(desired_loans, conforming_limits, by = "year")

#Question 1.4
#calc deviation
loans_with_limits <- loans_with_limits %>% mutate(deviation =
                                                    log(original_balance) - log(conformingloanlimit))
loans_clean <- loans_with_limits[!is.na(loans_with_limits$deviation) &
                                   !is.na(loans_with_limits$initial_interest_rate), ]
loans_filtered <- subset(loans_clean, abs(deviation) <= 0.5) #remove
deviations outside [-0.5, 0.5]

#Question 1.5
#create bins
deviation_range <- range(loans_filtered$deviation)
boundary <- 0.5
num_bins <- 200
bin_width <- boundary / (num_bins / 2)
intervals <- seq(-boundary, boundary, by = bin_width)
intervals <- round(intervals, digits = 5)
loans_filtered$bin <- cut(loans_filtered$deviation, breaks = intervals)
#cut data into respective bins


#Question 1.6
#create bin dataset
num_loans <- nrow(loans_filtered)
loans_per_bin <- table(loans_filtered$bin)
bin_share <- loans_per_bin / num_loans #calculate share of loans in bin
mean_interest <- tapply(loans_filtered$initial_interest_rate,
                        loans_filtered$bin, mean)
bin_data <- data.frame(bin = names(loans_per_bin),
                       loans_in_bin = as.integer(loans_per_bin),
                       share = as.double(bin_share),
                       mean_rate = as.double(mean_interest))
bin_data <- bin_data %>% mutate(bin_label = ((intervals[1:200] +
                                                bin_width)*200))
#Question 1.7

#calculate variation around CLL
#using weighted average of interest rates in two bins before CLL and two bins after CLL
weighted_interest_rate_before <- (442/7255)*6.14 + (6813/7255)*6.27
weighted_interest_rate_after <- (65/110)*6.30 + (45/110)*7.09
diff <- weighted_interest_rate_after - weighted_interest_rate_before

#Question 2
#subset to loans only in 2003 within 100000 of CLL (which is 322700)
loans_2003 <- subset(loans_filtered, year == 2003 & original_balance >=
                       222700 & original_balance <= 422700)
#create 2003 bins:
num_loans_2003 <- nrow(loans_2003)
loans_per_bin_2003 <- table(loans_2003$bin)
bin_share_2003 <- loans_per_bin_2003 / num_loans_2003
mean_interest_2003 <- tapply(loans_2003$initial_interest_rate,
                             loans_2003$bin, mean)
bin_data_2003 <- data.frame(bin = names(loans_per_bin_2003),
                            loans_in_bin = as.integer(loans_per_bin_2003),
                            share = as.double(bin_share_2003),
                            mean_rate = as.double(mean_interest_2003))
bin_data_2003 <- bin_data_2003 %>% mutate(bin_label = ((intervals[1:200] +
                                                          bin_width)*200))
bin_data_2003 <- subset(bin_data_2003, !is.na(mean_rate))

#Question 2.1
#plot the jump
bin_data_2003 %>%
  select(bin_label, mean_rate) %>%
  mutate(D = as.factor(ifelse(bin_label >= 0, 1, 0))) %>%
  ggplot(aes(x = bin_label, y = mean_rate, color = D)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Deviation from CLL", y = "Mean Interest Rate", title = "Mean
Interest Rate by Loan Size")

#Question 2.2

#plot densities
ggplot(bin_data_2003, aes(x = bin_label, y=share)) +
  geom_bar(stat='identity') +
  labs(x = "Deviation from CLL", y = "Loan Share", title = "Loan Size
Density (2003)")

#Question 2.3
#recenter data
loans_2003 <- loans_2003 %>% mutate(recentered_dev = deviation+0.1)
#bunching algorithm
bunchit(z_vector = loans_2003$recentered_dev,
        zstar = 0.1,
        binv = "max",
        binwidth = 0.005,
        poly = 13,
        bins_l = 50,
        bins_r = 50,
        t0 = 0.03,
        t1 = 0.09)