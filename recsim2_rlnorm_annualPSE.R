#Based on recsim2_rlnorm
  #Simulating 100 random lognormal values separately for each year
    #e.g., instead of simulating 100 x 23 year time series, draw Y1 x 100 times 
  #Mean for each year based on actual annual DWG landings and simulated annual PSE (could be replaced with actual PSE values)

############################### Set a simulation with a mean at the ACL and pse of 50

#################################### ACL Single Year Average #####################################
library(tidyverse)
library(zoo)
library(slider)
library(dplyr)

## rec acl
rec_acl <- 56668


# Original data
values <- c(97567, 70855, 62834, 119194, 136541, 131796, 18672, 22069, 48081, 59094,
            26609, 110125, 75474, 153669, 32258, 49441, 15619, 101224, 88807, 57491,
            29663, 38848, 68438)

ggplot(data.frame(values), aes(values)) + geom_density(fill='chartreuse') + theme_bw()

# Proportional standard error - separate PSE value for each year
  #SD per year = annual landings * annual PSE

pse <- rnorm(23, 0.4, 0.1) #random here, but can be replaced with known PSE

mean_value <- values
n_repeats <- 100
sd_val <- pse * mean_value

###  Separate simulation of 100 values for each year ###

nyears <- 1:length(values)

annualsim <- lapply(nyears, function(x) {
  location <- log(mean_value[x]^2/sqrt(sd_val[x]^2 + mean_value[x]^2))
  shape <- sqrt(log(1+(sd_val[x]^2/mean_value[x]^2)))
  
  rlnorm(n_repeats, mean = location, sd = shape)
  
})

# simulated_df_acl <- map_dfc(1:23, ~ rlnorm(n_repeats, mean = location, sd = shape)) %>%
#   set_names(paste0("Sample", 1:23))

#Estimating mean/sd for rlnorm
# location <- log(mean_value^2/sqrt(sd_val^2 + mean_value^2))
# shape <- sqrt(log(1+(sd_val^2/mean_value^2)))

#Check simulated distribution
# lnorm_test <- rlnorm(100000, mean=location, sd=shape)
# hist(lnorm_test)
# ggplot(data.frame(lnorm_test), aes(lnorm_test)) + geom_density(fill='chartreuse') + theme_bw()
# mean(lnorm_test)
# sd(lnorm_test)

#Simulate 100 samples per value - lognormal distribution
#set.seed(456)
# simulated_df_acl <- map_dfc(1:n_repeats, ~ rlnorm(length(values), mean = location, sd = shape)) %>%
#   set_names(paste0("Sample", 1:n_repeats))


#Format to match JF simulated_df_acl output to run remaining script

simulated_df_acl <- as.data.frame(do.call(rbind, annualsim))

names(simulated_df_acl) <- paste0('Sample', 1:n_repeats)

# Pivot to long format
simulated_long_acl <- simulated_df_acl %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = starts_with("Sample"),
    names_to = "Sample",
    values_to = "Value"
  ) %>%
  mutate(
    Sample = str_remove(Sample, "Sample"),
    Value = pmax(Value, 0)#,# recode negative values to 0
    # Value = pmax(Value, 120000) # recode value > 120k to 120k
  )

#Cap - set maximum value; replace simulated values over cap with cap
cap <- max(values)

#Check percent above cap before adjusting
length(simulated_long_acl$Value[simulated_long_acl$Value>cap])/length(simulated_long_acl$Value)

#Replace
simulated_long_acl$Value <- ifelse(simulated_long_acl$Value > cap, cap, simulated_long_acl$Value)


# Count per ID and convert to percentage
percent_above_acl <- simulated_long_acl %>%
  mutate(Value_over = if_else(Value > 56668, 1, 0))

### Percent of values > 56668
Percent_over_1_yr_acl <- sum(percent_above_acl$Value_over/nrow(simulated_long_acl))
Percent_over_1_yr_acl


percent_above_plot_data_acl <- percent_above_acl %>% 
  mutate(Year = ID + 2000)

df_summary_acl <- percent_above_plot_data_acl %>%
  group_by(Year) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

p1_acl <- ggplot(percent_above_plot_data_acl , aes(x = Year, y = Value, group = Sample, color = as.factor(Sample))) +
  geom_line(alpha = 0.4) +  # faint lines for clarity
  theme_minimal() +
  labs(
    title = "Simulated Values by Year and Sample",
    x = "Year",
    y = "Value",
    color = "Sample"
  ) +
  theme(legend.position = "none") +
  geom_hline(yintercept = rec_acl, color = "purple", linetype = "dotted", size = 1)
p2_acl <- p1_acl +
  geom_line(
    data = df_summary_acl,
    aes(x = Year, y = mean_value),
    color = "black",
    size = 1.2,
    inherit.aes = FALSE
  )

p2_acl

#ggsave(file="1year_acl_pse_10.png")
#################################### ACL Single Year Average #####################################





#################################### Three year ACL Year Average #####################################


# Proportional standard error
# pse <- 0.3  # 50%
# mean_value <- rec_acl
# n_repeats <- 100
# sd_val <- pse * mean_value * sqrt(length(values))
# 
# #mean/sd for rlnorm
# location <- log(mean_value^2/sqrt(sd_val^2 + mean_value^2))
# shape <- sqrt(log(1+(sd_val^2/mean_value^2)))


# Simulate 100 samples per value
#set.seed(456)
# simulated_df_acl_3yr <- map_dfc(1:n_repeats, ~ rlnorm(length(values), mean = location, sd = shape)) %>%
#   set_names(paste0("Sample", 1:n_repeats))
# 
# # Pivot to long format
# simulated_long_acl_3yr <- simulated_df_acl_3yr %>%
#   mutate(ID = row_number()) %>%
#   pivot_longer(
#     cols = starts_with("Sample"),
#     names_to = "Sample",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Sample = str_remove(Sample, "Sample"),
#     Value = pmax(Value, 0)
#   )
# 
# 
# #Cap - set maximum value; replace simulated values over cap with cap
# 
# cap <- max(values)
# 
# simulated_long_acl_3yr$Value <- ifelse(simulated_long_acl_3yr$Value > cap, cap, simulated_long_acl_3yr$Value)
# 
# 
# 
# simulated_long_acl_3yr <- simulated_long_acl_3yr %>% 
#   arrange(Sample, ID) %>% 
#   mutate(Year=ID+2000)

#No resample - just use same random numbers generated above

simulated_long_acl_3yr <- simulated_long_acl %>%
  arrange(Sample, ID) %>%
  mutate(Year = ID + 2000)


df_sma <- simulated_long_acl_3yr %>%
  arrange(Sample, Year) %>%      # order by Sample and Year
  group_by(Sample) %>%           # calculate separately for each Sample
  mutate(Rolling3yr_Avg = slide_dbl(Value,
                                    mean,
                                    .before = 2,     # include current year + 2 previous = 3-year window
                                    .complete = FALSE)) %>%
  ungroup()
head(df_sma, 50)
# Check


### 
library(tidyverse)

# Count per ID and convert to percentage
percent_above_acl_3yr <- df_sma %>%
  mutate(Value_over = if_else(Rolling3yr_Avg > 56668, 1, 0))

### Percent of values > 56668
Percent_over_3_yr_acl <- sum(percent_above_acl_3yr$Value_over/nrow(df_sma))
Percent_over_3_yr_acl 


percent_above_plot_data_acl_3yr <- df_sma

df_summary_acl_3yr <- percent_above_plot_data_acl_3yr %>%
  group_by(Year) %>%
  summarise(mean_3yrvalue = mean(Rolling3yr_Avg, na.rm = TRUE))

p1_acl_3yr <- ggplot(percent_above_plot_data_acl_3yr , aes(x = Year, y = Rolling3yr_Avg, group = Sample, color = as.factor(Sample))) +
  geom_line(alpha = 0.4) +  # faint lines for clarity
  theme_minimal() +
  labs(
    title = "Simulated Values by Year and Sample",
    x = "Year",
    y = "Value",
    color = "Sample"
  ) +
  theme(legend.position = "none") +
  geom_hline(yintercept = rec_acl, color = "purple", linetype = "dotted", size = 1)
p2_acl_3yr <- p1_acl_3yr +
  geom_line(
    data = df_summary_acl_3yr,
    aes(x = Year, y = mean_3yrvalue),
    color = "black",
    size = 1.2,
    inherit.aes = FALSE
  )

p2_acl_3yr

#ggsave(file="3year_acl_pse_10.png")



############################### Set a simulation with a mean at the ACL and pse of 50

### Summary Table 

out <- data.frame(Annual_AM=Percent_over_1_yr_acl,
                  Three_Year_AM=Percent_over_3_yr_acl, PSE='annual' )
