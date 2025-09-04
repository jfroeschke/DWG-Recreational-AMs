library(tidyverse)
library(zoo)
# Create the data frame
df <- data.frame(
  Year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
           2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
           2020, 2021, 2022, 2023),
  Snowy_Grouper_Comm = c(184381,175591,134999,218137,180487,182647,171616,175531,199782,183998,
                         90180,132971,168759,108689,159857,108980,94830,87587,89416,91430,
                         99072,91362,76075,64877),
  Speckled_Hind_Comm = c(64242,62366,48220,82000,101745,88636,64620,79784,41187,68292,
                         15359,24925,43344,34922,72241,55550,41151,51061,60618,67082,
                         36187,41451,27776,34297),
  Warsaw_Grouper_Comm = c(161543,145278,217031,265480,176895,164292,140662,86376,88622,117695,
                          56496,61661,86212,103074,75426,55502,44635,44362,35976,33590,
                          22707,17419,15012,12056),
  Yellowedge_Grouper_Comm = c(1349383,873682,925582,1291967,1020564,918521,824952,1002080,946423,972112,
                              443887,558908,667785,673349,773621,735218,709349,677926,677310,804558,
                              665406,681679,461661,514547),
  Total_Comm_Landings = c(1759549,1256917,1325832,1857584,1479691,1354096,1201850,1343771,1276014,1342097,
                          605922,778465,966100,920034,1081145,955250,889965,860936,863320,996660,
                          823372,831911,580524,625777),
  Snowy_Grouper_Rec = c(NA,2804,5763,695,3273,1771,1610,1035,2426,1727,
                        11177,8108,69469,50297,61282,12174,3365,2167,6335,5401,
                        4883,11873,15335,10362),
  Speckled_Hind_Rec = c(NA,3076,1413,13222,25546,158,42667,5316,958,697,
                        14006,2419,4115,205,508,778,14666,345,363,5665,
                        222,288,838,2856),
  Warsaw_Grouper_Rec = c(NA,90316,61520,48588,89214,29522,84972,9498,17434,42449,
                         5507,6621,35329,18774,72897,3636,8773,8969,55304,3225,
                         18865,2216,2850,2906),
  Yellowedge_Grouper_Rec = c(NA,1370,2159,329,1162,105090,2546,2822,1252,3209,
                             28403,9461,1212,6198,18982,15669,22637,4139,39221,74516,
                             33522,15286,19826,52314),
  Total_Rec_Landings = c(NA,97567,70855,62834,119194,136541,131796,18672,22069,48081,
                         59094,26609,110125,75474,153669,32258,49441,15619,101224,88807,
                         57491,29663,38848,68438),
  Total_Landings = c(1773466,1354484,1396687,1920418,1598885,1490637,1333646,1362443,1298083,1390178,
                     665016,805074,1076225,995508,1234814,987508,939406,876555,964544,1085467,
                     880863,861574,619372,694215)
)

library(slider)
rec <- df %>% select(Total_Rec_Landings) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Year=2000:2023)

# Compute 3-year rolling average
rec <- rec %>%
  filter(Year > 2000) %>% 

  mutate(Rolling3yr_Avg = slide_dbl(Total_Rec_Landings,
                                    mean,
                                    .before = 2,
                                    .complete = FALSE))



# Compute means (ignore NAs)
mean_total <- mean(rec$Total_Rec_Landings, na.rm = TRUE)
mean_rolling <- mean(rec$Rolling3yr_Avg, na.rm = TRUE)

## rec acl
rec_acl <- 56668

# Convert to long format
df_long <- rec %>%
  pivot_longer(cols = c(Total_Rec_Landings, Rolling3yr_Avg),
               names_to = "Type",
               values_to = "Value")



# Plot
p1 <- ggplot(df_long, aes(x = Year, y = Value, color = Type)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  # Horizontal lines for averages
  geom_hline(yintercept = mean_total, color = "steelblue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = mean_rolling, color = "orange", linetype = "dashed", size = 1) +
  geom_hline(yintercept = rec_acl, color = "purple", linetype = "dotted", size = 1) +
  scale_color_manual(values = c("Total_Rec_Landings" = "steelblue", "Rolling3yr_Avg" = "orange")) +
  labs(title = "Total Recreational Landings vs 3-Year Rolling Average",
       y = "Landings",
       color = "Metric") +
  theme_minimal(base_size = 14)

library(plotly)
ggplotly(p1)

rec2 <- rec %>% 
  mutate(Above_ACL = if_else(Total_Rec_Landings > 56668, 1, 0)) %>% 
  mutate(Above_3_yr_ACL = if_else(Rolling3yr_Avg > 56668, 1, 0))

total_Above_ACL <- sum(rec2$Above_ACL)
total_3_yr_Above_ACL <- sum(rec2$Above_3_yr_ACL)

### simulation: 
# Original data
values <- c(97567, 70855, 62834, 119194, 136541, 131796, 18672, 22069, 48081, 59094,
            26609, 110125, 75474, 153669, 32258, 49441, 15619, 101224, 88807, 57491,
            29663, 38848, 68438)

# Proportional standard error
pse <- 0.5  # 50%

# Simulate one sample per value
set.seed(123)  # for reproducibility
simulated_values <- rnorm(length(values), mean = values, sd = values * pse)




n_repeats <- 100

# Simulate 5 samples per value
set.seed(123)  # reproducibility
simulated_df <- tibble(
  Original = values,
  Sample1 = rnorm(length(values), mean = values, sd = values * pse),
  Sample2 = rnorm(length(values), mean = values, sd = values * pse),
  Sample3 = rnorm(length(values), mean = values, sd = values * pse),
  Sample4 = rnorm(length(values), mean = values, sd = values * pse),
  Sample5 = rnorm(length(values), mean = values, sd = values * pse)
)

# Pivot to long format
simulated_long <- simulated_df %>%
  mutate(ID = row_number()) %>%  # optional: keep track of row number
  pivot_longer(
    cols = starts_with("Sample"),
    names_to = "Sample",
    values_to = "Value"
  ) %>%
  mutate(Sample = str_remove(Sample, "Sample")) %>% 
  mutate(Value = pmax(Value, 0))# keep iteration as number

#################################### Single Year Average #####################################

# Proportional standard error
pse <- 0.1  # 50%
n_repeats <- 100

# Simulate 100 samples per value
set.seed(123)
simulated_df <- map_dfc(1:n_repeats, ~ rnorm(length(values), mean = values, sd = values * pse)) %>%
  set_names(paste0("Sample", 1:n_repeats))

# Pivot to long format
simulated_long <- simulated_df %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = starts_with("Sample"),
    names_to = "Sample",
    values_to = "Value"
  ) %>%
  mutate(
    Sample = str_remove(Sample, "Sample"),
    Value = pmax(Value, 0)  # recode negative values to 0
  )

simulated_long

### 
library(tidyverse)

# Count per ID and convert to percentage
percent_above <- simulated_long %>%
  mutate(Value_over = if_else(Value > 56668, 1, 0))

### Percent of values > 56668
Percent_over_1_yr <- sum(percent_above$Value_over/nrow(simulated_long))
Percent_over_1_yr


percent_above_plot_data <- percent_above %>% 
  mutate(Year = ID + 2000)

p1 <- ggplot(percent_above_plot_data , aes(x = Year, y = Value, group = Sample, color = as.factor(Sample))) +
  geom_line(alpha = 0.4) +  # faint lines for clarity
  theme_minimal() +
  labs(
    title = "Simulated Values by Year and Sample",
    x = "Year",
    y = "Value",
    color = "Sample"
  ) +
  theme(legend.position = "none") 

p2 <- p1 +
  geom_line(
    data = rec2,
    aes(x = Year, y = Total_Rec_Landings),
    color = "black",
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = rec_acl, color = "purple", linetype = "dotted", size = 1)
ggsave(file="1year.png")
#################################### Single Year Average #####################################

#################################### three Year Average #####################################

# Proportional standard error
values_3 <- rec2$Rolling3yr_Avg

pse <- 0.5  # 50%
n_repeats <- 100

# Simulate 100 samples per value
set.seed(123)
simulated_df_3 <- map_dfc(1:n_repeats, ~ rnorm(length(values_3), mean = values_3, sd = values_3 * pse)) %>%
  set_names(paste0("Sample", 1:n_repeats))

# Pivot to long format
simulated_long_3 <- simulated_df_3 %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = starts_with("Sample"),
    names_to = "Sample",
    values_to = "Value"
  ) %>%
  mutate(
    Sample = str_remove(Sample, "Sample"),
    Value = pmax(Value, 0)  # recode negative values to 0
  )

simulated_long_3

### 
library(tidyverse)

# Count per ID and convert to percentage
percent_above_3 <- simulated_long_3 %>%
  mutate(Value_over = if_else(Value > 56668, 1, 0))

### Percent of values > 56668
Percent_over_3_yr <- sum(percent_above_3$Value_over/nrow(simulated_long))
Percent_over_3_yr

### Percent of values > 56668
Percent_over_3_yr <- sum(percent_above_3$Value_over/nrow(simulated_long_3))

percent_above_plot_data_3 <- percent_above_3 %>% 
  mutate(Year = ID + 2000)

p3 <- ggplot(percent_above_plot_data_3 , aes(x = Year, y = Value, group = Sample, color = as.factor(Sample))) +
  geom_line(alpha = 0.4) +  # faint lines for clarity
  theme_minimal() +
  labs(
    title = "Simulated Values by Year and Sample",
    x = "Year",
    y = "Value",
    color = "Sample"
  ) +
  theme(legend.position = "none") 

p4 <- p3 +
  geom_line(
    data = rec2,
    aes(x = Year, y = Rolling3yr_Avg),
    color = "black",
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = rec_acl, color = "purple", linetype = "dotted", size = 1)
ggsave(file="3year.png")

