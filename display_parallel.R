# Investigate the effect of display advertising using parallel slopes regression

# import data as display_data

library(readr)

display_data <- read_csv("display_data.csv")

glimpse(display_data)


# summarise performance of campaign

library(dplyr)
library(knitr)

display_data %>%
  summarise(cost = sum(spend),
            clicks = sum(clicks),
            impressions = sum(impressions),
            transactions = sum(transactions),
            revenue = sum(revenue)) %>%
  kable()


# Plot revenue against spend scatter plot

library(ggplot2)

ggplot(display_data, aes(x = spend, y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Daily Revenue Against Campaign Spend",
       x = "Daily spend (£)",
       y = "Daily revenue (£)") +
  theme_minimal()


# Determine R-squared correlation coefficient

lm(revenue ~ spend, data = display_data) %>%
  summary()


# Look at top and bottom of dataset

head(display_data, n = 5)
tail(display_data, n = 5)


# Change display variable to a factor

display_data$display <- factor(display_data$display)

class(display_data$display)


# Re-draw scatterplot to show display on or off

ggplot(display_data, aes(x = spend, y = revenue, colour = display)) +
  geom_point() +
  labs(title = "Daily Revenue Against Campaign Spend",
       x = "Daily spend (£)",
       y = "Daily revenue (£)") +
  theme_minimal()



# Build multiple regression model

mult_mod <- lm(revenue ~ spend + display, data = display_data)

summary(mult_mod)


# Calculate model values and plot parallel slopes regression lines

# Augment model

library(broom)

aug_mod <- augment(mult_mod)


ggplot(aug_mod, aes(x = spend, y = revenue, colour = display)) +
  geom_point() +
  labs(title = "Daily Revenue Against Campaign Spend",
       x = "Daily spend (£)",
       y = "Daily revenue (£)") +
  geom_line(aes(y = .fitted)) +
  theme_minimal()











