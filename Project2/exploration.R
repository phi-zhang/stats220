library(tidyverse)
library(patchwork)
library(glmtoolbox)
library(ggthemes)

learning_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTUYXrWpNfyheP7lJBuhxufXLqbY9GIwkiBvNINx9uAsGYK_CKtbK_5NCQWYFByRLhVu-EEoLIMg5X9/pub?gid=744125984&single=true&output=csv")

# Rename variables
learning_data <- learning_data %>%
  rename(timestamp = "Timestamp",
         gender = "What is your gender?",
         snack_days = "How many days did you eat snacks over the past week, by your estimate?",
         snacks_per_day = "How many snacks did you eat per day, on average?",
         full_meals = "How many full meals did you eat per day, on average?",
         snack_types = "What types of snack did you eat?")

# Remove invalid observation
learning_data <- subset(learning_data, snack_days <= 7)

# Plot the snack type counts as a bar graph
snack_types_plottable <- learning_data %>%
  separate_rows(snack_types, sep = ", ") %>%
  select(gender, full_meals, snack_types) %>%
  filter(grepl("F|S|P|D", snack_types)) %>%
  mutate(snack_types = case_match(snack_types,
                                  "Fried snacks such as chips (hot chips/french fries)" ~ "fried",
                                  "Sweet snacks such as cookies" ~ "sweet",
                                  "Preserved savoury snacks such as chips (packaged chips/crisps)" ~ "preserved_savoury",
                                  "Fresh natural snacks such as salad" ~ "fresh_natural",
                                  "Preserved natural snacks such as pickles" ~ "preserved_natural",
                                  "Drinks such as bubble tea" ~ "drinks"))

snack_types_plot_base <- snack_types_plottable %>% ggplot() +
  scale_y_continuous(name = "Count") +
  theme_solarized()

snack_types_plot <- snack_types_plot_base +
  geom_bar(aes(x = snack_types), fill = "#BB9966") +
  scale_x_discrete(name = "Snack type", labels = c('Drinks (1)', 'Fresh (2)', 'Fried (3)', 'Preserved \n natural (4)', 'Savoury (5)', 'Sweet (6)')) +
  ggtitle("Snack types eaten by gender and full meals per day")

snack_types_gender <- snack_types_plot_base +
  geom_bar(aes(x = snack_types, fill = gender)) +
  scale_fill_discrete(name = "Gender") +
  scale_x_discrete(name = "Snack type", labels = c('1', '2', '3', '4', '5', '6'))

snack_types_meals <- snack_types_plot_base +
  geom_bar(aes(x = snack_types, fill = full_meals)) +
  scale_fill_discrete(name = "Full meals") +
  scale_x_discrete(name = "Snack type", labels = c('1', '2', '3', '4', '5', '6'))

snack_types_plot / (snack_types_gender + snack_types_meals)

# Bar graphs for meal and gender
bar_graph_base <- learning_data %>% ggplot() +
  scale_y_continuous(name = "Count") +
  theme_solarized()

meals_bar_graph <- bar_graph_base +
  geom_bar(aes(x = full_meals), fill = "#BB9966") +
  scale_x_discrete(name = "Full meals")

gender_bar_graph <- bar_graph_base +
  geom_bar(aes(x = gender), fill = "#BB9966") +
  scale_x_discrete(name = "Gender", labels = c("Male", "Female", "Other", "Undisclosed"))

meals_by_gender <- bar_graph_base +
  geom_bar(aes(x = gender, fill = full_meals)) +
  scale_fill_discrete(name = "Full meals") +
  scale_x_discrete(name = "Gender", labels = c("Male", "Female", "Other", "Undisclosed"))

(meals_bar_graph + gender_bar_graph) / meals_by_gender

# Box plots for snacks per day

snacks_per_day_gender <- learning_data %>%
  ggplot(aes(x = gender, y = snacks_per_day, color = gender)) +
  geom_boxplot(alpha = 0, color = "#BB9966") +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4, show_guide = FALSE) +
  theme_solarized() +
  ggtitle("Snacks per day by gender") +
  scale_x_discrete(name = "Gender", labels = c("Male", "Female", "Other", "Undisclosed")) +
  scale_y_discrete(name = "Snacks per day")

snacks_per_day_meals <- learning_data %>%
  ggplot(aes(x = full_meals, y = snacks_per_day, color = full_meals)) +
  geom_boxplot(alpha = 0, color = "#BB9966") +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4, show_guide = FALSE) +
  theme_solarized() +
  ggtitle("Snacks per day by full meals") +
  scale_x_discrete(name = "Full meals") +
  scale_y_discrete(name = "Snacks per day")

snacks_per_day_gender + snacks_per_day_meals

# Box plots for number of days when snacks were eaten

snack_days_gender <- learning_data %>%
  ggplot(aes(x = gender, y = snack_days, color = gender)) +
  geom_boxplot(alpha = 0, color = "#BB9966") +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4, show_guide = FALSE) +
  theme_solarized() +
  ggtitle("Snacks days by gender") +
  scale_x_discrete(name = "Gender", labels = c("Male", "Female", "Other", "Undisclosed")) +
  scale_y_continuous(name = "Days snacks were eaten")

snack_days_meals <- learning_data %>%
  ggplot(aes(x = full_meals, y = snacks_per_day, color = full_meals)) +
  geom_boxplot(alpha = 0, color = "#BB9966") +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4, show_guide = FALSE) +
  theme_solarized() +
  ggtitle("Snacks days by full meals") +
  scale_x_discrete(name = "Full meals") +
  scale_y_continuous(name = "Days snacks were eaten")

snack_days_gender + snack_days_meals
