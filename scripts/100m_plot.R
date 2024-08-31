# Load packages ----
library(dplyr)  # cleaning
library(tidyr)  # cleaning
library(lubridate)  # cleaning dates and times
library(ggplot2)  # plotting
library(forcats)  # dealing with factors
library(stringr)  # wrapping text
library(showtext) 

# Clean the data ----

# Read raw data file
m100 <- read.csv("data/100m_all_time_performances.csv")

# Remove 'A' character using gsub() function and update to numeric type
m100$time <- gsub("A", "", m100$time)
m100$time <- as.numeric(m100$time)

# Convert dates
m100$birth_date <- dmy(m100$birthdate)
m100$performance_date <- dmy(m100$date)

# Fix the dates that are before 1970 which are getting converted to 2070
m100$birth_date<- if_else(m100$birth_date > ymd("2024-01-01"), 
                          m100$birth_date - years(100), 
                          m100$birth_date)

# Calculate age
m100$age <- floor(as.numeric(difftime(m100$performance_date, m100$birth_date, units = "days") / 365.25))

# Add column for year
m100$year <- year(m100$performance_date)


# Calculate the counts for each performance group ----
 
# Sub 10
m100_by_age_counts_sub10 <- m100_by_age %>% 
  group_by(age) %>% 
  summarise(counts = n()) %>% 
  arrange(age) %>% 
  mutate(group = "sub10")

# Sub 9.9
m100_by_age_counts_sub99 <- m100_by_age %>% 
  filter(time < 9.9) %>% 
  group_by(age) %>% 
  summarise(counts = n()) %>% 
  arrange(age) %>% 
  mutate(group = "sub99")

# Sub 9.8
m100_by_age_counts_sub98 <- m100_by_age %>% 
  filter(time < 9.8) %>% 
  group_by(age) %>% 
  summarise(counts = n()) %>% 
  arrange(age) %>% 
  mutate(group = "sub98")

# Bind data frames
m100_by_age_counts_all <- rbind(m100_by_age_counts_sub10, m100_by_age_counts_sub99, m100_by_age_counts_sub98)

m100_by_age_counts_all$group <- factor(m100_by_age_counts_all$group, levels = c("sub10", "sub99", "sub98"))



# Render the plot ----

font_add_google("Roboto")

showtext_auto()

m100_age_plot <- m100_by_age_counts_all %>% 
  ggplot(mapping = aes(x = age, y = fct_rev(group), size = counts, label = counts)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#F1F2E9"),
    plot.background = element_rect(fill = "#F1F2E9"),
    text = element_text(family = "Roboto"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 14, face = "bold", color = "#070D06", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 9, color = "#070D06", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, face = "italic", hjust = 0, margin = margin(t = 10)),
    panel.grid.major.y = element_line(color = "#637368", linewidth = 0.25),
    axis.line = element_blank(),
    axis.ticks.x = element_line(color = "#070D06"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 10, color = "#070D06", margin = margin(t = 5)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9, color = "#070D06", margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, color = "#070D06")
  ) +
  labs(
    title = "World class speed after 30 is rare",
    subtitle = str_wrap("Running the 100m in under 10 seconds becomes increasingly rare for athletes as they age through their 30s. Sub 9.9 performances are even less likely after 30, and are almost non-existent after 35. Sub 9.8? It has only been done 48 times by 11 different men at any age, and only one (Gatlin) has ever done it after 30. There is an expression in sports that says 'Speed Kills', and that is true. But equally as true is that 'Aging Kills Speed'.", 150),
    caption = "Data: alltime-athletics.com\nViz: Tim Fulton, PhD",
    x = "Age"
  ) +
  geom_point(
    shape = 21,
    color = "#182612",
    fill = "#5C7345"
  ) +
  geom_text(
    size = 2.8,
    color = "#F1F2E9",
    family = "Roboto"
  ) +
  guides(size = "none") +
  scale_size_continuous(range = c(5, 17)) + 
  scale_x_continuous(
    breaks = seq(18, 40, 1),
    labels = c(rep("", 2), 20, rep("", 4), 25, rep("", 4), 30, rep("", 4), 35, rep("", 4), 40),
    expand = expansion(mult = 0.03)
  ) +
  scale_y_discrete(labels = c("Sub 9.8\nPerformances", "Sub 9.9\nPerformances", "Sub 10\nPerformances"))


# Save the plot ----
ggsave("plots/100m_age_plot.png",
       plot = m100_age_plot,
       width = 9,
       height = 4,
       units = "in",
       dpi = 600)

