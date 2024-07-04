library(tidyverse)
library(patchwork)
library(ggtext)


youtube_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyY2RSB9T_ULUN0VoOZDvd9_SMROVtH7JgJ1_94AL04DJM8FcSsN_TdjKHV-pNBWomyl6YkfcFVau4/pub?gid=0&single=true&output=csv")

palette <- c("#cadff4", "#1167ad", "#31476d", "#91679d", "#4781fd", "#ad4444")
ggplot_theme <- theme(
                      plot.background = element_rect(fill = palette[1]),
                      plot.margin = unit(c(0.1, 0.3, 0.1, 0.3), 'in'),
                      panel.background = element_rect(fill = palette[1]),
                      panel.border = element_rect(color = palette[2], fill = NA),
                      panel.grid = element_line(color = palette[2]),
                      panel.grid.major.y = element_line(linewidth = 0.3),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      legend.background = element_rect(fill = palette[1]),
                      axis.ticks = element_line(color = palette[2], linewidth = 0.3),
                      text = element_text(color = palette[3]),
                      axis.text = element_text(color = palette[2])
)

# Plot 1: views and comments over time
youtube_data <- youtube_data %>%
  mutate(month_year = paste(str_sub(datePublished, 3, 4), str_sub(datePublished, 6, 7), sep = "/"),
         channelName = str_sub(channelName, start = 2))

youtube_data_means <- youtube_data %>%
  group_by(month_year, channelName) %>%
  summarise(mean_views = mean(viewCount)) %>%
  ungroup()

youtube_data_over_7mil <- youtube_data %>%
  filter(viewCount >= 7000000)

plot1 <- ggplot() +
  geom_point(data = youtube_data,
             aes(x = month_year, y = viewCount, color = channelName),
             alpha = 0.4,
             size = 5,
             shape = 4) +
  geom_point(data = youtube_data_means,
             aes(x = month_year, y = mean_views, color = channelName),
             shape = 18,
             size = 3) +
  geom_text(data = youtube_data_over_7mil,
            aes(x = month_year, y = viewCount, color = channelName, label = viewCount),
            nudge_x = 2.5) +
  scale_color_manual(values = c(palette[5], palette[6])) +
  guides(color = "none") +
  labs(title = str_wrap("Kanae and Kuzuha have gotten a reasonably consistent number of views on their videos over time, with some stand-out more popular videos."),
       x = "Year/month",
       y = "View count",
       subtitle = "Blue: Kanae. Red: Kuzuha.\nDiamonds: average views that month. Labels: number of views. Purple line: 1,000,000 views.") +
  scale_y_continuous(labels = scales::label_number()) +
  geom_hline(aes(yintercept = 1000000),
             color = palette[4]) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.subtitle = element_markdown()) +
  ggplot_theme

ggsave("plot1.png", plot1, width = 12, height = 4)

# Plot 2: top 5 videos by views from each channel
kanae_top_videos <- youtube_data %>%
  filter(channelName == "Kanae") %>%
  arrange(desc(viewCount)) %>%
  slice(1:5) %>%
  arrange(viewCount)

kuzuha_top_videos <- youtube_data %>%
  filter(channelName == "Kuzuha") %>%
  arrange(desc(viewCount)) %>%
  slice(1:5) %>%
  arrange(viewCount)

plot2a <- kanae_top_videos %>%
  ggplot(aes(y = reorder(title, viewCount), x = viewCount)) +
  geom_col(fill = palette[5], alpha = 0.3, width = 0.5) +
  scale_y_discrete(labels = paste0("<img src = '", kanae_top_videos$thumbnailUrl, "' height = '40'/>")) +
  theme(axis.text.y = element_markdown()) +
  geom_text(aes(label = str_wrap(title, 39, whitespace_only = FALSE)),
            hjust = 1,
            color = palette[2]) +
  labs(subtitle = "Kanae",
       x = "Number of views",
       y = NULL) +
  scale_x_continuous(labels = scales::label_number())

plot2b <- kuzuha_top_videos %>%
  ggplot(aes(y = reorder(title, viewCount), x = viewCount)) +
  geom_col(fill = palette[6], alpha = 0.3, width = 0.5) +
  scale_y_discrete(labels = paste0("<img src = '", kuzuha_top_videos$thumbnailUrl, "' height = '40'/>")) +
  theme(axis.text.y = element_markdown()) +
  geom_text(aes(label = str_wrap(title, 28, whitespace_only = FALSE)),
            hjust = 1,
            color = palette[2]) +
  labs(subtitle = "Kuzuha",
       x = "Number of views",
       y = NULL) +
  scale_x_continuous(labels = scales::label_number())

plot2 <- plot2a + plot2b +
  plot_annotation(title = "Out of those, their most viewed videos are their song covers.") &
  ggplot_theme

ggsave("plot2.png", plot2, width = 12, height = 4)

# Plot 3: overall boxplot comparisons for each channel

plot3_a <- youtube_data %>%
  ggplot(aes(y = channelName, x = duration)) +
  geom_jitter(aes(color = channelName), alpha = 0.3) +
  geom_boxplot(aes(color = channelName, fill = channelName),
               alpha = 0.3) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_color_manual(values = c(palette[5], palette[6])) +
  scale_fill_manual(values = c(palette[5], palette[6])) +
  guides(color = "none", fill = "none") +
  labs(y = "Channel", x = "Duration of video (seconds)")

plot3_b <- youtube_data %>%
  ggplot(aes(y = channelName, x = viewCount)) +
  geom_jitter(aes(color = channelName), alpha = 0.3) +
  geom_boxplot(aes(color = channelName, fill = channelName),
               alpha = 0.3) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_color_manual(values = c(palette[5], palette[6])) +
  scale_fill_manual(values = c(palette[5], palette[6])) +
  guides(color = "none", fill = "none") +
  labs(y = "Channel", x = "Number of views")

plot3_c <- youtube_data %>%
  ggplot(aes(y = channelName, x = commentCount)) +
  geom_jitter(aes(color = channelName), alpha = 0.3) +
  geom_boxplot(aes(color = channelName, fill = channelName),
               alpha = 0.3) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_color_manual(values = c(palette[5], palette[6])) +
  scale_fill_manual(values = c(palette[5], palette[6])) +
  guides(color = "none", fill = "none") +
  labs(y = "Channel", x = "Number of comments")

plot3 <- plot3_a / plot3_b / plot3_c +
  plot_annotation(title = str_wrap("Overall, they tend to have similar video durations, views, and comments, with Kuzuha's being somewhat higher.")) &
  ggplot_theme

ggsave("plot3.png", plot3, width = 12, height = 4)
