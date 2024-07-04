library(tidyverse)
library(jsonlite)
library(magick)
library(patchwork)
library(ggthemes)
json_data <- fromJSON(readLines("pixabay_data.json"))
pixabay_photo_data <- json_data$hits

# Have a look at what's in the dataframe
View(pixabay_photo_data)
summary(pixabay_photo_data$views) # To figure out the upper quartile of views

# The required data manipulation
selected_photos <- pixabay_photo_data %>%
  filter(views > 15000) %>% # Upper quartile - reduces observations to about 50
  mutate(
    tagged_flower = ifelse(str_detect(tags, "flower"), "Yes", "No"),
    likes_per_thousand_views = likes / views * 1000,
    downloads_per_thousand_views = downloads / views * 1000
  ) %>%
  select(
    # Seems interesting to see what user-related metrics correlate with likes/downloads
    pageURL, previewURL, tagged_flower, collections, comments,
    downloads_per_thousand_views, likes_per_thousand_views
  )

View(selected_photos)
summary(selected_photos)
write_csv(selected_photos, "selected_photos.csv")

# Looking for interesting summary values
summary(selected_photos)

# Summary values
median_collections <- median(selected_photos$collections, na.rm = TRUE)
mean_collections <- mean(selected_photos$collections, na.rm = TRUE)

median_comments <- median(selected_photos$comments, na.rm = TRUE)
mean_comments <- mean(selected_photos$comments, na.rm = TRUE)

tagged_flower_count <- sum(selected_photos$tagged_flower == "Yes", na.rm = TRUE)

# Grouped data and summary values
selected_photos_grouped <- selected_photos %>%
  select(tagged_flower,
         likes_per_thousand_views,
         downloads_per_thousand_views) %>%
  group_by(tagged_flower) %>%
  summarise(median_likes = median(likes_per_thousand_views),
            median_downloads = median(downloads_per_thousand_views))

View(selected_photos_grouped)

# Creating and writing a GIF of the images
my_photos_gif <- image_read(selected_photos$previewURL) %>%
  image_scale(geometry_area(width = 150)) %>%
  image_animate(fps = 2)

my_photos_gif

image_write(my_photos_gif, "my_photos.gif")

# Yay, plots

## Likes plots
likes_by_tag_comments <- selected_photos %>%
  ggplot(aes(x = comments,
             y = likes_per_thousand_views,
             color = tagged_flower)) +
  geom_point() +
  theme_solarized_2(light = FALSE) +
  labs(title = "Likes per thousand views by comments and collections",
       x = "Number of comments",
       y = "Likes/1000 views",
       color = "'Flower'/'flowers' tag")

likes_by_tag_collections <- selected_photos %>%
  ggplot(aes(x = comments,
             y = likes_per_thousand_views,
             color = tagged_flower)) +
  geom_point() +
  theme_solarized_2(light = FALSE) +
  labs(x = "Number of collections",
       y = "Likes/1000 views",
       color = "'Flower'/'flowers' tag",
       caption = "Source: Pixabay")

likes_plot <- likes_by_tag_comments / likes_by_tag_collections
likes_plot

## Downloads plots
downloads_by_tag_comments <- selected_photos %>%
  ggplot(aes(x = comments,
             y = downloads_per_thousand_views,
             color = tagged_flower)) +
  geom_point() +
  theme_solarized_2(light = FALSE) +
  labs(title = "Downloads per thousand views by comments and collections",
       x = "Number of comments",
       y = "Downloads/1000 views",
       color = "'Flower'/'flowers' tag")

downloads_by_tag_collections <- selected_photos %>%
  ggplot(aes(x = collections,
             y = downloads_per_thousand_views,
             color = tagged_flower)) +
  geom_point() +
  theme_solarized_2(light = FALSE) +
  labs(x = "Number of collections",
       y = "Downloads/1000 views",
       color = "'Flower'/'flowers' tag",
       caption = "Source: Pixabay")

downloads_plot <- downloads_by_tag_comments / downloads_by_tag_collections
downloads_plot
