library(magick)
library(tidyverse)

palette <- c("#cadff4", "#1167ad", "#31476d", "#91679d", "#4781fd", "#ad4444")

blank_slide <- image_blank(width = 1200,
                           height = 400,
                           color = palette[1])

title <- blank_slide %>%
  image_annotate("Chronoir's streaming careers",
                 gravity = "center",
                 size = "56",
                 font = "Noto Sans",
                 location ="+0-40",
                 color = palette[3]) %>%
  image_annotate("Examining some random videos of Kanae's and Kuzuha's",
                 style = "italic",
                 gravity = "center",
                 size = "36",
                 location = "+0+20",
                 color = palette[2])

plot1 <- image_read("plot1.png") %>%
  image_scale("1200x400")
plot1_comments <- plot1 %>%
  image_blur(radius = 20, sigma = 3) %>%
  image_annotate(str_wrap("It looks like Kuzuha especially has some videos which went quite viral. For both VTubers, older viral videos seem to have had more time to accumulate views than newer ones.", 60),
                 gravity = "center",
                 size = "30",
                 color = palette[2],
                 boxcolor = palette[1],
                 font = "Noto Sans")

plot2 <- image_read("plot2.png") %>%
  image_scale("1200x400")
plot2_comments <- plot2 %>%
  image_blur(radius = 20, sigma = 3) %>%
  image_annotate(str_wrap("This sample doesn't include all their videos; Kuzuha's cover of KING by Kanaria is notorious for its (at the time of writing) 47 million views. But it makes sense that their song covers would be watched and rewatched by a bigger audience than their gaming streams.", 60),
                 gravity = "center",
                 size = "30",
                 color = palette[2],
                 boxcolor = palette[1],
                 font = "Noto Sans")

plot3 <- image_read("plot3.png") %>%
  image_scale("1200x400")
plot3_comments <- plot3 %>%
  image_blur(radius = 20, sigma = 3) %>%
  image_annotate(str_wrap("Both VTubers post streams which are hours long, though Kanae streams for somewhat fewer hours. While Kuzuha usually gets a higher amount and bigger range of views, Kanae's and Kuzuha's usual amounts of comments don't differ much.", 60),
                 gravity = "center",
                 size = "30",
                 color = palette[2],
                 boxcolor = palette[1],
                 font = "Noto Sans")

conclusion <- blank_slide %>%
  image_annotate(str_wrap("As ChroNoiR approach their 6th anniversary as a unit, Kanae and Kuzuha are still doing consistently well in their solo careers.", 70),
                 gravity = "northwest",
                 size = "30",
                 location = "+10+10",
                 color = palette[2],
                 font = "Noto Sans") %>%
  image_annotate(str_wrap("Kuzuha especially has videos which often become hits even outside of his regular viewers.", 70),
                 gravity = "east",
                 size = "30",
                 location = "+10-30",
                 color = palette[2],
                 font = "Noto Sans") %>%
  image_annotate(str_wrap("Despite mainly being gamers, with hours-long game streams, Kanae's and Kuzuha's music videos in particular do quite well. It speaks to their overall successes as well-rounded entertainers that they've maintained large and varied fanbases for all these years.", 70),
                 location = "+0+10",
                 gravity = "south",
                 size = "30",
                 color = palette[3],
                 font = "Noto Sans")

frames <- c(rep(title, 5),
            rep(plot1, 10),
            rep(plot1_comments, 9),
            rep(plot2, 12),
            rep(plot2_comments, 9),
            rep(plot3, 10),
            rep(plot3_comments, 9),
            rep(conclusion, 12))

data_story <- image_animate(frames, fps = 1)

image_write(data_story, "data_story.gif")
