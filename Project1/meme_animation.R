library(magick)

# Define a function to make the frames needed to animate one row

create_animation_frames <- function(image, text, dimension = 600) {
  cat_frames <- rep(image, times = 20)
  for (i in 5:9) {
    cat_frames[i] <- image_rotate(image, 72 * i - 4) %>%
      image_extent(paste(dimension, "x", dimension))
  }

  blank_image <- image_blank(
    width = dimension,
    height = dimension,
    color = "black"
  )
  text_image <- blank_image %>%
    image_annotate(text,
      color = "white",
      font = "Impact", size = dimension / 10,
      gravity = "center", style = "Oblique"
    )
  text_frames <- c(rep(blank_image, times = 10), rep(text_image, times = 10))

  all_frames <- cat_frames
  for (i in 1:20) {
    all_frames[i] <- image_append(c(cat_frames[i], text_frames[i]))
  }
  return(all_frames)
}

# Define a function to animate each row, one after the other

animate_meme <- function(image_vector, text_vector) {
  all_rows_frames <- rep(rep(image_vector[1], times = 20),
    times = length(image_vector)
  )
  for (i in 1:length(image_vector)) {
    row_frames <- create_animation_frames(image_vector[i], text_vector[i])
    end_index <- i * length(all_rows_frames) / length(image_vector)
    all_rows_frames[(end_index - 19):end_index] <- row_frames
  }

  meme_animation <- image_animate(all_rows_frames, fps = 10)
  return(meme_animation)
}

# Use the functions to animate the meme and save it as a GIF

rows <- 4
image_vector <- c(
  image_read("cat_1.png"), image_read("cat_2.png"),
  image_read("cat_3.png"), image_read("cat_4.png")
)
text_vector <- c("Meow...", "Meow?", "Meow!", "Meowwwww~!")
animated_meme <- animate_meme(image_vector, text_vector)
image_write(animated_meme, "cat_meme_animated.gif")
