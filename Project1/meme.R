library(magick)

# Define a function to make the panels for one row of the meme

make_row_vector <- function(image, text, dimension = 600, border = "brown") {
  image_panel <- image %>%
    image_extent(paste(dimension, "x", dimension)) %>%
    image_border(
      color = border,
      geometry = paste(dimension / 100, "x", dimension / 100)
    )
  text_panel <- image_blank(
    width = dimension,
    height = dimension,
    color = "black"
  ) %>%
    image_annotate(text,
      color = "white",
      font = "Impact", size = dimension / 10,
      gravity = "center", style = "Oblique"
    ) %>%
    image_border(
      color = border,
      geometry = paste(dimension / 100, "x", dimension / 100)
    )
  return(c(image_panel, text_panel))
}

# Define a function to make the full meme

make_meme <- function(image_vector, text_vector) {
  rows_vector <- image_vector
  for (i in 1:length(image_vector)) {
    rows_vector[i] <- image_append(
      make_row_vector(image_vector[i], text_vector[i])
    )
  }
  return(rows_vector %>% image_append(stack = TRUE))
}

# Use the functions to make the meme, and then write the meme

image_vector <- c(
  image_read("cat_1.png"), image_read("cat_2.png"),
  image_read("cat_3.png"), image_read("cat_4.png")
)
text_vector <- c("Meow...", "Meow?", "Meow!", "Meowwwww~!")
meme <- make_meme(image_vector, text_vector)
image_write(meme, "cat_meme.png")