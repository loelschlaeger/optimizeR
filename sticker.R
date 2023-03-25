### define font
library("showtext")
font_add_google("Martel", "my_font")
showtext_auto()

### create image
library("ggplot2")
p <- ggplot() +
  theme_void()
plot(p)

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 0.9,
  s_width = 2,
  s_height = 2,
  ### package name
  package = "optimizeR",
  p_x = 1,
  p_y = 1.35,
  p_color = "black",
  p_family = "my_font",
  p_fontface = "plain",
  p_size = 20,
  ### sticker
  h_size = 1.2,
  h_fill = "#d4e8dc",
  h_color = "black",
  spotlight = FALSE,
  l_x = 0.9,
  l_y = 1.4,
  l_width = 2,
  l_height = 1,
  l_alpha = 0.8,
  white_around_sticker = FALSE,
  ### URL
  url = "loelschlaeger.de/optimizeR",
  u_x = 1,
  u_y = 0.1,
  u_color = "black",
  u_family = "my_font",
  u_size = 5,
  u_angle = 30,
  ### save file
  filename = "../optimizeR_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)
