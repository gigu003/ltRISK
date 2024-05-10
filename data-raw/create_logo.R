## code to prepare `create_logo` dataset goes here
library(ggplot2)
library(hexSticker)
library(dplyr)
x <- seq(0, 10, length.out = 40)  # 在 0 到 10 之间生成 100 个等间距的点作为 X 值
y <- 2 + 3 * cos(x)
data <- data.frame(X = x, Y = y)


pp <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(color= "red", size = 0.01, alpha = 0.5) +
  theme_void() +
  theme_transparent()+
  theme(legend.position = "none")

sticker(pp,
        s_x = 1, s_y = 0.75,
        s_width = 1.2, s_height = .5,
        package = "ltRISK",
        p_size = 25,
        p_color = "#27514c",
        h_size = 0.5,
        h_color = "#0f1e1d",
        h_fill = "#c1d1cf",
        url = "https://packages.chenq.site/ltrisk",
        u_size = 4.5,
        u_color = "#0f1e1d",
        filename = "logo.png",
        dpi = 300)
use_logo("logo.png")

