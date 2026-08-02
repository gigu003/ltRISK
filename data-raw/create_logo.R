## code to prepare `create_logo` dataset goes here
library(ggplot2)
library(hexSticker)
library(dplyr)
x <- seq(0, 10, length.out = 40)
y <- 2 + 3 * cos(x)
data <- data.frame(X = x, Y = y)


pp <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(color = "red", size = 0.02, alpha = 0.6) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")

sticker(
  pp,
  s_x = 1,
  s_y = 0.75,
  s_width = 1.2,
  s_height = .4,
  package = "ltRISK",
  p_size = 40,
  p_color = "#27514c",
  h_size = 0.3,
  h_color = "#0f1e1d",
  h_fill = "#c1d1cf",
  u_size = 4,
  u_color = "#0f1e1d",
  filename = "logo.png",
  dpi = 400
)
use_logo("logo.png")


# 建议安装最新版本（如果没有的话）
remotes::install_github("GuangchuangYu/hexSticker")

library(ggplot2)
library(hexSticker)
library(dplyr)
library(showtext)

## 添加谷歌字体，让文字更精致（强烈推荐）
font_add_google("Orbitron", "orbitron") # 科技感标题
font_add_google("Roboto Mono", "roboto") # 干净副标题感（备用）
showtext_auto()

set.seed(42) # 固定随机种子，让每次结果一致

# 生成更自然的波动轨迹 + 轻微噪声
x <- seq(0, 12, length.out = 120)
trend <- 0.08 * x # 轻微上升趋势（长期风险累积感）
y <- 2 + 3.2 * cos(0.8 * x) + trend + rnorm(120, 0, 0.25)

data <- data.frame(x = x, y = y)

# 核心ggplot图形
p <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "#e63946", size = 1.1, alpha = 0.95) + # 鲜艳红色主线
  geom_point(color = "#e63946", size = 1.4, alpha = 0.85) + # 红色点
  geom_area(fill = "#e63946", alpha = 0.15) + # 淡淡红色填充区，增加层次
  coord_fixed(ratio = 4) + # 拉长图形，更有“时间序列”感
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))

# 终极美化版 hexSticker
sticker(
  p,
  package = "ltRISK",
  p_size = 38, # 包名大小
  p_color = "#ffffff", # 白色文字，在深色背景上更醒目
  p_family = "orbitron", # 科技感字体
  p_fontface = "bold",
  p_y = 1.45, # 文字向上提一点，避免被六边形切掉

  s_x = 1, # 子图位置
  s_y = 0.82,
  s_width = 1.8, # 拉宽图形
  s_height = 0.9,

  h_fill = "#0a1e1c", # 极深青绿，几乎黑，高级感爆棚
  h_color = "#e63946", # 六边形边框用同样的红色，呼应主题
  h_size = 0.3, # 边框加粗

  spotlight = TRUE, # 开启聚光灯效果（超级加分！）
  l_x = 1,
  l_y = 0.9,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.3, # 聚光灯透明度

  filename = "ltRISK_logo.png",
  dpi = 600 # 更高分辨率
)
