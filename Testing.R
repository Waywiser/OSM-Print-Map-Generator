x = seq(-1.5, 3.5, 0.1)

y = c( rep(1.0, 22), rep(0.2, 12), rep(0.7, 7), rep(1,10))

ref = data.frame(x = x, y = y)

library(dplyr)
library(ggplot2)
library(ggpattern)

coral = system.file("img", "https://i.pinimg.com/originals/0d/c6/f0/0dc6f0f6af86d72b22fd7346010697e1.jpg", package="ggpattern")

p = ggplot(ref, aes(x = x, y = y))+
  scale_y_reverse(lim = c(1, 0))+
  theme_classic()+
  geom_ribbon_pattern(aes(x = x, ymin = 1, ymax = y),
                      color = "darkblue",
                      fill = NA,
                      size = 1.5,
                      pattern = 'image',
                      pattern_type = 'squish',
                      pattern_filename = coral) +
  geom_ribbon(aes(x = x, ymin = 0, ymax = y), fill = "lightblue")

p

