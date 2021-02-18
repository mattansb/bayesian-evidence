library(ggplot2)
library(see)
library(purrr)

m <- 4^seq(-3, 0.5, by = 0.2)

L <-
  map(m, ~ stat_function(
    aes(color = .x),
    size = 1,
    # alpha = 1 - .x/2,
    alpha = 0.3,
    fun = dnorm,
    xlim = c(-3, 5),
    args = list(mean = .x*1.5, sd = .x + 1)
  ))

ggplot() +
  L +
  scale_color_material_c() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#182933"),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  labs(x = "", y = "")
  
ggsave("bg_main.png", width = 16, height = 9, scale = 0.4)
