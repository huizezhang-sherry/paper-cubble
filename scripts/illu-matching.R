library(tidyverse)
set.seed(123)
dt <- tibble(
  id = factor(c(rep("A", 31), rep("a", 31)), levels = c("A", "a")),
  date = rep(1:31, 2),
  val = c(
    c(rnorm(5), 10, rnorm(7), 5, rnorm(8), 7, rnorm(8)),
    c(
      rnorm(6, sd = 0.5), 7, rnorm(7, sd = 0.5), rnorm(6, sd = 0.5),
      4, rnorm(5, sd = 0.5), 6, rnorm(4, sd = 0.5)
    )
  )
) |> mutate(val = ifelse(val < 0, 0, val))

circle <- tibble(
  x = c(6, 14, 23, 7, 21, 27),
  y = c(10, 5, 7, 7, 4, 6),
  id = factor(c(rep("A", 3), rep("a", 3)), levels = c("A", "a")),
  xend = x + 5,
  match = factor(c("yes", "no", rep("yes", 2), "no", "yes"), levels = c("yes", "no"))
) 

errorbar <- bind_rows(
  circle |> filter(id == "A"),
  circle |> filter(id == "A") |> mutate(id = "a")
) |>
  mutate(id = factor(id, c("A", "a")))

ggplot() +
  geom_rect(data = errorbar, aes(xmin = x, xmax = xend, ymin = 0, ymax = 10), fill = "grey90") +
  geom_line(data = dt, aes(x = date, y = val, group = id), color = "black") +
  geom_vline(data = circle |> filter(id == "A"), 
             aes(xintercept = x), linetype = "longdash", color = "grey10", lwd = 0.2) +
  geom_point(data = circle, aes(x = x, y = y, color = match), size = 3) +
  facet_wrap(vars(id), nrow = 2) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom") +
  labs(x = "Time", y = "Value")

ggsave(filename = "figures/illu-matching.png", width = 10, height = 5)
