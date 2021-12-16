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
) %>% mutate(val = ifelse(val < 0, 0, val))

circle <- tibble(
  x = c(6, 14, 23, 7, 21, 27),
  y = c(10, 5, 7, 7, 4, 6),
  id = factor(c(rep("A", 3), rep("a", 3)), levels = c("A", "a")),
  xend = x + 5
)

errorbar <- bind_rows(
  circle %>% filter(id == "A"),
  circle %>% filter(id == "A") %>% mutate(id = "a")
) %>%
  mutate(id = factor(id, c("A", "a")))

fallin <- tibble(
  id = factor(rep("a", 2), c("A", "a")),
  x = c(7, 27),
  y = -0.5,
  label = "\U2714"
)

ggplot() +
  geom_line(data = dt, aes(x = date, y = val, group = id), color = "grey60") +
  geom_vline(data = circle, aes(xintercept = x), linetype = "longdash", color = "grey60") +
  geom_errorbar(data = errorbar, aes(xmin = x, xmax = xend, y = -0.5), width = 0.5) +
  geom_point(data = circle, aes(x = x, y = y), color = "brown") +
  geom_text(data = fallin, aes(x = x, y = y, label = label), color = "brown", size = 5) +
  facet_wrap(vars(id), nrow = 2) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Value")

ggsave(filename = "figures/illu-matching.png", width = 10, height = 5)
