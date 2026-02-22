# Packages
# install.packages(c("tidyverse", "TTR", "gganimate", "gifski"))
library(tidyverse)
library(TTR)
library(gganimate)

# Read Excel File 
library(readxl)
gas <- read_excel("gasoline.xlsx")
glimpse(gas)
head(gas)

# install.packages(c("tidyverse", "TTR", "gganimate", "gifski"))
library(tidyverse)
library(TTR)
library(gganimate)

gas2 <- gas %>%
  mutate(
    Week  = as.numeric(Week),
    Sales = as.numeric(Sales)
  )

# k starts at 1 (naive) and increases
k_values <- c(1, 2, 3, 4, 6, 8, 10)

sma_long <- tidyr::crossing(gas2, tibble(k = k_values)) %>%
  group_by(k) %>%
  mutate(
    SMA = if (first(k) == 1) Sales else as.numeric(TTR::SMA(Sales, n = first(k)))
  ) %>%
  ungroup()

p <- ggplot(sma_long, aes(x = Week)) +
  # grey points (actuals)
  geom_point(aes(y = Sales), size = 5, alpha = 0.20, color = "grey40") +
  # red line (actuals)
  geom_line(aes(y = Sales), linewidth = 2, color = "tomato") +
  # blue MA points + line
  geom_point(aes(y = SMA), size = 5, alpha = 0.12, color = "grey40", na.rm = TRUE) +
  geom_line(aes(y = SMA), linewidth = 2, color = "cornflowerblue", na.rm = TRUE) +
  # match your y-range look (adjust if you want)
  ylim(0, 25) +
  labs(
    title = "Simple Moving Average (SMA)",
    subtitle = "k = {closest_state}",
    x = "Week",
    y = "Sales"
  ) +
  # make k BIG + BOLD
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 26),
    plot.subtitle = element_text(face = "bold", size = 28),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)
  ) +
  transition_states(k, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

anim <- animate(
  p,
  nframes = 160, fps = 20, width = 1100, height = 650,
  renderer = gifski_renderer()
)

anim_save("sma_k_animation.gif", anim)
