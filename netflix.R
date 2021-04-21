library(tidyverse)
library(lubridate)
library(ggdark)
library(ggfx)

tuesdata <- tidytuesdayR::tt_load('2021-04-20')
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix

data <-
  netflix %>%
  mutate(date_parse = mdy(date_added)) %>%
  mutate(month_added = floor_date(date_parse, unit = "month")) %>%
  filter(month_added > ymd("2016-04-01")) %>%
  group_by(month_added, type) %>%
  count() %>%
  ungroup() %>%
  mutate(id = rep(1 : 57, each = 2))


sysfonts::font_add_google(name = "Bebas Neue", "Bebas")
showtext::showtext_auto()


plot_shows <-
  ggplot(
    data, 
    aes(x = month_added, y = type)) +
  geom_raster(
    aes(x = id, y = type, fill = n)
  ) +
  scale_fill_gradient(low = "#3E0000", high = "#F10B00") +
  scale_x_continuous(
    name = "",
    breaks = c(9, 21, 33, 45, 57),
    labels = c(2017, 2018, 2019, 2020, 2021)
  ) +
  ylab(NULL) +
  guides(fill = guide_colourbar(
    barwidth = 15,
    barheight = 0.5,
    frame.colour = "black",
    ticks = FALSE,
    title = "Number of Titles per type Added each Month",
    title.hjust = 0.5,
    title.position = "bottom"
  )) +
  labs(
    title = "NETFLIX titles added per month", family = "Bebas", colour = "white", size = rel(1.2),
    caption = "Data from Kaggle | Visualisation by @IanARowe #TidyTuesday", colour = "white"
  ) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="white", family = "Bebas", size = rel(1.3)),
    axis.text.y  = element_text(hjust=1),
    legend.background = element_rect(fill="black"),
    text = element_text(family = "Bebas", colour = "white"),
    plot.title = element_text(size = rel(2))
  ) 

jpeg("netflix_monthly_titles.jpeg", width = 9, height = 4, units = "in", res = 300)
plot_shows
dev.off()
