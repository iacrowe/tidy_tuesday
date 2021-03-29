library(tidyverse)
library(lubridate)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

year_data <- 
  roll_calls %>%
  select(rcid, date) %>%
  mutate(year = year(date))

data <- 
  unvotes %>%
  left_join(issues) %>%
  left_join(year_data) %>%
  filter(year > 1970) %>%
  drop_na()

selected_data <-
  data %>%
  filter(country %in% c("United States", "United Kingdom", "China", "India"))

plot_data <-
  selected_data %>%
  group_by(year, issue, country) %>%
  count(vote) %>% 
  filter(vote == "yes")

ggplot(plot_data) +
  geom_line(
    aes(x = year,
        y = n,
        colour = country)
  ) +
  facet_wrap(~issue, nrow = 3) +
  xlab("Year") +
  ylab("Number of Yes votes") +
  ggthemes::theme_wsj() + 
  ggthemes::scale_color_wsj() +
  theme(legend.position = "top") +
  labs(
    title = "UN 'yes' votes from the US, the UK, China, and India",
    subtitle = "Soft power in action?"
  )