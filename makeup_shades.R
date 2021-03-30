library(tidyverse)
library(tidytext)


sysfonts::font_add_google(name = "Indie Flower", "Indie")
showtext::showtext_auto()


tuesdata <- tidytuesdayR::tt_load('2021-03-30')

all <- tuesdata$allNumbers

## get US baby names from https://www.ssa.gov/oact/babynames/limits.html
baby_names <- 
  read_csv("yob2019.txt", col_names = FALSE) %>% 
  rename(
    name = X1,
    sex = X2,
    number = X3
  )

female_names <-
  baby_names %>% filter(sex == "FALSE")


top <-
  all %>% inner_join(female_names) %>%
  group_by(name) %>%
  summarise(number = max(number)) %>%
  arrange(-number) %>%
  slice_head(n = 18) %>%
  rowid_to_column("rank")

plot_data <-
  top %>% left_join(all) %>%
  group_by(name) %>%
  arrange(lightness) %>%
  mutate(count_name = row_number()) %>%
  ungroup() %>%
  mutate(
    name = as_factor(name),
    name = fct_reorder(name, lightness, mean)
  )


names_to_makeup <-
  ggplot(
  data = plot_data,
  aes(x = count_name, y = 1, fill = hex)) + 
  geom_tile() +
  scale_fill_manual(values = plot_data$hex) +
  facet_wrap(~fct_reorder(plot_data$name, plot_data$lightness, mean, .desc = FALSE), ncol = 3) +
  theme_void() +
  labs(
    title = "Make-up shades associated with female names"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(family = "Indie", size = 14, hjust = 0.05),
    plot.title = element_text(family = "Indie", size = 32)
  ) 

jpeg("makeup_shades.jpeg", width = 9, height = 9, units = "in", res = 300)
names_to_makeup
dev.off()




