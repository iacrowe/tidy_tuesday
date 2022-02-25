library(tidyverse)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-02-22')

freedom <- tuesdata$freedom

freedom_legend <-
  freedom %>%
  filter(
    Region_Name == "Africa" | Region_Name == "Oceania",
    year <2000
  ) %>%
  mutate(Region_Name = "Legend") %>%
  group_by(Region_Name, year) %>%
  ungroup()

freedom_plot_data <-
  bind_rows(freedom, freedom_legend) %>%
  mutate(
    Region_Name = as_factor(Region_Name),
    Region_Name = fct_relevel(Region_Name, "Americas", "Europe", 
                              "Asia", "Africa", "Oceania", "Legend"))

mean_vals <-
  freedom_plot_data %>%
  group_by(Region_Name, year) %>%
  summarise(mean_CL = mean(CL), mean_PR = mean(PR))

text_fig_rights <-
  tibble(
    year = c(2008, 2008),
    PR = c(1, 7),
    descr = c("most political rights", "least political rights"),
    Region_Name = as_factor("Legend")
  )

text_fig_mean <-
  tibble(
    year = 2008,
    PR = 4,
    descr = "regional mean",
    Region_Name = as_factor("Legend")
  )

regions_pr_plot <-
  ggplot() +
  geom_jitter(
    data = freedom_plot_data,
    aes(x = year, y = PR, colour = PR), alpha = 0.5
  ) +
  geom_line(
    data = mean_vals,
    aes(x = year, y = mean_PR), colour = "skyblue", size = 1
  ) +
  geom_text(
    data = text_fig_rights,
    aes(x = year, y = PR, label = descr, colour = PR)
  ) +
  geom_text(
    data = text_fig_mean,
    aes(x = year, y = PR, label = descr), colour = "skyblue", hjust = 0.7
  ) +
  scale_colour_continuous(
    trans = "reverse") +
  xlab("Year") +
  scale_y_continuous(name = element_blank()) +
  facet_wrap(~Region_Name, nrow = 2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill="black"),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(colour="white", size = 14),
    panel.background = element_rect(fill="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="white"),
    axis.text.y  = element_blank(),
    legend.background = element_rect(fill="black"),
    text = element_text(colour = "white", face = "bold", size = 10),
    plot.title = element_text(size = rel(2))
  ) +
  labs(
    title = "Global changes in political rights",
    caption = "TidyTuesday | Plot by @IanARowe | Data from Freedom House & the UN"
  )

ggsave("political_rights.jpeg", last_plot(), device = "jpeg", width = 8, height = 6, units = "in")

