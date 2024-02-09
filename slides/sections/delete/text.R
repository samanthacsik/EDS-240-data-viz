
# https://albert-rapp.de/posts/ggplot2-tips/08_fonts_and_icons/08_fonts_and_icons.html
# https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(showtext)
library(ggtext)
library(gghighlight)

#..........................import fonts..........................
font_add_google(name = "Josefin Sans", family = "josefin")
font_add_google(name = "Sen", family = "sen")
font_add(family = "fa-brands",
         regular = here::here("fonts", "Font Awesome 6 Brands-Regular-400.otf")) # "fontawesome-free-6.5.1-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf"
font_add(family = "fa-reg",
         regular = here::here("fonts", "Font Awesome 6 Free-Regular-400.otf")) # "fontawesome-free-6.5.1-desktop/otfs/Font Awesome 6 Free-Regular-400.otf"
font_add(family = "fa-solid",
         regular = here::here("fonts", "Font Awesome 6 Free-Solid-900.otf")) # "fontawesome-free-6.5.1-desktop/otfs/Font Awesome 6 Free-Solid-900.otf"


# enable {showtext} font rendering
showtext_auto()

#..........................import data...........................
jobs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

jobs_clean <- jobs |>

  # add cols (needed for dumbbell plot) ----
mutate(percent_male = 100 - percent_female, # % of females within each industry was already included
       difference_earnings = total_earnings_male - total_earnings_female) |>  # diff in earnings between M & F

  # rearrange columns ----
relocate(year, major_category, minor_category, occupation,
         total_workers, workers_male, workers_female,
         percent_male, percent_female,
         total_earnings, total_earnings_male, total_earnings_female, difference_earnings,
         wage_percent_of_male) |>

  # drop rows with missing earning data ----
drop_na(total_earnings_male, total_earnings_female) |>

  # make occupation a factor ----
mutate(occupation = as.factor(occupation)) |>

  # ---- this next step is for creating our dumbbell plots ----

# classify jobs by percentage male or female ----
mutate(group_label = case_when(
  percent_female >= 75 ~ "Occupations that are 75%+ female",
  percent_female >= 45 & percent_female <= 55 ~ "Occupations that are 45-55% female",
  percent_male >= 75 ~ "Occupations that are 75%+ male"
))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              create subset df                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....guarantee the same random samples each time we run code.....
set.seed(0)

#.........get 10 random jobs that are 75%+ female (2016).........
f75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ female") |>
  slice_sample(n = 10)

#..........get 10 random jobs that are 75%+ male (2016)..........
m75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ male") |>
  slice_sample(n = 10)

#........get 10 random jobs that are 45-55%+ female (2016).......
f50 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 45-55% female") |>
  slice_sample(n = 10)

#.......combine dfs & relevel factors (for plotting order).......
subset_jobs <- rbind(f75, m75, f50) |>
  mutate(group_label = fct_relevel(group_label, "Occupations that are 75%+ female",
                                   "Occupations that are 45-55% female", "Occupations that are 75%+ male"))



#........................some calculations.......................
max_diff <- subset_jobs |>
  slice_max(order_by = difference_earnings, n = 1)

# of the jobs considered, the largest pay gap between male and female workers exists in a female dominated occupation, where male median salaries are 34% higher than females

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                create ggplot                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.........................create palette.........................

earnings_pal <- c("males" = "#2D7787",
                  "females" = "#FC6B4B",
                  dark_text = "#0C1509",
                  light_text = "#575D55") # 323A30, 6A7368

monochromeR::view_palette(earnings_pal)

#..........................create plot...........................

plot <- ggplot(subset_jobs) +
geom_segment(aes(x = total_earnings_female, xend = total_earnings_male,
                 y = fct_reorder(occupation, total_earnings), yend = occupation)) +
  geom_point(aes(x = total_earnings_male, y = occupation),
             color = earnings_pal["males"], size = 3.25) +
  geom_point(aes(x = total_earnings_female, y = occupation),
             color = earnings_pal["females"], size = 3.25) +
  facet_wrap(~group_label, nrow = 3, scales = "free_y") +
  scale_x_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k"),
                     breaks = c(25000, 50000, 75000, 100000, 125000))

plot

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            update theme & labels                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot +
  labs(title = "Earnings by Occupation and Sex",
       subtitle = "Median earnings of full-time male vs. female workers by occupation in 2016",
       caption = "Data Source: TidyTuesday (March 5, 2019)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold",
                              size = 25,
                              color = earnings_pal["dark_text"]),
    plot.subtitle = element_text(size = 17,
                                 color = earnings_pal["light_text"],
                                 margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "lines")),
    plot.caption = element_text(color = earnings_pal["light_text"],
                                margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "lines")),
    strip.text.x = element_text(face = "bold",
                                size = 12,
                                hjust = 0),
    panel.spacing.y = unit(x = 1, "lines"),
    axis.text = element_text(color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 10),
    axis.title = element_blank()
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                import fonts                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot +
  labs(title = "Earnings by Occupation and Sex",
       subtitle = "Median earnings of full-time male vs. female workers by occupation in 2016",
       caption = "Data Source: TidyTuesday (March 5, 2019)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "josefin",
                              face = "bold",
                              size = 25),
    plot.subtitle = element_text(family = "sen",
                                 size = 17,
                                 color = earnings_pal["light_text"],
                                 margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "lines")),
    plot.caption = element_text(family = "sen",
                                face = "italic", # NOTE: this no longer applies since the typeface "sen" does not exist in an italic font style
                                color = earnings_pal["light_text"],
                                margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "lines")),
    strip.text.x = element_text(family = "josefin",
                                face = "bold",
                                size = 12,
                                hjust = 0),
    panel.spacing.y = unit(1, "lines"),
    axis.text = element_text(family = "sen",
                             color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 10),
    axis.title = element_blank()
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           add legend to subtitle                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot +
  labs(title = "Earnings by Occupation and Sex",
       subtitle = "Median earnings of full-time <span style='color:#2D7787;font-size:20pt;'>**male**</span> versus <span style='color:#FC6B4B;font-size:20pt;'>**female**</span> workers by occupation in 2016",
       caption = "Data Source: TidyTuesday (March 5, 2019)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "josefin",
                              face = "bold",
                              size = 25),
    plot.subtitle = ggtext::element_textbox_simple(family = "sen",
                                                   size = 17,
                                                   color = earnings_pal["light_text"],
                                                   margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "lines")),
    plot.caption = element_text(family = "sen",
                                face = "italic", # NOTE: this no longer applies since the typeface "sen" does not exist in an italic font style
                                color = earnings_pal["light_text"],
                                margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "lines")),
    strip.text.x = element_text(family = "josefin",
                                face = "bold",
                                size = 12,
                                hjust = 0),
    panel.spacing.y = unit(1, "lines"),
    axis.text = element_text(family = "sen",
                             color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 10),
    axis.title = element_blank()
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            add fontawesome icons                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#........................create subtitle.........................
money_icon <- "&#xf3d1"

subtitle <- glue::glue("Median earnings <span style='font-family:fa-reg;'>{money_icon};</span>
                       of full-time
                       <span style='color:#2D7787;font-size:20pt;'>**male**</span>
                       versus <span style='color:#FC6B4B;font-size:20pt;'>**female**</span>
                       workers by occupation in 2016")

#.........................create caption.........................
github_icon <- "&#xf09b"
github_username <- "samanthacsik"

caption <- glue::glue(
  "Data Source: TidyTuesday (March 5, 2019) |
  <span style='font-family:fa-brands;'>{github_icon};</span>
  {github_username}"
)

#........................add icons to plot.......................
plt <- plot +
  labs(title = "Earnings by Occupation and Sex",
       subtitle = subtitle,
       caption = caption) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "josefin",
                              face = "bold",
                              size = 25),
    plot.subtitle = ggtext::element_textbox_simple(family = "sen",
                                                   size = 17,
                                                   color = earnings_pal["light_text"],
                                                   margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "lines")),
    plot.caption = ggtext::element_textbox(family = "sen",
                                           face = "italic", # NOTE: this no longer applies since the typeface "sen" does not exist in an italic font style
                                           color = earnings_pal["light_text"],
                                           margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "lines")),
    strip.text.x = element_text(family = "josefin",
                                face = "bold",
                                size = 12,
                                hjust = 0),
    panel.spacing.y = unit(1, "lines"),
    axis.text = element_text(family = "sen",
                             color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 10),
    axis.title = element_blank()
  )

showtext_opts(dpi = 320)
ragg::agg_png(here::here("plot_good_agg.png"), res = 320, width = 12, height = 13, units = "in")
plt
dev.off()

ggsave("plot_good.png", plt, width = 8, height = 8)


