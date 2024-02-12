# may need to restart R: https://github.com/wilkelab/ggtext/issues/68

# lobs collected & acclimated to one of 4 temperatures
# metabolism was measured & foraging trials were conducted


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.........................load libraries.........................
library(tidyverse)

#..........................read in data..........................
lobs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DkDVcl_9rlaqznHfa_v1V1jtZqcuL75Q6wvAHpnCHuk/edit#gid=2143433533") |>
  mutate(temp = as.factor(temp))

mono <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1o0-89RFp2rI2y8hMQWy-kquf_VIzidmhmVDXQ02JjCA/edit#gid=164128885")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            create lobster plot                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................create theme..........................
lob_theme <- function(){
  theme_light() +
    theme(
      axis.title.x = ggtext::element_markdown(size = 13,
                                              margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "lines")),
      axis.title.y = ggtext::element_markdown(size = 13,
                                              margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")),
      axis.text = element_text(color = "black", size = 12),
      panel.border = element_rect(colour = "black", size = 0.7),
      panel.grid = element_blank(),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.position = c(0.95, 0.95),
      legend.justification = c(0.95, 0.95),
      legend.box.background = element_rect(color = "black", size = 1.1)

    )
}

#..........................create scales.........................
lob_palette <- c("11" = "#7B8698",
                 "16" = "#BAD7E5",
                 "21" = "#DC7E7C",
                 "26" = "#7D3E40")

lob_shapes <-  c("11" = 15,
                 "16" = 16,
                 "21" = 17,
                 "26" = 18)

lob_sizes <- c("11" = 6,
               "16" = 6,
               "21" = 6,
               "26" = 7)

#........................create plot text........................
x_axis_lab <- glue::glue("Resting Metabolic Rate<br>
                         (mg O<sub>2</sub> kg<sup>-1</sup> min<sup>-1</sup>)")

y_axis_lab <- glue::glue("Maximum Consumption Rate<br>
                         (prey consumed predator<sup>-1</sup> 24hr<sup>-1</sup>)")

#............................plot data...........................
lob_plot <- ggplot(lobs, aes(x = SMR, y = avg_eaten,
                 color = temp, shape = temp, size = temp)) +
  geom_point() +
  scale_color_manual(values = lob_palette, name = "Temperature (ºC)") +
  scale_shape_manual(values = lob_shapes, name = "Temperature (ºC)") +
  scale_size_manual(values = lob_sizes, name = "Temperature (ºC)") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x = x_axis_lab,
       y = y_axis_lab) +
  lob_theme()

lob_plot


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         geom_text() & geom_label()                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# geom_text needs:
  # coordinates where we want our annotation to be
  # label (text)

# geom_text() will receive any globally-mapped aesthetics (just like any other geoms that you use!)
# annotate() only works with the information you provide it directly

# annotate():
  # needs geom specified
  # will not create a legend


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         geom_text() vs. annotate()                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# issue with geom_text() -- it inherits aesthetics from ggplot() ----
lob_plot +
  geom_text(
    x = 0.1,
    y = 25,
    label = "Important lobsters",
    size = 4,
    color = "black",
    hjust = "inward",
    show.legend = FALSE
  ) +
  geom_rect(
    xmin = 0.25, xmax = 0.85,
    ymin = 8.5, ymax = 18,
    alpha = 0.5,
    fill = "gray40", color = "black",
    show.legend = FALSE
  )

# use annotate instead! ----
lob_plot +
  annotate(
    geom = "text",
    x = 0.1,
    y = 25,
    label = "Important lobsters",
    size = 4,
    color = "black",
    hjust = "inward"
  ) +
  annotate(
    geom = "rect",
    xmin = 0.25, xmax = 0.85,
    ymin = 8.5, ymax = 18,
    alpha = 0.5,
    fill = "gray70", color = "black"
  ) +
  annotate(
    geom = "curve",
    x = 0.3, xend = 0.5,
    y = 23.8, yend = 19,
    curvature = -0.15,
    arrow = arrow(length = unit(0.3, "cm"))
  )


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      geom_text() for labeling points                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..............annotate each point using geom_text/label().............

# geom_text() draws plain text ----
lob_plot +
  geom_text(aes(label = lobster_id),
            show.legend = FALSE)

# geom_label() draws a label with outline / background ----
lob_plot +
  geom_label(aes(label = lobster_id),
            show.legend = FALSE)

# or just label a select few with annotate() ----
lob_plot +
  annotate(
    geom = "text",
    x = 0.3, y = 20.1,
    label = "IV10",
    hjust = "left"
    ) +
  annotate(
    geom = "curve",
    x = 0.3, xend = 0.184,
    y = 20, yend = 9.43,
    arrow = arrow(length = unit(0.3, "cm")),
    linewidth = 0.6
    ) +
  annotate(
    geom = "text",
    x = 1.19,
    y = 5.25,
    label = "IV19",
    hjust = "right"
    ) +
  annotate(
    geom = "curve",
    x = 1.2, xend = 1.31,
    y = 5, yend = 14,
    arrow = arrow(length = unit(0.3, "cm")),
    linewidth = 0.6
    )

#..............annotate each point using {ggrepel}...............

# geom_text_repel() ----
lob_plot +
  ggrepel::geom_text_repel(aes(label = lobster_id),
                           size = 4,
                           color = "gray10",
                           nudge_x = 0.1, nudge_y = 0.3,
                           arrow = arrow(length = unit(2, "mm")))

# geom_label_repel() ----
lob_plot +
  ggrepel::geom_label_repel(aes(label = lobster_id),
                           size = 4,
                           color = "gray10",
                           nudge_x = 0.1, nudge_y = 0.3,
                           arrow = arrow(length = unit(2, "mm")))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              annotating facets                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#......................using {gghighlight}.......................
lob_plot +
  gghighlight::gghighlight() +
  facet_wrap(~temp) +
  theme(legend.position = "none")

ggplot(lobs, aes(x = SMR, y = avg_eaten,
       color = temp, shape = temp, size = temp)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(~temp)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            adjust text alignment                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c(
    "bottom-left", "top-left",
    "bottom-right", "top-right", "center"
  )
)

# no adjustments ----
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))

# clip = "off" + adjust x-axis limits ----
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text)) +
  coord_cartesian(clip = "off") + # THIS IS IMPORTANT
  scale_x_continuous(limits = c(0.9, 2.1))

# vjust & hjust ----
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text),
            vjust = "inward", hjust = "inward")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  Mono Lake                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# from Allison Horst!

ggplot(data = mono, aes(x = year, y = lake_level)) +

  # gray box behind data we want to highlight ----
  geom_rect(aes(xmin = 1941,
                xmax = 1983,
                ymin = 6350,
                ymax = 6440),
            fill = "gray90") +

  # plot data as line graph ----
  geom_line() +

  # add labs ----
  labs(x = "\nYear",
       y = "Lake surface level\n(feet above sea level)\n",
       title = "Mono Lake levels (1850 - 2017)\n",
       caption = "Data: Mono Basin Clearinghouse") +
  scale_x_continuous(limits = c(1850, 2020),
                     expand = c(0,0),
                     breaks = seq(1850, 2010, by = 20)) +
  scale_y_continuous(limits = c(6350, 6440),
                     breaks = c(6370, 6400, 6430),
                     expand = c(0,0),
                     labels = scales::label_comma()) +
  annotate("text", x = 1962, y = 6425,
           label = "unrestricted diversions\n(1941 - 1983)",
           size = 3) +
  theme_light() +
  theme(plot.title.position = "plot",
        axis.text.y = element_text(face = "italic")) +
  geom_hline(yintercept = 6360, linetype = "dashed") +
  annotate("text",
           x = 1910,
           y = 6367,
           label = "Decreased shrimp abundance expected\n(6,360 feet above sea level)",
           size = 3)
