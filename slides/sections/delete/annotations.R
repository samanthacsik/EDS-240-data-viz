# may need to restart R: https://github.com/wilkelab/ggtext/issues/68

# lobs collected & acclimated to one of 4 temperatures
# metabolism was measured & foraging trials were conducted

#.........................load libraries.........................
library(tidyverse)

#..........................read in data..........................
lobs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DkDVcl_9rlaqznHfa_v1V1jtZqcuL75Q6wvAHpnCHuk/edit#gid=2143433533") |>
  mutate(temp = as.factor(temp))

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
      legend.position = c(0.85, 0.85),
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

#..............annotate each point using geom_text/label().............

# geom_text() draws plain text ----
lob_plot +
  geom_text(aes(label = lobster_id),
            show.legend = FALSE)

# geom_label() draws a label with outline / background ----
lob_plot +
  geom_label(aes(label = lobster_id),
            show.legend = FALSE)

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
                           nudge_x = 0.1, nudge_y = 0.3)

#..................annotate with {gghighlight}...................
lob_plot +



# ggplot(lobs, aes(x = SMR, y = avg_eaten)) +
#   geom_errorbar(aes(ymin = avg_eaten - sd_eaten, ymax = avg_eaten + sd_eaten), width = 0.02, color = "gray") +
#   geom_point(aes(color = temp, shape = temp, size = temp)) +
#   geom_smooth(method = "lm", color = "gray36", size = 0.75, level = 0.95) +
#   scale_color_manual(values = c("lightslategray", "lightblue", "lightcoral", "indianred4"), name = "Temperature (°C)", labels = c("11", "16", "21", "26")) +
#   scale_shape_manual(values = c(15, 16, 17, 18), name = "Temperature (°C)", labels = c("11", "16", "21", "26")) +
#   scale_size_manual(values = c(4, 4, 4, 5), name = "Temperature (°C)", labels = c("11", "16", "21", "26")) +
#   labs(x = expression(atop("Standard Metabolic Rate", paste("(",mg~O[2]~kg^-1~min^-1,")"))),
#        y = expression(atop("Maximum Consumption Rate", paste("(prey consumed" ~ predator^{-1} ~ "24",~hr^{-1},")")))) +
#   scale_x_continuous(breaks = seq(0, 1.5, by = 0.2)) +
#   theme_classic() +
#   theme(axis.text = element_text(color = "black", size = 12),
#         axis.title = element_text(size = 13),
#         panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
#         plot.caption = element_text(size = 10, hjust = 0),
#         legend.title = element_text(size = 8),
#         legend.text = element_text(size = 8),
#         legend.position = c(0.95, 0.95),
#         legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.box.background = element_rect(color = "black", size = 1.1),
#         legend.margin = margin(3, 3, 3, 3)) +
#   annotate(geom = "curve", x = 0.3, y = 20, xend = 0.184, yend = 9.43,
#            curature = 0.2, arrow = arrow(length = unit(2, "mm"))) +
#   annotate(geom = "text", x = 0.3, y = 20.1, label = "IV10", hjust = "left") +
#   annotate(geom = "curve", x = 1.2, y = 5, xend = 1.31, yend = 14,
#            curvature = 0.3, arrow = arrow(length = unit(3, "mm"))) +
#   annotate(geom = "text", x = 1.19, y = 5.25, label = "IV19", hjust = "right")
