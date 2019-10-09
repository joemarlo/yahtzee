require(tidyverse)

#theme for ggplot
seashell.theme <- theme(legend.position = "none",
                        panel.grid.minor = element_line(color = NA),
                        panel.grid.major = element_line(color = "seashell"),
                        panel.background = element_rect(fill = "seashell2"),
                        plot.background = element_rect(fill = "seashell",
                                                       color = NA),
                        axis.title = element_text(color = "gray30",
                                                  size = 12),
                        strip.background = element_rect(fill = NA),
                        strip.text = element_text(size = 10,
                                                  face = "bold"),
                        plot.title = element_text(color = "gray30",
                                                  size = 14,
                                                  face = "bold"),
                        text = element_text(family = "AvantGarde"))
