###############################################################################
# ggplot2 theme for clean charts
###############################################################################

library(showtext)
font_add_google("Jost")
showtext_auto()

update_geom_defaults("label", list(family = "Jost"))
update_geom_defaults("text", list(family = "Jost"))

theme_clean <- function(base_size = 14) {
  return <- theme_classic(base_size, base_family="Jost") +
    theme(plot.title = element_text(size = rel(1.2)),
          plot.subtitle = element_text(size = rel(1.1)),
          plot.caption = element_text(size = rel(.8), color = "grey50",
                                      ,
                                      margin = margin(t = 10)),
          plot.tag = element_text(size = rel(.9), color = "grey50",
                                  ),
          strip.text = element_text(size = rel(.9)),
          strip.text.x = element_text(margin = margin(t = 1, b = 1)),
          panel.border = element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line(linewidth = .2),
          legend.title = element_text(size = rel(0.8)),
          legend.position = "bottom")
  
  return
}

# Set ggplot2 theme
theme_set(theme_clean())

# Set global default family for geom_richtext
update_geom_defaults("richtext", list(family = "Jost"))

