theme_ceo <- function(){
  font = "sans"

    theme(
      # panel
      panel.grid = element_blank(),
      panel.background = element_blank(),
      #panel.spacing = element_blank(),
      # text
      plot.title.position = "plot",
      plot.title = element_text(
        family = font,
        size = 12,
        face = 'bold',
        hjust = 0,
        vjust = 0),
      plot.subtitle = element_text(
        family = font,
        size = 8,
        hjust = 0,
        vjust = 0,
        face = "italic"),
      plot.caption = element_text(
        family = font,
        size = 6,
        hjust = 1),
      # axis
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      # legend
      legend.position = c(.95, .05),
      legend.justification = c(1, 0), 
      legend.background = element_rect(fill = alpha("white",.8), color = NA), 
      legend.title = element_text(
        family = font,
        size = 8,
        face = 'bold',
        hjust = .5,
        vjust = 0),
      legend.text = element_text(
        family = font,
        size = 6,
        hjust = 0,
        vjust = 0),
      # plot
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )
}
