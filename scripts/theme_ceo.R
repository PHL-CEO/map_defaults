theme_ceo <- function() {
  theme_minimal(base_size = 8, base_family = "serif") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .1, color = "grey9"),
      axis.text = element_text(color = "grey7"),
      axis.ticks.x = element_line(size = 0.5, color = "grey9"),
      axis.ticks.y = element_line(size = 0.5, color = "grey9"),
      axis.title = element_text(color = "grey3"),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
      plot.subtitle = element_text(color = "grey4", size= 8),
      plot.title = element_text(color = "grey4", size= 12),
      plot.title.position = "plot", # This aligns the plot title to the very left edge
      plot.caption = element_text(hjust = 0, color = "grey6"),
      plot.caption.position = "plot",
      plot.margin = margin(.5,.5,.5,.5,"cm"),
      strip.text = element_text(color = "grey7")) 
}