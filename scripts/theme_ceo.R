theme_ceo <- function(){
  font = "sans"
  theme_minimal() %+replace%
    
    theme(
      panel.grid.major = element_blank(),

      panel.grid.minor = element_blank(),
      
      panel.background = element_blank(),
      
      plot.title = element_text(
        family = font,
        size = 14,
        face = 'bold',
        hjust = 0,
        vjust = 0,
        margin = margin(2, b = 10)),
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        margin = margin(2, b = 10),
        face = "italic"),
      
      plot.caption = element_text(
        family = font,
        size = 10,
        hjust = 1),
      
      axis.ticks = element_blank(),
      
      axis.line = element_blank(),
      
      axis.title = element_blank(),
      
      axis.text = element_blank()

      
    )
}
