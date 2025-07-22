theme_IWK <- function ( maincolor = "#555555", bgcolor = "#eeeeee") { 
  theme_bw(base_size=12, base_family="") %+replace% 
    theme(
      panel.background  = element_rect(fill = bgcolor, color = NA),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = maincolor, linetype = "dotted"),
      
      axis.ticks = element_line(color = maincolor),
      axis.line.x = element_line(color = maincolor),
      
      plot.background = element_rect(fill = bgcolor, color = NA), 
      
      
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = "right",
      legend.key = element_rect(fill = "transparent", color = NA),
      
      
      strip.background.x = element_rect(fill = maincolor, color = maincolor),
      strip.text = element_text(color = "#ffffff", hjust = 0,
                                margin = margin(2,2,3,2, "pt"), 
                                face = "italic")
    )
}
