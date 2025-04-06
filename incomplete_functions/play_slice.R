library(ggplot2)
library(ggproto)

geom_higherline <- function(mapping = NULL, data = NULL,
                            position = "identity", na.rm = FALSE, 
                            show.legend = NA, inherit.aes = TRUE, ...) {
  
  newdata <- data[y] <- y + 2
  
  layer(
    geom = "line", stat = "identity",  
    mapping = mapping,  # Don't need to specify y explicitly here
    data = newdata, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, ...)
  )
}



df <- data.frame(x = 1:10, y = 1:10)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_higherline(color = "red") +
  scale_y_log10()

