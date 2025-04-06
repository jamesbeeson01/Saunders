library(tidyverse)

Orange <- Orange %>% mutate(
  Tree_num = as.numeric(Tree),
  Tree_char = as.character(Tree)
)

glimpse(Orange)

tree.lm <- lm(age ~ circumference*Tree_num, Orange)
tree.lm %>% summary()

lm.text <- as.character(summary(tree.lm)[[1]])[2]

lm.text

ggplot(Orange, aes(circumference)) +
  geom_slice(lm, Tree = 1) +
  geom_slice(lm, Tree = 2) +
  geom_slice(lm, Tree = 3) +
  geom_slice(lm, Tree = 4)


function(model, ...) {
  # get info from lm:
  #   
  #   
  
}

# needs to know: 
#   Which one is x?
#   Which coefficient is specified?
#   What values are passed to which coefficient?
#   

# Options
#   User specifies values of every coefficient in a vector
#   User passes a data.frame for a predict
#   User passes column names with values
geom_specify <- function(model, vals, resolution = 500, color = "blue", interval = "confidence") {
  
  # Extract predictor values from model
  x <- vals[1]
  predictor_name <- names(model$model)[2]  # Get predictor variable name
  
  # Create a sequence of x values across the range
  x_seq <- seq(min(x), max(x), length.out = resolution)
  
  # Create a new data frame for prediction with correct column name
  new_data <- setNames(data.frame(x_seq), predictor_name)
  
  # Obtain predictions with confidence intervals
  preds <- predict(lm, new_data, interval = interval)
  
  # Apply the transformation to the predicted fit and its confidence bounds
  predicted_data <- data.frame(
    x    = x_seq,
    y    = trans(preds[, "fit"]),
    ymin = trans(preds[, "lwr"]),
    ymax = trans(preds[, "upr"])
  )
  
  # Return ggplot2 layers: a confidence ribbon and a fitted line.
  # These can be added to an existing ggplot.
  list(
    geom_ribbon(
      data = predicted_data,
      mapping = aes(x = x, ymin = ymin, ymax = ymax), 
      fill = color, alpha = 0.2,
      inherit.aes = FALSE
    ),
    geom_line(
      data = predicted_data, 
      mapping = aes(x = x, y = y),
      color = color, size = 1,
      inherit.aes = FALSE
    )
  )
}

ggplot(Orange(aes(circumference, age))) +
  
  
  plus_two <- function(data, aesthetic) {
    
    
    
  }





geom_slice <- function(model, resolution = 500, color = "blue", interval = "confidence") {
  
  # Extract predictor values from model
  x <- model$model[[2]]
  predictor_name <- names(model$model)[2]  # Get predictor variable name
  
  # Create a sequence of x values across the range
  x_seq <- seq(min(x), max(x), length.out = resolution)
  
  # Create a new data frame for prediction with correct column name
  new_data <- setNames(data.frame(x_seq), predictor_name)
  
  # Obtain predictions with confidence intervals
  preds <- predict(lm, new_data, interval = interval)
  
  # Apply the transformation to the predicted fit and its confidence bounds
  predicted_data <- data.frame(
    x    = x_seq,
    y    = trans(preds[, "fit"]),
    ymin = trans(preds[, "lwr"]),
    ymax = trans(preds[, "upr"])
  )
  
  # Return ggplot2 layers: a confidence ribbon and a fitted line.
  # These can be added to an existing ggplot.
  list(
    geom_ribbon(
      data = predicted_data,
      mapping = aes(x = x, ymin = ymin, ymax = ymax), 
      fill = color, alpha = 0.2,
      inherit.aes = FALSE
    ),
    geom_line(
      data = predicted_data, 
      mapping = aes(x = x, y = y),
      color = color, size = 1,
      inherit.aes = FALSE
    )
  )
}