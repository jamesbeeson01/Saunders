#' Confidence Interval Layers for ggplot2
#'
#' Creates a confidence ribbon and a line of best fit based on a linear model.
#' This function is similar to using `geom_smooth(method = "lm")` but allows for a
#' transformation of the predicted values (and their confidence intervals) via a supplied function.
#'
#' @param lm A fitted linear model object (`lm()`), with a single predictor.
#' @param trans A function to transform the predicted values and intervals (e.g., `exp` for log-transformed models). Default is `identity` (no transformation).
#' @param resolution An integer specifying the number of points at which to evaluate the model. Default is 500.
#'
#' @return A list of ggplot2 layers: a confidence ribbon and a fitted line.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   
#'   # Example dataset
#'   df <- data.frame(x = 1:10, y = exp(1:10 / 2) + rnorm(10))
#'
#'   # Fit a model on the log scale
#'   fit <- lm(log(y) ~ x, data = df)
#'
#'   # Plot original data with points, and add a confidence ribbon and line
#'   ggplot(df, aes(x, y)) +
#'     geom_point() +
#'     geom_confidence(fit, trans = exp)
#' }
geom_confidence <- function(lm, trans = identity, resolution = 500) {
  # Ensure the model has only one predictor
  if (length(lm$coefficients) > 2) {
    stop("geom_confidence() only supports models with a single predictor.")
  }
  
  # Extract predictor values from model
  x <- lm$model[[2]]
  predictor_name <- names(lm$model)[2]  # Get predictor variable name
  
  # Create a sequence of x values across the range
  x_seq <- seq(min(x), max(x), length.out = resolution)
  
  # Create a new data frame for prediction with correct column name
  new_data <- setNames(data.frame(x_seq), predictor_name)
  
  # Obtain predictions with confidence intervals
  preds <- predict(lm, new_data, interval = "confidence")
  
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
    geom_ribbon(data = predicted_data,
                aes(x = x, ymin = ymin, ymax = ymax), 
                fill = "blue", alpha = 0.2),
    geom_line(data = predicted_data, 
              aes(x = x, y = y),
              color = "blue", size = 1)
  )
}
