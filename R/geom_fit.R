#' Model Prediction Layers for ggplot2
#'
#' Creates prediction or confidence interval ribbons and a line of best fit based on a statistical model, attempting to mimic geom_smooth, but with finer controls specific to a provided linear model.
#' This function uses `predict()` to generate values across the range of the primary predictor
#' variable found in the model. It allows for specifying the type of interval ('confidence' or 'prediction'),
#' applying a transformation function to the predictions and intervals, and providing additional
#' predictor values via the `newdata` argument. It returns ggplot2 layers suitable for adding to an existing plot.
#' Note: Assumes the model has a structure where the primary predictor can be inferred as the first variable in the model (i.e. beta 1). This must be your x variable.
#'
#' @param model A linear model (lm()) to create a line from using predict(). The function attempts to automatically extract the primary predictor (x) variable from the model object.
#' @param newdata An optional data frame containing values for other predictor variables in the model (if any) to be held constant during prediction. If `NA` (default), predictions are made only based on the primary predictor sequence. Note: The current internal implementation uses `cbind`, ensure `newdata` is structured appropriately (e.g., a single row data frame with the constant values).
#' @param resolution An integer specifying the number of points across the range of the primary predictor (x) at which to evaluate the model. Default is 500.
#' @param color A character string specifying the color for the resultant line and ribbon. Default is "blue".
#' @param interval A character string specifying the type of interval required: "confidence" (default) or "prediction". Passed to `predict()`.
#' @param trans A function to transform the predicted values and interval bounds (e.g., `exp` for log-transformed response models). Default is `identity` (no transformation).
#' @param se Removes confidence band if specified FALSE (similar to geom_smooth). Defaults to TRUE
#' 
#' @return A list containing two ggplot2 layer objects: a `geom_ribbon` for the interval and a `geom_line` for the fitted values.
#' 
#' @importFrom ggplot2 aes geom_ribbon geom_line ggplot geom_point
#' @importFrom dplyr %>%
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Example dataset with two predictors
#' set.seed(123)
#' df <- data.frame(
#'   x = 1:20,
#'   group = factor(rep(c("A", "B"), each = 10)),
#'   y = 0.5 * (1:20) + ifelse(rep(c("A", "B"), each = 10) == "B", 5, 0) + rnorm(20)
#' )
#'
#' # Fit a linear model
#' fit <- lm(y ~ x + group, data = df)
#'
#' # Base plot
#' p <- ggplot(df, aes(x = x, y = y, color = group))
#'
#' # Add prediction lines and confidence intervals for each group
#' # Note: geom_fit varies 'x', holds 'group' constant via 'newdata'
#' p +
#'   geom_fit(fit, newdata = data.frame(group = "A"), color = "red") +
#'   geom_fit(fit, newdata = data.frame(group = "B"), color = "blue", interval = "prediction")
#'
#' # Example with log transformation
#' df_log <- data.frame(x = 1:10, y = exp(0.5 + 0.2*(1:10) + rnorm(10, sd = 0.1)))
#' fit_log <- lm(log(y) ~ x, data = df_log)
#' ggplot(df_log, aes(x = x, y = y)) +
#'  geom_point() +
#'  geom_fit(fit_log, trans = exp, color = "purple")
#' }

geom_fit <- function(model, newdata = NA, resolution = 500, color = "blue", interval = "confidence", trans = identity, se = TRUE) {

  # Extract predictor values from model
  x <- model$model[[2]]
  predictor_name <- names(model$model)[2]  # Get predictor variable name
  
  # Create a sequence of x values across the range
  x_seq <- seq(min(x), max(x), length.out = resolution)
  
  # Create a new data frame for prediction with correct column name
  new_data <- setNames(data.frame(x_seq), predictor_name) %>% cbind(newdata)
  
  # Obtain predictions with confidence intervals
  preds <- predict(model, new_data, interval = interval)
  
  # Apply the transformation to the predicted fit and its confidence bounds
  predicted_data <- data.frame(
    x    = x_seq,
    y    = trans(preds[, "fit"]),
    ymin = trans(preds[, "lwr"]),
    ymax = trans(preds[, "upr"])
  )
  
  # Return ggplot2 layers: a confidence ribbon and a fitted line.
  # These can be added to an existing ggplot.
  if (se) {
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
        color = color, linewidth = 1,
        inherit.aes = FALSE
      )
    )
  } else {
    geom_line(
      data = predicted_data, 
      mapping = aes(x = x, y = y),
      color = color, linewidth = 1,
      inherit.aes = FALSE
    )
  }
}
