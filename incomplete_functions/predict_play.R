library(ggplot2)
library(dplyr) # For %>%

# Modified geom_predict function
geom_predict_inherited <- function(model, newdata = NULL, resolution = 500, interval = "confidence", trans = identity, ...) {
  
  # --- Input Validation ---
  if (is.null(newdata) || !is.data.frame(newdata) || nrow(newdata) == 0) {
    stop("`newdata` must be a non-empty data frame containing values for predictor variables (excluding the main sequence variable).")
  }
  if (ncol(newdata) != length(labels(terms(model))) - 1) {
    warning("Number of columns in `newdata` does not match the number of predictors (excluding the response and the sequence variable) in the model formula. Ensure all necessary interaction/grouping variables are provided.")
  }
  
  # --- Identify Variables ---
  model_terms <- terms(model)
  response_var <- all.vars(model_terms)[1] # Get response variable name (though not directly used for prediction sequence)
  predictor_vars <- labels(model_terms)   # Get all predictor variable names
  
  # Identify the variable to create the sequence over
  # Heuristic: Assume it's the first predictor if not specified.
  # A more robust approach might require an explicit argument.
  sequence_var <- predictor_vars[1] # Assuming the first predictor is the one to sequence over
  other_predictors <- setdiff(predictor_vars, sequence_var)
  
  # Ensure newdata contains necessary columns (excluding the sequence variable)
  if (!all(other_predictors %in% names(newdata))) {
    missing_preds <- setdiff(other_predictors, names(newdata))
    stop("`newdata` is missing required predictor columns: ", paste(missing_preds, collapse=", "))
  }
  # Ensure newdata contains only ONE row if there are other predictors,
  # as this function call generates one line/ribbon.
  if (length(other_predictors) > 0 && nrow(newdata) != 1) {
    stop("`newdata` should contain exactly one row specifying the values for grouping/interaction variables for this specific prediction line.")
  }
  
  
  # --- Create Sequence Data ---
  # Extract the specific predictor column used in the model's data
  # This handles cases where the variable might have been transformed in the formula
  model_data <- model$model
  if (!sequence_var %in% names(model_data)) {
    stop("Could not find the sequence variable '", sequence_var, "' in the model's internal data. Check model formula.")
  }
  x_values_from_model <- model_data[[sequence_var]]
  
  # Create a sequence of values for the primary predictor
  x_seq <- seq(min(x_values_from_model), max(x_values_from_model), length.out = resolution)
  
  # Create the prediction data frame
  # Start with the sequence variable
  pred_grid <- setNames(data.frame(x_seq), sequence_var)
  
  # Add the fixed values for other predictors from newdata
  # Use cbind only if newdata actually has columns to add
  if (ncol(newdata) > 0) {
    # Ensure newdata only has the columns needed (other_predictors)
    newdata_subset <- newdata[, intersect(names(newdata), other_predictors), drop = FALSE]
    # Replicate the single row of newdata to match the length of x_seq
    newdata_rep <- newdata_subset[rep(1, nrow(pred_grid)), , drop = FALSE]
    pred_grid <- cbind(pred_grid, newdata_rep)
  }
  
  
  # --- Obtain Predictions ---
  # Use suppressWarnings to handle potential messages from predict.lm about new levels if factors are involved
  preds <- suppressWarnings(predict(model, pred_grid, interval = interval, se.fit = FALSE)) # se.fit=FALSE if not needed
  
  # Check if prediction produced the expected columns
  if (!all(c("fit", "lwr", "upr") %in% colnames(preds))) {
    stop("Prediction did not return 'fit', 'lwr', and 'upr' columns. Check the model type and `interval` argument.")
  }
  
  # --- Apply Transformation ---
  predicted_data <- data.frame(
    x    = x_seq, # Use generic 'x' for internal aes mapping
    y    = trans(preds[, "fit"]),
    ymin = trans(preds[, "lwr"]),
    ymax = trans(preds[, "upr"])
  )
  
  # --- Add Grouping Variables for Aesthetics ---
  # Add the columns from the original newdata back into the predicted_data.
  # This allows ggplot2 to map aesthetics correctly using inherit.aes = TRUE.
  # Replicate the single row from the original newdata.
  if (ncol(newdata) > 0) {
    newdata_rep_orig <- newdata[rep(1, nrow(predicted_data)), , drop = FALSE]
    predicted_data <- cbind(predicted_data, newdata_rep_orig)
  }
  
  
  # --- Define Core Mappings ---
  # These map the *calculated* prediction values (x, y, ymin, ymax)
  # Other aesthetics (like color, fill, linetype) will be inherited from the main ggplot call
  mapping <- aes(x = x, y = y, ymin = ymin, ymax = ymax)
  
  # --- Create Layers ---
  # inherit.aes = TRUE is crucial for inheriting mappings from ggplot()
  # We map fill = after_stat(color) in the ribbon to link fill to the inherited color.
  # Alternatively, map fill explicitly if color and fill should map to different variables.
  ribbon_layer <- geom_ribbon(
    mapping = mapping,
    data = predicted_data,
    inherit.aes = TRUE, # Inherit aesthetics like color (used for fill)
    alpha = 0.2,        # Keep alpha fixed, or allow it via ...
    ...                 # Pass other fixed aesthetics (like alpha) or arguments
  )
  # Manually map fill to the same variable as color if color is inherited
  # This requires knowing the name of the color variable... tricky without full Stat/Geom
  # A simpler approach for now: Let ribbon inherit color, ggplot maps it to fill by default
  # if fill isn't specified. Let's try mapping fill = aes(fill = ..color..) ? No that's old.
  # Let's try mapping fill to the *same variable* the user mapped color to.
  
  # We need to find the variable mapped to color/fill in the parent plot
  # This is hard from *within* this function structure.
  # Workaround: Assume user maps color, and we want fill to match.
  # We added the grouping columns to `predicted_data`.
  # If `aes(color=Tree_num)` is global, `inherit.aes=TRUE` should map both
  # `color` (for line) and `fill` (for ribbon) to `Tree_num` in `predicted_data`.
  
  line_layer <- geom_line(
    mapping = mapping,
    data = predicted_data,
    inherit.aes = TRUE, # Inherit aesthetics like color
    linewidth = 1,      # Use linewidth instead of size for lines >= ggplot2 3.4.0
    ...                 # Pass other fixed aesthetics (like linewidth) or arguments
  )
  
  # Return the list of layers
  list(ribbon_layer, line_layer)
}

# --- Example Usage ---
library(ggplot2)
library(dplyr)

# Ensure Tree_num is treated as a factor for distinct colors/groups
Orange$Tree_num <- factor(Orange$Tree)

# Model
tree.lm <- lm(age ~ circumference * Tree_num, data = Orange)

# Plotting
ggplot(Orange, aes(x = circumference, y = age, color = Tree_num)) +
  geom_point() +
  geom_predict_inherited(tree.lm, newdata = data.frame(Tree_num = factor(1, levels=levels(Orange$Tree_num)))) +
  geom_predict_inherited(tree.lm, newdata = data.frame(Tree_num = factor(2, levels=levels(Orange$Tree_num)))) +
  geom_predict_inherited(tree.lm, newdata = data.frame(Tree_num = factor(3, levels=levels(Orange$Tree_num)))) +
  geom_predict_inherited(tree.lm, newdata = data.frame(Tree_num = factor(4, levels=levels(Orange$Tree_num)))) +
  geom_predict_inherited(tree.lm, newdata = data.frame(Tree_num = factor(5, levels=levels(Orange$Tree_num)))) +
  labs(title = "Orange Tree Growth Prediction by Tree Number",
       x = "Circumference (mm)",
       y = "Age (days)") +
  theme_minimal()
