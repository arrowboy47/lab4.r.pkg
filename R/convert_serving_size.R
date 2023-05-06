#' Converts serving size provided by a recipe to desired serving size
#'
#' @param old numeric value for serving size from original recipe (required)
#' @param new numeric value for serving size user wants (required)
#' @param ingredients vector with list of ingredients (required)
#' @param amounts vector with quantity recipe calls for (required)
#' @param units vector with unit names for ingredients (optional)
#'
#' @return a dataframe with new ingredient quantities for recipe
#'
#' @export


convert_serving_size <- function(old, new, ingredients, amounts, units = NULL) {
  #used chatgpt for assistance

  # Convert amounts to a numeric vector
  amounts <- as.numeric(amounts)

  # Calculate the conversion factor
  conversion_factor <- new / old

  # Multiply each amount by the conversion factor

  new_amounts <- amounts * conversion_factor

  # Combine the ingredients and new amounts into a data frame and return it
  if (is.null(units)) {
    data.frame(ingredient = ingredients, amount = new_amounts)
  }
  else {
    data.frame(ingredient = ingredients, amount = new_amounts, units = units)
  }

}
