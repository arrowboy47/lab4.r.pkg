#' unit conversion for tsp, tbsp, fl oz, cup, pt, qt, gal
#'
#' @param amount numeric quantity for ingredient
#' @param from_unit string of original unit of measurement. Must be one of following: "tsp", "tbsp", "fl oz", "cup", "pt", "qt", "gal"
#' @param to_unit string of desired unit of measurement. Must be one of following: "tsp", "tbsp", "fl oz", "cup", "pt", "qt", "gal"
#'
#' @return string with new amount and unit of measurement.
#'

convert_units <- function(amount, from_unit, to_unit) {
  # Define a lookup table for unit conversion factors relative to teaspoons
  conversion_factors <- data.frame(
    unit = c("tsp", "tbsp", "fl oz", "cup", "pt", "qt", "gal"),
    factor = c(1, 3, 6, 48, 96, 192, 768)
  )

  # Look up the conversion factors for the input and output units
  from_factor <- conversion_factors$factor[conversion_factors$unit == from_unit]
  to_factor <- conversion_factors$factor[conversion_factors$unit == to_unit]


  # If the units are valid, convert the amount using the conversion factors
  if (!is.na(from_factor) & !is.na(to_factor)) {

    new_amount <- amount * from_factor / to_factor
    return(glue::glue("{amount} {from_unit} is equal to {round(new_amount, 2)} {to_unit}."))
  }

  else {
    stop("Invalid input: Please specify valid units, such as 'tsp', 'tbsp', 'fl oz', 'cup', 'pt', 'qt', or 'gal'.")
  }
}
