
#' After using find_recipe, which gives the name of recipes and the recipe number, use this function to get
#' the ingredients, directions, and link for one recipe. Must use the recipe number because some recipes have the same name.
#' @param x Dataset of recipes
#' @param index Row number of the recipe
#' @export
get_recipe <- function(x, index) {
  library(jsonlite)

  # If there is no recipe with that number, output explains that, try again with number from find_recipe
  if (index > nrow(x) || index < 1) {
    cat("Option not available.\n")
    return(invisible())
  }

  # Gets the recipe title, ingredients, and directions that match that number
  r <- x[index, ]
  ingredients <- fromJSON(r$ingredients)
  directions <- fromJSON(r$directions)

  # Outputs the recipe's title
  cat("Recipe:", r$title, "\n")

  # Outputs list of the recipe's ingredients
  cat("\nIngredients:\n")
  for (i in ingredients) {
    cat("-", i, "\n")
  }

  # Outputs the recipe's directions in order
  cat("\nDirections:\n")
  for (i in seq_along(directions)) {
    cat(i, ". ", directions[i], "\n", sep = "")
  }

  # Outputs the link to the recipe's website
  cat("\nLink:\n", r$link, "\n")
}
