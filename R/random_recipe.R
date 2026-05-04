
#' Finds a random recipe and returns the entire row of information. Title, ingredients, directions, and link.
#' @param x Dataset of recipes
#' @export
random_recipe <- function(x) {
  library(jsonlite)

  # Function chooses a random row in the dataset
  r <- x[sample(nrow(x), 1), ]

  # Gets the ingredients and directions for that rows recipe
  ingredients <- fromJSON(r$ingredients)
  directions <- fromJSON(r$directions)

  # Outputs the recipe's name
  cat("Recipe:", r$title, "\n\n")

  # Outputs list of the recipe's ingredients
  cat("Ingredients:\n")
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
