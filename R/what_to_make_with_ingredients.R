
#' This function allows the user to filter recipes by their ingredients. Finds recipes that include all of the ingredients you want to use.
#' @param x Dataset of recipes
#' @param input Vector of ingredient names to search for
#' @export
what_to_make_with_ingredients <- function(x, input) {
  library(jsonlite)

  matches <- x

  for (i in input) {
    matches <- matches[sapply(matches$ingredients_list, function(ing) {
      ing_vec <- tolower(fromJSON(ing))
      tolower(i) %in% ing_vec
    }), ]
  }

  if (nrow(matches) == 0) {
    cat("No recipe found.\n")
    return(invisible())
  }

  cat("Recipes with those ingredients:\n\n")

  for (i in 1:nrow(matches)) {
    cat(i, '.', " Recipe ", rownames(matches)[i], ": ", matches$title[i], "\n", sep = "")
  }
  return(invisible(matches))
}
