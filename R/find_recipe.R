
#' Searches for a specific recipe based on the users input, whether it be the name of a meal or a singular food item.
#' Outputs the name of the recipes and their number.
#' @param x Dataset of recipes
#' @param input Keyword or phrase to search for in recipe titles
#' @export
find_recipe <- function(x, input) {

  # Searches through every recipe name and matches the user input to all similar recipes
  matches <- x[grepl(input, x$title, ignore.case = TRUE), ]

  # If there are no recipes with a similar word or phrase, outputs no recipe
  if(nrow(matches) == 0) {
    cat("No recipe found.\n")
    return(invisible())
  }

  # Outputs all of the recipe names that have that word or phrase, and their recipe number
  cat('Recipes Found:\n')

  for(i in 1:nrow(matches)) {
    cat(i, '.', " Recipe ", rownames(matches)[i], ": ", matches$title[i], '\n', sep = '')
  }
  return(invisible(matches))
}

