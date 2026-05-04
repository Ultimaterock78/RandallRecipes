
# Analysis of the main ingredients used within recipes.
# Ranks them from most used to least, and provides a count of how many recipes each ingredient is used in.

#' Cleans ingredient data, combines the same ingredients that have slightly different names
#' This fixes the count for graph, all instances are counted together instead of separately
#' @param x Vector of ingredient names
#' @export
clean_ingredients <- function(x) {
  x <- tolower(x)
  x <- gsub("eggs?", "egg", x)
  x <- gsub("margarine", "butter", x)
  x <- gsub("white sugar", "sugar", x)
  x
}

#' Creates a horizontal bar graph of the top 10 or (user chooses), ingredients used in recipes
#' Counts how many recipes each ingredient is in, then shows it in decreasing order
#' @param x Dataset of recipes
#' @param top_n Number of top ingredients to display
#' @export
top_ingredients_graph <- function(x, top_n = 10) {
  library(jsonlite)
  library(ggplot2)

  # Extracts ingredients from ingredient_list column in data set
  all_ingredients <- unlist(lapply(x$ingredients_list, function(i) {
    fromJSON(i)
  }))

  # Apply clean_ingredients function
  all_ingredients <- clean_ingredients(all_ingredients)

  # Counts the frequency of every ingredient
  freq <- sort(table(all_ingredients), decreasing = TRUE)

  # Picks the top number of ingredients, 10 or the user input
  df <- data.frame(
    ingredient = names(freq)[1:top_n],
    count = as.numeric(freq[1:top_n])
  )

  # Creates the bar graph with labels and formatting
  ggplot(df, aes(x = reorder(ingredient, count), y = count)) +
    geom_col() +
    geom_text(aes(label = count), hjust = -0.2) +
    coord_flip() +
    labs(
      title = "Comparison of Ingredients Used in Recipes",
      x = "Ingredient",
      y = "Number of Recipes"
    ) +
  theme(plot.title = element_text(hjust = 0.5))
}

