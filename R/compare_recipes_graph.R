
# Lets user compare the number of ingredients and steps from two different recipes.
# Useful when deciding between two recipes and want to choose one that takes less effort or requires less ingredients.

#' Provide the recipe numbers for the two recipes being compared
#' @param x Dataset of recipes
#' @param recipe1 Row number of the first recipe
#' @param recipe2 Row number of the second recipe
#' @export
compare_recipes_graph <- function(x, recipe1, recipe2) {
  library(jsonlite)
  library(ggplot2)
  library(tidyr)

  r1 <- x[recipe1, ]
  r2 <- x[recipe2, ]

  # Creates a data frame that compares the ingredients and steps of the two recipes
  df <- data.frame(
    Recipe = c(r1$title, r2$title),
    Ingredients = c(
      length(fromJSON(r1$ingredients)),
      length(fromJSON(r2$ingredients))
    ),
    Steps = c(
      length(fromJSON(r1$directions)),
      length(fromJSON(r2$directions))
    )
  )

  # Makes the data capable of being applied to ggplot
  df_long <- pivot_longer(df, cols = c(Ingredients, Steps),
                          names_to = "Metric",
                          values_to = "Count")

  # Creates a vertical grouped bar graph, with labels and formatting
  # Recipes are side by side with their own steps and ingredients bars together
  ggplot(df_long, aes(x = Recipe, y = Count, fill = Metric)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = Count),
      position = position_dodge(width = 0.9),
      vjust = -0.2
    ) +
    labs(
      title = "Recipe Comparison: Ingredients and Steps",
      x = "Recipe",
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
