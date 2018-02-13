#' Pomological Theme
#' 
#' [ggplot2] plot theme based on the USDA Pomological Watercolors paintings.
#' 
#' @references https://usdawatercolors.nal.usda.gov/pom
#' @seealso [ggplot2::theme]
#' @param base_family Base text family
#' @param base_size Base text size
#' @param text.color Color of all text (except axis text, see `axis.text.color`)
#' @param plot.background.color Color of plot background, passed to `plot.background`
#' @param panel.grid.color Color of panel grid, passed to `panel.grid`
#' @param panel.grid.linetype Linetype of panel grid, passed to `panel.grid`
#' @param axis.text.color Color of axis text
#' @param axis.text.size Size of axis text
#' @param base_theme Starting theme of plot, default is 
#'   [ggplot2::theme_minimal()]. Any elements set by `theme_pomological()` will
#'   overwrite the `base_theme` unless the specific parameter is explicitly set
#'   to `NULL`.
#'
#' @section Fonts:
#' Complete the pomological watercolor theme with a handwriting or cursive font.
#' The following fonts from [Google Fonts](https://fonts.google.com) work well.
#' Visit the links below to install on your system.
#' 
#' - [Homemade Apple](https://fonts.google.com/specimen/Homemade+Apple/)
#' - [Amatic SC](https://fonts.google.com/specimen/Amatic+SC/)
#' - [Mr. Bedfort](https://fonts.google.com/specimen/Mr+Bedfort/)
#'
#' @examples
#' library(ggplot2)
#' basic_iris_plot <- ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
#'   geom_point(size = 2)
#' 
#' # Pomological Theme  
#' basic_iris_plot + theme_pomological()
#' 
#' # Don't change panel grid color
#' basic_iris_plot + 
#'   theme_pomological(
#'     panel.grid.color = NULL
#'   )
#'   
#' # White background
#' basic_iris_plot +
#'   theme_pomological_nobg()
#' 
#' @export
theme_pomological <- function(
  base_family = 'Homemade Apple', 
  base_size = 16,
  text.color = NULL,
  plot.background.color = NULL,
  panel.grid.color = NULL,
  panel.grid.linetype = 'dashed',
  axis.text.color = NULL,
  axis.text.size = base_size * 14/16,
  base_theme = ggplot2::theme_minimal()
) {
  check_font(base_family)
  
  base_theme + 
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_family, 
        size = base_size, 
        colour = ifelse(hasArg(text.color), text.color, pomological_base$dark_blue)
      ),
      plot.background = ggplot2::element_rect(
        fill = ifelse(hasArg(plot.background.color), plot.background.color, pomological_base$paper), 
        colour = NA
      ),
      panel.grid = ggplot2::element_line(
        colour = ifelse(hasArg(panel.grid.color), panel.grid.color, pomological_base$light_line),
        linetype = panel.grid.linetype),
      panel.grid.major = ggplot2::element_line(
        colour = ifelse(hasArg(panel.grid.color), panel.grid.color, pomological_base$light_line),
        linetype = panel.grid.linetype),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = ifelse(hasArg(axis.text.color), axis.text.color, pomological_base$medium_line), 
        size = axis.text.size)
    )
}

#' @describeIn theme_pomological Pomological theme with white (transparent) background
#' @export
theme_pomological_nobg <- function(...) {
  dots <- list(...)
  dots$plot.background.color <- 'transparent'
  do.call('theme_pomological', args = dots)
}

font_urls <- data.frame(
  name = c("Homemade Apple", "Amatic SC", "Mr. Bedfort"),
  url  = c(
    "https://fonts.google.com/specimen/Homemade+Apple/",
    "https://fonts.google.com/specimen/Amatic+SC/",
    "https://fonts.google.com/specimen/Mr+Bedfort/"
  )
)

check_font <- function(font_name) {
  if (!requireNamespace('extrafont', quietly = TRUE)) {
    warning("The font \"", font_name, "\" may or may not be installed on your system.",
            "Please install the package `extrafont` if you'd like me to be able to check for you.")
  } else {
    if (!font_name %in% extrafont::fonts()) {
      if (font_name %in% font_urls$name) {
        warning("Unable to find font '", font_name, "'. ", 
                "If recently installed, please run `extrafonts::font_import()`. ",
                "To install, visit: ", font_urls[font_urls$name == font_name, 'url'])
      } else {
        warning("Unable to find font '", font_name, "'. ", 
                "If recently installed, please run `extrafonts::font_import()`. ")
      }
    }
  }
}