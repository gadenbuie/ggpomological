#' Pomological Theme
#' 
#' [ggplot2] plot theme based on the USDA Pomological Watercolors paintings.
#' 
#' @references https://usdawatercolors.nal.usda.gov/pom
#' @seealso [ggplot2::theme]
#' @param base_family Base text family. See **Fonts** in [theme_pomological()] 
#'   for some examples from Google Fonts options, including `"Mr De Haviland"`, 
#'   `"Homemade Apple"`, `"Marck Script"`, and `"Mr. Bedfort"`. For the 
#'   authentic pomological look, use `"Homemade Apple"` or `"Mr De Haviland"`. 
#'   Set to `NULL` or use [theme_pomological_plain()] for no change to fonts.
#' @param base_size Base text size
#' @param text.color Color of all text (except axis text, see `axis.text.color`)
#' @param plot.background.color Color of plot background, passed to `plot.background`
#' @param panel.border.color Color of plot panel border
#' @param with.panel.grid If `FALSE` gridlines in plot are removed
#' @param panel.grid.color,panel.grid.linetype Color and linetype of panel grid, passed to `panel.grid`
#' @param axis.text.color,axis.text.size Color and size of axis text
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
#' - [Mr. De Haviland](https://fonts.google.com/specimen/Mr+De+Haviland)
#' - [Marck Script](https://fonts.google.com/specimen/Marck+Script/)
#' - [Mr. Bedfort](https://fonts.google.com/specimen/Mr+Bedfort/)
#' 
#' Fonts with R are notoriously tricky, so these may not work well for you. If
#' you have installed the fonts but they aren't showing up or working, you can
#' always try running `extrafont::font_import()` or `extrafont::load_fonts()` in
#' the session or RMarkdown document. Or you can use [theme_pomological_plain()].
#'
#' @examples
#' library(ggplot2)
#' basic_iris_plot <- ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
#'   geom_point(size = 2) +
#'   # with pomological color scale
#'   scale_color_pomological()
#' 
#' # Pomological Theme  
#' basic_iris_plot + 
#'   theme_pomological() 
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
#' # Plain plot without font or background
#' basic_iris_plot +
#'   theme_pomological_plain()
#' 
#' @export
theme_pomological <- function(
  base_family = "Homemade Apple", 
  base_size = 14,
  text.color = pomological_base$dark_blue,
  plot.background.color = pomological_base$paper,
  panel.border.color = pomological_base$light_line,
  with.panel.grid = FALSE,
  panel.grid.color = pomological_base$light_line,
  panel.grid.linetype = "dashed",
  axis.text.color = pomological_base$medium_line,
  axis.text.size = base_size * 3/4,
  base_theme = ggplot2::theme_minimal()
) {
  if (!is.null(base_family)) check_font(base_family)
  
  base_theme +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_family, 
        size = base_size, 
        colour = text.color
      ),
      plot.background = ggplot2::element_rect(
        fill = plot.background.color, 
        colour = NA
      ),
      panel.grid = ggplot2::element_line(
        colour = panel.grid.color,
        linetype = panel.grid.linetype),
      panel.border = ggplot2::element_rect(
        color = panel.border.color,
        fill = NA,
        linetype = "solid",
        size = 0.75
      ),
      panel.grid.major = if (!with.panel.grid) ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = axis.text.color, 
        size = axis.text.size)
    )
}

#' @describeIn theme_pomological Pomological theme with white (transparent) background
#' @export
theme_pomological_nobg <- function(...) {
  dots <- list(...)
  dots$plot.background.color <- "transparent"
  do.call("theme_pomological", args = dots)
}

#' @describeIn theme_pomological A "plain" pomological theme with white 
#'   background and normal fonts.
#' @export
theme_pomological_plain <- function(...) {
  dots <- list(...)
  dots$plot.background.color <- "transparent"
  if (!"base_family" %in% names(dots)) dots["base_family"] <- ""
  if (!"base_size" %in% names(dots)) dots["base_size"] <- 11
  do.call("theme_pomological", args = dots)
}

font_urls <- data.frame(
  name = c("Mr De Haviland", "Homemade Apple", "Marck Script", "Mr. Bedfort"),
  url  = c(
    "https://fonts.google.com/specimen/Mr+De+Haviland",
    "https://fonts.google.com/specimen/Homemade+Apple/",
    "https://fonts.google.com/specimen/Marck+Script/",
    "https://fonts.google.com/specimen/Mr+Bedfort/"
  )
)

check_font <- function(font_name) {
  if (!requireNamespace("extrafont", quietly = TRUE)) {
    warning("The font \"", font_name, "\" may or may not be installed on your system.",
            "Please install the package `extrafont` if you'd like me to be able to check for you.",
            call. = FALSE)
  } else {
    if (!font_name %in% extrafont::fonts()) {
      if (font_name %in% font_urls$name) {
        warning("Font '", font_name, "' isn't in the extrafonts font list (but it may still work). ", 
                "If recently installed, you can try running `extrafonts::font_import()`. ",
                "To install, visit: ", font_urls[font_urls$name == font_name, "url"],
                call. = FALSE)
      } else {
        warning("Font '", font_name, "' isn't in the extrafonts font list (but it may still work). ", 
                "If recently installed, you can try running `extrafonts::font_import()`. ",
                call. = FALSE)
      }
    }
  }
}
