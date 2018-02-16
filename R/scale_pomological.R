pomological_palette <- c(
   "#c03728" #red
  ,"#919c4c" #green darkish
  ,"#fd8f24" #orange brighter
  ,"#f5c04a" #yelloww
  ,"#e68c7c" #pink
  ,"#828585" #light grey
  ,"#c3c377" #green light
  ,"#4f5157" #darker blue/grey
  ,"#6f5438" #lighter brown
)

pomological_base <- list(
  "paper"       = "#fffeea",
  "paper_alt"   = "#f8eed1",
  "light_line"  = "#efe1c6",
  "medium_line" = "#a89985",
  "darker_line" = "#6b452b",
  "black"       = "#3a3e3f",
  "dark_blue"   = "#2b323f"
)

#' Pomological Color and Fill Scales
#' 
#' Color scales based on the USDA Pomological Watercolors paintings.
#' 
#' @references https://usdawatercolors.nal.usda.gov/pom
#' @seealso [ggplot2::scale_colour_discrete] [ggplot2::scale_fill_discrete]
#' @inheritDotParams ggplot2::discrete_scale
#' @name scale_pomological
NULL

pomological_pal <- function() scales::manual_pal(pomological_palette)

#' @rdname scale_pomological
#' @export
scale_colour_pomological <- function(...) ggplot2::discrete_scale("colour", "pomological", pomological_pal(), ...)

#' @rdname scale_pomological
#' @export
scale_color_pomological <- scale_colour_pomological

#' @rdname scale_pomological
#' @export
scale_fill_pomological <- function(...) ggplot2::discrete_scale('fill', 'pomological', pomological_pal(), ...)