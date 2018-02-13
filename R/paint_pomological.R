#' Paint a ggpomological watercolor
#' 
#' Uses [magick] to paint a pomological watercolor. (Paints your plot onto a 
#' pomological watercolor style paper, with texture overlay.)
#' 
#' @references https://usdawatercolors.nal.usda.gov/pom
#' @seealso [theme_pomological]
#' @param pomo_gg A pomologically styled ggplot2 object. See [theme_pomological()]
#' @param width Width of output image in pixels
#' @param height Height of output image in pixels
#' @param pointsize Text size for plot text
#' @param outfile Optional name for output file if you'd like to save the image
#' @param pomological_background Paper image, defaults to paper texture provided
#'   by ggpomological.
#' @param pomological_overlay Overlay texture. Set to `NULL` for no texture.
#' @inheritDotParams magick::image_graph res clip antialias
#' @export
paint_pomological <- function(
  pomo_gg, 
  width = 800, 
  height = 500, 
  pointsize = 16, 
  outfile = NULL,
  pomological_background = pomological_images("background"), 
  pomological_overlay = pomological_images("overlay"),
  ...
) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The package magick is required for `paint_pomological()`. ",
         "Please install with `install.packages('magick')`")
  }
  if (!file.exists(pomological_background)) {
    warning(paste0("Cannot find file \"", pomological_background, "\""), call. = FALSE)
  }
  
  # Paint figure
  gg_fig <- magick::image_graph(width, height, bg = "transparent", pointsize = pointsize, ...)
  print(pomo_gg)
  dev.off()
  
  if (!is.null(pomological_overlay) && file.exists(pomological_overlay)) {
    pomo_over <- magick::image_read(pomological_overlay)
    pomo_over <- magick::image_resize(pomo_over, paste0(width, "x", height, "!"))
    gg_fig <- magick::image_composite(gg_fig, pomo_over, "blend", compose_args = "15")
  }
  
  # Paint background
  if (file.exists(pomological_background)) {
    pomo_bg <- magick::image_read(pomological_background)
    pomo_bg <- magick::image_resize(pomo_bg, paste0(width, "x", height, "!"))
    pomo_bg <- magick::image_crop(pomo_bg, paste0(width, "x", height))
    
    # Paint figure onto background
    pomo_img <- magick::image_composite(pomo_bg, gg_fig)
  } else pomo_img <- gg_fig
  
  if (!is.null(outfile)) {
    # Do you want your picture framed?
    magick::image_write(pomo_img, outfile)
  }
  pomo_img
}

pomological_images <- function(which = c("background", "overlay")) {
  which <- match.arg(which)
  exts <- c("background" = ".png", "overlay" = ".jpg")
  system.file("images", paste0("pomological_", which, exts[which]),
              package = "ggpomological")
}