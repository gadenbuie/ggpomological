# # load all colors
# x <- readLines("pomological.css")
# x <- stringr::str_extract(x, "#[0-9a-f]{6}")
# x <- x[!is.na(x)]
# 
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# col2hsv <- function(x) rgb2hsv(col2rgb(x))
# 
# 
# dist2ref_color <- function(color, ref_color) {
#   stopifnot(length(ref_color) == 1)
#   x <- col2hsv(c(color, ref_color)) %>% 
#     t %>%
#     dist %>%
#     as.matrix %>%
#     {tibble('ref_color' = .[length(color) + 1, 1:length(color)])}
#   names(x) <- ref_color
#   x
# }
# 
# compare_to_ggplot <- function(compare_to_ggplot) {
#   pomo_gg <- map_dfr(set_names(compare_to_ggplot), ~ as_tibble(t(col2hsv(.))), .id = "color") %>% 
#     bind_cols(
#       map_dfc(gg_color_hue(set_names(length(compare_to_ggplot))), ~ dist2ref_color(compare_to_ggplot, .))
#     ) %>% 
#     tidyr::gather('ggplot_color', 'dist', -color:-v) %>% 
#     # group_by(color) %>% 
#     # do(dist = min(.$dist), ggplot_color = filter(., dist == min(.$dist))$ggplot_color) %>% 
#     # mutate(dist = map_dbl(dist, ~ .), ggplot_color = map_chr(ggplot_color, ~ .)) %>% 
#     # ungroup %>% 
#     mutate(ggplot_color = factor(ggplot_color, gg_color_hue(length(compare_to_ggplot)))) %>% 
#     arrange(ggplot_color)
#   warning(glue::glue("Palette has {length(compare_to_ggplot)} colors"), call. = FALSE)
#   ggplot(pomo_gg) +
#     aes(x = ggplot_color, y = dist, fill = color, label = color) +
#     geom_label(color = 'white')+
#     # geom_point(shape = 15, size = 5) +
#     scale_fill_identity()
# }
# 
# data_frame(
#   'color' = color_options,
#   # 'group' = sample(c('pomo', 'logical'), length(color_options), replace = TRUE),
#   'x' = pmap_chr(tidyr::crossing(letters, letters), ~paste0(..1, ..2))[1:length(color_options)],
#   'y' = 1:length(color_options)
# ) %>% 
#   ggplot() +
#   aes(x, y, fill = color) +
#   # geom_point(size = 8)+
#   geom_col()+
#   geom_text(aes(label = color), hjust = -0.1) +
#   scale_fill_identity() +
#   coord_flip() +
#   theme_minimal() +
#   #theme_xkcd() +
#   theme(
#     text = element_text(family = 'gWriting', size = 16),
#     plot.background = element_rect(fill = base_colors["paper_light"], color = NA),
#     panel.grid = element_line(color = "#efe1c6"),
#     axis.text = element_text(color = "#655843", size = 14)
#   )
# 
# ordered_plot <- function(color_options, dichromat = FALSE) {
#   if (dichromat) {
#     dichr_type <- sample(c("deutan", "protan", "tritan"), 1)
#     message(glue::glue("color blindness: {dichr_type}"))
#     color_options <- dichromat::dichromat(color_options, dichr_type)
#   }
#   data_frame(
#     color = color_options,
#     x = 1,
#     y = 1:length(color_options)
#   ) %>% 
#     ggplot() +
#     aes(x, y, fill = color, label = color) +
#     geom_tile() +
#     geom_label(color = 'white') +
#     scale_fill_identity()+
#     scale_y_continuous(breaks = 1:length(color_options), labels = 1:length(color_options))+
#     theme_minimal()
# }