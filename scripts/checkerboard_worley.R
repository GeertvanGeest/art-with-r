library(ggplot2)
library(ambient)
library(dplyr)

sample_palette <- function(seed = NULL, n = 10) {
  if (!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()
}

blank_canvas <- ambient::long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
)

plot_painted_canvas <- function(canvas, palette = NULL) {
  if (is.null(palette)) {
    palette <- c("#e5ddc8", "#01949a", "#004369", "#db1f48")
  }
  canvas |>
    ggplot(aes(x, y, fill = paint)) +
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

gen_scope <- function(x, y, seed) {
  worley_cell <- ambient::fracture(
    fractal = billow,
    noise = gen_worley,
    x = x,
    y = y,
    octaves = 5,
    seed = seed
  )

  checkerboard_dist <- ambient::gen_checkerboard(x,
    y,
    value = "distance",
    frequency = 10
  )
  return(normalise(worley_cell) + 0.1 * normalise(checkerboard_dist))
}

outdir <- "../output/checkerboard_worley"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

for (i in 15:50) {
  pal <- sample_palette(seed = i, n = 10)

  pic <- blank_canvas |>
    mutate(paint = gen_scope(x, y, seed = i)) |>
    plot_painted_canvas(palette = pal)

  png(file.path(outdir, paste0("/checkerboard_worley_", i, ".png")),
    width = 1000, height = 1000
  )
  print(pic)
  dev.off()
}
