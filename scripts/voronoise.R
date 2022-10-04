library(voronoise)
library(tidyr)
library(dplyr)

sample_palette <- function(seed = NULL, n = 10) {
  if (!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()
}
outdir <- "../output/voronoise"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

for (i in 1:100) {
  set.seed(i)
  npix <- 200
  data <- tibble(
    x = runif(npix, min = -1.5, max = 1.5),
    y = runif(npix, min = -1.5, max = 1.5),
    val = runif(npix)
  )

  p <- data |>
    ggplot(aes(x, y, fill = val)) +
    theme_void() +
    coord_equal(xlim = c(-1.8, 1.8), ylim = c(-1.8, 1.8)) +
    scale_fill_gradientn(colours = sample_palette(seed = i, n = 7)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_voronoi_tile(
      max.radius = .18,
      radius = .01,
      size = 1,
      colour = "black",
      show.legend = FALSE
    )

  png(file.path(outdir, paste0("voronoise_", i, ".png")))
  print(p)
  dev.off()
}
