library(Rcpp)


sourceCpp(file = "unbox-grid.cpp")

sample_palette <- function(seed = NULL, n = 10) {
  if (!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()
}

pretty_boxes <- function(color_seed,
                         box_seed,
                         iterations = 100000000,
                         layers = 5,
                         pixels = 4000,
                         background = "black",
                         border = 4,
                         trim = .001) {
  set.seed(box_seed)

  mat <- unboxer_grid(
    iterations = iterations,
    layers = layers,
    pixels = pixels,
    border = border
  )

  colors <- c(background, sample_palette(color_seed, n = 1023))

  zlim <- quantile(mat, c(trim, 1 - trim))
  mat[mat < zlim[1]] <- zlim[1]
  mat[mat > zlim[2]] <- zlim[2]

  op <- par(mar = c(0, 0, 0, 0))

  image(
    z = mat,
    axes = FALSE,
    asp = 1,
    useRaster = TRUE,
    col = colors
  )
  par(op)
}

outdir <- "../output/pretty_boxes"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

for (i in c(203, 221, 224)) {
  write(paste("Generating", i), stdout())

  out_file <- file.path(outdir, paste0("pretty_box_", i, ".png"))
  png(out_file, width = 4000, height = 4000)

  pretty_boxes(
    iterations = 1e8,
    background = "black",
    pixels = 4000,
    layers = 5,
    color_seed = i,
    box_seed = 230
  )
  dev.off()
}
