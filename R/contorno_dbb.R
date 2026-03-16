#' Grafico de contorno para Box Behnken
#'
#' @param fit objeto retornado por dbb_fit()
#' @param x nome do fator para eixo x
#' @param y nome do fator para eixo y
#' @param z.fix valor fixo do terceiro fator, default = 0
#' @param nlevels numero de curvas de nivel
#'
#' @export
contorno_dbb <- function(fit, x, y, z.fix = 0, nlevels = 10) {

  modelo <- fit$modelo
  fatores <- fit$fatores

  if (!(x %in% fatores)) {
    stop("O fator x informado nao esta no modelo.")
  }

  if (!(y %in% fatores)) {
    stop("O fator y informado nao esta no modelo.")
  }

  if (x == y) {
    stop("Os fatores x e y devem ser diferentes.")
  }

  restantes <- setdiff(fatores, c(x, y))

  if (length(restantes) != 1) {
    stop("Esta funcao foi preparada para 3 fatores.")
  }

  zvar <- restantes

  seq_x <- seq(-1, 1, length.out = 100)
  seq_y <- seq(-1, 1, length.out = 100)

  grade <- expand.grid(
    temp_x = seq_x,
    temp_y = seq_y
  )

  names(grade) <- c(x, y)
  grade[[zvar]] <- z.fix
  grade <- grade[, fatores]

  pred <- stats::predict(modelo, newdata = grade)
  matriz_z <- matrix(pred, nrow = length(seq_x), ncol = length(seq_y))

  paleta_contorno <- grDevices::colorRampPalette(c("#dceeff", "#9ecae1", "#4f8fc0", "#08306b"))

  graphics::filled.contour(
    x = seq_x,
    y = seq_y,
    z = matriz_z,
    color.palette = paleta_contorno,
    nlevels = nlevels,
    xlab = x,
    ylab = y,
    main = paste(
      "Grafico de contorno:", x, "vs", y,
      "\n", zvar, "fixado em", z.fix
    ),
    plot.axes = {
      graphics::axis(1)
      graphics::axis(2)
      graphics::contour(
        x = seq_x,
        y = seq_y,
        z = matriz_z,
        add = TRUE,
        drawlabels = TRUE,
        col = "gray25",
        lwd = 1
      )
    }
  )
}
