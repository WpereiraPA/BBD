#' Gráfico de contorno para Box-Behnken
#'
#' Gera o gráfico de contorno de resposta para dois fatores do modelo ajustado,
#' mantendo os demais fatores fixados em zero.
#'
#' @param fit objeto da classe \code{bbd_fit}.
#' @param x1 nome do primeiro fator.
#' @param x2 nome do segundo fator.
#' @param n número de pontos da grade. Padrão \code{140}.
#' @param mostrar_pontos se TRUE, exibe os pontos experimentais no gráfico.
#'
#' @return Invisivelmente, uma lista com grade e matriz de predições.
#' @export
contorno_bbd <- function(fit, x1, x2, n = 140, mostrar_pontos = FALSE) {

  if (!inherits(fit, "bbd_fit")) {
    stop("O objeto fit precisa ser da classe 'bbd_fit'.")
  }

  if (missing(x1) || missing(x2)) {
    stop("Os argumentos 'x1' e 'x2' são obrigatórios.")
  }

  if (!all(c(x1, x2) %in% fit$fatores)) {
    stop("x1 e x2 precisam estar entre os fatores do modelo.")
  }

  if (x1 == x2) {
    stop("x1 e x2 devem ser diferentes.")
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 20) {
    stop("O argumento 'n' deve ser numérico e maior ou igual a 20.")
  }

  xr <- range(fit$dados[[x1]], na.rm = TRUE)
  yr <- range(fit$dados[[x2]], na.rm = TRUE)

  dx <- diff(xr)
  dy <- diff(yr)

  xs <- seq(xr[1] - 0.04 * dx, xr[2] + 0.04 * dx, length.out = n)
  ys <- seq(yr[1] - 0.04 * dy, yr[2] + 0.04 * dy, length.out = n)

  grade <- expand.grid(xs, ys, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grade) <- c(x1, x2)

  outros_fatores <- setdiff(fit$fatores, c(x1, x2))
  if (length(outros_fatores) > 0) {
    for (f in outros_fatores) {
      grade[[f]] <- 0
    }
  }

  grade <- grade[, fit$fatores, drop = FALSE]

  z <- stats::predict(fit$modelo, newdata = grade)
  zmat <- matrix(z, nrow = n, ncol = n)

  zmin <- min(zmat, na.rm = TRUE)
  zmax <- max(zmat, na.rm = TRUE)

  if (zmin >= 40 && zmax <= 47) {
    niveis_rotulo <- c(41, 42, 43, 44, 45, 46)
    niveis_fill <- seq(40.5, 46.5, by = 0.5)
  } else {
    niveis_rotulo <- pretty(c(zmin, zmax), n = 7)
    niveis_fill <- seq(zmin, zmax, length.out = 13)
  }

  pal <- grDevices::colorRampPalette(
    c("darkgreen", "green3", "chartreuse3", "yellow2", "goldenrod1", "thistle3")
  )

  cls <- grDevices::contourLines(
    x = xs,
    y = ys,
    z = zmat,
    levels = niveis_rotulo
  )

  xlim_inf <- min(xs)
  xlim_sup <- max(xs)
  ylim_inf <- min(ys)
  ylim_sup <- max(ys)

  titulo_resposta <- if (!is.null(fit$resposta) && !is.na(fit$resposta) && nzchar(fit$resposta)) {
    fit$resposta
  } else {
    "Resposta"
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(mar = c(5.2, 5.2, 4.2, 8.5))

  graphics::filled.contour(
    x = xs,
    y = ys,
    z = zmat,
    levels = niveis_fill,
    color.palette = pal,
    xlab = x1,
    ylab = x2,
    main = paste("Gráfico de Contorno da", titulo_resposta),
    key.title = graphics::title(main = titulo_resposta, cex.main = 0.82),
    key.axes = graphics::axis(4, cex.axis = 0.9),
    plot.axes = {
      graphics::axis(1, cex.axis = 0.9)
      graphics::axis(2, cex.axis = 0.9)

      graphics::contour(
        x = xs,
        y = ys,
        z = zmat,
        levels = niveis_rotulo,
        add = TRUE,
        drawlabels = FALSE,
        col = "gray10",
        lwd = 1.2
      )

      for (cl in cls) {

        ok <- which(
          cl$x > (xlim_inf + 0.06 * diff(range(xs))) &
            cl$x < (xlim_sup - 0.04 * diff(range(xs))) &
            cl$y > (ylim_inf + 0.05 * diff(range(ys))) &
            cl$y < (ylim_sup - 0.06 * diff(range(ys)))
        )

        if (length(ok) > 0) {

          nivel_norm <- if ((zmax - zmin) > 0) {
            (cl$level - zmin) / (zmax - zmin)
          } else {
            0.5
          }

          if (nivel_norm >= 0.85) {
            i <- ok[max(1, round(length(ok) * 0.12))]
            desloc <- 0.060 * diff(range(ys))
          } else if (nivel_norm >= 0.70) {
            i <- ok[max(1, round(length(ok) * 0.24))]
            desloc <- 0.040 * diff(range(ys))
          } else if (nivel_norm >= 0.55) {
            i <- ok[max(1, round(length(ok) * 0.38))]
            desloc <- 0.022 * diff(range(ys))
          } else if (nivel_norm >= 0.35) {
            i <- ok[max(1, round(length(ok) * 0.50))]
            desloc <- 0.012 * diff(range(ys))
          } else if (nivel_norm >= 0.20) {
            i <- ok[max(1, round(length(ok) * 0.55))]
            desloc <- 0.030 * diff(range(ys))
          } else {
            i <- ok[max(1, round(length(ok) * 0.50))]
            desloc <- 0.018 * diff(range(ys))
          }

          graphics::text(
            x = cl$x[i],
            y = cl$y[i] + desloc,
            labels = format(round(cl$level, 2), nsmall = 2, decimal.mark = ","),
            cex = 0.85,
            col = "black"
          )
        }
      }

      if (mostrar_pontos) {
        graphics::points(
          fit$dados[[x1]],
          fit$dados[[x2]],
          pch = 15,
          cex = 0.7,
          col = "black"
        )
      }

      graphics::box()
    }
  )

  invisible(
    list(
      x = xs,
      y = ys,
      z = zmat,
      grade = grade
    )
  )
}
