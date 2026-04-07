#' Gráfico de superfície para Box-Behnken
#'
#' Gera o gráfico de superfície de resposta para dois fatores do modelo ajustado,
#' mantendo os demais fatores fixados em zero.
#'
#' @param fit objeto da classe \code{bbd_fit}.
#' @param x1 nome do primeiro fator.
#' @param x2 nome do segundo fator.
#' @param n número de pontos da grade. Padrão \code{45}.
#'
#' @return Invisivelmente, uma lista com grade e matriz de predições.
#' @export
superficie_bbd <- function(fit, x1, x2, n = 45) {

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

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 10) {
    stop("O argumento 'n' deve ser numérico e maior ou igual a 10.")
  }

  xs <- seq(min(fit$dados[[x1]], na.rm = TRUE), max(fit$dados[[x1]], na.rm = TRUE), length.out = n)
  ys <- seq(min(fit$dados[[x2]], na.rm = TRUE), max(fit$dados[[x2]], na.rm = TRUE), length.out = n)

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

  zlim <- range(zmat, na.rm = TRUE)

  pal <- grDevices::colorRampPalette(
    c("darkgreen", "green3", "chartreuse3", "yellow2", "goldenrod1", "thistle3")
  )
  cols <- pal(160)

  nrz <- nrow(zmat)
  ncz <- ncol(zmat)

  zfacet <- zmat[-1, -1] +
    zmat[-1, -ncz] +
    zmat[-nrz, -1] +
    zmat[-nrz, -ncz]

  zfacet <- c(zfacet / 4, zlim)

  idx <- cut(
    zfacet,
    breaks = length(cols),
    include.lowest = TRUE,
    labels = FALSE
  )

  facetcol <- cols[idx]

  titulo_resposta <- if (!is.null(fit$resposta) && !is.na(fit$resposta) && nzchar(fit$resposta)) {
    fit$resposta
  } else {
    "Resposta"
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(mar = c(4.4, 4.6, 3.8, 1.6))

  graphics::persp(
    x = xs,
    y = ys,
    z = zmat,
    zlim = zlim,
    theta = 55,
    phi = 24,
    r = 3.5,
    expand = 0.70,
    col = facetcol,
    border = grDevices::adjustcolor("black", alpha.f = 0.35),
    ticktype = "detailed",
    shade = 0.5,
    ltheta = 50,
    lphi = 25,
    xlab = x1,
    ylab = x2,
    zlab = titulo_resposta,
    cex.lab = 1.10,
    cex.axis = 0.85,
    main = paste("Superfície de Resposta de", titulo_resposta)
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
