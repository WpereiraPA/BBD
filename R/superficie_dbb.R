#' Grafico de superficie de resposta para Box Behnken
#'
#' @param fit objeto retornado por dbb_fit()
#' @param x nome do fator para eixo x
#' @param y nome do fator para eixo y
#' @param z.fix valor fixo do terceiro fator, default = 0
#'
#' @export
superficie_dbb <- function(fit, x, y, z.fix = 0) {

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

  seq_x <- seq(-1, 1, length.out = 30)
  seq_y <- seq(-1, 1, length.out = 30)

  grade <- expand.grid(
    temp_x = seq_x,
    temp_y = seq_y
  )

  names(grade) <- c(x, y)
  grade[[zvar]] <- z.fix
  grade <- grade[, fatores]

  pred <- stats::predict(modelo, newdata = grade)
  matriz_z <- matrix(pred, nrow = length(seq_x), ncol = length(seq_y))

  paleta_azul <- grDevices::colorRampPalette(c("#cfe9ff", "#6fb6ff", "#1f5aa6", "#081d58"))

  z_facet <- (matriz_z[-1, -1] + matriz_z[-1, -ncol(matriz_z)] +
                matriz_z[-nrow(matriz_z), -1] + matriz_z[-nrow(matriz_z), -ncol(matriz_z)]) / 4

  n_cores <- 120
  cores <- paleta_azul(n_cores)

  idx <- cut(
    z_facet,
    breaks = n_cores,
    include.lowest = TRUE,
    labels = FALSE
  )

  cores_faces <- matrix(cores[idx], nrow = nrow(z_facet), ncol = ncol(z_facet))

  graphics::persp(
    x = seq_x,
    y = seq_y,
    z = matriz_z,
    theta = 35,
    phi = 22,
    expand = 0.72,
    col = cores_faces,
    shade = 0.45,
    border = "white",
    ltheta = 135,
    lphi = 25,
    xlab = x,
    ylab = y,
    zlab = "Resposta prevista",
    ticktype = "detailed",
    cex.lab = 1.05,
    cex.axis = 0.8,
    mgp = c(2.7, 0.7, 0),
    main = paste(
      "Superficie de resposta:", x, "vs", y,
      "\n", zvar, "fixado em", z.fix
    )
  )
}
