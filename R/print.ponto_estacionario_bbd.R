#' @export
print.ponto_estacionario_bbd <- function(x, digits = 4, ...) {

  cat("\nPonto estacionário do modelo ajustado BBD\n\n")

  classif <- x$classificacao

  classif_txt <- switch(
    tolower(classif),
    "máximo" = "Máximo",
    "maximo" = "Máximo",
    "mínimo" = "Mínimo",
    "minimo" = "Mínimo",
    "sela" = "Sela",
    classif
  )

  cat("Classificação do ponto estacionário:", classif_txt, "\n\n")

  cat("Coordenadas codificadas:\n")

  ponto <- x$ponto
  if (is.data.frame(ponto) && nrow(ponto) >= 1) {
    vals <- as.numeric(ponto[1, ])
    names(vals) <- names(ponto)

    for (nm in names(vals)) {
      cat(nm, "=", format(round(vals[nm], digits), nsmall = digits), "\n")
    }
  } else {
    cat("Ponto não disponível.\n")
  }

  cat("\nResposta estimada no ponto:\n")
  cat("Rendimento =", format(round(x$resposta_estimada, digits), nsmall = digits), "\n\n")

  cat("Autovalores da matriz B:\n")
  autoval <- round(x$autovalores, digits)
  subs <- c("₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉")
  rotulos <- paste0("λ", subs[seq_along(autoval)], " = ", format(autoval, nsmall = digits))
  cat(paste(rotulos, collapse = ", "), "\n\n")

  tol <- 1e-10

  interpretacao <- if (all(is.na(x$autovalores))) {
    "Não foi possível interpretar os autovalores da matriz B."
  } else if (all(x$autovalores < -tol)) {
    "Como todos os autovalores são negativos, a matriz é definida negativa, caracterizando o ponto como um máximo local."
  } else if (all(x$autovalores > tol)) {
    "Como todos os autovalores são positivos, o ponto é classificado como mínimo local."
  } else {
    "Como há autovalores com sinais mistos, o ponto é classificado como ponto de sela."
  }

  cat("Interpretação:\n")
  cat(interpretacao, "\n\n")

  status_txt <- if (is.numeric(x$convergencia)) {
    if (isTRUE(x$convergencia == 0)) "Convergência obtida com sucesso." else "Não foi possível obter convergência."
  } else {
    as.character(x$convergencia)
  }

  cat("Status:\n")
  cat(status_txt, "\n")

  invisible(x)
}
