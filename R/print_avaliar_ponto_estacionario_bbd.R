#' @export
print.avaliar_ponto_estacionario_bbd <- function(x, digits = 4, ...) {

  cat("\nAvaliação do ponto estacionário em relação ao objetivo\n\n")

  objetivo_txt <- if (x$objetivo == "max") "Maximização" else "Minimização"
  cat("Objetivo considerado:", objetivo_txt, "\n\n")

  classif_txt <- switch(
    tolower(x$classificacao),
    "máximo" = "Máximo",
    "maximo" = "Máximo",
    "mínimo" = "Mínimo",
    "minimo" = "Mínimo",
    "sela" = "Sela",
    x$classificacao
  )

  cat("Classificação matemática do ponto:\n")
  cat(classif_txt, "\n\n")

  cat("Coordenadas codificadas:\n")
  ponto <- unlist(x$ponto)

  for (i in seq_along(ponto)) {
    cat(names(ponto)[i], "=", format(round(ponto[i], digits), nsmall = digits), "\n")
  }

  cat("\nResposta estimada no ponto:\n")
  nome_resp <- if (!is.null(x$nome_resposta) && nzchar(x$nome_resposta)) {
    x$nome_resposta
  } else {
    "Resposta"
  }
  cat(nome_resp, "=", format(round(x$resposta_estimada, digits), nsmall = digits), "\n\n")

  cat("Adequação ao objetivo:\n")
  cat(tools::toTitleCase(x$adequacao_objetivo), "\n\n")

  cat("Conclusão:\n")
  cat(x$conclusao, "\n")

  invisible(x)
}
