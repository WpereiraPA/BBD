#' @export
print.otimo_bbd <- function(x, digits = 4, ...) {

  cat("\nPonto ótimo do modelo BBD\n\n")

  objetivo_txt <- if (x$objetivo == "min") "Minimizar" else "Maximizar"
  cat("Objetivo:", objetivo_txt, "\n\n")

  cat("Coordenadas codificadas:\n")
  ponto <- unlist(x$ponto)

  for (i in seq_along(ponto)) {
    cat(names(ponto)[i], "=", format(round(ponto[i], digits), nsmall = digits), "\n")
  }

  cat("\nResposta estimada:\n")
  nome_resp <- if (!is.null(x$nome_resposta)) x$nome_resposta else "Resposta"
  cat(nome_resp, "=", format(round(x$resposta, digits), nsmall = digits), "\n")

  cat("\nStatus:\n")
  status <- ifelse(x$convergencia == 0,
                   "Convergência obtida com sucesso.",
                   "Falha na convergência.")
  cat(status, "\n")

  # Comparação com ponto estacionário
  if (!is.null(x$ponto_estacionario)) {
    dif <- sum(abs(unlist(x$ponto) - unlist(x$ponto_estacionario)))

    if (!is.na(dif)) {
      cat("\nComparação com ponto estacionário:\n")

      if (dif < 1e-4) {
        cat("O ponto ótimo coincide com o ponto estacionário.\n")
      } else {
        cat("O ponto ótimo não coincide com o ponto estacionário.\n")
      }
    }
  }

  invisible(x)
}
