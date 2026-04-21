#' @export
print.bbd_fit <- function(x, digits = 4, ...) {

  cat("\nModelo ajustado - Box-Behnken\n\n")

  nome_resp <- if (!is.null(x$nome_resposta)) x$nome_resposta else "Resposta"

  cat("Variável resposta:", nome_resp, "\n")
  cat("Fatores:", paste(x$fatores, collapse = ", "), "\n\n")

  cat("Métricas do ajuste:\n")
  cat("R² =", format(round(x$r2, digits), nsmall = digits), "\n")
  cat("R² ajustado =", format(round(x$r2_ajustado, digits), nsmall = digits), "\n")

  erro <- summary(x$modelo)$sigma
  cat("Erro padrão residual =", format(round(erro, digits), nsmall = digits), "\n")

  if (!is.null(x$aviso)) {
    cat("\nObservação:\n")
    cat(x$aviso, "\n")
  }

  invisible(x)
}
