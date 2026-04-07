#' Métricas do ajuste Box-Behnken
#'
#' Retorna métricas principais do modelo ajustado.
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#'
#' @return data.frame com métricas do modelo.
#' @export
metricas_bbd <- function(fit) {
  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!is.list(fit) || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  sm <- summary(fit$modelo)
  fstat <- sm$fstatistic

  p_modelo <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

  tab <- data.frame(
    Metrica = c(
      "R2",
      "R2_Ajustado",
      "Erro_Padrao_Residual",
      "F_Modelo",
      "GL1",
      "GL2",
      "p_valor_modelo",
      "N_Observacoes"
    ),
    Valor = c(
      unname(sm$r.squared),
      unname(sm$adj.r.squared),
      unname(sm$sigma),
      unname(fstat[1]),
      unname(fstat[2]),
      unname(fstat[3]),
      unname(p_modelo),
      nobs(fit$modelo)
    )
  )

  tab
}
