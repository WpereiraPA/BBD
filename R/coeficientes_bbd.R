#' Coeficientes do modelo Box-Behnken
#'
#' Retorna a tabela de coeficientes do modelo ajustado.
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#'
#' @return data.frame com estimativas, erro padrão, estatística t e p-valor.
#' @export
coeficientes_bbd <- function(fit) {
  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!is.list(fit) || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  sm <- summary(fit$modelo)
  tab <- as.data.frame(sm$coefficients)
  tab$Termo <- rownames(tab)
  rownames(tab) <- NULL

  cols <- c("Termo", setdiff(names(tab), "Termo"))
  tab <- tab[, cols, drop = FALSE]

  names(tab) <- c("Termo", "Estimativa", "Erro_Padrao", "t_valor", "p_valor")

  tab
}
