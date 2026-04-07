#' Tabela organizada dos efeitos do modelo BBD
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#' @param alpha Nível de significância. Padrão 0.05.
#'
#' @return data.frame com efeitos, erro padrão, p-valor e significância.
#' @export
tabela_efeitos_bbd <- function(fit, alpha = 0.05) {
  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!is.list(fit) || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  tab <- coeficientes_bbd(fit)

  tab$Significativo <- ifelse(tab$p_valor <= alpha, "Sim", "Não")

  tab
}
