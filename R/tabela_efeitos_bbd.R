#' Tabela organizada dos efeitos do modelo BBD
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#' @param alpha Nivel de significancia. Padrao 0.05.
#'
#' @return data.frame com efeitos, erro padrao, p-valor e significancia.
#' @export
tabela_efeitos_bbd <- function(fit, alpha = 0.05) {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!inherits(fit, "bbd_fit") || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("O argumento 'alpha' deve ser um numero entre 0 e 1.")
  }

  tab <- coeficientes_bbd(fit)

  if (!"p_valor" %in% names(tab)) {
    stop("A tabela de coeficientes nao possui a coluna 'p_valor'.")
  }

  tab$Significativo <- ifelse(
    is.na(tab$p_valor),
    NA_character_,
    ifelse(tab$p_valor <= alpha, "Sim", "Não")
  )

  return(tab)
}
