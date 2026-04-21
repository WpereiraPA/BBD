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

  if (!inherits(fit, "bbd_fit") || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  sm <- summary(fit$modelo)

  if (is.null(sm$coefficients)) {
    stop("Não foi possível extrair a tabela de coeficientes do modelo.")
  }

  tab <- as.data.frame(sm$coefficients)

  if (ncol(tab) < 4) {
    stop("A tabela de coeficientes retornada pelo modelo está incompleta.")
  }

  tab$Termo <- rownames(tab)
  rownames(tab) <- NULL

  cols <- c("Termo", setdiff(names(tab), "Termo"))
  tab <- tab[, cols, drop = FALSE]

  names(tab)[1:5] <- c("Termo", "Estimativa", "Erro_Padrao", "t_valor", "p_valor")

  return(tab)
}
