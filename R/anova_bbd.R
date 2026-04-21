#' ANOVA do modelo Box-Behnken
#'
#' Retorna a tabela de análise de variância do modelo ajustado.
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#'
#' @return data.frame com a tabela ANOVA.
#' @export
anova_bbd <- function(fit) {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!inherits(fit, "bbd_fit") || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  an <- stats::anova(fit$modelo)

  if (is.null(an)) {
    stop("Não foi possível extrair a tabela ANOVA do modelo.")
  }

  tab <- as.data.frame(an)

  if (ncol(tab) == 0) {
    stop("A tabela ANOVA retornada pelo modelo está vazia.")
  }

  tab$Termo <- rownames(tab)
  rownames(tab) <- NULL

  cols <- c("Termo", setdiff(names(tab), "Termo"))
  tab <- tab[, cols, drop = FALSE]

  names(tab) <- sub("Pr\\(>F\\)", "p_valor", names(tab))
  names(tab) <- sub("Sum Sq", "SQ", names(tab))
  names(tab) <- sub("Mean Sq", "QM", names(tab))
  names(tab) <- sub("F value", "F", names(tab))

  return(tab)
}
