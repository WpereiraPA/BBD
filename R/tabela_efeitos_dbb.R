#' Tabela organizada dos efeitos do modelo DBB
#'
#' @param fit objeto retornado por dbb_fit()
#'
#' @return data.frame ordenado por magnitude do efeito
#' @export
tabela_efeitos_dbb <- function(fit) {

  modelo <- fit$modelo
  resumo <- summary(modelo)

  tabela <- as.data.frame(resumo$coefficients)
  tabela$Efeito <- rownames(tabela)
  rownames(tabela) <- NULL
  tabela$Magnitude <- abs(tabela$Estimate)

  tabela[order(-tabela$Magnitude), ]
}
