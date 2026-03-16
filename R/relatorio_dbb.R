#' Relatorio estatistico completo no padrao brasileiro
#'
#' @param fit objeto ajustado por dbb_fit()
#' @return lista com ANOVA, coeficientes, R2 e R2 ajustado formatados
#' @export
relatorio_dbb <- function(fit){

  modelo <- fit$modelo

  anova_tab <- as.data.frame(stats::anova(modelo))
  coef_tab  <- as.data.frame(summary(modelo)$coefficients)

  r2 <- summary(modelo)$r.squared
  r2aj <- summary(modelo)$adj.r.squared

  formata <- function(tab){
    tab[] <- lapply(tab, function(x){
      if(is.numeric(x)) format(x, decimal.mark = ",", digits = 6)
      else x
    })
    tab
  }

  anova_tab <- formata(anova_tab)
  coef_tab  <- formata(coef_tab)

  r2  <- format(r2, decimal.mark = ",")
  r2aj <- format(r2aj, decimal.mark = ",")

  resultado <- list(
    ANOVA = anova_tab,
    Coeficientes = coef_tab,
    R2 = r2,
    R2_ajustado = r2aj
  )

  return(resultado)
}
