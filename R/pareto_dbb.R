#' Grafico de Pareto dos efeitos DBB
#'
#' @param fit objeto retornado por dbb_fit()
#' @param alpha nivel de significancia
#'
#' @export
pareto_dbb <- function(fit, alpha = 0.05) {

  modelo <- fit$modelo
  resumo <- summary(modelo)

  tabela <- as.data.frame(resumo$coefficients)
  tabela$Efeito <- rownames(tabela)
  rownames(tabela) <- NULL

  tabela <- tabela[tabela$Efeito != "(Intercept)", ]
  tabela$Magnitude <- abs(tabela$Estimate)

  tabela$Efeito <- gsub("I\\(A\\^2\\)", "A2", tabela$Efeito)
  tabela$Efeito <- gsub("I\\(B\\^2\\)", "B2", tabela$Efeito)
  tabela$Efeito <- gsub("I\\(C\\^2\\)", "C2", tabela$Efeito)

  tabela <- tabela[order(tabela$Magnitude), ]

  df_res <- modelo$df.residual
  t_crit <- stats::qt(1 - alpha / 2, df_res)

  graphics::barplot(
    tabela$Magnitude,
    names.arg = tabela$Efeito,
    horiz = TRUE,
    las = 1,
    col = "steelblue",
    xlab = "Magnitude do efeito",
    main = "Grafico de Pareto"
  )

  graphics::abline(v = t_crit * resumo$sigma, col = "red", lwd = 2, lty = 2)
}
