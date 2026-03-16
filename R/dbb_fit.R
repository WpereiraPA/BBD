#' Ajustar modelo Box Behnken
#'
#' @param dados data.frame com os fatores e a resposta
#' @param resposta nome da coluna resposta entre aspas
#'
#' @return Lista com formula, modelo, summary, anova, coeficientes, residuos e metricas
#' @export
dbb_fit <- function(dados, resposta) {

  colunas_ignorar <- c("Ensaio", "Ensaios", "Run", "Ordem", "Tratamento")

  fatores <- setdiff(names(dados), c(resposta, colunas_ignorar))

  if (length(fatores) < 2) {
    stop("O modelo precisa de pelo menos dois fatores experimentais.")
  }

  termo_linear <- paste(fatores, collapse = " + ")

  interacoes <- utils::combn(fatores, 2, function(x) paste(x, collapse = ":"))
  termo_interacao <- paste(interacoes, collapse = " + ")

  quadrados <- paste0("I(", fatores, "^2)")
  termo_quadratico <- paste(quadrados, collapse = " + ")

  formula_txt <- paste(
    resposta, "~",
    termo_linear, "+",
    termo_interacao, "+",
    termo_quadratico
  )

  formula_modelo <- stats::as.formula(formula_txt)
  modelo <- stats::lm(formula_modelo, data = dados)

  resumo <- summary(modelo)
  anova_mod <- stats::anova(modelo)
  residuos <- stats::residuals(modelo)
  sq_residuos <- sum(residuos^2)

  aviso <- NULL
  if (sq_residuos < 1e-10) {
    aviso <- "Ajuste essencialmente perfeito. A ANOVA com teste F pode ser instavel."
  }

  list(
    formula = formula_modelo,
    fatores = fatores,
    modelo = modelo,
    summary = resumo,
    anova = anova_mod,
    coeficientes = stats::coef(modelo),
    residuos = residuos,
    soma_quadrados_residuos = sq_residuos,
    r2 = resumo$r.squared,
    r2_ajustado = resumo$adj.r.squared,
    aviso = aviso
  )
}
