#' Ajustar modelo Box-Behnken
#'
#' Ajusta o modelo quadrático de um planejamento Box-Behnken a partir
#' dos fatores codificados e da variável resposta.
#'
#' @param dados data.frame com os fatores e a resposta.
#' @param resposta nome da coluna resposta entre aspas.
#'
#' @return Lista da classe \code{bbd_fit} com fórmula, modelo, resumo,
#' ANOVA, coeficientes, resíduos e métricas do ajuste.
#' @export
bbd_fit <- function(dados, resposta) {

  if (missing(dados) || !is.data.frame(dados)) {
    stop("O argumento 'dados' deve ser um data.frame.")
  }

  if (missing(resposta) || !is.character(resposta) || length(resposta) != 1 || is.na(resposta) || trimws(resposta) == "") {
    stop("O argumento 'resposta' deve ser uma string não vazia.")
  }

  if (!resposta %in% names(dados)) {
    stop("A coluna de resposta informada não foi encontrada em 'dados'.")
  }

  colunas_ignorar <- c("Ensaio", "Ensaios", "Run", "Ordem", "Tratamento")

  fatores <- setdiff(names(dados), c(resposta, colunas_ignorar))

  if (length(fatores) < 2) {
    stop("O modelo precisa de pelo menos dois fatores experimentais.")
  }

  if (anyNA(dados[[resposta]])) {
    stop("A coluna de resposta contém valores ausentes. Preencha todos os valores antes do ajuste.")
  }

  if (!is.numeric(dados[[resposta]])) {
    stop("A coluna de resposta precisa ser numérica.")
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
    aviso <- "Ajuste essencialmente perfeito. A ANOVA com teste F pode ser instável."
  }

  resultado <- list(
    formula = formula_modelo,
    resposta = resposta,
    fatores = fatores,
    dados = dados,
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

  class(resultado) <- "bbd_fit"

  return(resultado)
}
