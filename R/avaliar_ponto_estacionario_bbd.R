#' Avaliar ponto estacionário em relação ao objetivo
#'
#' Interpreta o ponto estacionário do modelo ajustado em função do objetivo
#' de otimização desejado, indicando se ele é compatível com maximização
#' ou minimização.
#'
#' @param fit objeto retornado por \code{bbd_fit()}.
#' @param objetivo objetivo desejado para a resposta:
#'   \code{"max"} para maximizar ou \code{"min"} para minimizar.
#'
#' @return Lista com a classificação do ponto estacionário, objetivo,
#' adequação ao objetivo e conclusão interpretativa.
#' @export
avaliar_ponto_estacionario_bbd <- function(fit, objetivo = "max") {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!inherits(fit, "bbd_fit")) {
    stop("O objeto 'fit' deve ser da classe 'bbd_fit'.")
  }

  if (!is.character(objetivo) || length(objetivo) != 1 || is.na(objetivo)) {
    stop("O argumento 'objetivo' deve ser uma string: 'max' ou 'min'.")
  }

  objetivo <- tolower(trimws(objetivo))

  if (objetivo %in% c("max", "maximizar", "máximo", "maximo")) {
    objetivo <- "max"
  } else if (objetivo %in% c("min", "minimizar", "mínimo", "minimo")) {
    objetivo <- "min"
  } else {
    stop("O argumento 'objetivo' deve ser 'max' ou 'min'.")
  }

  pe <- ponto_estacionario_bbd(fit)

  classificacao <- pe$classificacao

  adequacao_objetivo <- if (classificacao == "sela") {
    "inconclusivo"
  } else if (objetivo == "max" && classificacao == "máximo") {
    "compatível"
  } else if (objetivo == "min" && classificacao == "mínimo") {
    "compatível"
  } else {
    "não compatível"
  }

  conclusao <- if (adequacao_objetivo == "compatível") {

    if (objetivo == "max") {
      "O ponto estacionário do modelo é compatível com o objetivo de maximização, indicando uma solução interna de máximo local na superfície ajustada."
    } else {
      "O ponto estacionário do modelo é compatível com o objetivo de minimização, indicando uma solução interna de mínimo local na superfície ajustada."
    }

  } else if (adequacao_objetivo == "inconclusivo") {

    "O ponto estacionário do modelo é classificado como ponto de sela, não representando um máximo ou mínimo definido. Nesse caso, a busca da melhor condição deve ser feita por otimização numérica ou análise da superfície e das bordas da região experimental."

  } else {

    if (objetivo == "max") {
      "O ponto estacionário do modelo é classificado como mínimo local. Portanto, ele não representa a condição desejada de maximização. Nesse caso, a região de máximo deve ser investigada por otimização numérica ou análise das bordas da região experimental."
    } else {
      "O ponto estacionário do modelo é classificado como máximo local. Portanto, ele não representa a condição desejada de minimização. Nesse caso, a região de mínimo deve ser investigada por otimização numérica ou análise das bordas da região experimental."
    }
  }

  resultado <- list(
    ponto = pe$ponto,
    classificacao = classificacao,
    autovalores = pe$autovalores,
    matriz_B = pe$matriz_B,
    resposta_estimada = pe$resposta_estimada,
    nome_resposta = pe$nome_resposta,
    objetivo = objetivo,
    adequacao_objetivo = adequacao_objetivo,
    conclusao = conclusao
  )

  class(resultado) <- "avaliar_ponto_estacionario_bbd"

  return(resultado)
}
