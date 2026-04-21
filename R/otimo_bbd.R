#' Encontrar ponto otimo previsto
#'
#' @param fit objeto retornado por bbd_fit()
#' @param objetivo objetivo da otimizacao: "max" para maximizar ou "min" para minimizar
#' @export
otimo_bbd <- function(fit, objetivo = "max") {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!inherits(fit, "bbd_fit") || is.null(fit$modelo) || is.null(fit$fatores)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  if (!is.character(objetivo) || length(objetivo) != 1 || is.na(objetivo)) {
    stop("O argumento 'objetivo' deve ser uma string: 'max' ou 'min'.")
  }

  objetivo <- tolower(trimws(objetivo))

  if (!objetivo %in% c("max", "min")) {
    stop("O argumento 'objetivo' deve ser 'max' ou 'min'.")
  }

  modelo <- fit$modelo
  fatores <- fit$fatores
  k <- length(fatores)

  if (k < 2) {
    stop("O modelo precisa ter pelo menos dois fatores.")
  }

  func_obj <- function(x) {
    novo <- as.data.frame(as.list(x))
    names(novo) <- fatores

    pred <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = novo)),
      error = function(e) NA_real_
    )

    if (is.na(pred) || !is.finite(pred)) {
      return(Inf)
    }

    if (objetivo == "max") {
      return(-pred)
    } else {
      return(pred)
    }
  }

  inicio <- rep(0, k)

  resultado <- tryCatch(
    stats::optim(
      par = inicio,
      fn = func_obj,
      method = "L-BFGS-B",
      lower = rep(-1, k),
      upper = rep(1, k)
    ),
    error = function(e) NULL
  )

  ponto_est <- tryCatch(
    ponto_estacionario_bbd(fit),
    error = function(e) NULL
  )

  if (is.null(resultado) || is.null(resultado$par) || any(!is.finite(resultado$par))) {

    ponto_otimo <- stats::setNames(rep(NA_real_, k), fatores)
    resposta_otima <- NA_real_
    convergencia <- 1
    valor_otimizado <- NA_real_

  } else {

    ponto_otimo <- resultado$par
    names(ponto_otimo) <- fatores

    novo_otimo <- as.data.frame(as.list(ponto_otimo))

    resposta_otima <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = novo_otimo)),
      error = function(e) NA_real_
    )

    convergencia <- resultado$convergence

    if (objetivo == "max") {
      valor_otimizado <- -resultado$value
    } else {
      valor_otimizado <- resultado$value
    }
  }

  saida <- list(
    ponto = ponto_otimo,
    resposta = resposta_otima,
    convergencia = convergencia,
    valor_otimizado = valor_otimizado,
    objetivo = objetivo,
    nome_resposta = fit$nome_resposta
  )

  if (!is.null(ponto_est)) {
    saida$ponto_estacionario <- ponto_est$ponto
    saida$classificacao <- ponto_est$classificacao
    saida$autovalores <- ponto_est$autovalores
    saida$matriz_B <- ponto_est$matriz_B
    saida$resposta_estacionaria <- ponto_est$resposta_estimada
    saida$convergencia_ponto_estacionario <- ponto_est$convergencia
  }

  class(saida) <- "otimo_bbd"

  return(saida)
}
