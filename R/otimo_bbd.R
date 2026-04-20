#' Encontrar ponto otimo previsto
#'
#' @param fit objeto retornado por bbd_fit()
#' @export
otimo_bbd <- function(fit) {

  modelo <- fit$modelo
  fatores <- fit$fatores

  func_obj <- function(x) {
    novo <- as.data.frame(as.list(x))
    names(novo) <- fatores
    pred <- stats::predict(modelo, newdata = novo)
    -pred
  }

  inicio <- rep(0, length(fatores))

  resultado <- stats::optim(
    par = inicio,
    fn = func_obj,
    method = "L-BFGS-B",
    lower = rep(-1, length(fatores)),
    upper = rep(1, length(fatores))
  )

  ponto_otimo <- resultado$par
  names(ponto_otimo) <- fatores

  novo_otimo <- as.data.frame(as.list(ponto_otimo))
  resposta_otima <- as.numeric(stats::predict(modelo, newdata = novo_otimo))

  ponto_est <- ponto_estacionario_bbd(fit)

  list(
    ponto = ponto_otimo,
    resposta = resposta_otima,
    convergencia = resultado$convergence,
    valor_otimizado = -resultado$value,
    ponto_estacionario = ponto_est$ponto,
    classificacao = ponto_est$classificacao,
    autovalores = ponto_est$autovalores,
    matriz_B = ponto_est$matriz_B,
    resposta_estacionaria = ponto_est$resposta_estimada,
    convergencia_ponto_estacionario = ponto_est$convergencia
  )
}
