#' Encontrar ponto otimo previsto
#'
#' @param fit objeto retornado por dbb_fit()
#' @export
otimo_dbb <- function(fit) {

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
  resposta_otima <- stats::predict(modelo, newdata = novo_otimo)

  list(
    ponto = ponto_otimo,
    resposta = resposta_otima,
    convergencia = resultado$convergence,
    valor_otimizado = -resultado$value
  )
}
