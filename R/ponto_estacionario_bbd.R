#' Calcular ponto estacionario do modelo Box-Behnken
#'
#' Calcula o ponto estacionario de um modelo quadratico ajustado por
#' \code{bbd_fit()}, usando a forma matricial da superficie de resposta.
#'
#' @param fit Objeto retornado por \code{bbd_fit()}.
#'
#' @return Lista com o ponto estacionario, classificacao, autovalores,
#' matriz B, resposta estimada no ponto e status de convergencia.
#' @export
ponto_estacionario_bbd <- function(fit) {
  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!is.list(fit) || is.null(fit$modelo) || is.null(fit$fatores)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  modelo <- fit$modelo
  fatores <- fit$fatores
  coefs <- stats::coef(modelo)
  k <- length(fatores)

  if (k < 2) {
    stop("O modelo precisa ter pelo menos dois fatores.")
  }

  b <- stats::setNames(rep(0, k), fatores)
  B <- matrix(0, nrow = k, ncol = k, dimnames = list(fatores, fatores))

  for (f in fatores) {
    if (f %in% names(coefs)) {
      b[f] <- unname(coefs[f])
    }

    termo_quad <- paste0("I(", f, "^2)")
    if (termo_quad %in% names(coefs)) {
      B[f, f] <- 2 * unname(coefs[termo_quad])
    }
  }

  combinacoes <- utils::combn(fatores, 2, simplify = FALSE)

  for (par in combinacoes) {
    termo_1 <- paste(par, collapse = ":")
    termo_2 <- paste(rev(par), collapse = ":")

    beta_ij <- 0

    if (termo_1 %in% names(coefs)) {
      beta_ij <- unname(coefs[termo_1])
    } else if (termo_2 %in% names(coefs)) {
      beta_ij <- unname(coefs[termo_2])
    }

    B[par[1], par[2]] <- beta_ij
    B[par[2], par[1]] <- beta_ij
  }

  autovalores <- tryCatch(
    eigen(B, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) rep(NA_real_, k)
  )

  tol <- 1e-10

  classificacao <- if (all(is.na(autovalores))) {
    "não determinado"
  } else if (all(autovalores < -tol)) {
    "máximo"
  } else if (all(autovalores > tol)) {
    "mínimo"
  } else {
    "sela"
  }

  solucao <- tryCatch(
    solve(B, b),
    error = function(e) NULL
  )

  if (is.null(solucao) || any(!is.finite(solucao))) {
    ponto_df <- stats::setNames(
      as.data.frame(as.list(rep(NA_real_, k))),
      fatores
    )

    resposta_estimada <- NA_real_
    convergencia <- 1
  } else {
    ponto <- -as.numeric(solucao)
    names(ponto) <- fatores

    ponto_df <- as.data.frame(as.list(ponto))
    resposta_estimada <- as.numeric(stats::predict(modelo, newdata = ponto_df))
    convergencia <- 0
  }

  resultado <- list(
    ponto = ponto_df,
    classificacao = classificacao,
    autovalores = autovalores,
    matriz_B = B,
    resposta_estimada = resposta_estimada,
    convergencia = convergencia,
    status = if (convergencia == 0) "sucesso" else "falha"
  )

  class(resultado) <- "ponto_estacionario_bbd"

  return(resultado)
}
