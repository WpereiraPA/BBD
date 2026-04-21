#' Calcular ponto estacionário do modelo Box-Behnken
#'
#' Calcula o ponto estacionário de um modelo quadrático ajustado por
#' \code{bbd_fit()}, usando a forma matricial da superfície de resposta.
#'
#' @param fit Objeto retornado por \code{bbd_fit()}.
#' @param objetivo objetivo desejado para a resposta: \code{"max"} para maximizar
#'   ou \code{"min"} para minimizar. Se \code{NULL}, apenas a classificação
#'   matemática do ponto é retornada.
#'
#' @return Lista com o ponto estacionário, classificação, autovalores,
#' matriz B, resposta estimada no ponto e status de convergência.
#' @export
ponto_estacionario_bbd <- function(fit, objetivo = NULL) {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!inherits(fit, "bbd_fit") || is.null(fit$modelo) || is.null(fit$fatores)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  if (!is.null(objetivo)) {
    if (!is.character(objetivo) || length(objetivo) != 1 || is.na(objetivo)) {
      stop("O argumento 'objetivo' deve ser NULL, 'max' ou 'min'.")
    }

    objetivo <- tolower(trimws(objetivo))

    if (objetivo %in% c("max", "maximizar", "máximo", "maximo")) {
      objetivo <- "max"
    } else if (objetivo %in% c("min", "minimizar", "mínimo", "minimo")) {
      objetivo <- "min"
    } else {
      stop("O argumento 'objetivo' deve ser NULL, 'max' ou 'min'.")
    }
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

    termo_quad <- grep(paste0("^I\\(", f, "\\^2\\)$"), names(coefs), value = TRUE)

    if (length(termo_quad) == 1) {
      B[f, f] <- 2 * unname(coefs[termo_quad])
    }
  }

  combinacoes <- utils::combn(fatores, 2, simplify = FALSE)

  for (par in combinacoes) {

    padrao <- paste0("(", par[1], ":", par[2], "|", par[2], ":", par[1], ")")
    termo_inter <- grep(padrao, names(coefs), value = TRUE)

    beta_ij <- 0

    if (length(termo_inter) == 1) {
      beta_ij <- unname(coefs[termo_inter])
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

  if (abs(det(B)) < 1e-12) {
    solucao <- NULL
  } else {
    solucao <- tryCatch(solve(B, b), error = function(e) NULL)
  }

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

  adequacao_objetivo <- NULL
  mensagem_objetivo <- NULL

  if (!is.null(objetivo)) {

    adequacao_objetivo <- if (classificacao == "sela") {
      "inconclusivo"
    } else if (objetivo == "max" && classificacao == "máximo") {
      "compatível"
    } else if (objetivo == "min" && classificacao == "mínimo") {
      "compatível"
    } else {
      "não compatível"
    }

    mensagem_objetivo <- if (adequacao_objetivo == "compatível") {
      if (objetivo == "max") {
        "O ponto estacionário é compatível com o objetivo de maximização."
      } else {
        "O ponto estacionário é compatível com o objetivo de minimização."
      }
    } else if (adequacao_objetivo == "inconclusivo") {
      "O ponto estacionário é um ponto de sela, portanto não define diretamente máximo ou mínimo para o objetivo especificado."
    } else {
      if (objetivo == "max") {
        "O ponto estacionário não é compatível com o objetivo de maximização."
      } else {
        "O ponto estacionário não é compatível com o objetivo de minimização."
      }
    }
  }

  resultado <- list(
    ponto = ponto_df,
    classificacao = classificacao,
    autovalores = autovalores,
    matriz_B = B,
    resposta_estimada = resposta_estimada,
    convergencia = convergencia,
    status = if (convergencia == 0) "sucesso" else "falha",
    nome_resposta = fit$nome_resposta,
    objetivo = objetivo,
    adequacao_objetivo = adequacao_objetivo,
    mensagem_objetivo = mensagem_objetivo
  )

  class(resultado) <- "ponto_estacionario_bbd"

  return(resultado)
}
