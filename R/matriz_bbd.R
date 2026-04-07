#' Gera matriz Box-Behnken codificada
#'
#' Gera a matriz codificada de um planejamento Box-Behnken para \code{k} fatores,
#' incluindo combinações em pares com níveis -1 e 1, mantendo os demais fatores
#' em 0, e adicionando pontos centrais.
#'
#' O planejamento Box-Behnken é definido para \code{k >= 3}. Na prática,
#' seu uso costuma ser mais viável entre 3 e 5 fatores, podendo ser expandido
#' conforme a viabilidade experimental.
#'
#' @param k Número de fatores. Deve ser inteiro e maior ou igual a 3.
#' @param fatores Vetor de nomes dos fatores. Se \code{NULL}, serão usados
#'   os nomes \code{A}, \code{B}, \code{C}, ..., conforme o número de fatores.
#' @param pontos_centrais Número de pontos centrais. Se \code{NULL}, será usado
#'   um valor prático padrão em função de \code{k}.
#' @param incluir_ensaio Se \code{TRUE}, adiciona a coluna \code{Ensaio}.
#' @param resposta Nome da coluna de resposta. O padrão é \code{"Y"}.
#'
#' @return Um \code{data.frame} com a matriz Box-Behnken codificada.
#' @export
matriz_bbd <- function(k,
                       fatores = NULL,
                       pontos_centrais = NULL,
                       incluir_ensaio = TRUE,
                       resposta = "Y") {

  if (missing(k) || length(k) != 1 || !is.numeric(k) || is.na(k) || k %% 1 != 0 || k < 3) {
    stop("O argumento 'k' deve ser um número inteiro maior ou igual a 3.")
  }

  if (is.null(fatores)) {
    letras_padrao <- LETTERS
    if (k <= length(letras_padrao)) {
      fatores <- letras_padrao[seq_len(k)]
    } else {
      fatores <- paste0("X", seq_len(k))
    }
  } else {
    if (!is.character(fatores) || length(fatores) != k) {
      stop("O argumento 'fatores' deve ser um vetor de caracteres com comprimento igual a 'k'.")
    }
    if (anyNA(fatores) || any(trimws(fatores) == "")) {
      stop("Os nomes em 'fatores' não podem ser vazios ou NA.")
    }
    if (anyDuplicated(fatores) > 0) {
      stop("Os nomes em 'fatores' devem ser únicos.")
    }
  }

  if (is.null(pontos_centrais)) {
    pontos_centrais <- if (k == 3) {
      3
    } else if (k <= 5) {
      5
    } else {
      6
    }
  }

  if (!is.numeric(pontos_centrais) ||
      length(pontos_centrais) != 1 ||
      is.na(pontos_centrais) ||
      pontos_centrais %% 1 != 0 ||
      pontos_centrais < 1) {
    stop("O argumento 'pontos_centrais' deve ser um número inteiro maior ou igual a 1.")
  }

  pares <- utils::combn(k, 2, simplify = FALSE)
  blocos <- vector("list", length(pares))

  for (i in seq_along(pares)) {
    par_atual <- pares[[i]]

    combinacoes <- expand.grid(
      v1 = c(-1, 1),
      v2 = c(-1, 1),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    bloco <- matrix(0, nrow = nrow(combinacoes), ncol = k)
    bloco[, par_atual[1]] <- combinacoes$v1
    bloco[, par_atual[2]] <- combinacoes$v2

    blocos[[i]] <- bloco
  }

  matriz_fatorial <- do.call(rbind, blocos)
  matriz_fatorial <- as.data.frame(matriz_fatorial, stringsAsFactors = FALSE)
  names(matriz_fatorial) <- fatores

  matriz_centros <- as.data.frame(
    matrix(0, nrow = pontos_centrais, ncol = k),
    stringsAsFactors = FALSE
  )
  names(matriz_centros) <- fatores

  matriz <- rbind(matriz_fatorial, matriz_centros)

  if (isTRUE(incluir_ensaio)) {
    matriz <- cbind(
      Ensaio = seq_len(nrow(matriz)),
      matriz
    )
  }

  if (!is.null(resposta)) {
    if (!is.character(resposta) || length(resposta) != 1 || is.na(resposta) || trimws(resposta) == "") {
      stop("O argumento 'resposta' deve ser uma string não vazia.")
    }

    if (resposta %in% names(matriz)) {
      stop("O nome da resposta não pode coincidir com o nome de um fator ou com 'Ensaio'.")
    }

    matriz[[resposta]] <- NA_real_
  }

  rownames(matriz) <- NULL

  return(matriz)
}
