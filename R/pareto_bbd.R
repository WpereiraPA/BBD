#' Gráfico de Pareto dos efeitos padronizados para Box-Behnken
#'
#' Gera o gráfico de Pareto com base nos efeitos padronizados do modelo ajustado.
#'
#' @param fit Objeto ajustado por \code{bbd_fit()}.
#' @param alpha Nível de significância para a linha crítica. Padrão \code{0.05}.
#' @param cor_barras Cor das barras. Padrão \code{"steelblue3"}.
#'
#' @return Invisivelmente, um data.frame com os termos e efeitos padronizados.
#' @export
pareto_bbd <- function(fit, alpha = 0.05, cor_barras = "steelblue3") {

  if (missing(fit)) {
    stop("O argumento 'fit' é obrigatório.")
  }

  if (!is.list(fit) || is.null(fit$modelo)) {
    stop("Objeto 'fit' inválido. Use um objeto retornado por 'bbd_fit()'.")
  }

  sm <- summary(fit$modelo)

  if (is.null(sm$coefficients)) {
    stop("Não foi possível obter os coeficientes do modelo.")
  }

  coef_tab <- as.data.frame(sm$coefficients, stringsAsFactors = FALSE)
  coef_tab$Termo <- rownames(coef_tab)
  rownames(coef_tab) <- NULL

  if (nrow(coef_tab) <= 1) {
    stop("Não há termos suficientes no modelo para gerar o gráfico de Pareto.")
  }

  coef_tab <- coef_tab[coef_tab$Termo != "(Intercept)", , drop = FALSE]
  coef_tab$efeito_padronizado <- abs(coef_tab[, "t value"])

  formatar_termo <- function(x) {
    x <- gsub(":", "×", x, fixed = TRUE)

    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x <- gsub("AA", "A²", x, fixed = TRUE)
    x <- gsub("BB", "B²", x, fixed = TRUE)
    x <- gsub("CC", "C²", x, fixed = TRUE)
    x <- gsub("DD", "D²", x, fixed = TRUE)
    x <- gsub("EE", "E²", x, fixed = TRUE)
    x <- gsub("FF", "F²", x, fixed = TRUE)
    x <- gsub("GG", "G²", x, fixed = TRUE)
    x <- gsub("HH", "H²", x, fixed = TRUE)
    x <- gsub("II", "I²", x, fixed = TRUE)
    x <- gsub("JJ", "J²", x, fixed = TRUE)

    x
  }

  coef_tab$Termo_grafico <- vapply(coef_tab$Termo, formatar_termo, character(1))
  coef_tab <- coef_tab[order(coef_tab$efeito_padronizado, decreasing = TRUE), , drop = FALSE]

  gl_res <- sm$df[2]

  if (is.null(gl_res) || is.na(gl_res) || gl_res <= 0) {
    stop("Não foi possível determinar os graus de liberdade residuais do modelo.")
  }

  valor_critico <- stats::qt(1 - alpha / 2, df = gl_res)
  xmax <- max(c(coef_tab$efeito_padronizado, valor_critico), na.rm = TRUE) * 1.15

  resposta_titulo <- if (!is.null(fit$resposta) && !is.na(fit$resposta) && nzchar(fit$resposta)) {
    fit$resposta
  } else {
    "Resposta"
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(
    mar = c(5.5, 8.5, 6.3, 2.0),
    mgp = c(3.2, 0.9, 0),
    xpd = NA
  )

  barpos <- graphics::barplot(
    height = coef_tab$efeito_padronizado,
    names.arg = coef_tab$Termo_grafico,
    horiz = TRUE,
    las = 1,
    col = cor_barras,
    border = "gray20",
    xlim = c(0, xmax),
    cex.names = 1.20,
    cex.axis = 1.05,
    main = paste0(
      "Pareto dos Efeitos Padronizados\n(",
      resposta_titulo,
      "; α = ",
      format(alpha, nsmall = 2),
      ")"
    ),
    xlab = "Efeitos padronizados (|t|)",
    ylab = "Termos",
    cex.main = 1.35,
    cex.lab = 1.20
  )

  y_inf <- min(barpos) - 0.45
  y_sup <- max(barpos) + 0.45

  graphics::segments(
    x0 = valor_critico,
    y0 = y_inf,
    x1 = valor_critico,
    y1 = y_sup,
    col = "red",
    lty = 2,
    lwd = 1.2
  )

  rotulo_critico <- format(round(valor_critico, 3), nsmall = 3, decimal.mark = ",")

  graphics::text(
    x = valor_critico,
    y = y_sup + 0.45,
    labels = rotulo_critico,
    col = "red",
    cex = 1.0,
    font = 2
  )

  invisible(
    data.frame(
      termo = coef_tab$Termo,
      termo_grafico = coef_tab$Termo_grafico,
      efeito_padronizado = coef_tab$efeito_padronizado,
      valor_critico = valor_critico,
      stringsAsFactors = FALSE
    )
  )
}
