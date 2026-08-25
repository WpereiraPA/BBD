#' Exporta relatório completo do BBD
#'
#' @param fit objeto da classe bbd_fit
#' @param arquivo nome do arquivo a ser salvo, sem extensão
#' @param formato "txt" ou "doc"
#' @param objetivo NULL para não incluir ótimo, "min" para minimizar
#'   a resposta ou "max" para maximizar
#'
#' @return invisivelmente, o caminho completo do arquivo gerado
#' @export
exportar_relatorio_bbd <- function(fit,
                                   arquivo = "Relatorio_BBD",
                                   formato = "txt",
                                   objetivo = NULL) {

  if (!inherits(fit, "bbd_fit")) {
    stop("O objeto precisa ser da classe 'bbd_fit'.")
  }

  if (!formato %in% c("txt", "doc")) {
    stop("O formato deve ser 'txt' ou 'doc'.")
  }

  if (!is.null(objetivo) && !objetivo %in% c("min", "max")) {
    stop("O argumento 'objetivo' deve ser NULL, 'min' ou 'max'.")
  }

  pasta_destino <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "BBD_Relatorios")

  if (!dir.exists(pasta_destino)) {
    dir.create(pasta_destino, recursive = TRUE)
  }

  arq <- file.path(pasta_destino, paste0(arquivo, ".", formato))

  fmt <- function(x, digits = 4) {
    format(round(x, digits), nsmall = digits, decimal.mark = ",")
  }

  formatar_termo <- function(x) {
    x <- as.character(x)
    x <- gsub(":", "×", x, fixed = TRUE)
    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x
  }

  sm <- summary(fit$modelo)
  an <- as.data.frame(stats::anova(fit$modelo))
  co <- as.data.frame(sm$coefficients)

  co$Termo <- rownames(co)
  rownames(co) <- NULL
  co <- co[, c("Termo", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(co) <- c("Termo", "Estimativa", "Erro_Padrao", "t", "p_valor")
  co$Termo <- formatar_termo(co$Termo)

  efeitos <- co[co$Termo != "(Intercept)",
                c("Termo", "Estimativa", "Erro_Padrao", "t", "p_valor")]
  efeitos$Efeito <- 2 * efeitos$Estimativa
  efeitos <- efeitos[, c("Termo", "Efeito", "Estimativa", "Erro_Padrao", "t", "p_valor")]

  an$Termo <- rownames(an)
  rownames(an) <- NULL
  an <- an[, c("Termo", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]

  beta <- stats::coef(fit$modelo)
  nomes <- names(beta)
  nomes_eq <- formatar_termo(nomes)
  nomes_eq[nomes == "(Intercept)"] <- ""

  eq <- paste0("Y = ", fmt(beta[1], 4))

  if (length(beta) > 1) {
    for (i in 2:length(beta)) {
      sinal <- ifelse(beta[i] >= 0, " + ", " - ")
      eq <- paste0(eq, sinal, fmt(abs(beta[i]), 4), "*", nomes_eq[i])
    }
  }

  otimo_out <- NULL
  ot_mensagem <- NULL

  if (!is.null(objetivo)) {
    if (exists("otimo_bbd", mode = "function")) {
      ot <- tryCatch(
        otimo_bbd(fit, objetivo = objetivo),
        error = function(e) {
          warning("Erro ao calcular o ótimo: ", e$message)
          NULL
        }
      )

      if (!is.null(ot)) {
        nome_resp <- if (!is.null(fit$nome_resposta) && nzchar(fit$nome_resposta)) {
          fit$nome_resposta
        } else if (!is.null(fit$resposta) && nzchar(fit$resposta)) {
          fit$resposta
        } else {
          "Resposta"
        }

        termos <- c(names(ot$ponto), nome_resp)
        valores <- c(as.numeric(ot$ponto), as.numeric(ot$resposta))

        otimo_out <- data.frame(
          Termo = termos,
          Valor = valores,
          stringsAsFactors = FALSE
        )

        if (!is.null(ot$mensagem)) {
          ot_mensagem <- ot$mensagem
        } else {
          lim_inf <- vapply(fit$fatores, function(f) min(fit$dados[[f]], na.rm = TRUE), numeric(1))
          lim_sup <- vapply(fit$fatores, function(f) max(fit$dados[[f]], na.rm = TRUE), numeric(1))
          tol <- 1e-6
          p <- as.numeric(ot$ponto)
          no_limite <- any(abs(p - lim_inf) <= tol | abs(p - lim_sup) <= tol, na.rm = TRUE)
          ot_mensagem <- ifelse(no_limite, "Ótimo localizado no limite da região experimental.", "Ótimo localizado no interior da região experimental.")
        }
      }
    } else {
      warning("A função 'otimo_bbd' não foi encontrada no ambiente. O ótimo não será incluído.")
    }
  }

  sink(arq)
  on.exit(sink(), add = TRUE)

  cat("========================================\n")
  cat(" RELATÓRIO DO PLANEJAMENTO BOX-BEHNKEN\n")
  cat("========================================\n\n")

  cat("Variável resposta: ", fit$resposta, "\n\n", sep = "")

  cat("----------------------------------------\n")
  cat("ESTATÍSTICAS DO MODELO\n")
  cat("----------------------------------------\n")
  cat("R² = ", fmt(sm$r.squared, 4), "\n", sep = "")
  cat("R² ajustado = ", fmt(sm$adj.r.squared, 4), "\n", sep = "")
  cat("Erro padrão residual = ", fmt(sm$sigma, 5), "\n\n", sep = "")

  cat("----------------------------------------\n")
  cat("ANOVA\n")
  cat("----------------------------------------\n")

  an_out <- an
  an_out$Df <- format(an_out$Df, trim = TRUE)
  names(an_out)[names(an_out) == "Df"] <- "GL"
  an_out$`Sum Sq` <- fmt(an_out$`Sum Sq`, 6)
  an_out$`Mean Sq` <- fmt(an_out$`Mean Sq`, 6)
  an_out$`F value` <- ifelse(is.na(an_out$`F value`), "", fmt(an_out$`F value`, 4))
  an_out$`Pr(>F)` <- ifelse(is.na(an_out$`Pr(>F)`), "", fmt(an_out$`Pr(>F)`, 6))

  print(an_out, row.names = FALSE, right = TRUE)
  cat("\n")

  cat("----------------------------------------\n")
  cat("COEFICIENTES DO MODELO\n")
  cat("----------------------------------------\n")

  co_out <- co
  co_out$Estimativa <- fmt(co_out$Estimativa, 5)
  co_out$Erro_Padrao <- fmt(co_out$Erro_Padrao, 5)
  co_out$t <- fmt(co_out$t, 3)
  co_out$p_valor <- fmt(co_out$p_valor, 6)

  print(co_out, row.names = FALSE, right = TRUE)
  cat("\n")

  cat("----------------------------------------\n")
  cat("EFEITOS ESTIMADOS\n")
  cat("----------------------------------------\n")

  efeitos_out <- efeitos
  efeitos_out$Efeito <- fmt(efeitos_out$Efeito, 5)
  efeitos_out$Estimativa <- fmt(efeitos_out$Estimativa, 5)
  efeitos_out$Erro_Padrao <- fmt(efeitos_out$Erro_Padrao, 5)
  efeitos_out$t <- fmt(efeitos_out$t, 3)
  efeitos_out$p_valor <- fmt(efeitos_out$p_valor, 6)

  print(efeitos_out, row.names = FALSE, right = TRUE)
  cat("\n")

  if (!is.null(otimo_out)) {
    cat("----------------------------------------\n")
    cat("ÓTIMO PREVISTO PELO MODELO\n")
    cat("----------------------------------------\n")

    if (objetivo == "min") {
      cat("Objetivo considerado: minimizar a resposta.\n\n")
    } else {
      cat("Objetivo considerado: maximizar a resposta.\n\n")
    }

    otimo_imp <- otimo_out
    otimo_imp$Valor <- fmt(otimo_imp$Valor, 5)

    print(otimo_imp, row.names = FALSE, right = TRUE)

    if (!is.null(ot_mensagem)) {
      cat("\nObservação: ", ot_mensagem, "\n", sep = "")
    }
    cat("\n")
  }

  cat("----------------------------------------\n")
  cat("EQUAÇÃO AJUSTADA\n")
  cat("----------------------------------------\n")
  cat(eq, "\n\n")

  cat("----------------------------------------\n")
  cat("INTERPRETAÇÃO BÁSICA\n")
  cat("----------------------------------------\n")
  cat("O modelo quadrático foi ajustado considerando efeitos lineares,\n")
  cat("quadráticos e de interação entre os fatores.\n")
  cat("A ANOVA permite verificar a contribuição de cada termo no modelo.\n")
  cat("Os coeficientes mostram a direção e a intensidade da influência dos fatores.\n")
  cat("Os efeitos estimados facilitam a interpretação prática da magnitude das mudanças.\n")
  if (!is.null(otimo_out)) {
    cat("O ponto ótimo previsto foi calculado com base no modelo ajustado.\n")
  }
  cat("\nRelatório gerado automaticamente pelo pacote BBD.\n")

  caminho <- normalizePath(arq, winslash = "/", mustWork = FALSE)
  message("Relatório salvo em:\n", caminho)

  invisible(arq)
}
