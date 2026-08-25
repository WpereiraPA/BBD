#' Exporta resultados do BBD para Excel
#'
#' Exporta os resultados numéricos do planejamento Box-Behnken para Excel,
#' incluindo dados, métricas, ANOVA, coeficientes, efeitos, ponto ótimo
#' e ponto estacionário.
#'
#' @param fit objeto da classe bbd_fit
#' @param arquivo nome do arquivo Excel
#' @param usar_desktop se TRUE, salva na Área de Trabalho, em uma pasta
#'   chamada BBD_Resultados
#' @param alpha nível de significância para destacar efeitos
#' @param fatores vetor opcional com os fatores a considerar nos gráficos;
#'   se NULL, usa fit$fatores
#' @param objetivo objetivo da otimização: "max" para maximizar ou "min"
#'   para minimizar. Padrão \code{"max"}.
#'
#' @return invisivelmente, o caminho do arquivo gerado
#' @export
exportar_excel_bbd <- function(fit,
                               arquivo = NULL,
                               usar_desktop = TRUE,
                               alpha = 0.05,
                               fatores = NULL,
                               objetivo = "max") {

  if (is.null(arquivo)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
    arquivo <- paste0("relatorio_bbd_", timestamp, ".xlsx")
  }

  .exportar_excel_bbd_base(
    fit = fit,
    arquivo = arquivo,
    usar_desktop = usar_desktop,
    alpha = alpha,
    fatores = fatores,
    incluir_graficos = FALSE,
    objetivo = objetivo
  )
}

#' Exporta resultados completos do BBD para Excel com gráficos
#'
#' Exporta os resultados do planejamento Box-Behnken para Excel, incluindo
#' tabelas, ponto ótimo, ponto estacionário, gráfico de Pareto,
#' superfícies de resposta e gráficos de contorno.
#'
#' @param fit objeto da classe bbd_fit
#' @param arquivo nome do arquivo Excel
#' @param usar_desktop se TRUE, salva na Área de Trabalho, em uma pasta
#'   chamada BBD_Resultados
#' @param alpha nível de significância para destacar efeitos
#' @param fatores vetor opcional com os fatores a considerar nos gráficos;
#'   se NULL, usa fit$fatores
#' @param objetivo objetivo da otimização: "max" para maximizar ou "min"
#'   para minimizar. Padrão \code{"max"}.
#'
#' @return invisivelmente, o caminho do arquivo gerado
#' @export
exportar_excel_completo_bbd <- function(fit,
                                        arquivo = NULL,
                                        usar_desktop = TRUE,
                                        alpha = 0.05,
                                        fatores = NULL,
                                        objetivo = "max") {

  if (is.null(arquivo)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    arquivo <- paste0("relatorio_completo_bbd_", timestamp, ".xlsx")
  }

  .exportar_excel_bbd_base(
    fit = fit,
    arquivo = arquivo,
    usar_desktop = usar_desktop,
    alpha = alpha,
    fatores = fatores,
    incluir_graficos = TRUE,
    objetivo = objetivo
  )
}

.exportar_excel_bbd_base <- function(fit,
                                     arquivo,
                                     usar_desktop,
                                     alpha,
                                     fatores,
                                     incluir_graficos = FALSE,
                                     objetivo = "max") {

  if (!inherits(fit, "bbd_fit")) {
    stop("O objeto precisa ser da classe 'bbd_fit'.")
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Instale o pacote 'openxlsx' para usar esta função.")
  }

  if (!is.character(arquivo) || length(arquivo) != 1 || is.na(arquivo) || trimws(arquivo) == "") {
    stop("O argumento 'arquivo' deve ser uma string não vazia.")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("O argumento 'alpha' deve ser um número entre 0 e 1.")
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

  if (!grepl("\\.xlsx$", arquivo, ignore.case = TRUE)) {
    arquivo <- paste0(arquivo, ".xlsx")
  }

  if (is.null(fatores)) {
    fatores <- fit$fatores
  }

  fatores <- as.character(fatores)

  if (!all(fatores %in% fit$fatores)) {
    stop("Todos os fatores informados precisam estar em fit$fatores.")
  }

  if (usar_desktop) {
    pasta_destino <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "BBD_Resultados")

    if (!dir.exists(pasta_destino)) {
      dir.create(pasta_destino, recursive = TRUE)
    }

    arquivo <- file.path(pasta_destino, basename(arquivo))
  }

  formatar_termo <- function(x) {
    x <- as.character(x)
    x <- gsub(":", " × ", x, fixed = TRUE)
    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x
  }

  interpretar_autovalores_local <- function(autovalores, tol = 1e-10, tol_zero = 1e-4) {

    if (all(is.na(autovalores))) {
      return("Não foi possível interpretar os autovalores da matriz B.")
    }

    texto_base <- if (all(autovalores < -tol)) {
      "Como todos os autovalores são negativos, a matriz é definida negativa, caracterizando o ponto como um máximo local."
    } else if (all(autovalores > tol)) {
      "Como todos os autovalores são positivos, a matriz é definida positiva, caracterizando o ponto como um mínimo local."
    } else {
      "Como os autovalores apresentam sinais mistos, o ponto é classificado como ponto de sela."
    }

    aviso_curvatura <- if (any(abs(autovalores) < tol_zero, na.rm = TRUE)) {
      " Há autovalores próximos de zero, o que pode indicar baixa curvatura em pelo menos uma direção."
    } else {
      ""
    }

    paste0(texto_base, aviso_curvatura)
  }

  nome_resposta <- if (!is.null(fit$nome_resposta) && nzchar(fit$nome_resposta)) {
    fit$nome_resposta
  } else if (!is.null(fit$resposta) && nzchar(fit$resposta)) {
    fit$resposta
  } else {
    "Resposta"
  }

  mensagem_otimo_excel <- function(ot, fit, fatores) {

    if (!is.null(ot$mensagem) && nzchar(ot$mensagem)) {
      return(ot$mensagem)
    }

    if (is.null(ot$ponto) || any(!is.finite(as.numeric(ot$ponto)))) {
      return("")
    }

    lim_inf <- vapply(
      fatores,
      function(f) min(fit$dados[[f]], na.rm = TRUE),
      numeric(1)
    )

    lim_sup <- vapply(
      fatores,
      function(f) max(fit$dados[[f]], na.rm = TRUE),
      numeric(1)
    )

    ponto <- as.numeric(ot$ponto)
    names(ponto) <- names(ot$ponto)

    tol_limite <- 1e-6

    no_limite <- any(
      abs(ponto - lim_inf) <= tol_limite |
        abs(ponto - lim_sup) <= tol_limite,
      na.rm = TRUE
    )

    if (isTRUE(no_limite)) {
      "Ótimo localizado no limite da região experimental."
    } else {
      "Ótimo localizado no interior da região experimental."
    }
  }

  interpretar_autovalores_excel <- function(autovalores) {
    if (exists("interpretar_autovalores_bbd", mode = "function")) {
      return(interpretar_autovalores_bbd(autovalores))
    }
    interpretar_autovalores_local(autovalores)
  }

  wb <- openxlsx::createWorkbook()

  estilo_cabecalho <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    fgFill = "#D9EAF7"
  )

  estilo_corpo <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight"
  )

  estilo_significativo <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    fgFill = "#FFF2CC",
    fontColour = "#C00000",
    textDecoration = "bold"
  )

  estilo_titulo <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 12,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    fgFill = "#D9EAF7"
  )

  aplicar_estilo_tabela <- function(nome_aba, df) {

    openxlsx::addStyle(
      wb = wb,
      sheet = nome_aba,
      style = estilo_cabecalho,
      rows = 1,
      cols = 1:ncol(df),
      gridExpand = TRUE,
      stack = TRUE
    )

    if (nrow(df) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = nome_aba,
        style = estilo_corpo,
        rows = 2:(nrow(df) + 1),
        cols = 1:ncol(df),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::freezePane(wb, nome_aba, firstRow = TRUE)
    openxlsx::setColWidths(wb, nome_aba, cols = 1:ncol(df), widths = "auto")
  }

  aplicar_estilo_bloco <- function(nome_aba, df, startRow, startCol = 1) {
    openxlsx::writeData(wb, nome_aba, df, startRow = startRow, startCol = startCol)

    openxlsx::addStyle(
      wb = wb,
      sheet = nome_aba,
      style = estilo_cabecalho,
      rows = startRow,
      cols = startCol:(startCol + ncol(df) - 1),
      gridExpand = TRUE,
      stack = TRUE
    )

    if (nrow(df) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = nome_aba,
        style = estilo_corpo,
        rows = (startRow + 1):(startRow + nrow(df)),
        cols = startCol:(startCol + ncol(df) - 1),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  # =======================
  # DADOS
  # =======================
  if (!is.null(fit$dados) && is.data.frame(fit$dados)) {
    dados_df <- fit$dados

    openxlsx::addWorksheet(wb, "Dados")
    openxlsx::writeData(wb, "Dados", dados_df)
    aplicar_estilo_tabela("Dados", dados_df)
  }

  # =======================
  # MÉTRICAS
  # =======================
  met_df <- data.frame(
    Métrica = c("R²", "R² ajustado", "Erro padrão residual"),
    Valor = c(
      fit$r2,
      fit$r2_ajustado,
      summary(fit$modelo)$sigma
    ),
    check.names = FALSE
  )

  openxlsx::addWorksheet(wb, "Métricas")
  openxlsx::writeData(wb, "Métricas", met_df)
  aplicar_estilo_tabela("Métricas", met_df)

  # =======================
  # ANOVA
  # =======================
  anova_df <- anova_bbd(fit)

  if ("Df" %in% names(anova_df)) {
    names(anova_df)[names(anova_df) == "Df"] <- "GL"
  }

  openxlsx::addWorksheet(wb, "ANOVA")
  openxlsx::writeData(wb, "ANOVA", anova_df)
  aplicar_estilo_tabela("ANOVA", anova_df)

  # =======================
  # COEFICIENTES
  # =======================
  coef_df <- coeficientes_bbd(fit)
  coef_df$Termo <- formatar_termo(coef_df$Termo)

  openxlsx::addWorksheet(wb, "Coeficientes")
  openxlsx::writeData(wb, "Coeficientes", coef_df)
  aplicar_estilo_tabela("Coeficientes", coef_df)

  # =======================
  # EFEITOS
  # =======================
  efeitos_df <- tabela_efeitos_bbd(fit, alpha = alpha)
  efeitos_df$Termo <- formatar_termo(efeitos_df$Termo)

  openxlsx::addWorksheet(wb, "Efeitos")
  openxlsx::writeData(wb, "Efeitos", efeitos_df)
  aplicar_estilo_tabela("Efeitos", efeitos_df)

  if ("Significativo" %in% names(efeitos_df) && nrow(efeitos_df) > 0) {
    linhas_sig <- which(efeitos_df$Significativo == "Sim")

    if (length(linhas_sig) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = "Efeitos",
        style = estilo_significativo,
        rows = linhas_sig + 1,
        cols = 1:ncol(efeitos_df),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  # =======================
  # ÓTIMO
  # =======================
  ot <- NULL

  if (exists("otimo_bbd", mode = "function")) {
    ot <- tryCatch(
      otimo_bbd(fit, objetivo = objetivo),
      error = function(e) NULL
    )
  }

  if (!is.null(ot)) {
    # Função auxiliar para forçar a vírgula nas strings
    fmt_num <- function(x) format(round(as.numeric(x), 4), decimal.mark = ",", nsmall = 4, trim = TRUE)

    df_objetivo <- data.frame(
      Item = "Objetivo",
      Valor = ifelse(!is.null(ot$objetivo) && ot$objetivo == "min", "Minimizar", "Maximizar"),
      check.names = FALSE
    )

    df_ponto <- data.frame(
      Item = names(ot$ponto),
      Valor = fmt_num(ot$ponto),
      check.names = FALSE
    )

    df_resposta <- data.frame(
      Item = nome_resposta,
      Valor = fmt_num(ot$resposta),
      check.names = FALSE
    )

    df_conv <- data.frame(
      Item = "Convergência",
      Valor = ifelse(isTRUE(ot$convergencia == 0), "sucesso", "falha"),
      check.names = FALSE
    )

    df_valor <- data.frame(
      Item = "Valor otimizado",
      Valor = fmt_num(ot$valor_otimizado),
      check.names = FALSE
    )

    df_obs <- data.frame(
      Item = "Observação",
      Valor = mensagem_otimo_excel(ot, fit, fatores),
      check.names = FALSE
    )

    otimo_df <- rbind(
      df_objetivo,
      df_ponto,
      df_resposta,
      df_conv,
      df_valor,
      df_obs
    )

    openxlsx::addWorksheet(wb, "Ótimo")
    openxlsx::writeData(wb, "Ótimo", otimo_df)
    aplicar_estilo_tabela("Ótimo", otimo_df)
  }

  # =======================
  # PONTO ESTACIONÁRIO
  # =======================
  pe <- NULL

  if (exists("ponto_estacionario_bbd", mode = "function")) {
    pe <- tryCatch(
      ponto_estacionario_bbd(fit),
      error = function(e) NULL
    )
  }

  if (!is.null(pe)) {
    aba_pe <- "Ponto Estacionário"
    openxlsx::addWorksheet(wb, aba_pe)

    openxlsx::writeData(
      wb,
      aba_pe,
      "Ponto estacionário do modelo Box-Behnken",
      startRow = 1,
      startCol = 1
    )

    openxlsx::mergeCells(wb, aba_pe, cols = 1:4, rows = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 1,
      cols = 1:4,
      gridExpand = TRUE,
      stack = TRUE
    )

    status_pe <- if (!is.null(pe$status) && nzchar(pe$status)) {
      pe$status
    } else if (!is.null(pe$convergencia) && isTRUE(pe$convergencia == 0)) {
      "sucesso"
    } else {
      "falha"
    }

    df_resumo_pe <- data.frame(
      Item = c(
        "Classificação do ponto estacionário",
        paste0("Resposta estimada (", nome_resposta, ")"),
        "Status"
      ),
      Valor = c(
        pe$classificacao,
        format(round(pe$resposta_estimada, 4), decimal.mark = ",", nsmall = 4, trim = TRUE),
        status_pe
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    df_ponto_pe <- data.frame(
      Fator = names(pe$ponto),
      Valor = round(as.numeric(pe$ponto[1, ]), 4),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    subs <- c("₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉")
    rotulos_auto <- if (length(pe$autovalores) <= length(subs)) {
      paste0("λ", subs[seq_along(pe$autovalores)])
    } else {
      paste0("λ", seq_along(pe$autovalores))
    }

    df_autovalores_pe <- data.frame(
      Autovalor = rotulos_auto,
      Valor = round(pe$autovalores, 4),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    interpretacao_pe <- data.frame(
      Interpretação = interpretar_autovalores_excel(pe$autovalores),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    df_matriz_B_pe <- as.data.frame(pe$matriz_B, check.names = FALSE)
    df_matriz_B_pe <- cbind(Termo = rownames(df_matriz_B_pe), df_matriz_B_pe, row.names = NULL)
    df_matriz_B_pe[] <- lapply(df_matriz_B_pe, function(col) {
      if (is.numeric(col)) round(col, 4) else col
    })

    openxlsx::writeData(wb, aba_pe, "Resumo", startRow = 3, startCol = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 3,
      cols = 1:2,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::mergeCells(wb, aba_pe, cols = 1:2, rows = 3)
    aplicar_estilo_bloco(aba_pe, df_resumo_pe, startRow = 4, startCol = 1)

    openxlsx::writeData(wb, aba_pe, "Coordenadas codificadas", startRow = 9, startCol = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 9,
      cols = 1:2,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::mergeCells(wb, aba_pe, cols = 1:2, rows = 9)
    aplicar_estilo_bloco(aba_pe, df_ponto_pe, startRow = 10, startCol = 1)

    openxlsx::writeData(wb, aba_pe, "Autovalores da matriz B", startRow = 15, startCol = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 15,
      cols = 1:2,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::mergeCells(wb, aba_pe, cols = 1:2, rows = 15)
    aplicar_estilo_bloco(aba_pe, df_autovalores_pe, startRow = 16, startCol = 1)

    openxlsx::writeData(wb, aba_pe, "Interpretação", startRow = 21, startCol = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 21,
      cols = 1:4,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::mergeCells(wb, aba_pe, cols = 1:4, rows = 21)
    aplicar_estilo_bloco(aba_pe, interpretacao_pe, startRow = 22, startCol = 1)

    openxlsx::writeData(wb, aba_pe, "Matriz B", startRow = 26, startCol = 1)
    openxlsx::addStyle(
      wb = wb,
      sheet = aba_pe,
      style = estilo_titulo,
      rows = 26,
      cols = 1:ncol(df_matriz_B_pe),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::mergeCells(wb, aba_pe, cols = 1:ncol(df_matriz_B_pe), rows = 26)
    aplicar_estilo_bloco(aba_pe, df_matriz_B_pe, startRow = 27, startCol = 1)

    openxlsx::setColWidths(wb, aba_pe, cols = 1:max(4, ncol(df_matriz_B_pe)), widths = "auto")
  }

  arquivos_tmp <- character(0)

  if (isTRUE(incluir_graficos)) {

    if (length(fatores) < 2) {
      stop("São necessários pelo menos dois fatores para exportar superfícies e contornos.")
    }

    # =======================
    # PARETO
    # =======================
    tmp_pareto <- tempfile(fileext = ".png")
    arquivos_tmp <- c(arquivos_tmp, tmp_pareto)

    grDevices::png(tmp_pareto, width = 2200, height = 1400, res = 220)
    graphics::par(cex = 1.35, cex.axis = 1.15, cex.lab = 1.2, cex.main = 1.3)
    pareto_bbd(fit, alpha = alpha)
    grDevices::dev.off()

    openxlsx::addWorksheet(wb, "Pareto")
    openxlsx::writeData(
      wb,
      "Pareto",
      enc2utf8("Gráfico dos Efeitos (Teste F)"),
      startRow = 1,
      startCol = 2
    )
    openxlsx::addStyle(
      wb = wb,
      sheet = "Pareto",
      style = estilo_cabecalho,
      rows = 1,
      cols = 2,
      stack = TRUE
    )
    openxlsx::insertImage(
      wb, "Pareto", tmp_pareto,
      startRow = 3, startCol = 2,
      width = 11, height = 7, units = "in"
    )

    # =======================
    # SUPERFÍCIES E CONTORNOS
    # =======================
    pares <- utils::combn(fatores, 2, simplify = FALSE)

    nome_aba_seguro <- function(prefixo, f1, f2) {
      nome <- paste(prefixo, f1, "x", f2)
      nome <- gsub("[^[:alnum:]_ ]", "_", nome)
      if (nchar(nome) > 31) {
        nome <- substr(nome, 1, 31)
      }
      nome
    }

    for (par_fatores in pares) {

      f1 <- par_fatores[1]
      f2 <- par_fatores[2]

      orientacoes <- list(
        c(f1, f2),
        c(f2, f1)
      )

      for (ori in orientacoes) {

        x_plot <- ori[1]
        y_plot <- ori[2]

        # -----------------------
        # SUPERFÍCIE
        # -----------------------
        tmp_sup <- tempfile(fileext = ".png")
        arquivos_tmp <- c(arquivos_tmp, tmp_sup)

        grDevices::png(tmp_sup, width = 2200, height = 1400, res = 220)
        superficie_bbd(fit, x1 = x_plot, x2 = y_plot)
        grDevices::dev.off()

        aba_sup <- nome_aba_seguro("Superf", x_plot, y_plot)
        openxlsx::addWorksheet(wb, aba_sup)
        openxlsx::writeData(
          wb,
          aba_sup,
          enc2utf8(paste0("Superfície de resposta: ", x_plot, " × ", y_plot)),
          startRow = 1,
          startCol = 2
        )
        openxlsx::addStyle(
          wb = wb,
          sheet = aba_sup,
          style = estilo_cabecalho,
          rows = 1,
          cols = 2,
          stack = TRUE
        )
        openxlsx::insertImage(
          wb, aba_sup, tmp_sup,
          startRow = 3, startCol = 2,
          width = 11, height = 7, units = "in"
        )

        # -----------------------
        # CONTORNO
        # -----------------------
        tmp_cont <- tempfile(fileext = ".png")
        arquivos_tmp <- c(arquivos_tmp, tmp_cont)

        grDevices::png(tmp_cont, width = 2200, height = 1400, res = 220)
        graphics::par(cex = 1.25, cex.axis = 1.1, cex.lab = 1.15, cex.main = 1.2)
        contorno_bbd(
          fit,
          x1 = x_plot,
          x2 = y_plot
        )
        grDevices::dev.off()

        aba_cont <- nome_aba_seguro("Cont", x_plot, y_plot)
        openxlsx::addWorksheet(wb, aba_cont)
        openxlsx::writeData(
          wb,
          aba_cont,
          enc2utf8(paste0("Gráfico de contorno: ", x_plot, " × ", y_plot)),
          startRow = 1,
          startCol = 2
        )
        openxlsx::addStyle(
          wb = wb,
          sheet = aba_cont,
          style = estilo_cabecalho,
          rows = 1,
          cols = 2,
          stack = TRUE
        )
        openxlsx::insertImage(
          wb, aba_cont, tmp_cont,
          startRow = 3, startCol = 2,
          width = 11, height = 7,
          units = "in"
        )
      }
    }
  }

  # =======================
  # SALVAR
  # =======================
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)

  arquivos_tmp <- unique(arquivos_tmp)
  arquivos_tmp <- arquivos_tmp[file.exists(arquivos_tmp)]

  if (length(arquivos_tmp) > 0) {
    unlink(arquivos_tmp, force = TRUE)
  }

  caminho <- normalizePath(arquivo, winslash = "/", mustWork = FALSE)
  message("Arquivo Excel salvo em:\n", caminho)

  invisible(caminho)
}
