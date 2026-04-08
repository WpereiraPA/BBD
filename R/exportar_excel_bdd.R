#' Exporta resultados do BBD para Excel com abas e gráficos
#'
#' @param fit objeto da classe bbd_fit
#' @param arquivo nome do arquivo Excel
#' @param usar_desktop se TRUE, salva na Área de Trabalho, em uma pasta
#'   chamada BBD_Resultados
#' @param alpha nível de significância para destacar efeitos
#' @param fatores vetor opcional com os fatores a considerar nos gráficos;
#'   se NULL, usa fit$fatores
#'
#' @return invisivelmente, o caminho do arquivo gerado
#' @export
exportar_excel_bbd <- function(fit,
                               arquivo = "relatorio_bbd.xlsx",
                               usar_desktop = TRUE,
                               alpha = 0.05,
                               fatores = NULL) {

  if (!inherits(fit, "bbd_fit")) {
    stop("O objeto precisa ser da classe 'bbd_fit'.")
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Instale o pacote 'openxlsx' para usar esta função.")
  }

  if (!is.character(arquivo) || length(arquivo) != 1 || is.na(arquivo) || trimws(arquivo) == "") {
    stop("O argumento 'arquivo' deve ser uma string não vazia.")
  }

  if (!grepl("\\.xlsx$", arquivo, ignore.case = TRUE)) {
    arquivo <- paste0(arquivo, ".xlsx")
  }

  if (is.null(fatores)) {
    fatores <- fit$fatores
  }

  fatores <- as.character(fatores)

  if (length(fatores) < 2) {
    stop("São necessários pelo menos dois fatores para exportar superfícies e contornos.")
  }

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
    x <- gsub(":", "×", x, fixed = TRUE)
    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x
  }

  wb <- openxlsx::createWorkbook()

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

  openxlsx::addWorksheet(wb, enc2utf8("Métricas"))
  openxlsx::writeData(wb, enc2utf8("Métricas"), met_df)
  openxlsx::freezePane(wb, enc2utf8("Métricas"), firstRow = TRUE)
  openxlsx::setColWidths(wb, enc2utf8("Métricas"), cols = 1:ncol(met_df), widths = "auto")

  # =======================
  # ANOVA
  # =======================
  anova_df <- anova_bbd(fit)

  openxlsx::addWorksheet(wb, "ANOVA")
  openxlsx::writeData(wb, "ANOVA", anova_df)
  openxlsx::freezePane(wb, "ANOVA", firstRow = TRUE)
  openxlsx::setColWidths(wb, "ANOVA", cols = 1:ncol(anova_df), widths = "auto")

  # =======================
  # COEFICIENTES
  # =======================
  coef_df <- coeficientes_bbd(fit)
  coef_df$Termo <- formatar_termo(coef_df$Termo)

  openxlsx::addWorksheet(wb, enc2utf8("Coeficientes"))
  openxlsx::writeData(wb, enc2utf8("Coeficientes"), coef_df)
  openxlsx::freezePane(wb, enc2utf8("Coeficientes"), firstRow = TRUE)
  openxlsx::setColWidths(wb, enc2utf8("Coeficientes"), cols = 1:ncol(coef_df), widths = "auto")

  # =======================
  # EFEITOS
  # =======================
  efeitos_df <- tabela_efeitos_bbd(fit, alpha = alpha)
  efeitos_df$Termo <- formatar_termo(efeitos_df$Termo)

  openxlsx::addWorksheet(wb, enc2utf8("Efeitos"))
  openxlsx::writeData(wb, enc2utf8("Efeitos"), efeitos_df)
  openxlsx::freezePane(wb, enc2utf8("Efeitos"), firstRow = TRUE)
  openxlsx::setColWidths(wb, enc2utf8("Efeitos"), cols = 1:ncol(efeitos_df), widths = "auto")

  style_sig <- openxlsx::createStyle(bgFill = "#C6EFCE")
  col_sig <- which(names(efeitos_df) == "Significativo")

  if (length(col_sig) == 1 && nrow(efeitos_df) > 0) {
    col_letra <- openxlsx::int2col(col_sig)

    openxlsx::conditionalFormatting(
      wb,
      sheet = enc2utf8("Efeitos"),
      cols = 1:ncol(efeitos_df),
      rows = 2:(nrow(efeitos_df) + 1),
      rule = paste0("$", col_letra, '2="Sim"'),
      style = style_sig,
      type = "expression"
    )
  }

  # =======================
  # PARETO
  # =======================
  tmp_pareto <- tempfile(fileext = ".png")
  grDevices::png(tmp_pareto, width = 2200, height = 1400, res = 220)
  graphics::par(cex = 1.35, cex.axis = 1.15, cex.lab = 1.2, cex.main = 1.3)
  pareto_bbd(fit)
  grDevices::dev.off()

  openxlsx::addWorksheet(wb, "Pareto")
  openxlsx::writeData(
    wb,
    "Pareto",
    enc2utf8("Gráfico de Pareto dos efeitos"),
    startRow = 1,
    startCol = 2
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
  arquivos_tmp <- character(0)

  nome_aba_seguro <- function(prefixo, f1, f2) {
    nome <- paste(prefixo, f1, "x", f2)
    nome <- gsub("[\\\\/:*?\\[\\]]", "_", nome)
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
      contorno_bbd(fit, x1 = x_plot, x2 = y_plot)
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
      openxlsx::insertImage(
        wb, aba_cont, tmp_cont,
        startRow = 3, startCol = 2,
        width = 11, height = 7, units = "in"
      )
    }
  }

  # =======================
  # SALVAR
  # =======================
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)

  arquivos_tmp <- unique(c(tmp_pareto, arquivos_tmp))
  arquivos_tmp <- arquivos_tmp[file.exists(arquivos_tmp)]

  if (length(arquivos_tmp) > 0) {
    unlink(arquivos_tmp, force = TRUE)
  }

  caminho <- normalizePath(arquivo, winslash = "/", mustWork = FALSE)
  message("Arquivo Excel salvo em:\n", caminho)

  invisible(caminho)
}
