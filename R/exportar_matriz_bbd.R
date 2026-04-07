#' Exporta matriz Box-Behnken
#'
#' Exporta a matriz gerada por \code{matriz_bbd()} para arquivo Excel ou CSV.
#'
#' @param matriz Um \code{data.frame} gerado por \code{matriz_bbd()}.
#' @param arquivo Nome do arquivo de saída. Deve terminar em \code{.xlsx} ou \code{.csv}.
#' @param aba Nome da aba, quando o formato for \code{.xlsx}.
#'
#' @return Invisivelmente, o caminho do arquivo exportado.
#' @export
exportar_matriz_bbd <- function(matriz,
                                arquivo = "matriz_bbd.xlsx",
                                aba = "Matriz_BBD") {

  if (missing(matriz) || !is.data.frame(matriz)) {
    stop("O argumento 'matriz' deve ser um data.frame gerado por 'matriz_bbd()'.")
  }

  if (!is.character(arquivo) || length(arquivo) != 1 || is.na(arquivo) || trimws(arquivo) == "") {
    stop("O argumento 'arquivo' deve ser uma string não vazia.")
  }

  ext <- tolower(tools::file_ext(arquivo))

  if (!ext %in% c("xlsx", "csv")) {
    stop("O arquivo deve ter extensão '.xlsx' ou '.csv'.")
  }

  if (ext == "csv") {
    utils::write.csv2(matriz, file = arquivo, row.names = FALSE)

    caminho <- normalizePath(arquivo, winslash = "/", mustWork = FALSE)
    message("Matriz exportada com sucesso em:\n", caminho)
    return(invisible(normalizePath(arquivo, winslash = "/", mustWork = FALSE)))
  }

  if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Para exportar em .xlsx, instale o pacote 'openxlsx'.")
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, aba)
    openxlsx::writeData(wb, sheet = aba, x = matriz)

    estilo_cabecalho <- openxlsx::createStyle(
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      border = "Bottom"
    )

    estilo_corpo <- openxlsx::createStyle(
      halign = "center",
      valign = "center"
    )

    openxlsx::addStyle(
      wb = wb,
      sheet = aba,
      style = estilo_cabecalho,
      rows = 1,
      cols = seq_len(ncol(matriz)),
      gridExpand = TRUE,
      stack = TRUE
    )

    if (nrow(matriz) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = aba,
        style = estilo_corpo,
        rows = 2:(nrow(matriz) + 1),
        cols = seq_len(ncol(matriz)),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::setColWidths(
      wb = wb,
      sheet = aba,
      cols = seq_len(ncol(matriz)),
      widths = "auto"
    )

    openxlsx::freezePane(wb, sheet = aba, firstRow = TRUE)
    openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)

    caminho <- normalizePath(arquivo, winslash = "/", mustWork = FALSE)

    message("Matriz exportada com sucesso em:\n", caminho)
    return(invisible(normalizePath(arquivo, winslash = "/", mustWork = FALSE)))
  }
}
