#' Ler dados do clipboard
#'
#' Importa dados copiados do Excel para o R.
#'
#' @return data.frame com os dados importados
#' @export
ler_clipboard_bbd <- function() {
  dados <- utils::read.table(
    file = "clipboard",
    header = TRUE,
    sep = "\t",
    dec = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  return(dados)
}
