#' Ler dados Box Behnken copiados do Excel
#'
#' @return data.frame
#' @export
read_clipboard_dbb <- function() {
  dados <- utils::read.table(
    "clipboard",
    header = TRUE,
    sep = "\t",
    dec = ",",
    stringsAsFactors = FALSE
  )

  as.data.frame(dados)
}
