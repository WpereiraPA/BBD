interpretar_autovalores_bbd <- function(autovalores, tol = 1e-10, tol_zero = 1e-4) {

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
