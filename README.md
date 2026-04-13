# BBD
Pacote em R para geração, análise e visualização de planejamentos Box-Behnken.

---

## 📌 O que o pacote faz

O pacote BBD foi desenvolvido para facilitar a aplicação prática de planejamentos Box-Behnken, integrando todas as etapas do experimento em um único fluxo.

Com ele, você pode:

- gerar a matriz experimental
- exportar a matriz para Excel
- importar dados diretamente do Excel
- ajustar modelos quadráticos de superfície de resposta
- realizar análise de variância (ANOVA)
- calcular coeficientes e efeitos
- gerar gráficos:
  - Pareto
  - superfície de resposta
  - contorno
- exportar relatório automático
- exportar resultados completos para Excel

---

## ⚙️ Instalação

```r
install.packages("remotes")
remotes::install_github("WpereiraPA/BBD")
library(BBD)
```

## 🚀 Fluxo de uso

### 1. Gerar matriz experimental

```r
m <- matriz_bbd(k = 3)
exportar_matriz_bbd(m)
```

### 2. Importar dados do Excel

```r
dados <- read_clipboard_bbd()
```

### 3. Ajustar o modelo

```r
fit <- bbd_fit(dados, resposta = "nome da sua resposta")

# exemplo

fit <- bbd_fit(dados, resposta = "Rendimento")

```

### 4. Análise
---
```r
anova_bbd(fit)
coeficientes_bbd(fit)
tabela_efeitos_bbd(fit)
```
### 5. Gráficos

```r
pareto_bbd(fit)
superficie_bbd(fit, "A", "B")
contorno_bbd(fit, "A", "B")
```
---
### 6. Exportação completa

```r
exportar_excel_bbd(fit)
exportar_relatorio_bbd(fit)
```
---

## 📊 Gráficos

### Pareto
Identifica os efeitos mais importantes no modelo.

### Superfície de resposta
Mostra o comportamento da variável resposta em função de dois fatores.

### Contorno
Representação bidimensional da superfície.

### Pontos experimentais no contorno

Os quadrados representam os pontos experimentais utilizados no planejamento.

Para exibir:

```r
contorno_bbd(fit, "A", "B", mostrar_pontos = TRUE)
```

##  Sobre o Box-Behnken
---

O planejamento Box-Behnken:

- utiliza níveis codificados (-1, 0, +1)
- não possui pontos axiais
- concentra os experimentos na região central
- é eficiente para ajuste de modelos quadráticos

---
##  Exportação de resultados

---

O pacote gera automaticamente:

### Excel
- métricas
- ANOVA
- coeficientes
- efeitos
- gráficos

### Relatório
- resumo estatístico
- interpretação básica
- equação do modelo
- (opcional) ponto ótimo

---
## Authors

- Augusto Henrique de Sousa Xavier (augustohpa12@gmail.com)
- Wanderley Xavier Pereira (wander.wx@gmail.com)

## Copyright and institutional context

Copyright is shared by:

- Augusto Henrique de Sousa Xavier
- Wanderley Xavier Pereira
- Centro Federal de Educacao Tecnologica de Minas Gerais (CEFET-MG)

## Development notes

This package was developed by the authors with support from artificial intelligence tools for code structuring, review and refinement. All methodological definitions, statistical logic and final implementation decisions are the responsibility of the authors.

## Citation and authorship

If you use this package in academic, technical or derived work, please cite the original authorship of the BBD package.

Citation of the original package is strongly encouraged in cases of use, modification, adaptation or extension.

## Institutional support

The development of this package was carried out in an academic context with institutional support from the Centro Federal de Educacao Tecnologica de Minas Gerais (CEFET-MG).

##  Status

Pacote em desenvolvimento contínuo com foco em aplicação prática e uso didático.
