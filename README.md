# BBD
Pacote em R para geração, análise e visualização de planejamentos Box-Behnken.

---

## O que o pacote faz

O pacote BBD foi desenvolvido para facilitar a aplicação prática de planejamentos Box-Behnken, integrando todas as etapas do experimento em um único fluxo.

Com ele, você pode:

- gerar a matriz experimental  
- exportar a matriz para Excel  
- importar dados diretamente do Excel  
- ajustar modelos quadráticos de superfície de resposta  
- realizar análise de variância (ANOVA)  
- calcular coeficientes e efeitos  
- identificar ponto ótimo (máximo ou mínimo)  
- calcular o ponto estacionário da superfície de resposta  
- classificar o tipo de ponto (máximo, mínimo ou sela)  
- gerar gráficos:
  - Pareto  
  - superfície de resposta  
  - contorno  
- exportar resultados para Excel (versão rápida e completa)  

---

## Instalação

```r
install.packages("remotes")
remotes::install_github("WpereiraPA/BBD")
library(BBD)
```

---

## Fluxo de uso

### 1. Gerar matriz experimental

```r
m <- matriz_bbd(k = 3)
exportar_matriz_bbd(m)
```

---

### 2. Importar dados do Excel

```r
dados <- read_clipboard_bbd()
```

---

### 3. Ajustar o modelo

```r
fit <- bbd_fit(dados, resposta = "Rendimento")
```

---

### 4. Análise

```r
anova_bbd(fit)
coeficientes_bbd(fit)
tabela_efeitos_bbd(fit)
```

---

### 5. Ponto ótimo

```r
# Maximizar (padrão)
otimo_bbd(fit, objetivo = "max")

# Minimizar
otimo_bbd(fit, objetivo = "min")
```

---

### 6. Ponto estacionário

```r
ponto_estacionario_bbd(fit)
```

Retorna:

- coordenadas do ponto  
- classificação (máximo, mínimo ou sela)  
- autovalores  
- resposta estimada  

📌 Observação:

- O ponto estacionário é obtido analiticamente a partir do modelo  
- O ponto ótimo é obtido por otimização numérica  
- Nem sempre coincidem

---

### 7. Avaliação do ponto estacionário em relação ao objetivo

```r
avaliar_ponto_estacionario_bbd(fit, objetivo = "max")
avaliar_ponto_estacionario_bbd(fit, objetivo = "min")

---

### 8. Gráficos

```r
pareto_bbd(fit)
superficie_bbd(fit, "A", "B")
contorno_bbd(fit, "A", "B")
```

---

## Exportação para Excel

### Exportação rápida

```r
exportar_excel_bbd(fit)
```

Inclui:

- Dados  
- Métricas  
- ANOVA  
- Coeficientes  
- Efeitos  
- Ponto ótimo  
- Ponto estacionário  

---

### Exportação completa

```r
exportar_excel_completo_bbd(fit)
```

Inclui tudo da versão anterior, além de:

- Gráfico de Pareto  
- Superfícies de resposta  
- Gráficos de contorno  

---

## Sobre o Box-Behnken

O planejamento Box-Behnken:

- utiliza níveis codificados (-1, 0, +1)  
- não possui pontos axiais  
- concentra os experimentos na região central  
- é eficiente para ajuste de modelos quadráticos  
- permite análise completa de superfícies de resposta  

---

## Authors

- Augusto Henrique de Sousa Xavier  
- Wanderley Xavier Pereira  

---

## Copyright and institutional context

Copyright is shared by:

- Augusto Henrique de Sousa Xavier
- Wanderley Xavier Pereira
- Centro Federal de Educação Tecnológica de Minas Gerais (CEFET-MG)

---
## Development notes

This package was developed by the authors with support from artificial intelligence tools for code structuring, review and refinement.

---
## Citation and authorship

If you use this package in academic, technical or derived work, please cite the original authorship of the BBD package.

Citation of the original package is strongly encouraged in cases of use, modification, adaptation or extension.

---

## Institutional support

The development of this package was carried out in an academic context with institutional support from the Centro Federal de Educação Tecnológica de Minas Gerais (CEFET-MG).

---

## Status

Pacote em desenvolvimento contínuo com foco em aplicação prática e uso didático.
