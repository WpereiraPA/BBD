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
```
## 🚀 Fluxo de uso

### 1. Gerar matriz experimental

```r
m <- matriz_bbd(k = 3)
exportar_matriz_bbd(m)
```
### 2. Importar dados do Excel

```r
dados <- ler_clipboard_bbd()
```

### 3. Ajustar o modelo

```r
fit <- bbd_fit(dados, resposta = "Rend")
```
### 4. Análise
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

### 6. Exportação completa
```r
exportar_excel_bbd(fit)
exportar_relatorio_bbd(fit)
```

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

O planejamento Box-Behnken:

- utiliza níveis codificados (-1, 0, +1)
- não possui pontos axiais
- concentra os experimentos na região central
- é eficiente para ajuste de modelos quadráticos
---
##  Exportação de resultados

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
## 👤 Autoria

Desenvolvido por Wanderley Xavier Pereira.
---
## 🏛️ Titularidade

Titularidade compartilhada entre:
- Wanderley Xavier Pereira  
- Centro Federal de Educação Tecnológica de Minas Gerais (CEFET-MG)
---
##  Apoio institucional

O desenvolvimento deste pacote contou com apoio institucional do  
Centro Federal de Educação Tecnológica de Minas Gerais (CEFET-MG),  
no âmbito das atividades acadêmicas do autor, sem financiamento específico.

---

##  Status

Pacote em desenvolvimento contínuo com foco em aplicação prática e uso didático.
