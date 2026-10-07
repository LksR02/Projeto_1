# Projeto I — Mudanças no consumo de água durante a pandemia de COVID-19

Apresentação em slides (Quarto/revealjs) da **Atividade Avaliativa 6**: análise da associação entre a
variável resposta sorteada, **`Consumo_água`**, e as variáveis explicativas do banco `Nutricao.xlsx`
(2.155 adultos, questionário online de 2022, pós-graduação em Nutrição da UFF).

## Estrutura

| Arquivo | Conteúdo |
|---|---|
| `index.qmd` | Slides: introdução, metodologia, resultados e conclusão |
| `R/funcoes.R` | Leitura/recodificação dos dados e funções de análise (qui-quadrado/Fisher, V de Cramér, resíduos, Kruskal-Wallis etc.) |
| `R/apresentacao.R` | Tabelas (`kableExtra`) e gráficos (`ggplot2`) dos slides |
| `styles.scss` | Tema dos slides |
| `docs/index.html` | Apresentação renderizada (publicada pelo GitHub Pages) |

## Como reproduzir

1. Coloque o arquivo do Google Classroom em `data/Nutricao.xlsx`. A pasta `data/` não é versionada,
   pois os dados são da disciplina e o repositório é público.
2. Instale os pacotes do R:
   ```r
   install.packages(c("readxl", "dplyr", "tidyr", "rstatix", "car", "ggplot2", "knitr", "kableExtra"))
   ```
3. Renderize na raiz do projeto:
   ```bash
   quarto render
   ```
   A saída vai para `docs/index.html`.

## Publicação

No GitHub: *Settings → Pages → Deploy from a branch → `main` / pasta `/docs`*. Depois, cole o link
gerado no Google Classroom.
