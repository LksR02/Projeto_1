# Projeto I — Mudanças no consumo de água durante a pandemia de COVID-19

Apresentação em slides (Quarto/revealjs) da **Atividade Avaliativa 6**: análise da associação entre a
variável resposta sorteada, **`Consumo_água`**, e as variáveis explicativas do banco `Nutricao.xlsx`
(2.155 adultos, questionário online de 2022, pós-graduação em Nutrição da UFF).

A apresentação tem **10 slides no total**, com roteiro nas notas para **11 minutos de fala e um
minuto de margem**, dentro do limite de 12 minutos. A capa foi integrada à introdução, sem
divisórias, apêndice ou slide de agradecimento adicional. As tabelas cobrem as 19 explicativas;
o exemplo de frutas está identificado como análise exploratória complementar.

Na apresentação, use as setas do teclado. Pressione **S** para consultar as notas do apresentador,
com o tempo e a fala sugerida para cada slide. O detalhamento da investigação permanece no
relatório local de estudo. A versão pública usa o visualizador padrão do Quarto/revealjs.

## Estrutura

| Arquivo | Conteúdo |
|---|---|
| `index.qmd` | Slides: introdução, metodologia, resultados e conclusão |
| `R/funcoes.R` | Leitura/recodificação dos dados e funções de análise (frequências esperadas, qui-quadrado/Fisher, resíduos padronizados, V de Cramér; Shapiro-Wilk, Bartlett/Levene, ANOVA + Tukey ou ANOVA de Welch + Games-Howell) |
| `R/apresentacao.R` | Tabelas (`kableExtra`) e gráficos (`ggplot2`) dos slides |
| `styles.scss` | Tema dos slides |
| `docs/index.html` | Apresentação renderizada (publicada pelo GitHub Pages) |

## Como reproduzir

1. Coloque o arquivo do Google Classroom em `data/Nutricao.xlsx`. A pasta `data/` não é versionada,
   pois os dados são da disciplina e o repositório é público.
2. Instale os pacotes do R:
   ```r
   install.packages(c("readxl", "dplyr", "tidyr", "rstatix", "car", "ggplot2", "knitr", "kableExtra", "gtsummary", "gt", "qqplotr"))
   ```
3. Renderize na raiz do projeto:
   ```bash
   quarto render
   ```
   A saída vai para `docs/index.html`.

## Publicação

Endereço público: <https://lksr02.github.io/Projeto_1/>.

No GitHub: *Settings → Pages → Deploy from a branch*. A branch atual do projeto é
`claude/peaceful-gauss-ukmx3h`; selecione a pasta `/docs`. Depois de enviar o HTML renderizado,
aguarde o build do Pages e verifique o endereço público antes de colar o link no Google Classroom.

## Investigação para preparação oral

Com a planilha em `data/Nutricao.xlsx`, execute na raiz:

```bash
Rscript R/estudo_dados.R
python3 R/gerar_estudo.py
```

A investigação utiliza os pacotes analíticos já listados, além de `jsonlite`. O gerador HTML usa
apenas a biblioteca padrão do Python. As análises estatísticas são realizadas em R.

O relatório local `data/estudo/estudo_completo.html` reúne o perfil das 38 variáveis, os 19 testes
principais sobre água, 18 cruzamentos com os demais hábitos, 12 relações complementares entre
explicativas, cinco comparações de idade e uma sensibilidade às linhas idênticas. As famílias
de comparações são identificadas; os cruzamentos adicionais são exploratórios.

O relatório e os resultados ficam na pasta `data/`, ignorada pelo Git. Eles não alteram os slides
publicados nem são incluídos automaticamente no GitHub Pages. A planilha original é preservada.
