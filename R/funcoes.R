# Funções de preparo dos dados e de análise do Projeto I
# Variável resposta: Consumo_água

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(rstatix)
  library(car)
})

# Nível de significância adotado em todo o projeto
ALFA <- 0.05

# Rótulos legíveis das variáveis explicativas
ROTULOS <- c(
  Genero                 = "Gênero",
  Raca_cor               = "Raça/cor",
  Regiao                 = "Região",
  Isolamento             = "Isolamento social",
  Trabalha               = "Trabalha",
  Renda_familiar         = "Renda familiar",
  Escolaridade           = "Escolaridade",
  Covid                  = "Diagnóstico de COVID-19",
  Consulta_Nutricionista = "Consulta com nutricionista",
  Dificuldade_Financeira = "Dificuldade financeira",
  cigarro                = "Fuma",
  Ansiedade              = "Ansiedade",
  Depressao              = "Depressão",
  Insonia                = "Insônia",
  Enxaqueca              = "Enxaqueca",
  Idade                  = "Idade (anos)",
  Peso                   = "Peso (kg)",
  Altura                 = "Altura (m)",
  IMC                    = "IMC (kg/m²)"
)

VARS_QUALI <- c(
  "Genero", "Raca_cor", "Regiao", "Isolamento", "Trabalha", "Renda_familiar",
  "Escolaridade", "Covid", "Consulta_Nutricionista", "Dificuldade_Financeira",
  "cigarro", "Ansiedade", "Depressao", "Insonia", "Enxaqueca"
)
VARS_QUANTI <- c("Idade", "Peso", "Altura", "IMC")

NIVEIS_RESPOSTA <- c("Diminuiu", "Não alterou", "Aumentou")

# Leitura e recodificação --------------------------------------------------
# - "Prefiro não responder/declarar" e "Não desejo informar" viram NA
#   (não resposta), e esses casos saem apenas da análise da variável em questão;
# - categorias muito raras são agrupadas para evitar frequências esperadas
#   muito baixas (Raça/cor e Escolaridade);
# - Gênero "Outro" (n = 6) não pode ser agrupado de forma substantiva com
#   nenhuma outra categoria e é tratado como NA na análise de Gênero.
ler_dados <- function(caminho = "data/Nutricao.xlsx") {
  read_excel(caminho) |>
    mutate(
      Consumo_agua = factor(`Consumo_água`, levels = NIVEIS_RESPOSTA),
      Genero = factor(
        na_if(na_if(Genero, "Prefiro não responder"), "Outro"),
        levels = c("Feminino", "Masculino")
      ),
      Raca_cor = case_when(
        Raca_cor == "Prefiro não declarar" ~ NA_character_,
        Raca_cor %in% c("Amarela", "Indígena", "Outro") ~ "Amarela/Indígena/Outra",
        TRUE ~ Raca_cor
      ) |>
        factor(levels = c("Branca", "Parda", "Preta", "Amarela/Indígena/Outra")),
      Regiao = dplyr::recode(Regiao, "Centro-oeste" = "Centro-Oeste") |>
        factor(levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")),
      Renda_familiar = dplyr::recode(
        Renda_familiar,
        "até R$ 1254,00"            = "Até R$ 1.254",
        "entre R$ 1.255 - R$ 8.640" = "R$ 1.255 a R$ 8.640",
        "mais de R$ 8.640"          = "Mais de R$ 8.640"
      ) |>
        factor(levels = c("Até R$ 1.254", "R$ 1.255 a R$ 8.640", "Mais de R$ 8.640")),
      Escolaridade = case_when(
        Escolaridade == "Não desejo informar" ~ NA_character_,
        Escolaridade %in% c("Analfabeto", "Ensino Fundamental Completo",
                            "Ensino Médio Completo") ~ "Até Ensino Médio",
        TRUE ~ Escolaridade
      ) |>
        factor(levels = c("Até Ensino Médio", "Ensino Superior Completo",
                          "Pós-graduação")),
      across(
        c(Isolamento, Trabalha, Covid, Consulta_Nutricionista,
          Dificuldade_Financeira, cigarro, Ansiedade, Depressao, Insonia,
          Enxaqueca),
        \(x) factor(x, levels = c("Não", "Sim"))
      )
    )
}

# Variáveis qualitativas -----------------------------------------------------

# Escolhe o teste pela regra de Cochran: qui-quadrado se nenhuma frequência
# esperada for < 1 e no máximo 20% delas forem < 5; caso contrário, Fisher
# (com valor-p por simulação de Monte Carlo quando a tabela é maior que 2x2).
testar_associacao <- function(tab) {
  esperadas <- suppressWarnings(chisq.test(tab, correct = FALSE))$expected
  usa_qui <- all(esperadas >= 1) && mean(esperadas < 5) <= 0.20
  if (usa_qui) {
    teste <- chisq.test(tab, correct = FALSE)
    list(teste = "Qui-quadrado", p = teste$p.value,
         estatistica = unname(teste$statistic), gl = unname(teste$parameter))
  } else {
    set.seed(2026)
    teste <- fisher.test(tab, simulate.p.value = TRUE, B = 1e5)
    list(teste = "Exato de Fisher", p = teste$p.value,
         estatistica = NA_real_, gl = NA_real_)
  }
}

# V de Cramér com interpretação de Cohen (1988), que depende dos graus de
# liberdade mínimos: gl* = min(linhas, colunas) - 1.
interpretar_v <- function(v, gl_min) {
  limites <- switch(as.character(min(gl_min, 3)),
    "1" = c(0.10, 0.30, 0.50),
    "2" = c(0.07, 0.21, 0.35),
    "3" = c(0.06, 0.17, 0.29)
  )
  cut(v, c(-Inf, limites, Inf), right = FALSE,
      labels = c("Desprezível", "Fraca", "Moderada", "Forte")) |>
    as.character()
}

# Tabela de contingência completa entre uma explicativa (linhas) e a resposta
# (colunas), com n (% por linha) e resíduos padronizados ajustados.
analisar_quali <- function(dados, var) {
  d <- dados |> filter(!is.na(.data[[var]]))
  tab <- table(d[[var]], d$Consumo_agua)
  res <- testar_associacao(tab)
  residuos <- suppressWarnings(chisq.test(tab, correct = FALSE))$stdres
  v <- unname(suppressWarnings(cramer_v(tab)))
  gl_min <- min(dim(tab)) - 1

  pct <- prop.table(tab, 1) * 100
  celulas <- matrix(
    sprintf("%d (%s%%)", tab, formatar_num(pct, 1)),
    nrow = nrow(tab), dimnames = dimnames(tab)
  )

  list(
    variavel = var,
    rotulo = ROTULOS[[var]],
    n = sum(tab),
    tabela = tab,
    celulas = celulas,
    residuos = residuos,
    teste = res$teste,
    estatistica = res$estatistica,
    gl = res$gl,
    p = res$p,
    v = v,
    intensidade = interpretar_v(v, gl_min)
  )
}

# Variáveis quantitativas ----------------------------------------------------

resumo_quanti <- function(dados, var) {
  dados |>
    group_by(Consumo_agua) |>
    summarise(
      n = n(),
      media = mean(.data[[var]]),
      dp = sd(.data[[var]]),
      mediana = median(.data[[var]]),
      q1 = quantile(.data[[var]], 0.25),
      q3 = quantile(.data[[var]], 0.75),
      .groups = "drop"
    )
}

# Escolha do teste para comparar a variável quantitativa entre as categorias
# da resposta:
# - normalidade em todos os grupos (Shapiro-Wilk) e variâncias homogêneas
#   (Levene)  -> ANOVA + Tukey;
# - normalidade, mas variâncias heterogêneas -> ANOVA de Welch + Games-Howell;
# - sem normalidade -> Kruskal-Wallis + Dunn (ajuste de Holm).
analisar_quanti <- function(dados, var) {
  form <- reformulate("Consumo_agua", response = var)

  normalidade <- dados |>
    group_by(Consumo_agua) |>
    shapiro_test(vars = var)
  normal <- all(normalidade$p >= ALFA)
  levene <- leveneTest(form, data = dados, center = median)
  homog <- levene$`Pr(>F)`[1] >= ALFA

  if (normal && homog) {
    teste <- anova_test(dados, form)
    tukey <- tukey_hsd(dados, form)
    res <- list(
      teste = "ANOVA", p = teste$p, efeito = teste$ges,
      medida_efeito = "η² generalizado",
      comparacoes = tukey |> select(group1, group2, p.adj)
    )
  } else if (normal) {
    teste <- welch_anova_test(dados, form)
    gh <- games_howell_test(dados, form)
    res <- list(
      teste = "ANOVA de Welch", p = teste$p, efeito = NA_real_,
      medida_efeito = NA_character_,
      comparacoes = gh |> select(group1, group2, p.adj)
    )
  } else {
    teste <- kruskal_test(dados, form)
    efeito <- kruskal_effsize(dados, form)
    dunn <- dunn_test(dados, form, p.adjust.method = "holm")
    res <- list(
      teste = "Kruskal-Wallis", p = teste$p, efeito = efeito$effsize,
      medida_efeito = "η²[H]",
      comparacoes = dunn |> select(group1, group2, p.adj)
    )
  }

  c(
    list(
      variavel = var,
      rotulo = ROTULOS[[var]],
      resumo = resumo_quanti(dados, var),
      shapiro_p_min = min(normalidade$p),
      levene_p = levene$`Pr(>F)`[1]
    ),
    res
  )
}

# Formatação -------------------------------------------------------------------

formatar_p <- function(p) {
  ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 3, decimal.mark = ","))
}

formatar_num <- function(x, digitos = 1) {
  formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = ".")
}
