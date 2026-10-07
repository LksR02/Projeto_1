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
    teste <- suppressWarnings(chisq.test(tab, correct = FALSE))
    list(teste = "Qui-quadrado", p = teste$p.value,
         estatistica = unname(teste$statistic), gl = unname(teste$parameter))
  } else {
    set.seed(2026)
    teste <- fisher.test(tab, simulate.p.value = TRUE, B = 1e5)
    list(teste = "Exato de Fisher", p = teste$p.value,
         estatistica = NA_real_, gl = NA_real_)
  }
}

# Classificação do V de Cramér adotada na disciplina (cap. 4 do livro):
# 0-0,10 muito fraca; 0,10-0,30 fraca; 0,30-0,50 moderada; > 0,50 forte.
interpretar_v <- function(v) {
  cut(v, c(-Inf, 0.10, 0.30, 0.50, Inf), right = FALSE,
      labels = c("Muito fraca", "Fraca", "Moderada", "Forte")) |>
    as.character()
}

# Tabela de contingência completa entre uma explicativa (linhas) e a resposta
# (colunas), com n (% por linha), frequências esperadas e resíduos padronizados
# (chisq.test()$stdres, como no livro).
analisar_quali <- function(dados, var) {
  d <- dados |> filter(!is.na(.data[[var]]))
  tab <- table(d[[var]], d$Consumo_agua)
  res <- testar_associacao(tab)
  qui <- suppressWarnings(chisq.test(tab, correct = FALSE))
  residuos <- qui$stdres
  v <- unname(suppressWarnings(cramer_v(tab)))

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
    esperadas = qui$expected,
    esperada_min = min(qui$expected),
    pct_esperadas_menor5 = 100 * mean(qui$expected < 5),
    residuos = residuos,
    teste = res$teste,
    estatistica = res$estatistica,
    gl = res$gl,
    p = res$p,
    v = v,
    intensidade = interpretar_v(v)
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

# Tamanho mínimo por grupo a partir do qual o Teorema Central do Limite garante
# normalidade aproximada da média amostral (cap. 8 do livro: n ≳ 30).
N_AMOSTRA_GRANDE <- 30

# Letras de comparações múltiplas: grupos que compartilham uma letra não
# diferem significativamente. Grupos ordenados da maior para a menor média.
letras_comparacoes <- function(medias, comparacoes) {
  grupos <- names(sort(medias, decreasing = TRUE))
  sig <- comparacoes |> filter(p.adj < ALFA)
  conjuntos <- list(grupos)
  for (k in seq_len(nrow(sig))) {
    par <- c(sig$group1[k], sig$group2[k])
    novos <- list()
    for (conj in conjuntos) {
      if (all(par %in% conj)) {
        novos <- c(novos, list(setdiff(conj, par[1]), setdiff(conj, par[2])))
      } else {
        novos <- c(novos, list(conj))
      }
    }
    # remove conjuntos contidos em outros (absorção)
    contido <- vapply(seq_along(novos), function(i) {
      any(vapply(seq_along(novos), function(j) {
        i != j && all(novos[[i]] %in% novos[[j]]) &&
          (length(novos[[i]]) < length(novos[[j]]) || i > j)
      }, logical(1)))
    }, logical(1))
    conjuntos <- novos[!contido]
  }
  # letras na ordem em que aparecem para o grupo de maior média
  ordem <- order(vapply(conjuntos, \(conj) min(match(conj, grupos)), numeric(1)))
  conjuntos <- conjuntos[ordem]
  vapply(grupos, function(g) {
    paste(letters[which(vapply(conjuntos, \(conj) g %in% conj, logical(1)))], collapse = "")
  }, character(1))[names(medias)]
}

# Comparação de uma variável quantitativa entre as categorias da resposta,
# seguindo o roteiro do livro (caps. 7 e 8) e da Atividade 5:
# 1. normalidade por grupo: Shapiro-Wilk (+ densidade e Q-Q plot nos slides);
#    com todos os grupos com n ≥ 30, o TCL dispensa a normalidade dos dados;
# 2. homocedasticidade: Bartlett se os dados forem normais, Levene caso contrário;
# 3. variâncias homogêneas -> ANOVA (oneway.test, var.equal = TRUE) + Tukey;
#    variâncias heterogêneas -> ANOVA de Welch (var.equal = FALSE) + Games-Howell.
analisar_quanti <- function(dados, var) {
  form <- reformulate("Consumo_agua", response = var)

  normalidade <- dados |>
    group_by(Consumo_agua) |>
    shapiro_test(vars = var)
  normal <- all(normalidade$p >= ALFA)
  amostra_grande <- all(table(dados$Consumo_agua) >= N_AMOSTRA_GRANDE)
  if (!normal && !amostra_grande) {
    stop("Grupos pequenos e sem normalidade: a ANOVA não é adequada para ", var)
  }

  if (normal) {
    homog_teste <- "Bartlett"
    homog_p <- bartlett.test(form, data = dados)$p.value
  } else {
    homog_teste <- "Levene"
    homog_p <- leveneTest(form, data = dados, center = median)$`Pr(>F)`[1]
  }
  homog <- homog_p >= ALFA

  modelo <- aov(form, data = dados)
  somas <- summary(modelo)[[1]]$`Sum Sq`
  eta2 <- somas[1] / sum(somas)

  if (homog) {
    teste <- oneway.test(form, data = dados, var.equal = TRUE)
    tukey <- TukeyHSD(modelo)$Consumo_agua
    pares <- do.call(rbind, strsplit(rownames(tukey), "-"))
    comparacoes <- tibble(group1 = pares[, 2], group2 = pares[, 1],
                          p.adj = tukey[, "p adj"])
    res <- list(teste = "ANOVA", sigla = "A", pos_teste = "Tukey")
  } else {
    teste <- oneway.test(form, data = dados, var.equal = FALSE)
    comparacoes <- games_howell_test(dados, form) |> select(group1, group2, p.adj)
    res <- list(teste = "ANOVA de Welch", sigla = "W", pos_teste = "Games-Howell")
  }

  resumo <- resumo_quanti(dados, var)
  medias <- setNames(resumo$media, as.character(resumo$Consumo_agua))

  c(
    list(
      variavel = var,
      rotulo = ROTULOS[[var]],
      resumo = resumo,
      shapiro = normalidade,
      normal = normal,
      amostra_grande = amostra_grande,
      homog_teste = homog_teste,
      homog_p = homog_p,
      estatistica = unname(teste$statistic),
      gl = unname(teste$parameter),
      p = teste$p.value,
      eta2 = eta2,
      comparacoes = comparacoes,
      letras = letras_comparacoes(medias, comparacoes)
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
