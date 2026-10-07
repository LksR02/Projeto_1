# Tabelas e gráficos usados nos slides

suppressPackageStartupMessages({
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(gtsummary)
  library(gt)
  library(qqplotr)
})

theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

# Escala divergente: Diminuiu (vermelho) <-> Não alterou (neutro) <-> Aumentou (azul)
CORES_RESPOSTA <- c(
  "Diminuiu"    = "#e34948",
  "Não alterou" = "#85847e",
  "Aumentou"    = "#2a78d6"
)

LIMIAR_RESIDUO <- qnorm(1 - ALFA / 2)  # 1,96

# Destaque das caselas influentes (cores distintas das da resposta)
COR_ACIMA <- "#e4def7"
COR_ABAIXO <- "#fbecc0"

tema_slides <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.background = element_rect(fill = "#fcfcfb", colour = NA),
      panel.grid.major.y = element_line(colour = "#e4e3df", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "#52514e"),
      axis.title = element_text(colour = "#52514e"),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", colour = "#0b0b0b")
    )
}

# Estilo comum das tabelas gt nos slides
estilo_gt <- function(tab, fonte = 17, espaco = 4) {
  tab |>
    tab_options(
      table.font.size = px(fonte),
      column_labels.font.size = px(fonte),
      data_row.padding = px(espaco),
      table.background.color = "#fcfcfb"
    )
}

p_negrito <- function(p) {
  ifelse(p < ALFA, sprintf("<b>%s</b>", formatar_p(p)), formatar_p(p))
}

# Tabela completa como no livro: tbl_summary(by = resposta, percent = "row"),
# add_p (Q = qui-quadrado, F = exato de Fisher), add_stat com o V de Cramér,
# bold_p e destaque das caselas influentes pelos resíduos padronizados
# (> 1,96 com ▲; < -1,96 com ▼). Como no livro, os resíduos só são destacados
# quando a associação global é significativa.
tabela_quali <- function(resultados, dados) {
  vars <- names(resultados)
  siglas <- vapply(resultados, \(r) if (r$teste == "Qui-quadrado") "Q" else "F", "")
  rotulos <- Map(\(r, s) sprintf("%s<sup>%s</sup> (n = %s)", r$rotulo, s, formatar_num(r$n, 0)),
                 resultados, siglas)
  testes <- lapply(siglas, \(s) if (s == "Q") "chisq.test.no.correct" else "fisher.test")
  argumentos <- lapply(siglas[siglas == "F"], \(s) list(simulate.p.value = TRUE, B = 1e5))

  cramer_fun <- function(data, variable, ...) {
    r <- resultados[[variable]]
    tibble(`**V de Cramér**` = sprintf("%s (%s)", formatar_num(r$v, 3), tolower(r$intensidade)))
  }

  # caselas influentes: variável, categoria, coluna da resposta e sinal
  influentes <- do.call(rbind, lapply(resultados, function(r) {
    if (r$p >= ALFA) return(NULL)
    z <- r$residuos
    idx <- which(abs(z) > LIMIAR_RESIDUO, arr.ind = TRUE)
    if (nrow(idx) == 0) return(NULL)
    data.frame(
      variable = r$variavel,
      label = rownames(z)[idx[, 1]],
      coluna = paste0("stat_", idx[, 2]),
      acima = z[idx] > 0
    )
  }))

  set.seed(2026)
  tbl <- dados |>
    select(Consumo_agua, all_of(vars)) |>
    tbl_summary(
      by = Consumo_agua,
      percent = "row",
      missing = "no",
      digits = all_categorical() ~ c(0, 1),
      label = rotulos
    ) |>
    add_p(test = testes, test.args = argumentos,
          pvalue_fun = label_style_pvalue(digits = 3)) |>
    add_stat(fns = everything() ~ cramer_fun) |>
    bold_p(t = ALFA) |>
    modify_spanning_header(all_stat_cols() ~ "**Consumo de água — n (% na linha)**") |>
    modify_header(
      label ~ "**Variáveis**",
      all_stat_cols() ~ "**{level}**",
      p.value ~ "**valor-p**"
    ) |>
    bold_labels() |>
    remove_footnote_header(everything())

  if (!is.null(influentes)) {
    tbl <- tbl |>
      modify_table_body(function(corpo) {
        for (k in seq_len(nrow(influentes))) {
          i <- influentes[k, ]
          linha <- corpo$variable == i$variable & corpo$row_type == "level" &
            corpo$label == i$label
          corpo[[i$coluna]][linha] <- paste(if (i$acima) "▲" else "▼",
                                            corpo[[i$coluna]][linha])
        }
        corpo
      })
  }

  tab <- tbl |>
    as_gt() |>
    fmt_markdown(columns = label) |>
    estilo_gt()

  for (k in seq_len(nrow(influentes %||% data.frame()))) {
    i <- influentes[k, ]
    tab <- tab |>
      tab_style(
        style = list(cell_fill(color = if (i$acima) COR_ACIMA else COR_ABAIXO),
                     cell_text(weight = "bold")),
        locations = cells_body(
          columns = all_of(i$coluna),
          rows = variable == i$variable & row_type == "level" & label == i$label
        )
      )
  }
  tab
}

# Verificação das suposições do qui-quadrado (regra de Cochran)
tabela_esperadas <- function(resultados) {
  tibble(
    Variavel = vapply(resultados, `[[`, "", "rotulo"),
    menor = vapply(resultados, \(r) formatar_num(r$esperada_min, 2), ""),
    pct5 = vapply(resultados, \(r) paste0(formatar_num(r$pct_esperadas_menor5, 0), "%"), ""),
    teste = vapply(resultados, `[[`, "", "teste")
  ) |>
    kbl(format = "html", escape = FALSE, align = c("l", "c", "c", "l"),
        col.names = c("Variável", "Menor freq. esperada", "% caselas com E < 5",
                      "Teste aplicado")) |>
    kable_styling(bootstrap_options = c("condensed", "hover"), full_width = FALSE)
}

# Diagnósticos das suposições da ANOVA para cada variável quantitativa
tabela_diagnosticos <- function(resultados) {
  linhas <- lapply(resultados, function(r) {
    sw <- setNames(r$shapiro$p, as.character(r$shapiro$Consumo_agua))
    data.frame(
      Variavel = r$rotulo,
      d = formatar_p(sw[["Diminuiu"]]),
      n = formatar_p(sw[["Não alterou"]]),
      a = formatar_p(sw[["Aumentou"]]),
      tcl = if (r$amostra_grande) "Sim (n ≥ 30 em todos)" else "Não",
      homog = sprintf("%s: %s", r$homog_teste, p_negrito(r$homog_p)),
      decisao = sprintf("%s + %s", r$teste, r$pos_teste)
    )
  })
  kbl(do.call(rbind, linhas), format = "html", escape = FALSE, row.names = FALSE,
      align = c("l", "c", "c", "c", "c", "c", "l"),
      col.names = c("Variável", names(CORES_RESPOSTA), "TCL", "Homocedasticidade",
                    "Teste escolhido")) |>
    kable_styling(bootstrap_options = c("condensed", "hover"), full_width = FALSE) |>
    add_header_above(c(" " = 1, "Shapiro-Wilk (valor-p)" = 3, " " = 3))
}

# Tabela resumo das quantitativas como no livro: tbl_summary com média (DP),
# add_p com oneway.test (var.equal = TRUE -> ANOVA "A"; FALSE -> Welch "W"),
# η² e letras das comparações múltiplas (Tukey após ANOVA; Games-Howell após Welch).
tabela_quanti <- function(resultados, dados) {
  vars <- names(resultados)
  rotulos <- lapply(resultados, \(r) sprintf("%s<sup>%s</sup>", r$rotulo, r$sigla))
  argumentos <- lapply(resultados, \(r) list(var.equal = r$teste == "ANOVA"))

  extras <- function(data, variable, ...) {
    r <- resultados[[variable]]
    letras <- if (r$p < ALFA) {
      paste(sprintf("%s = %s", names(r$letras), r$letras), collapse = "<br>")
    } else {
      "—"
    }
    tibble(
      `**η²**` = formatar_num(r$eta2, 3),
      `**Comparações Múltiplas**` = letras
    )
  }

  dados |>
    select(Consumo_agua, all_of(vars)) |>
    tbl_summary(
      by = Consumo_agua,
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous2() ~ c("{mean} ({sd})", "{median} [{p25}; {p75}]"),
      digits = list(all_continuous2() ~ 1, Altura ~ 2),
      label = rotulos
    ) |>
    add_p(
      test = everything() ~ "oneway.test",
      test.args = argumentos,
      pvalue_fun = label_style_pvalue(digits = 3)
    ) |>
    add_stat(fns = everything() ~ extras, location = everything() ~ "label") |>
    bold_p(t = ALFA) |>
    modify_spanning_header(all_stat_cols() ~ "**Consumo de água**") |>
    modify_header(
      label ~ "**Variáveis**",
      all_stat_cols() ~ "**{level}**<br>n = {n} ({style_percent(p)}%)",
      p.value ~ "**valor-p**"
    ) |>
    bold_labels() |>
    remove_footnote_header(everything()) |>
    modify_table_body(\(corpo) mutate(
      corpo,
      label = dplyr::case_when(
        row_type == "level" & startsWith(label, "Média") ~ "Média (DP)",
        row_type == "level" & startsWith(label, "Mediana") ~ "Mediana [Q1; Q3]",
        TRUE ~ label
      )
    )) |>
    as_gt() |>
    fmt_markdown(columns = c(label, `**Comparações Múltiplas**`)) |>
    estilo_gt(fonte = 15, espaco = 1)
}

# Gráficos ----------------------------------------------------------------------

grafico_resposta <- function(dados) {
  dados |>
    count(Consumo_agua) |>
    mutate(pct = n / sum(n),
           rotulo = sprintf("%s (%s%%)", formatar_num(n, 0), formatar_num(100 * pct, 1))) |>
    ggplot(aes(Consumo_agua, pct, fill = Consumo_agua)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = rotulo), vjust = -0.5, size = 5.5, colour = "#0b0b0b") +
    scale_fill_manual(values = CORES_RESPOSTA, guide = "none") +
    scale_y_continuous(labels = \(x) paste0(100 * x, "%"),
                       limits = c(0, 0.58), expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "% dos participantes") +
    tema_slides()
}

grafico_cramer <- function(resultados) {
  tibble(
    rotulo = vapply(resultados, `[[`, "", "rotulo"),
    v = vapply(resultados, `[[`, 0, "v"),
    p = vapply(resultados, `[[`, 0, "p")
  ) |>
    mutate(
      significancia = factor(ifelse(p < ALFA, "p < 0,05", "p ≥ 0,05"),
                             levels = c("p < 0,05", "p ≥ 0,05")),
      rotulo = reorder(rotulo, v)
    ) |>
    ggplot(aes(v, rotulo, fill = significancia)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = formatar_num(v, 3)), hjust = -0.15, size = 4.2,
              colour = "#52514e") +
    scale_fill_manual(values = c("p < 0,05" = "#2a78d6", "p ≥ 0,05" = "#c3c2b7")) +
    scale_x_continuous(limits = c(0, 0.165), expand = expansion(mult = c(0, 0))) +
    labs(x = "V de Cramér", y = NULL) +
    tema_slides() +
    theme(panel.grid.major.x = element_line(colour = "#e4e3df", linewidth = 0.3),
          panel.grid.major.y = element_blank())
}

dados_longos <- function(dados) {
  dados |>
    select(Consumo_agua, all_of(VARS_QUANTI)) |>
    pivot_longer(-Consumo_agua, names_to = "variavel", values_to = "valor") |>
    mutate(variavel = factor(ROTULOS[variavel], levels = ROTULOS[VARS_QUANTI]))
}

# Diagnóstico de normalidade: densidade por grupo
grafico_densidades <- function(dados) {
  dados_longos(dados) |>
    ggplot(aes(valor, colour = Consumo_agua, fill = Consumo_agua)) +
    geom_density(alpha = 0.12, linewidth = 0.8) +
    facet_wrap(~variavel, scales = "free", nrow = 1) +
    scale_fill_manual(values = CORES_RESPOSTA) +
    scale_colour_manual(values = CORES_RESPOSTA) +
    labs(x = NULL, y = "Densidade") +
    tema_slides()
}

# Diagnóstico de normalidade: gráfico quantil-quantil por grupo
grafico_qq <- function(dados) {
  dados_longos(dados) |>
    ggplot(aes(sample = valor, colour = Consumo_agua, fill = Consumo_agua)) +
    qqplotr::stat_qq_band(alpha = 0.2, colour = NA) +
    qqplotr::stat_qq_line(colour = "#0b0b0b", linewidth = 0.5) +
    qqplotr::stat_qq_point(size = 0.6, alpha = 0.5) +
    scale_fill_manual(values = CORES_RESPOSTA, guide = "none") +
    facet_wrap(Consumo_agua ~ variavel, scales = "free", ncol = 4,
               labeller = labeller(.multi_line = FALSE)) +
    scale_colour_manual(values = CORES_RESPOSTA, guide = "none") +
    labs(x = "Quantis teóricos (normal)", y = "Quantis amostrais") +
    tema_slides() +
    theme(strip.text.y = element_text(size = 11))
}

grafico_boxplots <- function(dados) {
  dados |>
    select(Consumo_agua, all_of(VARS_QUANTI)) |>
    pivot_longer(-Consumo_agua, names_to = "variavel", values_to = "valor") |>
    mutate(variavel = factor(ROTULOS[variavel], levels = ROTULOS[VARS_QUANTI])) |>
    ggplot(aes(Consumo_agua, valor, fill = Consumo_agua, colour = Consumo_agua)) +
    geom_boxplot(width = 0.55, alpha = 0.25, outlier.size = 0.8, linewidth = 0.6) +
    facet_wrap(~variavel, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = CORES_RESPOSTA, guide = "none") +
    scale_colour_manual(values = CORES_RESPOSTA, guide = "none") +
    labs(x = NULL, y = NULL) +
    tema_slides() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}
