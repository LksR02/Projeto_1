# Tabelas e gráficos usados nos slides

suppressPackageStartupMessages({
  library(knitr)
  library(kableExtra)
  library(ggplot2)
})

# Escala divergente: Diminuiu (vermelho) <-> Não alterou (neutro) <-> Aumentou (azul)
CORES_RESPOSTA <- c(
  "Diminuiu"    = "#e34948",
  "Não alterou" = "#85847e",
  "Aumentou"    = "#2a78d6"
)

LIMIAR_RESIDUO <- qnorm(1 - ALFA / 2)  # 1,96

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

# Texto do teste, por exemplo "Qui-quadrado: χ²(2) = 41,33; p < 0,001; V = 0,138 (fraca)"
descrever_teste <- function(r) {
  p_txt <- formatar_p(r$p)
  p_txt <- if (startsWith(p_txt, "<")) paste("p", p_txt) else paste("p =", p_txt)
  estat <- if (r$teste == "Qui-quadrado") {
    sprintf("χ²(%d) = %s; ", as.integer(r$gl), formatar_num(r$estatistica, 2))
  } else {
    ""
  }
  sprintf("%s: %s%s; V de Cramér = %s (%s); n = %s",
          r$teste, estat, p_txt, formatar_num(r$v, 3), tolower(r$intensidade),
          formatar_num(r$n, 0))
}

# Tabela completa para uma ou mais variáveis qualitativas.
# Caselas com resíduo padronizado ajustado > 1,96 (mais casos que o esperado sob
# independência) ficam em azul com ▲; < -1,96 (menos casos) em vermelho com ▼.
tabela_quali <- function(resultados, legenda = NULL) {
  linhas <- lapply(resultados, function(r) {
    celulas <- r$celulas
    for (i in seq_len(nrow(celulas))) {
      for (j in seq_len(ncol(celulas))) {
        z <- r$residuos[i, j]
        if (z > LIMIAR_RESIDUO) {
          celulas[i, j] <- sprintf('<span class="acima">▲ %s</span>', celulas[i, j])
        } else if (z < -LIMIAR_RESIDUO) {
          celulas[i, j] <- sprintf('<span class="abaixo">▼ %s</span>', celulas[i, j])
        }
      }
    }
    data.frame(
      Categoria = rownames(celulas),
      celulas,
      Total = formatar_num(rowSums(r$tabela), 0),
      check.names = FALSE
    )
  })
  corpo <- do.call(rbind, linhas)
  grupos <- setNames(
    vapply(linhas, nrow, integer(1)),
    vapply(resultados, \(r) sprintf("%s — %s", r$rotulo, descrever_teste(r)), "")
  )

  kbl(corpo, format = "html", escape = FALSE, row.names = FALSE,
      caption = legenda, align = c("l", "c", "c", "c", "r"),
      col.names = c("", names(CORES_RESPOSTA), "Total")) |>
    kable_styling(bootstrap_options = c("condensed", "hover"), full_width = FALSE) |>
    add_header_above(c(" " = 1, "Consumo de água — n (% na linha)" = 3, " " = 1)) |>
    pack_rows(index = grupos, label_row_css = "background:#f0efec; text-align:left;")
}

# Tabela das variáveis quantitativas: medidas-resumo por categoria + teste
tabela_quanti <- function(resultados) {
  abrev <- c("Diminuiu" = "D", "Não alterou" = "N", "Aumentou" = "A")
  linhas <- lapply(resultados, function(r) {
    digitos <- if (r$variavel == "Altura") 2 else 1
    resumo <- r$resumo |>
      mutate(txt = sprintf("%s (%s)<br>%s [%s; %s]",
                           formatar_num(media, digitos), formatar_num(dp, digitos),
                           formatar_num(mediana, digitos), formatar_num(q1, digitos),
                           formatar_num(q3, digitos)))
    sig <- r$comparacoes |> filter(p.adj < ALFA)
    comparacoes <- if (r$p >= ALFA) {
      "—"
    } else if (nrow(sig) == 0) {
      "nenhuma"
    } else {
      paste(sprintf("%s ≠ %s", abrev[sig$group1], abrev[sig$group2]), collapse = "<br>")
    }
    data.frame(
      Variavel = r$rotulo,
      Diminuiu = resumo$txt[1],
      Nao = resumo$txt[2],
      Aumentou = resumo$txt[3],
      Teste = r$teste,
      p = formatar_p(r$p),
      efeito = formatar_num(r$efeito, 3),
      comparacoes = comparacoes
    )
  })

  kbl(do.call(rbind, linhas), format = "html", escape = FALSE, row.names = FALSE,
      align = c("l", "c", "c", "c", "l", "c", "c", "c"),
      col.names = c("Variável", names(CORES_RESPOSTA), "Teste", "valor-p", "η²<sub>H</sub>",
                    "Comparações (Dunn-Holm)")) |>
    kable_styling(bootstrap_options = c("condensed", "hover"), full_width = FALSE) |>
    add_header_above(c(" " = 1, "Média (DP) / Mediana [Q1; Q3]" = 3, " " = 4))
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
