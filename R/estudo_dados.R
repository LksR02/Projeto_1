# Investigação de apoio ao Projeto I. Executar na raiz: Rscript R/estudo_dados.R
# O banco original permanece intacto. Saídas agregadas em data/estudo/ (ignorada).
# Métodos: capítulos 4, 7 e 8 do livro e atividade 5 do professor.
source("R/funcoes.R")
dir.create("data/estudo", recursive = TRUE, showWarnings = FALSE)
brutos <- readxl::read_excel("data/Nutricao.xlsx")
dados <- ler_dados("data/Nutricao.xlsx")

habitos <- setdiff(names(brutos)[20:38], "Consumo_água")
rotulos_habitos <- c(
  Tempo_Preparo_Refeicao = "Tempo de preparo das próprias refeições",
  Refeicoes_Diarias = "Número de refeições diárias",
  Delivery = "Delivery", Planejamento_Cardapio = "Planejamento do cardápio",
  Estoque_Alimentos = "Estoque de alimentos",
  Paes_Torradas_Biscoitos = "Pães, torradas e biscoitos",
  Batata_Macarrao_Arroz = "Batata, macarrão e arroz", Frutas = "Frutas",
  Legumes_Verduras = "Legumes e verduras", Ovos_Carnes = "Ovos e carnes",
  Leites_Derivados = "Leites e derivados", Doces = "Doces",
  Bebidas_Alcoolicas = "Bebidas alcoólicas", Refrigerante = "Refrigerantes",
  Produtos_Industrializados = "Produtos industrializados", Embutidos = "Embutidos",
  Tempo_Preparo_Pandemia = "Tempo dedicado à alimentação e preparo",
  atividade_fisica_pandemia = "Atividade física"
)
ROTULOS <- c(ROTULOS, rotulos_habitos, Consumo_água = "Consumo de água", Consumo_agua = "Consumo de água")
nome <- function(v) unname(ROTULOS[[v]])

# Declarados antes de calcular os cruzamentos complementares. Não é uma busca
# exaustiva de todos os pares; as perguntas avaliam sobreposição de perfis.
pares <- list(
 c("Renda_familiar", "Dificuldade_Financeira"),
 c("Escolaridade", "Renda_familiar"), c("Genero", "Ansiedade"),
 c("Genero", "Depressao"), c("Ansiedade", "Depressao"),
 c("Ansiedade", "Insonia"), c("Depressao", "Insonia"),
 c("Isolamento", "Trabalha"), c("Regiao", "Renda_familiar"),
 c("Regiao", "Escolaridade"), c("Dificuldade_Financeira", "Ansiedade"),
 c("Dificuldade_Financeira", "Depressao")
)
perguntas <- c(
 "A dificuldade financeira tem a mesma distribuição nas faixas de renda?",
 "As faixas de renda se distribuem igualmente nos níveis de escolaridade?",
 "A frequência de ansiedade difere entre categorias de gênero?",
 "A frequência de depressão difere entre categorias de gênero?",
 "Ansiedade e depressão são relatos independentes?",
 "Ansiedade e insônia são relatos independentes?",
 "Depressão e insônia são relatos independentes?",
 "Isolamento e situação de trabalho são independentes?",
 "O perfil de renda é semelhante nas cinco regiões?",
 "O perfil de escolaridade é semelhante nas cinco regiões?",
 "A ansiedade tem a mesma frequência segundo dificuldade financeira?",
 "A depressão tem a mesma frequência segundo dificuldade financeira?"
)

categorias <- function(x) {
 if (is.factor(x)) return(droplevels(x))
 n <- unique(x[!is.na(x)])
 ordem <- c(NIVEIS_RESPOSTA, "Não consumo", "Não tenho o hábito", "2", "3", "4", "5 ou mais")
 factor(x, levels = c(intersect(ordem, n), setdiff(sort(n), ordem)))
}
cat_res <- function(d, x, y) {
 z <- d[complete.cases(d[, c(x, y)]), ]
 tab <- table(categorias(z[[x]]), categorias(z[[y]]))
 t <- testar_associacao(tab)
 chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
 v <- sqrt(unname(chi$statistic) / (sum(tab) * min(dim(tab) - 1)))
 celulas <- as.data.frame(tab, stringsAsFactors = FALSE)
 names(celulas) <- c("categoria", "resposta", "n")
 celulas$pct_linha <- as.vector(100 * prop.table(tab, 1))
 celulas$esperada <- as.vector(chi$expected)
 celulas$residuo <- as.vector(chi$stdres)
 celulas$influente <- t$p < ALFA & abs(celulas$residuo) > qnorm(1 - ALFA / 2)
 list(x=x, y=y, rotulo_x=nome(x), rotulo_y=nome(y), n=sum(tab),
      teste=t$teste, p=t$p, estatistica=unname(chi$statistic), gl=unname(chi$parameter),
      v=v, intensidade=interpretar_v(v), esperada_min=min(chi$expected),
      pct_esperadas_menor5=100*mean(chi$expected < 5), celulas=celulas)
}
ajustar_familia <- function(rs) {
 ps <- vapply(rs, function(r) r$p, 0)
 pp <- p.adjust(ps, method="bonferroni")
 for (i in seq_along(rs)) {rs[[i]]$p_bonferroni <- unname(pp[i]); rs[[i]]$n_familia <- length(rs)}
 rs
}
quanti_res <- function(d, v) {
 r <- analisar_quanti(d, v)
 r$letras <- as.list(r$letras)
 r
}

prim_q <- setNames(lapply(VARS_QUALI, function(v) cat_res(dados,v,"Consumo_agua")), VARS_QUALI)
prim_n <- setNames(lapply(VARS_QUANTI, function(v) quanti_res(dados,v)), VARS_QUANTI)
prim <- ajustar_familia(c(prim_q, prim_n))
prim_q <- prim[VARS_QUALI]; prim_n <- prim[VARS_QUANTI]
outros <- ajustar_familia(setNames(lapply(habitos, function(v)cat_res(dados,v,"Consumo_agua")), habitos))
complementares <- ajustar_familia(lapply(seq_along(pares), function(i) {
 p <- pares[[i]]; r <- cat_res(dados,p[1],p[2]); r$pergunta <- perguntas[i]; r
}))

# Idade segundo perfis associados à água: todas as categorias destes grupos
# têm pelo menos 30 observações. Mesma rotina de diagnósticos e testes.
grupos_idade <- c("Escolaridade", "Renda_familiar", "Dificuldade_Financeira", "Ansiedade", "Depressao")
idade_contexto <- ajustar_familia(setNames(lapply(grupos_idade, function(g) {
 z <- dados[!is.na(dados[[g]]), ]; z$Consumo_agua <- droplevels(z[[g]])
 r <- quanti_res(z,"Idade"); r$grupo <- g; r$rotulo_grupo <- nome(g); r
}), grupos_idade))

descritivas <- lapply(names(brutos), function(v) {
 x <- brutos[[v]]
 if (is.numeric(x)) {
  list(variavel=v, rotulo=nome(v), tipo="Quantitativa", n=length(x), faltantes=sum(is.na(x)),
       media=mean(x), dp=sd(x), mediana=median(x), q1=unname(quantile(x,.25)),
       q3=unname(quantile(x,.75)), minimo=min(x), maximo=max(x))
 } else {
  tb <- as.data.frame(table(categorias(x)), stringsAsFactors=FALSE)
  names(tb) <- c("categoria","n");tb$pct <- 100*tb$n/sum(tb$n)
  list(variavel=v, rotulo=nome(v), tipo="Qualitativa", n=length(x),faltantes=sum(is.na(x)), categorias=tb)
 }
})

# Linhas repetidas não comprovam participantes repetidos. O cenário sem
# repetições é apenas uma sensibilidade, nunca substitui o banco principal.
unicos <- dados[!duplicated(brutos), ]
sens_q <- lapply(VARS_QUALI, function(v) cat_res(unicos,v,"Consumo_agua"))
sens_n <- lapply(VARS_QUANTI, function(v) quanti_res(unicos,v))
sens <- c(sens_q, sens_n)
sensibilidade <- data.frame(variavel=c(VARS_QUALI,VARS_QUANTI),
 p_original=vapply(prim,function(r)r$p,0),
 p_sem_repeticoes=vapply(sens,function(r)r$p,0),
 significativo_original=vapply(prim,function(r)r$p<ALFA,FALSE),
 significativo_sem_repeticoes=vapply(sens,function(r)r$p<ALFA,FALSE))
sensibilidade$p_bonferroni_original <- p.adjust(sensibilidade$p_original,"bonferroni")
sensibilidade$p_bonferroni_sem_repeticoes <- p.adjust(sensibilidade$p_sem_repeticoes,"bonferroni")

# Quadros descritivos para discutir composição, sem atribuir causalidade.
peso_genero <- dados |> group_by(Genero) |> summarise(n=n(),media=mean(Peso),dp=sd(Peso),.groups="drop")
genero_agua <- dados |> filter(!is.na(Genero)) |> count(Consumo_agua,Genero) |>
 group_by(Consumo_agua) |> mutate(pct_coluna=100*n/sum(n)) |> ungroup()

resultado <- list(
 origem=list(arquivo="Nutricao.xlsx",aba=readxl::excel_sheets("data/Nutricao.xlsx"),
             n=nrow(brutos),p=ncol(brutos),faltantes=sum(is.na(brutos)),
             linhas_repetidas=sum(duplicated(brutos)),
             linhas_excel_repetidas=which(duplicated(brutos))+1,
             erro_maximo_imc=max(abs(brutos$IMC-brutos$Peso/brutos$Altura^2)),
             preparo_divergentes=sum(brutos$Tempo_Preparo_Refeicao!=brutos$Tempo_Preparo_Pandemia)),
 descritivas=descritivas, agua_qualitativas=prim_q, agua_quantitativas=prim_n,
 agua_outros_habitos=outros, complementares=complementares,
 idade_contexto=idade_contexto, sensibilidade=sensibilidade,
 peso_genero=peso_genero,genero_agua=genero_agua,
 session=capture.output(sessionInfo())
)
jsonlite::write_json(resultado,"data/estudo/resultados.json",auto_unbox=TRUE,pretty=TRUE,digits=15,na="null")
saveRDS(resultado,"data/estudo/resultados.rds")

# Gráficos descritivos com ggplot2, pacote utilizado na disciplina.
library(ggplot2)
tema <- theme_minimal(base_size=12) + theme(panel.grid.major.y=element_blank(),
 legend.position="bottom", plot.title=element_text(face="bold",color="#173b60"),
 plot.background=element_rect(fill="white",colour=NA))
cores <- c("Diminuiu"="#d85250","Não alterou"="#93948f","Aumentou"="#2a78d6",
 "Não consumo"="#c3b695","Não tenho o hábito"="#aa9fc2")
des <- bind_rows(lapply(Filter(function(x)x$variavel %in% c("Consumo_água",habitos) &&
 x$variavel!="Refeicoes_Diarias",descritivas),function(x)mutate(x$categorias,variavel=x$rotulo)))
ord <- des |> filter(categoria=="Aumentou") |> arrange(pct) |> pull(variavel)
des$variavel <- factor(des$variavel,levels=ord)
des$categoria <- factor(des$categoria,levels=names(cores))
p <- ggplot(des,aes(pct,variavel,fill=categoria)) +
 geom_col(position=position_stack(reverse=TRUE),width=.7) +
 geom_text(aes(label=ifelse(pct>=9,paste0(round(pct),"%"),""),
 color=ifelse(categoria %in% c("Não consumo","Não tenho o hábito"),"escuro","claro")),
 position=position_stack(vjust=.5,reverse=TRUE),size=3) +
 scale_fill_manual(values=cores,drop=FALSE) +
 scale_colour_manual(values=c("escuro"="#252a31","claro"="white"),guide="none") +
 scale_x_continuous(expand=expansion(mult=c(0,0))) + coord_cartesian(xlim=c(0,100)) +
 labs(title="Mudanças relatadas nos hábitos durante a pandemia",x="% dos 2.155 participantes",y=NULL,fill=NULL) +
 tema + theme(plot.margin=margin(10,25,10,10))
ggsave("data/estudo/panorama_habitos.png",p,width=12,height=9,dpi=170)

rank <- bind_rows(lapply(list(prim_q,outros),function(rs)bind_rows(lapply(rs,function(r)
 tibble(variavel=r$rotulo_x,v=r$v,ajustado=r$p_bonferroni<ALFA)))))
rank$Painel <- rep(c("Explicativas previstas no projeto","Outros hábitos (complementar)"),c(length(prim_q),length(outros)))
rank$id <- paste(rank$variavel,rank$Painel,sep="__")
rank$id <- factor(rank$id,levels=rank$id[order(rank$v)])
p <- ggplot(rank,aes(v,id,fill=ajustado)) + geom_col(width=.7) +
 facet_wrap(~Painel,ncol=2,scales="free_y") +
 scale_y_discrete(labels=function(x)sub("__.*$","",x)) +
 scale_fill_manual(values=c("TRUE"="#2a78d6","FALSE"="#bcbdb7"),guide="none") +
 geom_vline(xintercept=.1,linetype="dashed",color="#999999",linewidth=.4) +
 scale_x_continuous(limits=c(0,.25)) +
 labs(title="Associações com a mudança no consumo de água",x="V de Cramér",y=NULL,
 caption="Azul: p Bonferroni < 0,05 na respectiva família. Cinza: não atinge esse limite.") +
 tema + theme(axis.text.y=element_text(size=9))
ggsave("data/estudo/associacoes_agua.png",p,width=13,height=8,dpi=170)

red <- bind_rows(lapply(prim_q[c("Ansiedade","Depressao","Insonia","Enxaqueca","Dificuldade_Financeira")],
 function(r)r$celulas |> filter(resposta=="Diminuiu") |> mutate(variavel=r$rotulo_x)))
red$variavel <- factor(red$variavel,levels=rev(unique(red$variavel)))
p <- ggplot(red,aes(pct_linha,variavel,fill=categoria)) +
 geom_col(position=position_dodge(width=.8),width=.65) +
 geom_text(aes(label=paste0(formatar_num(pct_linha),"%")),
 position=position_dodge(width=.8),hjust=-.15,size=4) +
 scale_fill_manual(values=c("Não"="#93948f","Sim"="#d85250")) +
 scale_x_continuous(limits=c(0,27),expand=expansion(mult=c(0,0))) +
 labs(title="Redução de água segundo relatos emocionais e dificuldade financeira",
 x="% que relatou redução de água, dentro de cada categoria",y=NULL,fill="Relatou a condição") + tema
ggsave("data/estudo/reducao_agua.png",p,width=11,height=5,dpi=170)
cat("Investigação concluída:",nrow(brutos),"registros;",length(prim),"testes principais;",
    length(outros),"outros hábitos;",length(complementares),"relações complementares;",
    length(idade_contexto),"comparações de idade.\n")
cat("Relações complementares:\n")
for(r in complementares)cat(r$rotulo_x,"x",r$rotulo_y,": V=",round(r$v,3)," p=",r$p,"\n")
cat("Outros hábitos:\n")
for(r in outros[order(vapply(outros,function(r)r$v,0),decreasing=TRUE)])cat(r$rotulo_x,": V=",round(r$v,3)," p=",r$p,"\n")
print(sensibilidade,row.names=FALSE)
