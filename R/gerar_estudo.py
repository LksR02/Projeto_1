"""Renderiza resultados agregados produzidos por estudo_dados.R em HTML local."""
import base64
import html
import json
from pathlib import Path


OUT = Path("data/estudo")
R = json.loads((OUT / "resultados.json").read_text())
D = {d["variavel"]: d for d in R["descritivas"]}
CORES = {"Diminuiu": "#d85250", "Não alterou": "#93948f", "Aumentou": "#2a78d6",
         "Não consumo": "#c3b695", "Não tenho o hábito": "#c3b695"}


def num(x, d=1):
    if x is None:
        return "—"
    return f"{x:,.{d}f}".replace(",", "X").replace(".", ",").replace("X", ".")


def pv(x):
    return "< 0,001" if x < .001 else num(x, 6)


def esc(x):
    return html.escape(str(x))


def table(headers, rows):
    return '<div class="scroll"><table><thead><tr>' + ''.join(
        f'<th>{esc(h)}</th>' for h in headers) + '</tr></thead><tbody>' + ''.join(
        '<tr>' + ''.join(f'<td>{v}</td>' for v in row) + '</tr>' for row in rows
    ) + '</tbody></table></div>'


def paragraph(text, cls=""):
    return f'<p class="{cls}">{text}</p>'


def figure(path, caption):
    return ('<figure><img alt="' + esc(caption) + '" src="data:image/png;base64,'
            + base64.b64encode(path.read_bytes()).decode() + '"><figcaption>'
            + esc(caption) + '</figcaption></figure>')


def cell(r, categoria, resposta):
    return next(c for c in r["celulas"] if c["categoria"] == categoria and c["resposta"] == resposta)


def pct(r, categoria, resposta):
    return num(cell(r, categoria, resposta)["pct_linha"]) + "%"


def cat_table(r):
    cats = list(dict.fromkeys(c['categoria'] for c in r['celulas']))
    resp = list(dict.fromkeys(c['resposta'] for c in r['celulas']))
    rows = []
    for cat in cats:
        cells = [cell(r, cat, y) for y in resp]
        row = [esc(cat), num(sum(c['n'] for c in cells), 0)]
        for c in cells:
            arrow = ('▲ ' if c['residuo'] > 0 else '▼ ') if c['influente'] else ''
            cls = 'acima' if c['residuo'] > 0 else 'abaixo'
            txt = f"{arrow}{num(c['n'], 0)} ({num(c['pct_linha'])}%)"
            row.append(f'<span class="{cls}">{txt}</span>' if arrow else txt)
        rows.append(row)
    h = table([r['rotulo_x'], 'n válido', *resp], rows)
    h += paragraph(f"{esc(r['teste'])}; p = {pv(r['p'])}; V de Cramér = {num(r['v'], 3)} "
                   f"({esc(r['intensidade'].lower())}); p ajustado na família = {pv(r['p_bonferroni'])}. "
                   f"Menor frequência esperada: {num(r['esperada_min'], 2)}; "
                   f"caselas com E &lt; 5: {num(r['pct_esperadas_menor5'])}%.", 'metodo')
    h += '<details><summary>Frequências esperadas e resíduos por casela</summary>' + table(
        ['Categoria', r['rotulo_y'], 'Observada', 'Esperada', 'Resíduo padronizado'],
        [[esc(c['categoria']), esc(c['resposta']), num(c['n'], 0), num(c['esperada'], 2),
          num(c['residuo'], 3)] for c in r['celulas']]) + '</details>'
    if r['esperada_min'] < 5:
        h += paragraph('Categorias pequenas: o teste global usa Fisher por Monte Carlo. '
                       'Os resíduos com aproximação normal são apenas pistas exploratórias, '
                       'especialmente nas caselas com esperadas pequenas.', 'atencao')
    return h


def quant_table(r):
    rows = []
    for s in r['resumo']:
        g = s['Consumo_agua']
        letra = r['letras'][g] if r['p'] < .05 else '—'
        rows.append([esc(g), num(s['n'], 0), num(s['media'], 2), num(s['dp'], 2),
                     f"{num(s['mediana'], 2)} [{num(s['q1'], 2)}; {num(s['q3'], 2)}]", letra])
    h = table(['Grupo', 'n', 'Média', 'DP', 'Mediana [Q1; Q3]', 'Letra'], rows)
    h += paragraph(f"{esc(r['teste'])}; p = {pv(r['p'])}; p ajustado na família = {pv(r['p_bonferroni'])}; "
                   f"{esc(r['homog_teste'])}: p = {pv(r['homog_p'])}. "
                   f"Comparações: {esc(r['pos_teste'])}.", 'metodo')
    h += '<details><summary>Diagnósticos e comparações múltiplas</summary>'
    h += table(['Grupo', 'Shapiro–Wilk: p'], [[esc(s['Consumo_agua']), pv(s['p'])] for s in r['shapiro']])
    h += table(['Grupo 1', 'Grupo 2', 'p ajustado do pós-teste'],
               [[esc(s['group1']), esc(s['group2']), pv(s['p.adj'])] for s in r['comparacoes']])
    return h + paragraph('Compartilhar uma letra significa que não foi detectada diferença a 5%; '
                          'não demonstra igualdade. As letras usam o ajuste do pós-teste entre pares, '
                          'distinto do Bonferroni entre testes globais.') + '</details>'


def ranking(rs):
    return table(['Variável / comparação', 'n', 'Teste', 'V', 'Intensidade', 'p', 'p Bonferroni'], [
        [esc(r.get('rotulo_x', r.get('rotulo', '')) + (' × ' + r['rotulo_y'] if r['y'] != 'Consumo_agua' else '')),
         num(r['n'], 0), esc(r['teste']), num(r['v'], 3), esc(r['intensidade']), pv(r['p']), pv(r['p_bonferroni'])]
        for r in sorted(rs, key=lambda r: -r['v'])])


# Os três gráficos são gerados no R junto com as análises.

sections=[]
def section(id,title,body):
    sections.append((id,title,f'<section id="{id}"><h2>{title}</h2>{body}</section>'))

o=R['origem']
section('leitura','1. Como interpretar este banco',
 paragraph('A unidade de análise é um registro de participante. A planilha tem <strong>2.155 registros e 38 variáveis</strong>: '
           '19 explicativas (15 qualitativas e quatro quantitativas) e 19 respostas sobre alimentação e comportamento. '
           'O consumo de água é o desfecho principal atribuído ao estudante. Os outros 18 desfechos são contexto e investigação complementar.')+
 paragraph('<strong>A resposta mede mudança percebida, não litros ingeridos.</strong> Aumentar o consumo não demonstra hidratação adequada; '
           'manter não informa se a ingestão já era suficiente. Não há medição anterior/posterior de volume de água, '
           'diagnóstico clínico confirmado dos relatos emocionais nem dados para estabelecer causas.')+
 table(['Verificação','Resultado / decisão'],[
  ['Células vazias','0. Recusas continuam sendo não resposta substantiva.'],
  ['Recusas','Gênero: 5; raça/cor: 28; escolaridade: 17. Exclusão apenas na análise da respectiva variável.'],
  ['Categorias pequenas','Gênero Outro: 6; raça/cor Indígena: 9 e Outro: 5. Mantidas; Fisher quando necessário.'],
  ['Escolaridade','Analfabeto (2), Fundamental (4) e Médio (742) agrupados como Até Ensino Médio (748).'],
  ['Linhas idênticas',f"{o['linhas_repetidas']} além da primeira ocorrência. Mantidas: não há identificador para comprovar duplicação de participantes."],
  ['IMC','Confere com peso/altura², dentro da precisão numérica do arquivo. Não é uma quarta medida antropométrica independente das outras duas.'],
  ['Dois tempos de preparo',f"Tempo_Preparo_Refeicao e Tempo_Preparo_Pandemia divergem em {o['preparo_divergentes']} registros ({num(100*o['preparo_divergentes']/o['n'])}%). Mantidos separados; consultar a redação original das perguntas."],
  ['Valores extremos','Idade 20–73 anos; peso 36–136,5 kg; altura 1,40–1,98 m; IMC 13,76–49,95. Não houve exclusão automática.'],
  ['Amostragem','Questionário online de 2022 segundo o enunciado. Representatividade e independência não podem ser comprovadas pelo arquivo.'],
 ])+paragraph('Trabalha não distingue trabalho presencial e remoto. Isolamento não informa duração ou intensidade. '
              'Renda familiar não é perda de renda nem renda por pessoa. Dificuldade financeira não mede diretamente acesso à água. '
              'Relatos de ansiedade e depressão não devem ser apresentados como diagnóstico clínico documentado.'))

section('metodos','2. Métodos e vínculo com a disciplina',
 table(['Pergunta','Procedimento','Material'],[
  ['Duas qualitativas são independentes?','Qui-quadrado; regra de Cochran. Fisher por Monte Carlo (100.000 réplicas, semente 2026) quando necessário.',
   '<a href="https://guilhermegetuff.github.io/LivroPratica2/Cap1.html">Testes de independência</a>'],
  ['Onde há desvios e qual a intensidade?','Resíduos padronizados e V de Cramér; porcentagens por linha.',
   '<a href="https://guilhermegetuff.github.io/LivroPratica2/Cap4.html">Tabelas resumo</a>'],
  ['As médias diferem entre grupos?','Densidades, Q-Q e Shapiro–Wilk; Bartlett quando normalidade plausível, Levene caso contrário; ANOVA + Tukey ou Welch + Games–Howell.',
   '<a href="https://guilhermegetuff.github.io/LivroPratica2/Cap5.html">Revisão dos testes</a> e <a href="https://guilhermegetuff.github.io/LivroPratica2/Atividades.html#atividade-avaliativa-5-a-anova-de-welch">atividade 5</a>'],
 ])+paragraph('Nível nominal de 5%. O critério de Bonferroni, ensinado para comparações múltiplas, também é aplicado aqui como '
              'verificação adicional entre testes globais, com famílias explícitas: 19 principais, 18 outros hábitos, 12 relações entre explicativas '
              'e cinco comparações de idade. Essa aplicação entre testes globais é uma extensão do mesmo ajuste, não uma exigência textual da atividade 6. '
              'O controle é por família; não há controle único de erro para as 54 perguntas em conjunto.')+
 paragraph('▲ e ▼ indicam resíduos acima ou abaixo do esperado sob independência, usando |resíduo| > 1,96 e p global < 0,05. '
           'Esse destaque local é exploratório: não foi corrigido pelo número de caselas. O p ajustado nas tabelas refere-se ao teste global.')+
 paragraph('O TCL se refere à distribuição das médias, não transforma os dados em normais. Os três grupos de água têm 273, 833 e 1.049 registros. '
           'Os diagnósticos gráficos existentes estão no apêndice dos slides. Sem controle simultâneo de outras variáveis, estas análises não estimam efeitos independentes, '
           'mediação ou mecanismos causais. Não foram aplicadas regressões ou métodos de aprendizado de máquina.'))

section('perfil','3. Quem respondeu e que mudanças relatou',
 paragraph('No banco completo, 74,3% se declararam mulheres, 42,1% residiam no Sudeste e 45,2% tinham pós-graduação. '
           'A idade média foi 34,85 anos e a mediana 32 anos. Ao excluir apenas as recusas, os denominadores mudam; '
           'por isso esses percentuais podem diferir dos slides, que usam casos válidos por variável.')+
 figure(OUT/'panorama_habitos.png','Percentuais sobre o banco completo. Não consumo e Não tenho o hábito são respostas válidas e distintas de Não alterou.')+
 paragraph('Na amostra, 62,0% relataram aumento de delivery; 55,7%, aumento do tempo dedicado à alimentação e preparo; '
           '50,7%, aumento do preparo das próprias refeições; 44,8%, aumento de doces; 45,5%, redução de atividade física. '
           'Esses relatos coexistem em uma amostra, mas as porcentagens marginais não demonstram que foram exatamente as mesmas pessoas. '
           'O número de refeições diárias descreve um nível (2, 3, 4 ou 5 ou mais), e não uma mudança, por isso não aparece nesse gráfico.'))

agua=D['Consumo_água']
body=table(['Mudança de água','n','%'],[[esc(c['categoria']),num(c['n'],0),num(c['pct'])+'%'] for c in agua['categorias']])
body+=paragraph('Percentuais arredondados podem somar 100,1%. Todas as contagens totalizam 2.155.')
body+=figure(OUT/'associacoes_agua.png','O painel da direita é complementar. Não substitui as explicativas exigidas para o projeto.')
body+=ranking(list(R['agua_qualitativas'].values()))
body+=paragraph('Treze das 15 qualitativas atingem p < 0,05. Oito permanecem abaixo do limite Bonferroni de 0,05/19: '
                'gênero, região, escolaridade, consulta com nutricionista, dificuldade financeira, ansiedade, depressão e insônia. '
                'A idade também permanece entre as quantitativas. Trabalho e fumo não apresentam evidência suficiente no limite nominal. '
                'Todos os V das explicativas são fracos ou muito fracos, conforme a classificação adotada no curso.')
body+=figure(OUT/'reducao_agua.png','Enxaqueca tem associação nominal, mas não atinge o limite Bonferroni dos 19 testes. As barras não representam efeitos causais.')
interpretacoes={
 'Genero':'Mulheres: redução de água em 14,7%; homens: 6,5%. O grupo Outro tem apenas seis registros e não sustenta uma conclusão estável sobre esse perfil.',
 'Raca_cor':'Pardos e pretos relatam mais aumento e brancos mais manutenção em relação ao esperado. A associação global não resiste ao Bonferroni principal.',
 'Regiao':'Aumento de água: Norte 59,7% e Sul 37,1%. Clima, temperatura e disponibilidade de água não foram medidos; não podem explicar estatisticamente essa diferença aqui.',
 'Isolamento':'Aumento de água: 50,8% com isolamento e 46,0% sem isolamento. Evidência nominal, sem atingir Bonferroni. Não demonstra que ficar em casa levou a beber mais água.',
 'Trabalha':'p = 0,123. Não foi detectada associação global; isso não prova ausência de relação em qualquer contexto.',
 'Renda_familiar':'Na maior renda, 44,3% mantiveram o consumo; nas outras faixas, 32,7% e 36,7%. Evidência nominal e muito fraca, que não atinge Bonferroni.',
 'Escolaridade':'Redução: 15,8% até Ensino Médio e 10,5% na pós-graduação. Escolaridade também se relaciona à renda e à idade, como mostram as análises complementares.',
 'Covid':'Aumento: 52,3% com relato de diagnóstico e 47,3% sem. Não há dados sobre momento, gravidade ou orientações recebidas; associação nominal, sem atingir Bonferroni.',
 'Consulta_Nutricionista':'Aumento: 56,1% com consulta e 46,9% sem. O acompanhamento pode ser uma hipótese interpretativa, mas não há randomização nem informação para atribuir o aumento à consulta.',
 'Dificuldade_Financeira':'Redução: 17,5% com dificuldade e 11,6% sem. O grupo com dificuldade tem menor manutenção e não apresenta aumento significativamente acima do esperado pelos resíduos.',
 'cigarro':'p = 0,093. Não há evidência global suficiente no nível adotado. A diferença descritiva de aumento não deve ser tratada como associação comprovada.',
 'Ansiedade':'Redução: 16,6% com ansiedade e 8,9% sem. O grupo com ansiedade também tem maior percentual bruto de aumento (50,1% contra 47,3%); o principal desvio está na redução e na menor manutenção.',
 'Depressao':'Redução: 21,2% com depressão e 11,0% sem. É relato autorreferido; não comprova diagnóstico clínico nem direção causal.',
 'Insonia':'Redução: 19,5% com insônia e 11,6% sem. Associação muito fraca que permanece após o ajuste principal.',
 'Enxaqueca':'Redução: 16,8% com enxaqueca e 12,0% sem. Associação nominal e muito fraca; não atinge Bonferroni.'}
for v,r in R['agua_qualitativas'].items():
    body+=f'<details><summary>{esc(r["rotulo_x"])}</summary>'+paragraph(interpretacoes[v])+cat_table(r)+'</details>'
for v,r in R['agua_quantitativas'].items():
    textos={'Idade':'As três médias diferem: Diminuiu 31,3; Aumentou 34,5; Não alterou 36,4 anos. '
                   'Não se trata de redução de idade nem de efeito da água sobre o envelhecimento.',
            'Peso':'Médias de 67,8; 70,9 e 70,4 kg, respectivamente. Diferença nominal (p = 0,021), sem atingir Bonferroni. '
                   'Peso é uma medida registrada, não emagrecimento durante a pandemia.',
            'Altura':'Não há evidência suficiente de diferença de médias (p ≈ 0,095).',
            'IMC':'Não há evidência suficiente de diferença de médias (p ≈ 0,146). O IMC é derivado de peso e altura.'}
    body+=f'<details><summary>{esc(r["rotulo"])}</summary>'+paragraph(textos[v])+quant_table(r)+'</details>'
section('agua','4. Consumo de água e explicativas do projeto',body)

out=R['agua_outros_habitos']
body=paragraph('Estes são os outros desfechos do banco, e não as 19 explicativas originalmente definidas pelo professor. '
               'Foram examinados para compreender o contexto comportamental. São associações transversais entre relatos, '
               'não fatores de exposição com anterioridade demonstrada.')
body+=ranking(list(out.values()))
body+=paragraph('Dezoito associações têm p nominal < 0,05; 17 permanecem após Bonferroni desta família. Delivery deixa de atingir o limite. '
                'Número de refeições fica muito próximo do limite (p ≈ 0,002765 e α ≈ 0,002778), exigindo cautela. '
                'Nenhuma relação desse conjunto é moderada ou forte segundo os pontos de corte do curso.')
body+=paragraph('<strong>Frutas:</strong> entre quem aumentou frutas, '+pct(out['Frutas'],'Aumentou','Aumentou')+
                ' aumentou água; entre quem diminuiu frutas, '+pct(out['Frutas'],'Diminuiu','Diminuiu')+
                ' diminuiu água. V = 0,214. É compatível com mudanças concomitantes nos hábitos; não demonstra que frutas aumentaram a ingestão de água.')
body+=paragraph('<strong>Atividade física:</strong> entre quem aumentou a prática, '+pct(out['atividade_fisica_pandemia'],'Aumentou','Aumentou')+
                ' aumentou água; entre quem diminuiu a prática, '+pct(out['atividade_fisica_pandemia'],'Diminuiu','Diminuiu')+
                ' diminuiu água. Não foram registrados intensidade, duração ou necessidade individual de hidratação.')
body+=paragraph('<strong>Delivery:</strong> embora 62,0% tenham relatado aumento, sua associação com a mudança de água é muito fraca '
                '(V = 0,059) e não atinge o ajuste. Frequência elevada de uma mudança não implica relação intensa com a outra.')
for r in out.values():
    body+=f'<details><summary>{esc(r["rotulo_x"])}</summary>'+cat_table(r)+'</details>'
section('habitos','5. Água e os outros 18 hábitos',body)

body=paragraph('As perguntas abaixo foram declaradas no script antes de calcular este conjunto, orientadas pelas relações substantivas '
               'e pelos achados já conhecidos dos slides. São exploratórias, não um plano confirmatório pré-registrado. '
               'Não houve busca entre todos os pares das 38 variáveis.')
body+=ranking(R['complementares'])
body+=paragraph('<strong>Renda e dificuldade financeira:</strong> dificuldade foi relatada por 48,2% na menor faixa de renda, '
                '20,9% na intermediária e 2,8% na maior. V = 0,348 (moderada). As duas variáveis têm significados diferentes '
                'e se sobrepõem; não devem ser interpretadas como explicações independentes sem uma análise que controle outros fatores.')
body+=paragraph('<strong>Escolaridade e renda:</strong> renda acima de R$ 8.640 ocorreu em 14,0% do grupo até Ensino Médio, '
                '21,2% do Superior e 50,0% da pós-graduação. V = 0,305 (moderada). Isso ajuda a entender por que os perfis de manutenção de água podem se sobrepor.')
body+=paragraph('<strong>Saúde emocional:</strong> depressão foi relatada por 27,2% de quem relatou ansiedade e 5,9% dos demais. '
                'Ansiedade e depressão têm V = 0,289. Não são quatro grupos inteiramente separados de pessoas ao analisar ansiedade, depressão, insônia e enxaqueca.')
body+=paragraph('<strong>Trabalho e isolamento:</strong> 65,1% de quem relatou isolamento trabalhava, contra 84,0% de quem não relatou isolamento. '
                'Isolamento não significou necessariamente deixar de trabalhar. O banco não identifica trabalho remoto.')
body+=paragraph('<strong>Dificuldade financeira e relatos emocionais:</strong> ansiedade em 58,4% com dificuldade e 46,3% sem; '
                'depressão em 26,4% e 13,9%, respectivamente. A sobreposição não comprova que dificuldade causou sofrimento ou que ele mediou a associação com água.')
for r in R['complementares']:
    body+=f'<details><summary>{esc(r["pergunta"])}</summary>'+cat_table(r)+'</details>'
body+='<h3>Idade e composição dos perfis</h3>'+paragraph('Médias por escolaridade: 26,9 anos até Ensino Médio, 34,8 no Superior e 41,0 na pós-graduação. '
              'Por ansiedade: 32,0 anos com relato e 37,5 sem; por dificuldade financeira: 31,5 e 35,6. '
              'As cinco comparações globais abaixo atingem o ajuste da família. Isso mostra composição etária diferente, '
              'sem demonstrar que a idade explica as outras associações com água.')
for r in R['idade_contexto'].values():
    body+=f'<details><summary>Idade segundo {esc(r["rotulo_grupo"].lower())}</summary>'+quant_table(r)+'</details>'
body+='<h3>Peso: um exemplo de possível confundimento</h3>'+paragraph('Entre quem reduziu água, 86,4% eram mulheres, contra 72,6% no grupo que manteve '
               'e 72,9% no que aumentou (percentuais por coluna, apenas respostas válidas de gênero). '
               'O peso médio foi 66,7 kg nas mulheres e 80,8 kg nos homens. A diferença de peso entre grupos de água pode refletir '
               'a composição desses grupos. Estes números ilustram uma hipótese de confundimento; não a comprovam nem a removem.')
section('relacoes','6. Como as explicativas se relacionam entre si',body)

sens=R['sensibilidade']
nominais=sum(s['significativo_original']!=s['significativo_sem_repeticoes'] for s in sens)
mudam=[s for s in sens if (s['p_bonferroni_original']<.05)!=(s['p_bonferroni_sem_repeticoes']<.05)]
body=paragraph('O cenário principal preserva os 2.155 registros. Um cenário hipotético com 2.138 linhas únicas serve apenas para verificar '
               'sensibilidade a possíveis repetições. Sem identificador, ele não pode ser chamado de banco corrigido.')
body+=paragraph(f'As decisões nominais a 5% mudam em {nominais} das 19 comparações. No ajuste dos 19 testes, '
                'a raça/cor cruza o limite entre cenários (p original ≈ 0,003490; cenário sem repetições ≈ 0,002490). '
                'Isso reforça a cautela com evidências próximas de um corte; não comprova que os registros repetidos sejam erros.')
body+=table(['Variável','p principal','p cenário','p Bonf. principal','p Bonf. cenário'],[
    [esc(s['variavel']),pv(s['p_original']),pv(s['p_sem_repeticoes']),pv(s['p_bonferroni_original']),pv(s['p_bonferroni_sem_repeticoes'])] for s in sens])
section('sensibilidade','7. Sensibilidade às linhas idênticas',body)

body=paragraph('A abertura pode explicar em cerca de 40 segundos o problema histórico e chegar à pergunta sobre hábitos:')
body+=('<blockquote>“Na pandemia, reduzir contatos foi uma das medidas adotadas para limitar a transmissão do coronavírus. '
      'Essa reorganização da vida levantou questões sobre alimentação, atividade física e autocuidado. Mas as pessoas tinham condições '
      'sociais, econômicas e emocionais diferentes. Neste estudo, analisamos os relatos de 2.155 adultos para investigar como essas '
      'características se associaram à mudança no consumo de água.”</blockquote>')
body+=paragraph('A justificativa histórica das medidas está na <a href="https://www.who.int/docs/default-source/coronaviruse/COVID-strategy-update-14april2020.pdf">'
                'estratégia de resposta da OMS de abril de 2020</a>. O questionário informa se a pessoa relatou isolamento; '
                'não pergunta por que aderiu ou deixou de aderir. Os dados não demonstram que uma mudança específica foi causada pela pandemia.')
body+=table(['Tempo','Pergunta que guia a fala','Conteúdo'],[
 ['0:00–0:40','Por que investigar hábitos nesse período?','Contexto e transição para autocuidado.'],
 ['0:40–1:40','Quem respondeu e o que foi medido?','Amostra, conveniência e mudança percebida de água.'],
 ['1:40–3:40','Como distinguimos padrões de variação amostral?','Testes qualitativos, resíduos, V e comparação das médias.'],
 ['3:40–4:20','Qual foi o padrão geral de água?','48,7% aumentaram; 12,7% diminuíram.'],
 ['4:20–7:20','Quais perfis se destacaram?','Emocional, dificuldade financeira, idade e síntese das outras tabelas.'],
 ['7:20–8:20','As características aparecem juntas?','Um exemplo: renda/dificuldade ou escolaridade/idade.'],
 ['8:20–9:10','Água acompanhou outros hábitos?','Frutas como exemplo complementar, identificado como outro desfecho.'],
 ['9:10–10:40','O que podemos concluir?','Associações pequenas, sobreposições, autorrelato e limites da generalização.'],
 ['10:40–12:00','Margem','Pausas, intervenções e fechamento.'],
 ])
body+=paragraph('Para manter o foco da atividade 6, os cruzamentos com os outros desfechos e entre explicativas devem ficar principalmente '
                'no material de estudo e no apêndice. A fala pode usar um ou dois exemplos. As tabelas obrigatórias com as 19 explicativas permanecem disponíveis.')
body+='<h3>Perguntas para ensaio</h3><ol>'+''.join(f'<li>{esc(q)}</li>' for q in [
 'O que significa aumentar água se o banco não mede litros por dia?',
 'Por que uma amostra de 2.155 pessoas ainda pode não representar o Brasil?',
 'Por que Fisher em gênero e raça/cor, mesmo com amostra total grande?',
 'O que o V informa que o valor-p não informa?',
 'Por que ansiedade pode ter associação com água se o percentual de aumento também é alto?',
 'Como você distingue renda familiar de dificuldade financeira?',
 'Por que o resultado de peso não demonstra que beber menos água emagrece?',
 'O que a relação entre escolaridade e idade muda na sua interpretação?',
 'Os cruzamentos complementares controlaram confundimento?',
 'Por que você manteve as linhas idênticas e o que mudou na sensibilidade?',
 'Por que as duas perguntas sobre preparo de refeições foram mantidas separadas?',
 'Quais achados perdem evidência após o ajuste entre testes?' ])+'</ol>'
section('fala','8. Como levar a investigação para 12 minutos',body)

body=paragraph('As frequências abaixo preservam as categorias originais, inclusive recusas. '
               'São a referência para compreender as 38 variáveis; as tabelas de análise usam as recodificações explicitadas na seção 1.')
for d in R['descritivas']:
    role='Explicativa' if d['variavel'] in list(R['agua_qualitativas'])+list(R['agua_quantitativas']) else ('Resposta principal' if d['variavel']=='Consumo_água' else 'Outro desfecho')
    body+=f'<details><summary>{esc(d["rotulo"])} <small>({esc(d["variavel"])})</small></summary>'
    body+=paragraph(f'{role}; {d["tipo"].lower()}; n = {num(d["n"],0)}; células vazias = {d["faltantes"]}.')
    if d['tipo']=='Qualitativa':body+=table(['Categoria original','n','% do total'],[[esc(c['categoria']),num(c['n'],0),num(c['pct'])+'%'] for c in d['categorias']])
    else:body+=table(['Média','DP','Mediana','Q1','Q3','Mínimo','Máximo'],[[num(d[k],2) for k in ['media','dp','mediana','q1','q3','minimo','maximo']]])
    body+='</details>'
body+=paragraph('Reprodução: executar Rscript R/estudo_dados.R e depois o gerador Python na raiz do projeto. '
                'As análises inferenciais são calculadas no R com as funções existentes do projeto. Python apenas organiza o relatório e desenha gráficos descritivos. '
                'A planilha original não foi modificada. Este relatório é local e não foi publicado.')
body+='<details><summary>Ambiente R utilizado</summary><pre>'+esc('\n'.join(R['session']))+'</pre></details>'
section('dicionario','9. Dicionário descritivo completo e reprodução',body)

css='''
:root{color-scheme:light}*{box-sizing:border-box}body{margin:0;background:#fbfaf6;color:#252a31;font:17px/1.6 system-ui,sans-serif}
header,main,footer{max-width:1120px;margin:auto;padding:32px}header{padding-top:55px;border-bottom:3px solid #173b60}
h1{font-size:36px;line-height:1.2;color:#173b60;margin:0 0 14px}h2{color:#173b60;font-size:27px;line-height:1.3;margin-top:0}h3{color:#173b60}
nav{display:flex;flex-wrap:wrap;gap:7px 20px;font-size:15px;padding-top:20px}a{color:#195889;text-underline-offset:3px}section{padding:25px 0 40px;border-bottom:1px solid #d9dcde;scroll-margin-top:15px}
table{width:100%;border-collapse:collapse;font-size:14px;line-height:1.45;margin:20px 0}th{text-align:left;background:#173b60;color:white;padding:12px}td{padding:10px;border-bottom:1px solid #ddd;vertical-align:top}tbody tr:nth-child(even){background:#f0f2f0}
.scroll{overflow:auto}details{background:#fff;border:1px solid #dce0e4;border-radius:5px;margin:12px 0;padding:14px 18px}details details{background:#f8f9fa}summary{cursor:pointer;color:#173b60;font-weight:650}summary small{font-weight:400;color:#606973}
.metodo{font-size:14px;color:#485764}.atencao{background:#fff3da;padding:15px;border-left:4px solid #b68a35}blockquote{margin:20px 0;background:#eaf1f8;border-left:5px solid #2a78d6;padding:22px;font-size:20px;color:#173b60}
.acima{background:#e4def7;font-weight:650;padding:3px}.abaixo{background:#fbecc0;font-weight:650;padding:3px}img{display:block;width:100%;height:auto}figure{margin:28px 0}figcaption{font-size:13px;color:#58636d;margin-top:8px}pre{white-space:pre-wrap;font-size:12px}li{margin:8px 0}footer{font-size:13px;color:#58636d}
@media(max-width:700px){header,main,footer{padding:20px}h1{font-size:28px}h2{font-size:24px}body{font-size:16px}}
@media print{body{background:white}details{break-inside:avoid}nav{display:none}a{color:inherit}header,main{padding:10px}section{break-before:page}img{max-height:210mm;object-fit:contain}}
'''
doc='<!DOCTYPE html><html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
doc+='<title>Estudo do banco de Nutrição — Projeto I</title><style>'+css+'</style></head><body>'
doc+='<header><h1>Consumo de água no contexto dos outros hábitos</h1><p>Investigação do banco de Nutrição · Projeto I · Lucas Rodrigo de Moura Aragão</p>'
doc+='<nav>'+''.join(f'<a href="#{id}">{esc(title)}</a>' for id,title,_ in sections)+'</nav></header><main>'
doc+=''.join(s for _,_,s in sections)+'</main><footer>Fonte dos resultados: planilha fornecida pelo estudante, aba Sheet 1. '
doc+='Métodos: material de Prática Estatística II e atividade avaliativa 5. Contexto histórico: OMS, estratégia de resposta de abril de 2020.</footer></body></html>'
(OUT/'estudo_completo.html').write_text(doc)
print(f'Relatório salvo: {OUT / "estudo_completo.html"}; {len(doc):,} caracteres; 38 perfis e 54 comparações globais.')
