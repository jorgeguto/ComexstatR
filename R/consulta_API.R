#Função para gerar filtros no padrão da API do Comex Stat
#' @title Pesquisa informações no sistema Comex Stat
#' @name pesquisar_comex_stat
#' @aliases pesquisar_comex_stat
#' @encoding UTF8
#' @usage
#' pesquisar_comex_stat(ano_inicial = "ano corrente" , ano_final = "ano corrente",
#' mes_inicial = "01", mes_final = "12", detalha_mes = FALSE,  tipo_op ="exp",
#' tipo_ord= "val",filtros = " ", filtros_esp = " ", detalhamentos = " ", faixa = FALSE,
#' valor_FOB = TRUE, valor_kg = TRUE, qtd_est = FALSE)
#' @param ano_inicial [int] ano inicial da consulta. ex:2018
#' @param ano_final [int] ano final da consulta. ex:2018
#' @param mes_inicial [int] mês inicial da consulta. ex:1
#' @param mes_final [int] mês inicial da consulta. ex:12
#' @param detalha_mes [logical] detalhar informacoes por mês. ex: TRUE
#' @param tipo_op [char] tipo da operação, exportação ou importação. Valores possíveis: 'exp','imp'
#' @param tipo_ord [char] tipo de ordenamento, valores ou detalhamento. Valores possíveis: 'val', 'det'
#' @param filtros [vetor de char] vetor com os filtros desejados. Valores possíveis: 'pais','blocos','uf', 'via', 'urf',
#' 'ncm', 'sh6', 'sh4', 'sh2', 'secao','cgce3', 'cgce2', 'cgce1', 'cuciit', 'cucisub', 'cucigru', 'cucidiv', 'cucisec'
#' @param filtros_esp [vetor de char] especificacao dos filtros escolhidos. Um vetor deve ser informado
#'para cada filtro com todos os itens a serem filtrados. Lista deve estar na ordem que os filtros foram passados.
#'Para especificação dos filtros é necessario consultar as tabelas auxiliares \code{\link{Tabelas_Auxiliares}}
#' @param faixa [logical] escolha se NCM será uma cesta ou uma faixa. ex: FALSE
#' @param detalhamentos [vetor de char] vetor com os detalhamentos desejados. Valores possíveis: 'pais','blocos','uf', 'via', 'urf',
#' 'ncm', 'sh6', 'sh4', 'sh2', 'secao','cgce3', 'cgce2', 'cgce1', 'cuciit', 'cucisub', 'cucigru', 'cucidiv', 'cucisec'
#' @param valor_FOB [logical] define se os valores FOB serão mostrados
#' @param valor_kg define se os valores de quilogramas líquidos serão mostrados
#' @param qtd_est define se a quantidade estatística do item sera mostrada. NCM deve estar entre os detalhamentos para que funcione
#' @description
#' Pesquisa estatísticas de importação e de exportação disponibilizadas pelo sistema Comex Stat e gera como resultado um dataframe.
#' @examples
#' pesquisar_comex_stat(ano_inicial = 2018, ano_final = 2018, mes_inicial = 1, mes_final = 12,
#'                      tipo_op = 'exp', tipo_ord = 'val', filtros = c('pais'), detalhamentos = c('pais','ncm'),
#'                      filtros_esp = list(c(160,249)))
#'
#'pesquisar_comex_stat(ano_inicial = 2018, ano_final = 2018, mes_inicial = 1, mes_final = 12,
#'                     detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val', detalhamentos = c('pais','ncm'),
#'                     filtros = c(), filtros_esp = c())
#'
#'
pesquisar_comex_stat <- function(ano_inicial = substr(Sys.Date(), 1,4 ) , ano_final = substr(Sys.Date(), 1,4 ),
                                   mes_inicial = 1, mes_final = 12, detalha_mes = FALSE,
                                   tipo_op ='exp', tipo_ord='val',filtros = c(), filtros_esp = c(),
                                   detalhamentos = c(), faixa = FALSE,
                                   valor_FOB = TRUE, valor_kg = TRUE, qtd_est = FALSE){

  # definição de listas com detalhamentos/filtros e nomes utilizados pela API
  lista_detalh_e_filtros <- c('pais','blocos','uf', 'via', 'urf', 'ncm', 'sh6', 'sh4', 'sh2', 'secao',
                           'cgce3', 'cgce2', 'cgce1', 'cuciit', 'cucisub', 'cucigru', 'cucidiv', 'cucisec',
                             'isicgru', 'isicdiv', 'isicsec', 'isiccla')

  lista_nomes <- c('noPaispt', 'noBlocopt', 'noUf', 'noVia', 'noUrf', 'noNcmpt', 'noSh6pt',
                                 'noSh4pt', 'noSh2pt', 'noSecpt', 'noCgceN3pt', 'noCgceN2pt', 'noCgceN1pt',
                                 'noCuciItempt', 'noCuciSubpt', 'noCuciPospt', 'noCuciCappt', 'noCuciSecpt',
                                 'noIsicGrouppt', 'noIsicDivisionpt', 'noIsicSectionpt', 'noIsicClasspt')

  #gera detalhamentos e filtros dos detalhamentos no padrão da API
  filtra_lista <- c()
  filtra <- c()
  detalha <- c()
  intervalo <- c()
  aux <- 1

  for (i in 1:length(lista_detalh_e_filtros)) {

    #preenche detalhamentos
    if (lista_detalh_e_filtros[i] %in% detalhamentos) {

      detalha <- c(detalha, glue::glue('{{"id":"{lista_nomes[i]}","text":""}}'))

    }

    #preenche lista de filtros e suas especificações
    if (lista_detalh_e_filtros[i] %in% filtros) {

      filtra_lista <- c(filtra_lista, glue::glue('{{"id":"{lista_nomes[i]}"}}'))

      if (faixa && (filtros[aux] %in% c('ncm', 'sh6', 'sh4', 'sh2', 'secao')) ) {

        filtra <- c(filtra, glue::glue('{{"item":[],"idInput":"{lista_nomes[i]}"}}'))
        intervalo <- glue::glue('"rangeFilter":[{{"id":"{lista_nomes[i]}","value":{{"rangeOne":"{filtros_esp[[aux]][1]}","rangeTwo":"{filtros_esp[[aux]][2]}"}}}}],')

      } else {

        if (length(filtros_esp[[aux]]) != 0) {

          filtra <- c(filtra, glue::glue('{{"item":[{lista_colapsada}],"idInput":"{lista_nomes[i]}"}}',
                                   lista_colapsada = toString(shQuote(filtros_esp[[aux]]))))

        }
      }

      aux <- aux + 1
    }
  }


  filtra_lista <- glue::glue_collapse(filtra_lista, sep = ",")
  ifelse(purrr::is_empty(filtra_lista), filtra_lista <- glue::glue('"filterList":[],') ,filtra_lista <- glue::glue('"filterList":[{filtra_lista}],'))
  filtra <- glue::glue_collapse(filtra, sep = ",")
  ifelse(purrr::is_empty(filtra), filtra <- glue::glue('"filterArray":[],') ,filtra <- glue::glue('"filterArray":[{filtra}],'))
  detalha <- glue::glue_collapse(detalha, sep = ",")
  ifelse(purrr::is_empty(detalha), detalha <- glue::glue('"detailDatabase":[],'),detalha <- glue::glue('"detailDatabase":[{detalha}],'))
  ifelse(is.null(intervalo) || purrr::is_empty(intervalo), intervalo <- '', intervalo <- intervalo)


  #converte tipo de operação e tipo de ordenamento para valores da API
  ifelse(tipo_op == "exp", tipo_op <-  1, tipo_op <- 2)
  ifelse(tipo_ord == "val", tipo_ord <- 1, tipo_ord <- 2)

  #transforma mês inicial e mês final para padrão da API ex: "01"
  mes_inicial <- stringr::str_pad(mes_inicial, width = 2, side = "left", pad = "0")
  mes_final <- stringr::str_pad(mes_final, width = 2, side = "left", pad = "0")

  #muda TRUE p/ true e FALSE p/ false (padrão API)
  ifelse(detalha_mes, detalha_mes <- 'true', detalha_mes <- 'false')
  ifelse(valor_FOB, valor_FOB <- 'true', valor_FOB <- 'false')
  ifelse(valor_kg, valor_kg <- 'true', valor_kg <- 'false')
  ifelse(qtd_est, qtd_est <- 'true', qtd_est <- 'false')

  #cria filtro para pesquisa
  filtro_cs <- glue::glue('{{"yearStart":"{ano_inicial}",
  "yearEnd":"{ano_final}",
  "typeForm":{tipo_op},
  "typeOrder":{tipo_ord},
  {filtra_lista}
  {filtra}
  {detalha}{intervalo}
  "monthDetail":{detalha_mes},
  "metricFOB":{valor_FOB},
  "metricKG":{valor_kg},
  "metricStatistic":{qtd_est},
  "monthStart":"{mes_inicial}",
  "monthEnd":"{mes_final}",
  "formQueue":"general",
  "langDefault":"pt"}}')


  #Codifica filtro para URL
  filtro_cs <- as.character(filtro_cs)
  filtro_cs <- iconv(filtro_cs, to = 'UTF8')
  filtro_cs <- utils::URLencode(filtro_cs)

  #endereço base da API
  comex_stat <- "http://api.comexstat.mdic.gov.br/general?filter="

  #cria a URL completa para a consulta
  url_completa <- paste0(comex_stat, filtro_cs)

  #consulta a API, extrai os dados e converte para um dataframe
  pesquisa_cs <- httr::GET(url_completa)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa_cs$status_code!=200) {
    return('Essa consulta é muito extensa (mais de 150 mil linhas) ou extrapolou o tempo de processamento permitido.')
  }

  pesquisa_cs <- httr::content(pesquisa_cs, "text", encoding = 'UTF8')
  pesquisa_cs <- jsonlite::fromJSON(pesquisa_cs,flatten = TRUE)
  pesquisa_cs <- as.data.frame(pesquisa_cs[[1]][[1]])

  if (length(pesquisa_cs)==0) {
    return('Esta consulta não trouxe nenhum resultado! Favor altere os parâmetros e tente novamente.')
  }

  if(qtd_est == 'true' & !('ncm' %in% detalhamentos)){
    pesquisa_cs <- dplyr::select(pesquisa_cs,-qtEstat)
    warning('NCM precisa estar em "detalhamentos" para que a quantidade estatística seja mostrada.')
  }

  return(pesquisa_cs)
}




