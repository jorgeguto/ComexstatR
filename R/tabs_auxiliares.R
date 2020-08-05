#'Pesquisar tabelas auxiliares
#'
#' @param pattern Trecho a ser pesquisado (código ou texto)
#' @name Tabelas_Auxiliares
#' @encoding UTF8
#' @description As funções \code{pesquisar_ncm}, \code{pesquisar_sh6}, \code{pesquisar_sh4},
#' \code{pesquisar_sh2} e \code{pesquisar_ncm_sec} se destinam a pesquisar os códigos referentes ao
#' Sistema Harmonizado.
#'
#' As funções \code{pesquisar_cgce3}, \code{pesquisar_cgce2} e \code{pesquisar_cgce1} servem para
#' pesquisar a Classificação por Grandes Categorias Econômicas (CGCE).
#'
#' As funções \code{pesquisar_cuci_item}, \code{pesquisar_cuci_sub}, \code{pesquisar_cuci_pos}, \code{pesquisar_cuci_cap}
#' e \code{pesquisar_cuci_sec}  são utilizadas para pesquisar a Classificação Uniforme
#' para o Comércio Internacional (CUCI).
#'
#' As funções \code{pesquisar_isic_classe}, \code{pesquisar_isic_grupo}, \code{pesquisar_isic_div}
#' e \code{pesquisar_isic_sec}  são utilizadas para pesquisar a Classificação Internacional Padrão por
#'Aatividades Econômicas (ISIC).'
#'
#' As funções \code{pesquisar_pais} e \code{pesquisar_blocos} se destinam para a pesquisa de países e blocos
#' econômicos.
#'
#' As funções \code{pesquisar_uf}, \code{pesquisar_mun} e \code{pesquisar_urf} servem, respectivamente,
#' para pesquisar a Unidade da Federação (UF), o município e a Unidade da Receita Federal (URF).
#'
#' A função \code{pesquisar_via} se destina a pesquisar o meio de transporte.
#'
#' As funções relacionadas ao SH, à CGCE, à CUCI e à ISIC podem ter como argumento trechos da descição ou do código.
#'
#' Para as demais funções o argumento deve ser um trecho da descrição.
#'
#' Para acessar a tabela completa basta executar a função sem argumento.
#'
#'
NULL


#função para consultar as NCMs
#' @usage
#' pesquisar_ncm('pattern')
#'
#' @examples
#'  pesquisar_ncm('010')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_ncm <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noNcmpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product/ncm', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::mutate(pesquisa,
                            text = substring(text,12))
  colnames(pesquisa) <- c('NCM', 'Descrição')

  return(pesquisa)
}


#função para consultar os SH6s
#' @usage
#' pesquisar_sh6('pattern')
#'
#' @examples
#'  pesquisar_sh6('aço')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_sh6 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noSh6pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/harmonized-system/subposition', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('SH6', 'Descrição')

  return(pesquisa)
}

#função para consultar os SH4s
#' @usage
#' pesquisar_sh4('pattern')
#'
#' @examples
#'  pesquisar_sh4('frutas')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_sh4 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noSh4pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/harmonized-system/position', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('SH4', 'Descrição')

  return(pesquisa)
}

#função para consultar os SH2s
#' @usage
#' pesquisar_sh2('pattern')
#'
#' @examples
#'  pesquisar_sh2('08')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_sh2 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noSh2pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/harmonized-system/chapter', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('SH2', 'Descrição')

  return(pesquisa)
}


#função para consultar as Seções da NCM
#' @usage
#' pesquisar_ncm_sec('pattern')
#'
#' @examples
#'  pesquisar_ncm_sec('animais')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_ncm_sec <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noSecpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/harmonized-system/section', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('NCM_SEC', 'Descrição')

  return(pesquisa)
}


#função para consultar os países
#' @usage
#' pesquisar_pais('pattern')
#'
#' @examples
#'  pesquisar_pais('ale')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_pais <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noPaispt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/countries', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  colnames(pesquisa) <- c('CO_PAIS', 'País')

  return(pesquisa)
}


#função para consultar os blocos
#' @usage
#' pesquisar_blocos('pattern')
#'
#' @examples
#'  pesquisar_blocos('mercosul')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_blocos <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noBlocopt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/blocks', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::distinct(pesquisa, id, .keep_all = TRUE)
  colnames(pesquisa) <- c('CO_BLOCO', 'Bloco')

  return(pesquisa)
}


#função para consultar as UFs
#' @usage
#' pesquisar_uf('pattern')
#'
#' @examples
#'  pesquisar_uf('bahia')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_uf <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noUf"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/states', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,id, text)
  colnames(pesquisa) <- c('CO_UF', 'UF')

  return(pesquisa)
}


#função para consultar os municípios
#' @usage
#' pesquisar_mun('pattern')
#'
#' @examples
#'  pesquisar_mun('são paulo')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_mun <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noMunMin"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/cities', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-noMunMin)
  colnames(pesquisa) <- c('CO_MUN', 'Município')

  return(pesquisa)
}


#função para consultar a Via
#' @usage
#' pesquisar_via('pattern')
#'
#' @examples
#'  pesquisar_via('aerea')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_via <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noVia"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/via', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  colnames(pesquisa) <- c('CO_VIA', 'Via')

  return(pesquisa)
}

#função para consultar a URF
#' @usage
#' pesquisar_urf('pattern')
#'
#' @examples
#'  pesquisar_urf('brasilia')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_urf <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noUrf"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/location/urf', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::mutate(pesquisa,
                            text = substring(text,11))
  colnames(pesquisa) <- c('CO_URF', 'URF')

  return(pesquisa)
}

#função para consultar CGCE Nível 3
#' @usage
#' pesquisar_cgce3('pattern')
#'
#' @examples
#'  pesquisar_cgce3('alimentos')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cgce3 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCgceN3pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-description/cgce-n3', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CGCE_N3', 'Descrição')

  return(pesquisa)
}


#função para consultar CGCE Nível 2
#' @usage
#' pesquisar_cgce2('pattern')
#'
#' @examples
#'  pesquisar_cgce2('insumos')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cgce2 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCgceN2pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-description/cgce-n2', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CGCE_N2', 'Descrição')

  return(pesquisa)
}


#função para consultar CGCE Nível 1
#' @usage
#' pesquisar_cgce1('pattern')
#'
#' @examples
#'  pesquisar_cgce1('consumo')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cgce1 <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCgceN1pt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-description/cgce-n1', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CGCE_N1', 'Descrição')

  return(pesquisa)
}


#função para consultar CUCI Item
#' @usage
#' pesquisar_cuci_item('pattern')
#'
#' @examples
#'  pesquisar_cuci_item('ovinos')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cuci_item <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCuciItempt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-category/item', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CUCI_ITEM', 'Descrição')

  return(pesquisa)
}

#função para consultar CUCI Subgrupo
#' @usage
#' pesquisar_cuci_sub('pattern')
#'
#' @examples
#'  pesquisar_cuci_sub('carne')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cuci_sub <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCuciSubpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-category/subposition', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CUCI_SUBGRUPO', 'Descrição')

  return(pesquisa)
}


#função para consultar CUCI Posição
#' @usage
#' pesquisar_cuci_pos('pattern')
#'
#' @examples
#'  pesquisar_cuci_pos('queijo')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cuci_pos <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCuciPospt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-category/position', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CUCI_POSICAO', 'Descrição')

  return(pesquisa)
}


#função para consultar CUCI Capítulo
#' @usage
#' pesquisar_cuci_cap('pattern')
#'
#' @examples
#'  pesquisar_cuci_cap('café')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cuci_cap <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCuciCappt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-category/chapter', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CUCI_CAPITULO', 'Descrição')

  return(pesquisa)
}


#função para consultar CUCI Seção
#' @usage
#' pesquisar_cuci_sec('pattern')
#'
#' @examples
#'  pesquisar_cuci_sec('obras')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_cuci_sec <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noCuciSecpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-category/section', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_CUCI_SECAO', 'Descrição')

  return(pesquisa)
}

#função para consultar ISIC Classe
#' @usage
#' pesquisar_isic_classe('pattern')
#'
#' @examples
#'  pesquisar_isic_classe('plásticos')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_isic_classe <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noIsicClasspt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-classification/class', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_ISIC_CLASSE', 'Descrição')

  return(pesquisa)
}


#função para consultar ISIC Grupo
#' @usage
#' pesquisar_isic_grupo('pattern')
#'
#' @examples
#'  pesquisar_isic_grupo('cultivo')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_isic_grupo <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noIsicGrouppt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-classification/group', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_ISIC_GRUPO', 'Descrição')

  return(pesquisa)
}


#função para consultar ISIC Divisão
#' @usage
#' pesquisar_isic_div('pattern')
#'
#' @examples
#'  pesquisar_isic_div('pesca')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_isic_div <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noIsicDivisionpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-classification/division', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_ISIC_DIVISAO', 'Descrição')

  return(pesquisa)
}


#função para consultar ISIC Seção
#' @usage
#' pesquisar_isic_sec('pattern')
#'
#' @examples
#'  pesquisar_isic_sec('agro')
#'
#' @rdname Tabelas_Auxiliares
pesquisar_isic_sec <- function(pattern='') {

  #Ajusta parâmetros da consulta
  params = list(
    `term` = pattern,
    `filter` = '{"id":"noIsicSectionpt"}'
  )

  #Faz a requisição
  pesquisa <- httr::GET(url = 'http://api.comexstat.mdic.gov.br/pt/product-classification/section', query = params)

  #Verifica se a resposta foi recebida corretamente
  if(pesquisa$status_code!=200) {

    return('Erro inesperado. Verifique a consulta ou tente novamente mais tarde')
  }

  #Trata o resultado da consulta
  pesquisa <- httr::content(pesquisa, type = 'text', encoding = 'UTF-8')
  pesquisa <- jsonlite::fromJSON(pesquisa, flatten = TRUE)

  #Verifica se não houve nenhum resultado
  resposta_vazia <- ifelse(length(pesquisa)==0, TRUE, FALSE)

  if (resposta_vazia) {
    return('Sem resultados')
  }

  #Formata resultado
  pesquisa <- dplyr::select(pesquisa,-text)
  colnames(pesquisa) <- c('CO_ISIC_SECAO', 'Descrição')

  return(pesquisa)
}


