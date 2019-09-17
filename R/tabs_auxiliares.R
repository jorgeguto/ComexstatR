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
#' e pesquisar_cuci_sec  são utilizadas para pesquisar a Classificação Uniforme
#' para o Comércio Internacional (CUCI).
#'
#' As funções \code{pesquisar_pais} e \code{pesquisar_blocos} se destinam para a pesquisa de países e blocos
#' econômicos.
#'
#' As funções \code{pesquisar_uf}, \code{pesquisar_mun} e \code{pesquisar_urf} servem, respectivamente,
#' para pesquisar a Unidade da Federação (UF), o município e a Unidade da Receita Federal (URF).
#'
#' A função \code{pesquisar_via} se destina a pesquisar o meio de transporte.
#'
#' Todas as funções podem ter como argumento texto ou código.
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   NCM$NO_NCM_POR,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   NCM$CO_NCM,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(NCM[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   SH6$NO_SH6_POR,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   SH6$CO_SH6,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(SH6[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   SH4$NO_SH4_POR,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   SH4$CO_SH4,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(SH4[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   SH2$NO_SH2_POR,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   SH2$CO_SH2,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(SH2[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  if (texto) {
    indice <- grep(pattern,
                   SEC$SEC,
                   ignore.case = TRUE)

  } else{
    pattern <- utils::as.roman(pattern)
    pattern <- glue::glue("\\<{pattern}\\>")
    indice <- grep(pattern,
                   SEC$SEC,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(SEC[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   PAIS$NO_PAIS,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   PAIS$CO_PAIS,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(PAIS[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   BLOCOS$NO_BLOCO,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   BLOCOS$CO_BLOCO,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(BLOCOS[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   UF$NO_UF,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   UF$CO_UF,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(UF[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   MUN$NO_MUN_MIN,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   MUN$CO_MUN_GEO,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(MUN[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   VIA$NO_VIA,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   VIA$CO_VIA,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(VIA[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   URF$NO_URF,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   URF$CO_URF,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(URF[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CGCE3$NO_CGCE_N3,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CGCE3$CO_CGCE_N3,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CGCE3[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CGCE2$NO_CGCE_N2,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CGCE2$CO_CGCE_N2,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CGCE2[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CGCE1$NO_CGCE_N1,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CGCE1$CO_CGCE_N1,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CGCE1[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CUCI_ITEM$NO_CUCI_ITEM,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CUCI_ITEM$CO_CUCI_ITEM,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CUCI_ITEM[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CUCI_SUB$NO_CUCI_SUB,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CUCI_SUB$CO_CUCI_SUB,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CUCI_SUB[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CUCI_GRUPO$NO_CUCI_POS,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CUCI_GRUPO$CO_CUCI_POS,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CUCI_GRUPO[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CUCI_DIV$NO_CUCI_CAP,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CUCI_DIV$CO_CUCI_CAP,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CUCI_DIV[indice, ]))
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

  ifelse(suppressWarnings(is.na(as.integer(pattern))), texto <- TRUE, texto <- FALSE)

  pattern <- glue::glue("\\<{pattern}")

  if (texto) {
    indice <- grep(pattern,
                   CUCI_SEC$NO_CUCI_SEC,
                   ignore.case = TRUE)

  } else{
    indice <- grep(pattern,
                   CUCI_SEC$CO_CUCI_SEC,
                   ignore.case = TRUE)
  }

  return(tibble::as_tibble(CUCI_SEC[indice, ]))
}



