library(dplyr)

extract_co_unidade_ensino <- function() {
  states <- sistec_extract_states() 
  cities <- lapply(states$Sigla, sistec_extract_cities) %>% 
    do.call(rbind, .)
  
  rfepts <- lapply(cities$Id, sistec_extract_co_unidade_ensino) %>% 
    do.call(rbind, .)

  rfepts %>% 
    na.omit() %>% 
    dplyr::left_join(cities, by = "Id") %>% 
    dplyr::select(-Id) %>% 
    dplyr::mutate(Cidade = toupper(Cidade)) %>% 
    utils::write.table("co_unidade_ensino.csv", row.names = FALSE, 
                       sep = ",", fileEncoding = "UTF-8")
}

get_sistec_website <- function(sistec_url){
  httr::GET(sistec_url) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    stringi::stri_unescape_unicode() %>% 
    stringr::str_replace_all("\"", "" ) 
}

sistec_extract_states <- function() {
  states <- "https://sistec.mec.gov.br/consulta-publica-unidade-ensino-federal/retorna-ufs"
  
  state_names <- get_sistec_website(states) %>% 
    stringr::str_extract_all("(?<=html:)(.*?)(?=,acao)") %>% 
    unlist() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(Estado = value)
  
  state_initials <- get_sistec_website(states) %>% 
    stringr::str_extract_all("(?<=DadosMunicipios.)(.*?)(?=,)") %>% 
    unlist() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(Sigla = value)
  
  cbind(state_names, state_initials)
}

sistec_extract_cities <- function(state) {
  cities <- paste0(
    "https://sistec.mec.gov.br/consulta-publica-unidade-ensino-federal/retorna-municipios-por-uf/sguf/",
    state
  )    
  
  get_sistec_website(cities) %>% 
    stringr::str_extract_all("(?<=requisitaAccUe.)(.*?)(?=.,css)") %>% 
    unlist() %>% 
    stringr::str_split(",", simplify = TRUE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(Id = V1, Cidade = V2) %>% 
    dplyr::tibble(Sigla = state)
}

sistec_extract_co_unidade_ensino <- function(city_id) {
  # there are two sources with co_unidade_ensino
  # This is just federal institutions
    rfepts <- paste0(
      "https://sistec.mec.gov.br/consulta-publica-unidade-ensino-federal/retorna-ue-por-municipio/municipio/",
      city_id
    )
    
    # # all institutions
    # rfepts2 <- paste0(
    #   "https://sistec.mec.gov.br/accordionrelatoriopublico/retornauepormunicipio/municipio/",
    #   city_id
    # )

  rfepts <- get_sistec_website(rfepts) %>% 
    stringr::str_extract_all("(?<=html:)(.*?)(?=,acao)") %>% 
    unlist()
  
  if(length(rfepts) == 0){
    dplyr::tibble(Instituto = NA, Campus = NA,
                  Co_unidade_ensino = NA, Id = city_id)
  } else{
    # in some cases " - " is missing
    # modify to blank campus when encounter "colégio técnico"
    rfepts <- dplyr::if_else(
      stringr::str_detect(rfepts, " - "),
      rfepts,
      dplyr::if_else(
        stringr::str_detect(rfepts, " CAMPUS"),
        stringr::str_replace(rfepts, " CAMPUS", " - CAMPUS"),
        dplyr::if_else(
          stringr::str_detect(rfepts, " CENTRO DE "), 
          stringr::str_replace(rfepts, " CENTRO DE ", " - CENTRO DE "),  
          dplyr::if_else(
            stringr::str_detect(rfepts, "COLÉGIO TÉCNICO"),
            paste0(stringr::str_remove(rfepts, " \\[.*$"), " - ", rfepts),
            dplyr::if_else(
              stringr::str_detect(rfepts, "\\(UNID"),
              stringr::str_replace(rfepts, "\\(UNID", "- UNID") %>% 
                stringr::str_remove( "\\)"),
              paste0(stringr::str_remove(rfepts, " \\[.*$"), " - ", rfepts) 
            )
          )
        )
      )
    )
    
    rfepts %>% 
      stringr::str_remove(" \\]") %>% 
      stringr::str_split(" - | \\[ ", simplify = TRUE) %>% 
      dplyr::as_tibble() %>% 
      dplyr::rename(Instituto = V1, Campus = V2, Co_unidade_ensino = V3) %>% 
      dplyr::tibble(Id = city_id)
  }
}

extract_co_unidade_ensino()
