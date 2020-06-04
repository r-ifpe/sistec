#' @importFrom stringr str_detect
compare_situation <- function(x){
  
  sistec <- x$sistec_rfept_linked$S_NO_STATUS_MATRICULA
  rfept <- x$sistec_rfept_linked$R_NO_STATUS_MATRICULA
  status_rfept <- status_rfept(x$rfept_complete)
  
  # existe_rfept <- !is.na(rfept)
  status_concluido <- str_detect(sistec, "CONCLU\u00cdDA") & # CONCLUÍDA
    str_detect(rfept, status_rfept$concluido) 
  status_integralizada <- str_detect(sistec, "INTEGRALIZADA") &
    str_detect(rfept, status_rfept$integralizada) 
  status_abandono <- str_detect(sistec, "ABANDONO") &
    str_detect(rfept, status_rfept$abandono)
  status_desligado <- str_detect(sistec, "DESLIGADO") &
    str_detect(rfept, status_rfept$desligado)
  status_em_curso <- str_detect(sistec, "EM_CURSO") &
    str_detect(rfept, status_rfept$em_curso)
  status_transferido <- str_detect(sistec, "TRANSF_EXT") &
    str_detect(rfept, status_rfept$transferido)
  
  if (is.null(status_rfept$reprovada)) {
    status_reprovada <- rep(FALSE, length(status_concluido))
  } else {
    status_reprovada <- str_detect(sistec, "REPROVADA") &
      str_detect(rfept, status_rfept$reprovada)
  }
  
  status <- status_abandono | status_concluido | status_integralizada | status_desligado |
    status_em_curso | status_transferido | status_reprovada
  
  status[is.na(status)] <- FALSE
  x$sistec_rfept_linked$S_NO_STATUS_IGUAL <- status
  class(x$sistec_rfept_linked) <- c("comparison_data_frame", class(x$rfept_complete)[-1])

  x
}

status_rfept <- function(x){
  UseMethod("status_rfept")
}

status_rfept.qacademico_table <- function(x){
  list(concluido = "Conclu\u00eddo|Formado", # Concluído
       integralizada = "Concludente|ENADE|V\u00ednculo", # Vínculo
       abandono = "Abandono",
       desligado = "Cancelamento|Jubilado",
       em_curso = "Matriculado|Trancado",
       transferido = "Transferido Externo")
}


status_rfept.sigaa_table <- function(x){
  list(concluido = "FORMADO", 
       integralizada = "CANCELADO", 
       abandono = "CANCELADO",
       desligado = "CANCELADO",
       em_curso = "CURSANDO|CONCLUINTE|TRANCADO|GRADUANDO",
       transferido = "CANCELADO",
       reprovada = "CANCELADO")
}