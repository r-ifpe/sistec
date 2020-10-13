output_screen <- function(input_sistec, input_rfept, comparison){ #linked_course_exist,

  if(all(!is.null(input_sistec), !is.null(input_rfept))){ # linked_course_exist
    response <- compare_screen(comparison)
  } else if(is.null(input_sistec) && is.null(input_rfept)){
    response <- "Selecione os arquivos do Sistec e do sistema acad\u00eamico."   
  } else if(is.null(input_rfept)){
    response <- "Selecione os arquivos do sistema acad\u00eamico."
  } else if(is.null(input_sistec)){
    response <- "Selecione os arquivos do sistec."
  } 

  response
}

compare_screen <- function(comparison){

 rfept <- rfept_table(comparison$rfept_complete)  
 shiny::HTML(paste(paste0("Compara\u00e7\u00e3o entre Sistec e ", rfept,
                          " realizada com sucesso!"),
                   "", "",
                   "Total de alunos:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_complete)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_complete)),                   
                   "Alunos sem CPF:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_cpf)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_without_cpf)),
                   "CPF's repetidos:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_wrong_cpf)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_wrong_cpf)),
                   "V\u00ednculos repetidos:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_duplicated_registry)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_duplicated_registry)),
                   "V\u00ednculos n\u00e3o encontrados:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_rfept)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_without_sistec)),
                   "Para inspe\u00e7\u00e3o manual:",
                   paste0("&emsp; - Sistec: ", nrow(comparison$sistec_pending)),
                   paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_pending)), 
                   "Erro no cadastro:",
                   paste0("&emsp; - Na data de in\u00edcio: ", nrow(comparison$wrong_beginning)),
                   "Situa\u00e7\u00f5es comparadas:",
                   paste0("&emsp; - Atualizadas: ", nrow(comparison$situation_updated)),
                   paste0("&emsp; - Desatualizadas: ", nrow(comparison$situation_to_update)),
                   sep = '<br/>'))
}

manual_screen <- function(){
  shiny::HTML(paste("MANUAL DO ARIA",
                    "", 
                    "- Alunos sem CPF's: CPF's em branco ou inv\u00e1lidos.", 
                    "&emsp; Sugest\u00e3o: Retificar o CPF ou averiguar se o aluno de fato existe.",
                    "",
                    "- CPF's repetidos: Alunos com nomes diferentes para o mesmo n\u00famero de CPF.", 
                    "&emsp; Sugest\u00e3o: Retificar o CPF ou averiguar se o aluno de fato existe.",
                    "", 
                    "- V\u00ednculos repetidos: Alunos com entradas repetidas no sistema acad\u00eamico ou Sistec",
                    "&emsp; <font color=\"#FF0000\"><b>URGENTE:</b></font> Alunos com entrada dupla no Sistec 
                    indicam ao MEC que a institui\u00e7\u00e3o apresenta mais alunos que do que de fato existem. Isso 
                    afeta no or\u00e7amento da institui\u00e7\u00e3o.", 
                    "&emsp; Sugest\u00e3o: Retificar a matr\u00edcula no sistema acad\u00eamico ou pedir matr\u00edcula 
                    extempor\u00e2nea no Sistec. Retifique os dados e execute o ARIA novamente.",
                    "",
                    "- V\u00ednculos n\u00e3o encontrados: Alunos que consistem no sistema acad\u00eamico mas n\u00e3o 
                    constam no Sistec e vice e versa.",
                    "&emsp; <font color=\"#FF0000\"><b>URGENTE:</b></font> Alunos n\u00e3o cadastrados no Sistec indicam 
                     ao MEC que a institui\u00e7\u00e3o apresenta menos alunos que do que de fato existem. Isso afeta no 
                     or\u00e7amento da institui\u00e7\u00e3o.", 
                    "&emsp; Sugest\u00e3o: Cadastre os alunos no Sistec e atualize o registro acad\u00eamico. Olhe a pasta 
                    \"Cadastrar no Sistec\", nela encontram-se os CPF's desses alunos e voc\u00ea pode realizar o cadastro 
                    desses alunos em lote. Retifique os dados e",
                    "&emsp; execute o ARIA novamente.",
                    "",
                    "- Erro no cadastro: Alunos com entrada encontrada, por\u00e9m o cadastro foi realizado em periodos 
                    diferentes. Ex.: matr\u00edcula em 2019.2 no sistema acad\u00eamico e no Sistec consta em 2020.1.",
                    "&emsp; <font color=\"#FF0000\"><b>ATEN\u00c7\u00c3O:</b></font> O or\u00e7amento da institui\u00e7\u00e3o 
                    basea-se nos alunos cadastrados no Sistec em rela\u00e7\u00e3o ao ano base. Se um aluno de 2019.2 for 
                    cadastrado em 2020.1 a institui\u00e7\u00e3o s\u00f3 receber\u00e1 os recursos referentes a 2020 e 
                    n\u00e3o a 2019.",
                    "&emsp; Sugest\u00e3o: Retificar a matr\u00edcula no sistema acad\u00eamico ou pedir matr\u00edcula 
                    extempor\u00e2nea no Sistec. Retifique os dados e execute o ARIA novamente.",
                    "",
                    "- Situa\u00e7\u00f5es comparadas: Alunos destualizados s\u00e3o aqueles que precisam apenas corrigir 
                    a situa\u00e7\u00e3o de matr\u00edcula. Os atualizados s\u00e3o aqueles em est\u00e3o totalmente corretos 
                    entre sistema acad\u00eamico e Sistec. Parab\u00e9ns!",
                    "",
                    "- Inspe\u00e7\u00e3o manual: Alunos n\u00e3o identificados pelo ARIA em nenhum dos quesitos anteriores.
                    Normalmente ser\u00e3o alunos provenientes de cursos FIC com menos de oito alunos ou situa\u00e7\u00f5es 
                    ainda n\u00e3o implementadas no ARIA.", 
                    "&emsp; Sugest\u00e3o: Retifique os dados das cr\u00edticas anteriores e execute o ARIA novamente.",
                    "","",
                    "Servidor, a atualiza\u00e7\u00e3o desses dados \u00e9 extremamente importante e impacta diretamente nas 
                    estat\u00edsticas da institui\u00e7\u00e3o que servem de base para formula\u00e7\u00e3o de 
                    pol\u00edticas p\u00fablicas.",
                    sep = '<br/>'))
}
