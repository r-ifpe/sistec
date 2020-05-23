#' @importFrom dplyr %>% 
compare_sistec_rfept <- function(sistec, rfept){

  sistec_rfept <- create_sistec_rfept_list(sistec, rfept) %>% 
    remove_invalid_cpf() %>% 
    remove_unliked_cpf() %>% 
    merge_sistec_rfept() %>% 
    compare_situation() %>% 
    split_situation()
  
  sistec_rfept <- linked_courses_data_frame(sistec_rfept)

  sistec_rfept
}
