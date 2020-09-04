#' @importFrom dplyr %>% 
compare_sistec_rfept <- function(sistec, rfept, linked_courses = NULL){

  sistec_rfept <- create_sistec_rfept_list(sistec, rfept, linked_courses) %>% 
    remove_invalid_cpf() %>% 
    remove_wrong_cpf() %>% 
    remove_duplicated_registry() %>% 
    remove_unlinked_cpf() %>% 
    merge_sistec_rfept() %>% 
    remove_unlinked_cpf() %>% 
    compare_situation() %>% 
    create_linked_courses_data_frame() %>% 
    split_situation() %>% 
    separate_wrong_beginning() %>% 
    pending_manual_inspection() %>% 
    clean_memory()
  
  class(sistec_rfept) <- c("comparison_list", class(sistec_rfept))

  sistec_rfept
}
