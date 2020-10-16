#' @importFrom dplyr %>% 
compare_sistec_rfept <- function(sistec, rfept, linked_courses = NULL){

  sistec_rfept <- create_sistec_rfept_list(sistec, rfept, linked_courses) %>% 
    remove_invalid_cpf() %>% 
    remove_unlinked_cpf() %>% 
    remove_wrong_cpf() %>% 
    remove_duplicated_registry() %>% 
    merge_sistec_rfept() %>% 
    create_linked_courses_data_frame() %>% 
    merge_duplicated_registry() %>% 
    compare_situation() %>% 
    split_situation() %>% 
    separate_wrong_beginning() %>% 
    separate_unlinked_entry() %>% 
    pending_manual_inspection() %>% 
    clean_memory()
  
  class(sistec_rfept) <- c("comparison_list", class(sistec_rfept))

  sistec_rfept
}
