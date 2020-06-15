#' @importFrom dplyr %>% 
compare_sistec_rfept <- function(sistec, rfept){

  sistec_rfept <- create_sistec_rfept_list(sistec, rfept) %>% 
    remove_invalid_cpf() %>% 
    remove_unliked_cpf() %>% 
    merge_sistec_rfept() %>% 
    update_unlinked_students() %>% 
    compare_situation() %>% 
    create_linked_courses_data_frame() %>% 
    split_situation() %>% 
    separate_wrong_registration() %>% 
    clean_memory()
  
  class(sistec_rfept) <- c("comparison_list", class(sistec_rfept))

  sistec_rfept
}
