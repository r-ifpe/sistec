#' @importFrom dplyr %>% 
compare_sistec_rfept <- function(sistec, rfept){

  sistec_rfept <- create_sistec_rfept_list(sistec, rfept) %>% 
    remove_invalid_cpf() %>% 
    remove_unliked_cpf() %>% 
    merge_sistec_rfept() %>% 
    compare_situation() %>% 
    create_linked_courses_data_frame() %>% 
    split_situation()
  
  class(sistec_rfept) <- c("comparison_list", class(sistec_rfept))

  sistec_rfept
}
