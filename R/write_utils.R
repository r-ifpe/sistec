#' @importFrom dplyr %>% syms
make_group_grid <- function(x, vars){
  x %>% 
    dplyr::select(!!!syms(vars)) %>% 
    dplyr::distinct() 
}

#' @importFrom dplyr %>% 
rfept_table <- function(x){
  pos <- stringr::str_which(class(x), "_table")
  table <- sub("_table", "", class(x)[pos])
  stringr::str_to_title(table)
}
