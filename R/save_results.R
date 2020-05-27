#' @importFrom dplyr %>% syms
save_results <- function(x, grid, path, file){
  if (is.null(grid)){
    save_without_subfolders(x, path, file)
  } else {
    save_with_subfolders(x, grid, path, file)
  }
}

save_without_subfolders <- function(x, path, file){
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(x, paste0(path, "/", file, ".xlsx"))
}

save_with_subfolders <- function(x, grid, path, file){
  grid_vars <- names(grid)
  qtd_vars <- length(names(x)) - length(grid_vars)
  vars <- names(x)[1:qtd_vars]
  
  # paths to save
  paths <- apply(grid, 1, function(e){
    paste0(path, "/", paste0(e, collapse = "/"))}) 
  
  # creating directories
  invisible(lapply(paths, function(e){
    dir.create(e, recursive = TRUE, showWarnings = FALSE)}))
  
  # expression to use in dplyr::filter
  a <- paste0(grid_vars, " == '")
  filter_exprs <- apply(grid, 1, function(e){
    paste0(paste0(a, e, "'"), collapse = " & ")})
  
  # write result in xlsx
  invisible(lapply(1:length(filter_exprs), function(e){
    openxlsx::write.xlsx(x %>% 
                           dplyr::filter(eval(parse(text = filter_exprs[e]))) %>% 
                           dplyr::select(!!!syms(vars)),
                         paste0(paths[e], "/", file, ".xlsx"))
  }))
}
