#' @importFrom dplyr %>% sym
create_pnp_critics_list <- function(students, ciclo) {
  ciclo <- separate_na_in_ciclo(ciclo)

  sistec <- dplyr::full_join(students, ciclo$complete,
    by = c("S_CO_CICLO_MATRICULA" = "C_CO_CICLO_MATRICULA"))

  list(
    sistec = sistec,
    ciclo_with_na = ciclo$with_na,
    pnp_student_critics = data.frame(),
    pnp_ciclo_critics = data.frame()
  )
}

separate_na_in_ciclo <- function(x) {
  with_na <- x[apply(is.na.data.frame(x), 1, any), ]
  complete <- stats::na.omit(x)
  
  list(complete = complete, with_na = with_na)
}