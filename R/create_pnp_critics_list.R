create_pnp_critics_list <- function(students, ciclo) {
  ciclo <- separate_na_in_table(ciclo)
  students <- separate_na_in_table(students)
 
  students_without_ciclo <- dplyr::anti_join(
    students$complete, ciclo$complete,
    by = c("S_CO_CICLO_MATRICULA" = "C_CO_CICLO_MATRICULA")
  )
  
  ciclo_without_students <- dplyr::anti_join(
    ciclo$complete, students$complete,
    by = c("C_CO_CICLO_MATRICULA" = "S_CO_CICLO_MATRICULA")
  )
  
  sistec <- dplyr::inner_join(
    students$complete, ciclo$complete,
    by = c("S_CO_CICLO_MATRICULA" = "C_CO_CICLO_MATRICULA")
  )

  list(
    sistec = sistec,
    ciclo_with_na = ciclo$with_na,
    students_with_na = students$with_na,
    pnp_student_critics = data.frame(),
    pnp_ciclo_critics = data.frame()
  )
}

separate_na_in_table <- function(x) {
  with_na <- x[apply(is.na.data.frame(x), 1, any), ]
  complete <- stats::na.omit(x)
  
  list(complete = complete, with_na = with_na)
}