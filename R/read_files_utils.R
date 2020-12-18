num_para_cpf <- function(cpf) {
  cpf <- stringr::str_pad(cpf, 11, pad = "0")
  stringr::str_replace(
    string = cpf,
    pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
    replacement = "\\1.\\2.\\3-"
  )
}

co_unidade_ensino <- function() {
  co_unidade_ensino <- utils::read.csv(system.file(
    "extdata/co_unidade_ensino/co_unidade_ensino.csv",
    package = "sistec"
  ),
  colClasses = "character", fileEncoding = "UTF-8"
  )

  dplyr::transmute(co_unidade_ensino,
    S_NO_CAMPUS = !!sym("Campus"),
    CO_UNIDADE_ENSINO = !!sym("Co_unidade_ensino")
  )
}

detect_sep <- function(x) {
  header <- readLines(x, n = 1, encoding = "UTF-8")
  comma_sep <- stringr::str_detect(header, ",")
  semicolon_sep <- stringr::str_detect(header, ";")
  tab_sep <- stringr::str_detect(header, "\t")
  if (comma_sep) {
    sep <- ","
  } else if (semicolon_sep) {
    sep <- ";"
  } else if (tab_sep) {
    sep <- "\t"
  } else {
    stop("Separador diferente de tab ou ; ou ,")
  }

  sep
}

detect_encoding <- function(x, sep, col) {
  latin1_characters <- paste0(
    "\xc9|\xc7|\xd5|\xca|\xda|\xc2|\xc1|\xcd", # upper
    "\xe9|\xe7|\xf5|\xea|\xfa|\xe2|\xe1|\xed" # lower
  )  # bug in \xc3
  
  windows <- grepl("windows", tolower(Sys.getenv("SystemRoot")))
  if (windows) {
    x <- utils::read.csv(x, header = TRUE, sep = sep, encoding = "latin1", nrows = 300)
  } else {
    x <- utils::read.csv(x, header = TRUE, sep = sep, encoding = "UTF-8", nrows = 300)
  }
  
  latin1 <- any(stringr::str_detect(
    x[[col]],
    latin1_characters
  ))
  
  encoding <- if (latin1) {
    "latin1"
  } else {
    "UTF-8"
  }
  encoding
}