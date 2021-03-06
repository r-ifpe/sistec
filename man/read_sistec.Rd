% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sistec.R
\name{read_sistec}
\alias{read_sistec}
\title{Read sistec files}
\usage{
read_sistec(path = "", start = NULL)
}
\arguments{
\item{path}{The sistec folder's path.}

\item{start}{A character with the date to start the comparison. The default is the minimum
value found in the data. The date has to be in this format: "yyyy.semester".
Ex.: "2019.1" or "2019.2".}
}
\value{
A data frame.
}
\description{
The package provides support if your data comes
from \href{http://portal.mec.gov.br/setec-secretaria-de-educacao-profissional-e-tecnologica}{setec}
or \href{https://sistec.mec.gov.br/}{web}. You just need to pass the folder's path were are your files.
See Details if you need help to download the data from Sistec.
}
\details{
You can download the Sistec's student registration using your proper account on
Sistec.  Be sure that your data has these variables:
\itemize{
\item On setec: "Nome Aluno", "Numero Cpf", "Co Ciclo Matricula", "Situacao Matricula",
"No Curso", "Dt Data Inicio" and "Unidade Ensino".
\item On web: "NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA", "NO_CICLO_MATRICULA",
"DT_DATA_INICIO" and "CO_UNIDADE_ENSINO".
}

Tip: To take every student for your institution/campus using web, search by student name and use " ".
}
\examples{
# this dataset is not a real one. It is just for test purpose.
sistec <- read_sistec(system.file("extdata/examples/sistec",
  package = "sistec"
))

sistec

# example selecting the period
sistec_2019_2 <- read_sistec(
  system.file("extdata/examples/sistec", package = "sistec"),
  start = "2019.2"
)

sistec_2019_2
}
