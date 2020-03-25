a <- sistec::read_qacademico("C:/Pesquisa/dados/sistec_app/arquivos/")
b <- sistec::read_sistec("C:/Pesquisa/dados/sistec_app/arquivos/")


t <- full_join(a,b, by = c("Cpf" = "NU_CPF"))
tt <- t

nrow(t) 

for (i in 1:1032){
  cpf_rnd <- stringi::stri_rand_strings(1,11, pattern = "[0-9]") %>% 
    stringr::str_replace(pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                         replacement = "\\1.\\2.\\3-")
  t$Cpf[i] <- cpf_rnd
}

for( i in 1:1032){
  if(!is.na(t$Nome[i])){
    if(!is.na(t$NO_ALUNO[i])){
      str <- stri_rand_strings(3, 6, "[a-zA-Z]")
      paste(str[1], str[2], str[3])
    }
  }
}


str <- stri_rand_strings(3, 6, "[a-zA-Z]")

paste(str[1], str[2], str[3])