a <- sistec::read_sistec("C:/Pesquisa/sistec/inst/extdata/test_datasets/sistec/")
b <- sistec::read_qacademico("C:/Pesquisa/sistec/inst/extdata/test_datasets/qacademico/")

d <- sistec::compare_sistec(a,b)

sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST")

x
grid
path
file

  grid_vars <- names(grid)
  qtd_vars <- length(names(x)) - length(grid_vars)
  vars <- names(x)[1:qtd_vars]
  
 rlang::parse_expr("RE")
  
x %>% 
  filter(eval(parse(text = filter_exprs[10])))

grid %>% 
  filter(eval(as.expression(t2)))

grid %>% 
  filter(CAMPUS == 'RECIFE' & INICIO == '2019.1')

t2 <- "CAMPUS == 'RECIFE' & INICIO == '2019.1'"

a1 <- apply(grid, 1, function(e){
  paste0(paste0(a, e, "'"), collapse = " & ")}) 

apply(grid, 1, function(e){
  
})









