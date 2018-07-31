###############################################################################
# RESOLUCAO - DESAFIO: LOCALIDADE E OPERADORA
# Pedro Carvalho Brom - pcbrom@gmail.com
# RDojo.com.br
###############################################################################




# bibliotecas -----------------------------------------------------------------

require(rvest)                              # raspagem de dados
require(dplyr)                              # para uso de pipes
require(stringr)                            # regex modo nutela
require(RSelenium)                          # coleta das operadoras (ver mais abaixo)

# lendo as tabelas ------------------------------------------------------------


# URL de interesse


URL_operadoras = "http://www.teleco.com.br/num_cel.asp"


# COLETANDO AS OPERADORAS POR COMPLETO

# Conversei com o Lucas do RDojo e fizemos alguns ajustes na tabela de referencia,
# mas ainda nao havia ficado 100% para analise de dados.

# Particularmente setava bem insatisfeito com o a tabela que coloquei como referencia
# entao apelei e montei uma tabela mais fiel a realidade.

# Essa parte das operadoras pode mudar para faixa verde pra cima hahaha.


rD = rsDriver(browser = "chrome")
remDr = rD[["client"]]
Sys.sleep(2)

remDr$navigate(URL_operadoras)

seletores = remDr$getPageSource()[[1]] %>%
  read_html() %>% 
  html_nodes("table tbody tr td span select option") %>% 
  html_text() %>% 
  {.[-c(1,2)]} %>% 
  paste0("//option[@value='", ., "']")

compilado = NULL
for (k in 1:length(seletores)) {
  
  webElem = remDr$findElement(using = 'xpath', seletores[k])$clickElement()
  Sys.sleep(1)
  
  oper = remDr$getPageSource()[[1]] %>%
    read_html() %>% 
    html_nodes(xpath = '//*/img') %>%
    html_attr('src') %>% 
    str_subset("http://www.teleco.com.br/imagens/logos/num_cel") %>% 
    str_extract(., "[aA-zZ]*.png$") %>% 
    str_remove("(.png)$")
  
  lista_range = remDr$getPageSource()[[1]] %>%
    read_html() %>% 
    html_nodes(xpath = "//table") %>% 
    html_table(fill = T) %>% 
    {.[[10]]} %>%
    {.[,2]} %>%
    {.[(grep("^(Faixa)", .) + 1):(length(.))]} %>% 
    {.[!is.na(.)]} %>%
    gsub("\n", "", .) %>% 
    gsub("\\s+", ";", .) %>% 
    strsplit(";")
  
  ls_range = NULL
  for (i in 1:length(lista_range)) {
    y = NULL
    for (j in 1:length(lista_range[[i]])) {
      x = lista_range[[i]][j] %>% 
        strsplit("-") %>% 
        unlist() %>% 
        str_extract("\\d+") %>% 
        as.numeric()
      x = seq(x[1], x[2])
      y = append(y, x)
    }
    y = paste0(y, collapse = ";")
    ls_range = append(ls_range, y)
  }
  z = cbind.data.frame("ddd" = seletores[k], "operadora" = oper, "lista_numeros" = ls_range)
  compilado = rbind.data.frame(compilado, z)
}

compilado$ddd = compilado$ddd %>% str_extract("\\d+")
write.csv(compilado, "operadoras_compilado.csv", row.names = F)

remDr$close()
rD[["server"]]$stop()
