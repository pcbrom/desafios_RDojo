###############################################################################
# RESOLUCAO - DESAFIO: RECONSTITUIR BASE
# Pedro Carvalho Brom - pcbrom@gmail.com
# RDojo.com.br
###############################################################################



###############################################################################
# PARTE 01 
###############################################################################



# ELEMENTOS QUA SERAO USADOS NA FUNCAO ----------------------------------------

# unzip file
unzip("specdata.zip")

# listar arquivos
arquivos = paste0("./specdata/", list.files("./specdata"))

# abrir um arquivo
x = read.csv(arquivos[1])

# remover pasta specdata
unlink("./specdata", recursive = T)



# FUNCAO PARA EXECUTAR O TRABALHO ---------------------------------------------

get_data = function() {
  
  # preparacao
  arquivo = tcltk::tk_choose.files()
  unzip(arquivo)
  nome_pasta = gsub("*.*/|(.zip)", "", arquivo)
  arquivos = paste0("./", nome_pasta, "/", list.files(paste0("./", nome_pasta)))

  # coletando
  cat("\niniciando importacao...\n")
  pb = txtProgressBar(min = 2, max = length(arquivos), style = 3)
  x = read.csv(arquivos[1], col.names = c("Date", "sulfate", "nitrate", "ID"))
  for (i in 2:length(arquivos)) {
    y = read.csv(arquivos[i], header = T, col.names = c("Date", "sulfate", "nitrate", "ID"))
    x = rbind(x, y)
    setTxtProgressBar(pb, i)
  }
  cat("\nimportacao completa...\n")
  
  # salvando
  write.csv2(x, "db_completo.csv")
  cat("\narquivo salvo em: db_completo.csv\n")
  
  # remover pasta descompactada
  unlink("./specdata", recursive = T)
  
}

# teste

get_data()



###############################################################################
# PARTE 02 
###############################################################################

