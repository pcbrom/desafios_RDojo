###############################################################################
# RESOLUCAO - DESAFIO: LOCALIDADE E OPERADORA
# Pedro Carvalho Brom - pcbrom@gmail.com
# RDojo.com.br
###############################################################################




# bibliotecas -----------------------------------------------------------------

require(rvest)                              # raspagem de dados
require(dplyr)                              # para uso de pipes
require(ggplot2)                            # graficos
require(stringr)                            # regex modo nutela

# lendo as tabelas ------------------------------------------------------------


# URL de interesse


URL_telefones = "https://www.dropbox.com/s/0htsalejlbgegks/localidade_operadora.html?dl=1"
URL_operadoras = "http://www.teleco.com.br/num_cel.asp"
URL_ddd = "https://pt.wikipedia.org/wiki/N%C3%BAmeros_de_telefone_no_Brasil"


# funcao para coleta

get_table = function(URL, exp_regular = NULL, vetor = FALSE) {
  x = URL %>%                               # usando a URL
    read_html() %>%                         # lendo o html
    html_nodes(xpath = '//table') %>%       # coletando o no da tabela de interesse
    html_table(fill = T)                    # fill = TRUE serve para preencher com NA as tabelas incompletas
  if(length(x) > 1) {
    x = x %>% 
      .[[grep(exp_regular, .)]]             # localizando e pegando somente a tabela de interesse
  } 
  if(vetor == TRUE) {
    x = x %>% 
      unlist() %>%                          # tirando do formato lista
      unname()                              # removendo os rotulos do vetor
  }
  return(x)
}


# raspando os telefones

telefones = get_table(URL = URL_telefones, vetor = T)
head(telefones)


# raspando operadoras

operadoras = get_table(URL = URL_operadoras, exp_regular = "Estado \\(DDD\\)") %>% 
  mutate(`Banda A (96 a 99)` = str_replace(`Banda A (96 a 99)`, "\\*", ""),
         `Banda B (91 a 94)` = str_replace(`Banda B (91 a 94)`, "\\*", ""))
operadoras


# raspando DDD

ddd = get_table(URL = URL_ddd, exp_regular = "Principais cidades\\(capitais em negrito\\)") %>% 
  select(Prefixo, Estado) %>%
  mutate(Prefixo = as.character(Prefixo),
         Estado = as.character(Estado),
         Estado = str_replace(Estado, "Distrito Federal.*", "Distrito Federal"))
ddd


# locais ----------------------------------------------------------------------

prefixos = telefones %>% 
  str_extract(" [0-9]{2} | [0-9]{3} ") %>%                                              # pegar apenas numeros com 2 ou tres digitos com um espaco nas laterais
  trimws("both") %>%                                                                    # remover os espacos laterais
  as.data.frame()                                                                       # forcar data.frame
names(prefixos) = "Prefixo"                                                             # renomeando a coluna para fazer left_join
prefixos$Prefixo = as.character(prefixos$Prefixo)                                       # ajustando formato
prefixos = left_join(prefixos, ddd)                                                     # juntando as informacoes das tabelas prefixos e ddd
prefixos$Estado = ifelse(is.na(prefixos$Estado), "Fora do Brasil", prefixos$Estado)     # arrumando os tels de fora
prefixos

# grafico

table(prefixos$Estado) %>%                          # contando frequencia
  as.data.frame() %>%                               # forcando o formato data.frame
  ggplot(.,                                         # entrando com os dados
         aes(x = reorder(Var1, Freq),               # colocando em ordem decrescente o eixo x
             y = Freq,                              # adicionando eixo y
             fill = Freq)                           # colocando coloracao na escala da frequencia
         ) +
  labs(
    title = "Usuários do grupo RDojo no WhatsApp",  # adicionando texto para rotulos e titulo
    subtitle = "Local de origem", 
    y = "quantidade", x = "localidade", 
    caption = ""
    ) +
  geom_bar(stat = "identity") +                     # criando as barras
  coord_flip()                                      # alterando as coordenadas para melhorar a estetica
  

# operadoras ------------------------------------------------------------------

prefixos = prefixos %>%
  mutate(cel_prefixo = telefones %>%                # criando uma nova coluna com as seguintes operacoes
           str_extract(" [0-9]{4}") %>%             # extrair os primeiros 4 digitos. os telefones de fora ficarao como NA
           substr(start = 2, stop = 3))             # extrair apenas os caracteres de interesse
prefixos

get_range = function(x) {
  inicio = x %>% 
    str_extract("[0-9]{2} a [0-9]{2}") %>% 
    substr(., 1, 2) %>% 
    as.numeric()
  fim = x %>% 
    str_extract("[0-9]{2} a [0-9]{2}") %>% 
    substr(., 6, 7) %>% 
    as.numeric()
  return(inicio:fim)
}

estados = operadoras[,1] %>% lapply(., get_range)
ddds = prefixos$Prefixo
cel_prefixo = prefixos$cel_prefixo

bA = 96:99
bB = 91:94

operadora = NULL
for (i in 1:nrow(prefixos)) {
  for (j in 1:length(estados)) {
    x = ddds[i] %in% estados[[j]]
    if(x) {
      linha = j
    }
  }
  if(is.na(cel_prefixo[i])) {
    op = "Fora do Brasil"
  } else if(cel_prefixo[i] %in% bA) {
    op = operadoras[linha, 2]
  } else if(cel_prefixo[i] %in% bB) {
    op = operadoras[linha, 3]
  } else if(linha %in% 1:5 & cel_prefixo[i] %in% 87:88) {
    op = "Oi"
  } else if (linha == 6 & cel_prefixo[i] %in% 87:88) {
    op = "Claro"
  } else if (linha %in% 7:8 & cel_prefixo[i] %in% 81:82) {
    op = "TIM"
  } else if (linha == 9 & cel_prefixo[i] %in% 81:87) {
    op = "TIM"
  } else if (linha %in% 1:2 & cel_prefixo[i] %in% 80:83) {
    op = "TIM"
  } else if (linha == 3 & cel_prefixo[i] %in% 81:84) {
    op = "Claro"
  } else if (linha == 4 & cel_prefixo[i] %in% 81:83) {
    op = "Claro"
  } else if (linha %in% 6:8 & cel_prefixo[i] %in% 84:85) {
    op = "Brasil Telecom"
  } else {
    op = "Vivo/Oi"
    if(linha == 5 & op == "Vivo/Oi") {
      op = "Vivo"
    } else {
      op = "Oi"
    }
  }
  operadora = append(operadora, op)
}

prefixos = cbind.data.frame(prefixos, operadora)
prefixos


# grafico

table(prefixos$operadora) %>%
  as.data.frame() %>%
  ggplot(., aes(x = reorder(Var1, Freq), y = Freq, fill = Freq)) +
  labs(title = "Usuários do grupo RDojo no WhatsApp",
       subtitle = "Operadoras", y = "quantidade", x = "localidade", 
       caption = "") +
  geom_bar(stat = "identity") +
  coord_flip()
