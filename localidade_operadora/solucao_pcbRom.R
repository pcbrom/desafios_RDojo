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
  
### PARTE 02 NO PROXIMO DOMINGO
