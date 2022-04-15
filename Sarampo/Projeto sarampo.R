library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("rio")

#abrir arquivos
data_exant <- read.dbf("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto Sarampo/entradas/EXANTNET.dbf")
data_gal_igg <- read.csv("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto Sarampo/entradas/data igg.csv", header = TRUE, sep = ";")
data_gal_igm <- read.csv("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto Sarampo/entradas/data igm.csv", header = TRUE, sep = ";")
data_gal_pcr <- read.csv("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto Sarampo/entradas/data pcr.csv", header = TRUE, sep = ";")

#ajustar datas
data_gal_igg$Data.de.Nascimento <- dmy(data_gal_igg$Data.de.Nascimento)
data_gal_igm$Data.de.Nascimento <- dmy(data_gal_igm$Data.de.Nascimento)
data_gal_pcr$Data.de.Nascimento <- dmy(data_gal_pcr$Data.de.Nascimento)
data_gal_igg$Data.do.1º.Sintomas <- dmy(data_gal_igg$Data.do.1º.Sintomas)
data_gal_igm$Data.do.1º.Sintomas <- dmy(data_gal_igm$Data.do.1º.Sintomas)
data_gal_pcr$Data.do.1º.Sintomas <- dmy(data_gal_pcr$Data.do.1º.Sintomas)

#ajustar datas
data_exant$DT_SIN_PRI <- as.Date(data_exant$DT_SIN_PRI, format="%d-%m-%Y")
data_exant$DT_NASC <- as.Date(data_exant$DT_NASC, format="%d-%m-%Y")

#função remover acentos
RemoveAcentos <- function(textoComAcentos) {
  if(!is.character(textoComAcentos)){
    on.exit()
  }
  letrasComAcentos <- "??????????????????????????????????????????????????Ǵ`^~?"
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "
  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  ) 
  return(textoSemAcentos)
}

RemoveAcentos(data_exant$NM_PACIENT)
RemoveAcentos(data_gal_igg$Paciente)
RemoveAcentos(data_gal_igm$Paciente)
RemoveAcentos(data_gal_pcr$Paciente)



#renomear código de município
cod_ibge <- c("250010", "250020", "250030", "250040", "250050", "250053",
              "250057", "250060", "250073", "250077", "250080", "250090",
              "250100", "250110", "250115", "250120", "250130", "250135",
              "250140", "250150", "250153", "250160", "250157", "250170",
              "250180", "250190", "250200", "250205", "250210", "250215",
              "250220", "250230", "250240", "250250", "250270", "250280",
              "250290", "250300", "250310", "250320", "250330", "250340",
              "250350", "250355", "250360", "250370", "250375", "250380",
              "250390", "250400", "250403", "250407", "250410", "250415",
              "250420", "250430", "250435", "250440", "250450", "250460",
              "250470", "250480", "250485", "250490", "250500", "250510",
              "250523", "250520", "250527", "250530", "250535", "250540",
              "250560", "250570", "250580", "250590", "250600", "250610",
              "250620", "250625", "250630", "250640", "250650", "250660",
              "250260", "250670", "250680", "250690", "250700", "250710",
              "250720", "250730", "250740", "250750", "251365", "250760",
              "250770", "250780", "250790", "250800", "250810", "250820",
              "250830", "250840", "250850", "250855", "250860", "250870",
              "250880", "250890", "250900", "250905", "250910", "250915",
              "250920", "250930", "250933", "250937", "250939", "250940",
              "250950", "250960", "250970", "250980", "250990", "251000",
              "251010", "251020", "251030", "251040", "251050", "251060",
              "251065", "251070", "251080", "251090", "251100", "251110",
              "251120", "251272", "251130", "251140", "251150", "251160",
              "251170", "251180", "251190", "251200", "251203", "251207",
              "251210", "251220", "251230", "251240", "251250", "251260",
              "251270", "251274", "251275", "251276", "251278", "251280",
              "251290", "251300", "251310", "251315", "251320", "251330",
              "251335", "251340", "251370", "251380", "251350", "251360",
              "251385", "251392", "251390", "251396", "251394", "251398",
              "251400", "250070", "251410", "251420", "251430", "251440",
              "251450", "251455", "251460", "251465", "251470", "251480",
              "251445", "251490", "251500", "251510", "251520", "251540",
              "251530", "251550", "251560", "251570", "251580", "251590",
              "251593", "251597", "251600", "251610", "251615", "251620",
              "251630", "251640", "251650", "251660", "251670", "251675",
              "251680", "251690", "251700", "251710", "251720", "250550",
              "251740")

cod_ibge <- data.frame(cod_ibge)
nm_municipios_PB <- c("Água Branca", "Aguiar", "Alagoa Grande", "Alagoa Nova",
                      "Alagoinha", "Alcantil","Algodão de Jandaíra", "Alhandra",
                      "Amparo", "Aparecida", "Araçagi", "Arara", "Araruna",
                      "Areia", "Areia de Baraúnas", "Areial", "Aroeiras",
                      "Assunção", "Baía da Traição", "Bananeiras", "Baraúna",
                      "Barra de Santa Rosa", "Barra de Santana", 
                      "Barra de São Miguel", "Bayeux", "Belém",
                      "Belém do Brejo do Cruz", "Bernardino Batista",
                      "Boa Ventura", "Boa Vista", "Bom Jesus", "Bom Sucesso",
                      "Bonito de Santa Fé", "Boqueirão", "Borborema",
                      "Brejo do Cruz", "Brejo dos Santos", "Caaporã",
                      "Cabaceiras", "Cabedelo", "Cachoeira dos Índios",
                      "Cacimba de Areia", "Cacimba de Dentro", "Cacimbas",
                      "Caiçara", "Cajazeiras", "Cajazeirinhas", "Caldas Brandão",
                      "Camalaú", "Campina Grande", "Capim", "Caraúbas",
                      "Carrapateira", "Casserengue", "Catingueira",
                      "Catolé do Rocha", "Caturité", "Conceição", "Condado",
                      "Conde", "Congo", "Coremas", "Coxixola", 
                      "Cruz do Espírito Santo", "Cubati", "Cuité",
                      "Cuité de Mamanguape", "Cuitegi", "Curral de Cima",
                      "Curral Velho", "Damião", "Desterro", "Diamante",
                      "Dona Inês", "Duas Estradas", "Emas", "Esperança",
                      "Fagundes", "Frei Martinho", "Gado Bravo", "Guarabira",
                      "Gurinhém", "Gurjão", "Ibiara", "Igaracy", "Imaculada",
                      "Ingá", "Itabaiana", "Itaporanga", "Itapororoca",
                      "Itatuba", "Jacaraú", "Jericó", "João Pessoa",
                      "Joca Claudino", "Juarez Távora", "Juazeirinho",
                      "Junco do Seridó", "Juripiranga", "Juru", "Lagoa",
                      "Lagoa de Dentro", "Lagoa Seca", "Lastro", "Livramento",
                      "Logradouro", "Lucena", "Mãe d'Água", "Malta",
                      "Mamanguape", "Manaíra", "Marcação", "Mari", 
                      "Marizópolis", "Massaranduba", "Mataraca", "Matinhas",
                      "Mato Grosso", "Maturéia", "Mogeiro", "Montadas",
                      "Monte Horebe", "Monteiro", "Mulungu", "Natuba",
                      "Nazarezinho", "Nova Floresta", "Nova Olinda",
                      "Nova Palmeira", "Olho d'Água", "Olivedos", "Ouro Velho",
                      "Parari", "Passagem", "Patos", "Paulista", "Pedra Branca",
                      "Pedra Lavrada", "Pedras de Fogo", "Pedro Régis",
                      "Piancó", "Picuí", "Pilar", "Pilões","Pilõezinhos",
                      "Pirpirituba", "Pitimbu", "Pocinhos","Poço Dantas",
                      "Poço de José de Moura", "Pombal", "Prata",
                      "Princesa Isabel", "Puxinanã", "Queimadas", "Quixabá",
                      "Remígio", "Riachão", "Riachão do Bacamarte",
                      "Riachão do Poço", "Riacho de Santo Antônio",
                      "Riacho dos Cavalos", "Rio Tinto", "Salgadinho",
                      "Salgado de São Félix", "Santa Cecília", "Santa Cruz",
                      "Santa Helena", "Santa Inês", "Santa Luzia",
                      "Santa Rita", "Santa Teresinha", "Santana de Mangueira",
                      "Santana dos Garrotes", "Santo André", "São Bentinho",
                      "São Bento", "São Domingos", "São Domingos do Cariri",
                      "São Francisco", "São João do Cariri",
                      "São João do Rio do Peixe", "São João do Tigre",
                      "São José da Lagoa Tapada", "São José de Caiana",
                      "São José de Espinharas", "São José de Piranhas",
                      "São José de Princesa", "São José do Bonfim",
                      "São José do Brejo do Cruz", "São José do Sabugi",
                      "São José dos Cordeiros", "São José dos Ramos",
                      "São Mamede", "São Miguel de Taipu",
                      "São Sebastião de Lagoa de Roça","São Sebastião do Umbuzeiro",
                      "São Vicente do Seridó", "Sapé", "Serra Branca",
                      "Serra da Raiz", "Serra Grande", "Serra Redonda",
                      "Serraria", "Sertãozinho","Sobrado", "Solânea",
                      "Soledade", "Sossêgo", "Sousa", "Sumé", "Tacima",
                      "Taperoá", "Tavares", "Teixeira", "Tenório", "Triunfo",
                      "Uiraúna", "Umbuzeiro", "Várzea", "Vieirópolis",
                      "Vista Serrana", "Zabelê")

nm_municipios_PB <- data.frame(nm_municipios_PB)
municipios_PB <- c(cod_ibge, nm_municipios_PB) 
municipios_PB <- data.frame(municipios_PB)

#renomear ID_MN_RESI para o left_join
data_exant <- rename(data_exant, cod_ibge = ID_MN_RESI)
data_exant <- left_join(data_exant, municipios_PB, by = "cod_ibge")

#criar chave
id_pessoa_exant <- paste(data_exant$NM_PACIENT,
                         data_exant$DT_NASC,
                         data_exant$cod_ibge)

id_pessoa_gal_igg <- paste(data_gal_igg$Paciente,
                           data_gal_igg$Data.de.Nascimento,
                           data_gal_igg$IBGE.Município.de.Residência)

id_pessoa_gal_igm <- paste(data_gal_igm$Paciente,
                           data_gal_igm$Data.de.Nascimento,
                           data_gal_igm$IBGE.Município.de.Residência)

id_pessoa_gal_pcr <- paste(data_gal_pcr$Paciente,
                           data_gal_pcr$Data.de.Nascimento,
                           data_gal_pcr$IBGE.Município.de.Residência)

#acrescentar coluna com ID_Pessoa
data_exant <- mutate(data_exant, id_pessoa_exant)
data_gal_igg <- mutate(data_gal_igg, id_pessoa_gal_igg)
data_gal_igm <- mutate(data_gal_igm, id_pessoa_gal_igm)
data_gal_pcr <- mutate(data_gal_pcr, id_pessoa_gal_pcr)

# separar duplicidades
DT_data_exant <- Sys.Date() - data_exant$DT_SIN_PRI #usar para verificar encerramento
DT_data_exant <- data.frame(DT_data_exant) #usar para verificar encerramento
sum(duplicated(data_exant$id_pessoa_exant))

duplicidade_exant <- data_exant[duplicated(data_exant$id_pessoa_exant), ]
duplicidade_exant <- subset(duplicidade_exant, 
                            select = -c(id_pessoa_exant)) 

#separar gal que não está notificado
data_gal_exant0 <- rename(data_gal_igg, id_pessoa_exant = id_pessoa_gal_igg)
data_gal_exant1 <- rename(data_gal_igm, id_pessoa_exant = id_pessoa_gal_igm)
data_gal_pcr <- rename(data_gal_pcr, id_pessoa_exant = id_pessoa_gal_pcr)

#juntar linhas de data_gal igg, igm
data_gal_soro <- bind_rows(data_gal_exant0, data_gal_exant1)

#cruzar gal pcr e sorologia com sinan
#separar gal_pcr que não está notificado
gal_pcr_notificar_sinan <- anti_join(data_gal_pcr, data_exant,
                                 by = "id_pessoa_exant")

gal_pcr_notificar_sinan_menor_5 <- filter(gal_pcr_notificar_sinan, 
                                          gal_pcr_notificar_sinan$Idade <5 & 
                                            gal_pcr_notificar_sinan$Tipo.Idade == "Ano(s)" |
                                            gal_pcr_notificar_sinan$Tipo.Idade == "Mês(es)" &
                                            gal_pcr_notificar_sinan$Resultado == "Detectável")

gal_pcr_notificar_sinan_menor_5 <- select (gal_pcr_notificar_sinan_menor_5,
                                   Requisição,
                                   Unidade.Solicitante,
                                   Data.do.1º.Sintomas,
                                   Paciente,
                                   Data.de.Nascimento,
                                   Nome.da.Mãe,
                                   Municipio.de.Residência,
                                   Data.da.Liberação,
                                   Resultado,
                                   id_pessoa_exant)

gal_pcr_notificar_sinan_menor_5 <- data.frame(gal_pcr_notificar_sinan_menor_5)
gal_pcr_notificar_sinan_menor_5 <- gal_pcr_notificar_sinan_menor_5[!duplicated(gal_pcr_notificar_sinan_menor_5), ]
gal_pcr_notificar_sinan_menor_5 <- subset(gal_pcr_notificar_sinan_menor_5, 
                                  select = -c(id_pessoa_exant)) 


gal_pcr_notificar_sinan_maior_5 <- filter(gal_pcr_notificar_sinan, 
                                          gal_pcr_notificar_sinan$Idade >4 & 
                                            gal_pcr_notificar_sinan$Tipo.Idade == "Ano(s)"&
                                            gal_pcr_notificar_sinan$Resultado == "Detectável") 

gal_pcr_notificar_sinan_maior_5 <- select (gal_pcr_notificar_sinan_maior_5,
                                   Requisição,
                                   Unidade.Solicitante,
                                   Data.do.1º.Sintomas,
                                   Paciente,
                                   Data.de.Nascimento,
                                   Nome.da.Mãe,
                                   Municipio.de.Residência,
                                   Data.da.Liberação,
                                   Resultado,
                                   id_pessoa_exant)


gal_pcr_notificar_sinan_maior_5 <- data.frame(gal_pcr_notificar_sinan_maior_5)
gal_pcr_notificar_sinan_maior_5 <- gal_pcr_notificar_sinan_maior_5[!duplicated(gal_pcr_notificar_sinan_maior_5), ]

gal_pcr_notificar_sinan_maior_5 <- subset(gal_pcr_notificar_sinan_maior_5, 
                            select = -c(id_pessoa_exant)) 


#separar gal_soro que não está notificado
gal_soro_notificar_sinan <- anti_join(data_gal_soro, data_exant,
                                     by = "id_pessoa_exant")

gal_soro_notificar_sinan_menor_5 <- filter(gal_soro_notificar_sinan, 
                                           gal_soro_notificar_sinan$Idade >4 & 
                                             gal_soro_notificar_sinan$Tipo.Idade == "Ano(s)"&
                                             gal_soro_notificar_sinan$Resultado == "Detectável") 

gal_soro_notificar_sinan_menor_5 <- select (gal_soro_notificar_sinan_menor_5,
                                           Requisição,
                                           Unidade.Solicitante,
                                           Data.do.1º.Sintomas,
                                           Paciente,
                                           Data.de.Nascimento,
                                           Nome.da.Mãe,
                                           Municipio.de.Residência,
                                           Data.da.Liberação,
                                           Resultado,
                                           id_pessoa_exant)

gal_soro_notificar_sinan_menor_5 <- distinct(gal_soro_notificar_sinan_menor_5)
gal_soro_notificar_sinan_menor_5 <- data.frame(gal_soro_notificar_sinan_menor_5)
gal_soro_notificar_sinan_menor_5 <- gal_soro_notificar_sinan_menor_5[!duplicated(gal_soro_notificar_sinan_menor_5), ]
gal_soro_notificar_sinan_menor_5 <- subset(gal_soro_notificar_sinan_menor_5, 
                                          select = -c(id_pessoa_exant)) 


gal_soro_notificar_sinan_maior_5 <- filter(gal_soro_notificar_sinan, 
                                           gal_soro_notificar_sinan$Idade >4 & 
                                             gal_soro_notificar_sinan$Tipo.Idade == "Ano(s)"&
                                             gal_soro_notificar_sinan$Resultado == "Detectável") 

gal_soro_notificar_sinan_maior_5 <- select (gal_soro_notificar_sinan_maior_5,
                                            Requisição,
                                            Unidade.Solicitante,
                                            Data.do.1º.Sintomas,
                                            Paciente,
                                            Data.de.Nascimento,
                                            Nome.da.Mãe,
                                            Municipio.de.Residência,
                                            Data.da.Liberação,
                                            Resultado,
                                            id_pessoa_exant)

gal_soro_notificar_sinan_maior_5 <- distinct(gal_soro_notificar_sinan_maior_5)
gal_soro_notificar_sinan_maior_5 <- data.frame(gal_soro_notificar_sinan_maior_5)
gal_soro_notificar_sinan_maior_5 <- gal_soro_notificar_sinan_maior_5[!duplicated(gal_soro_notificar_sinan_maior_5), ]
gal_soro_notificar_sinan_maior_5 <- subset(gal_soro_notificar_sinan_maior_5, 
                                           select = -c(id_pessoa_exant)) 


#foi para o sinan, mas não foi para o gal (separar por faixa etária?)
sinan_sem_gal_pcr <- anti_join(data_exant, data_gal_pcr,
                               by = "id_pessoa_exant")

sinan_sem_gal_soro <- anti_join(data_exant, data_gal_soro,
                               by = "id_pessoa_exant")

#separar notificação que tem resultado, porém não está preenchido
data_exant_sem_resul0 <- data_exant %>% filter(is.na(data_exant$ID_S1_IGM))
data_exant_sem_resul1 <- data_exant %>% filter(is.na(data_exant$ID_S1_IGG))
data_exant_sem_resul2 <- data_exant %>% filter(is.na(data_exant$ID_S1_IGM_))
data_exant_sem_resul3 <- data_exant %>% filter(is.na(data_exant$ID_S1_IGG_))
data_exant_sem_resul4 <- data_exant %>% filter(is.na(data_exant$ID_S1_IG_1))
data_exant_sem_resul5 <- data_exant %>% filter(is.na(data_exant$ID_S1_IGM))
data_exant_sem_resul6 <- data_exant %>% filter(is.na(data_exant$ID_S2_IGG))
data_exant_sem_resul7 <- data_exant %>% filter(is.na(data_exant$ID_S2_IGM_))
data_exant_sem_resul8 <- data_exant %>% filter(is.na(data_exant$ID_S2_IGG_))
data_exant_sem_resul9 <- data_exant %>% filter(is.na(data_exant$ID_S2_IG_1))


data_exant_sem_resultado <- bind_rows(data_exant_sem_resul0, data_exant_sem_resul1,
                                      data_exant_sem_resul2, data_exant_sem_resul3,
                                      data_exant_sem_resul4, data_exant_sem_resul5,
                                      data_exant_sem_resul6, data_exant_sem_resul7,
                                      data_exant_sem_resul8, data_exant_sem_resul9)

data_exant_sem_resultado <- distinct(data_exant_sem_resultado)
data_exant_sem_resultado <- data.frame(data_exant_sem_resultado)
data_exant_sem_resultado <- data.frame(data_exant_sem_resultado)

data_exant_inserir_resultado_gal_pcr <- left_join(data_gal_pcr, data_exant_sem_resultado, by = "id_pessoa_exant")
data_exant_inserir_resultado_gal_pcr <- select (data_exant_inserir_resultado_gal_pcr,
                                             Requisição,
                                             Data.do.1º.Sintomas,
                                             Paciente,
                                             Data.de.Nascimento,
                                             Municipio.de.Residência,
                                             Data.da.Liberação,
                                             Resultado,
                                             NU_NOTIFIC,
                                             DT_SIN_PRI,
                                             NM_PACIENT,
                                             DT_NASC,
                                             nm_municipios_PB)


data_exant_inserir_resultado_gal_soro <- left_join(data_gal_soro, data_exant_sem_resultado, by = "id_pessoa_exant")
data_exant_inserir_resultado_gal_soro <- select (data_exant_inserir_resultado_gal_soro,
                                                 Requisição,
                                                 Data.do.1º.Sintomas,
                                                 Paciente,
                                                 Data.de.Nascimento,
                                                 Municipio.de.Residência,
                                                 Data.da.Liberação,
                                                 Resultado,
                                                 NU_NOTIFIC,
                                                 DT_SIN_PRI,
                                                 NM_PACIENT,
                                                 DT_NASC,
                                                 nm_municipios_PB)
data_exant_inserir_resultado_gal_soro <- data_exant_inserir_resultado_gal_soro[!duplicated(data_exant_inserir_resultado_gal_soro), ]

#separar inconsistência de evolução
data_exant <- mutate(data_exant, DT_data_exant)

data_exant_evo_vazia <- data_exant %>% filter(is.na(data_exant$EVOLUCAO))
data_exant_corrigir_evolucao1 <- filter(data_exant,
                                        data_exant$EVOLUCAO != 1,
                                        data_exant$EVOLUCAO != 2,
                                        data_exant$EVOLUCAO != 3)
data_exant_corrigir_evolucao2 <- filter(data_exant,
                                        data_exant$EVOLUCAO == 9)

data_exant_corrigir_evolucao3 <- filter(data_exant,
                                        data_exant$DT_data_exant > '46 days',
                                        data_exant$EVOLUCAO != 1,
                                        data_exant$EVOLUCAO != 2,
                                        data_exant$EVOLUCAO != 3)

data_exant_corrigir_evolucao <-  bind_rows(data_exant_evo_vazia, 
                                           data_exant_corrigir_evolucao1,
                                           data_exant_corrigir_evolucao2,
                                           data_exant_corrigir_evolucao3)

data_exant_corrigir_evolucao <- data.frame(data_exant_corrigir_evolucao) 
data_exant_corrigir_evolucao <- data_exant_corrigir_evolucao[!duplicated(data_exant_corrigir_evolucao), ]


##################fazer o select das colunas

#separar óbito sem classificação

data_exant_obito_sem_class <- data_exant %>% filter(is.na(data_exant$CLASSI_FIN) & 
                                                      data_exant$EVOLUCAO == 2)
data_exant_obito_sem_class <- subset(data_exant_obito_sem_class, 
                                     select = -c(id_pessoa_exant)) 
data_exant_obito_sem_class <- subset(data_exant_obito_sem_class, 
                                     select = -c(DT_data_exant))
data_exant_obito_sem_class <- data_exant_obito_sem_class[!duplicated(data_exant_obito_sem_class), ]


data_outro_obito_sem_class <- data_exant %>% filter(is.na(data_exant$CLASSI_FIN) & 
                                                      data_exant$EVOLUCAO == 3)
data_outro_obito_sem_class <- subset(data_outro_obito_sem_class, 
                                     select = -c(id_pessoa_exant)) 
data_outro_obito_sem_class <- subset(data_outro_obito_sem_class, 
                                     select = -c(DT_data_exant))
data_outro_obito_sem_class <- data_outro_obito_sem_class[!duplicated(data_outro_obito_sem_class), ]


##saidas #nome da aba = #nome do objeto
export(list(duplicidades = duplicidade_exant,
            pcr_notificar_menor_5_anos = gal_pcr_notificar_sinan_menor_5,
            pcr_notificar_maior_5_anos = gal_pcr_notificar_sinan_maior_5,
            soro_notificar_menor_5_anos = gal_soro_notificar_sinan_menor_5,
            soro_notificar_maior_5_anos = gal_soro_notificar_sinan_maior_5,
            sinan_sem_gal_pcr = sinan_sem_gal_pcr,
            sinan_sem_gal_soro = sinan_sem_gal_soro,
            inserir_resul_gal_pcr = data_exant_inserir_resultado_gal_pcr,
            inserir_resul_gal_soro = data_exant_inserir_resultado_gal_soro,
            corrigir_evolucao = data_exant_corrigir_evolucao,
            obito_sem_class = data_outro_obito_sem_class),
       file = "Qualificar_sarampo.xlsx")

######## FIM