library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("rio")

#abrir arquivos
data_dengue <- read.dbf("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto arboviroses vs gal/entrada/DENGON.dbf")
data_chik <- read.dbf("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto arboviroses vs gal/entrada/CHIKON.dbf")
data_zika  <- read.dbf("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto arboviroses vs gal/entrada/NINDINET.dbf")
data_gal  <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto arboviroses vs gal/entrada/arboviroses.xlsx" , sheet = 1, col_names=TRUE)

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

#filtrar somente zika
data_zika <- data.frame(filter(data_zika, data_zika$ID_AGRAVO == 'A928')) 

data_dengue$RESUL_PCR_ <- as.numeric(levels(data_dengue$RESUL_PCR_))[data_dengue$RESUL_PCR_]

RemoveAcentos(data_dengue$NM_PACIENT)
RemoveAcentos(data_chik$NM_PACIENT)
RemoveAcentos(data_zika$NM_PACIENT)
RemoveAcentos(data_gal$Paciente)

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
data_dengue <- rename(data_dengue, cod_ibge = ID_MN_RESI)
data_dengue <- left_join(data_dengue, municipios_PB, by = "cod_ibge")

data_chik <- rename(data_chik, cod_ibge = ID_MN_RESI)
data_chik <- left_join(data_chik, municipios_PB, by = "cod_ibge")

data_zika <- rename(data_zika, cod_ibge = ID_MN_RESI)
data_zika <- left_join(data_zika, municipios_PB, by = "cod_ibge")

#separar gal por agravo
#gal_dengue
data_gal_dengue <- filter(data_gal,
                          data_gal$Exame == "Dengue, Biologia Molecular" |
                          data_gal$Exame == "Dengue, Detecção de Antígeno NS1" |
                          data_gal$Exame == "Dengue, Detecção de Antígeno NS2" |
                          data_gal$Exame == "Dengue, Isolamento Viral" |
                          data_gal$Exame == "Dengue, IgM")

#gal_chik
data_gal_chik <- filter(data_gal,
                          data_gal$Exame == "Chikungunya, Biologia Molecular" |
                          data_gal$Exame == "Chikungunya, IgG" |
                          data_gal$Exame == "Chikungunya, IgM" |
                          data_gal$Exame == "Chikungunya, Isolamento Viral" |
                          data_gal$Exame == "Chikungunya, Detecção de Antígeno NS1" |
                          data_gal$Exame == "Chikungunya, Detecção de Antígeno NS2")
                            
#gal_zika
data_gal_zika <- filter(data_gal,
                        data_gal$Exame == "Zika, Biologia Molecular" |
                          data_gal$Exame == "Zika,IgM" |
                          data_gal$Exame == "Zika,IgG" |
                          data_gal$Exame == "Zika, Isolamento Viral")

#filtrar dengue não preenchido resultado
data_dengue <- filter( data_dengue,
                       data_dengue$RES_CHIKS1 != "1" |
                       data_dengue$RES_CHIKS2 != "1" |
                       data_dengue$RESUL_PRNT != "1" |
                       data_dengue$RESUL_SORO != "1" |
                       data_dengue$RESUL_NS1 != "1" |
                       data_dengue$RESUL_VI_N != "1" |
                       data_dengue$RESUL_PCR_ != "1" |
                       data_dengue$RES_CHIKS1 != "2" |
                       data_dengue$RES_CHIKS2 != "2" |
                       data_dengue$RESUL_PRNT != "2" |
                       data_dengue$RESUL_SORO != "2" |
                       data_dengue$RESUL_NS1 != "2" |
                       data_dengue$RESUL_VI_N != "2" |
                       data_dengue$RESUL_PCR_ != "2")

                       
#filtrar chik não preenchido resultado
data_chik <- filter( data_chik,
                     data_chik$RES_CHIKS1 != "1" |
                       data_chik$RES_CHIKS2 != "1" |
                       data_chik$RESUL_PRNT != "1" |
                       data_chik$RESUL_SORO != "1" |
                       data_chik$RESUL_NS1 != "1" |
                       data_chik$RESUL_VI_N != "1" |
                       data_chik$RESUL_PCR_ != "1" |
                       data_chik$RES_CHIKS1 != "2" |
                       data_chik$RES_CHIKS2 != "2" |
                       data_chik$RESUL_PRNT != "2" |
                       data_chik$RESUL_SORO != "2" |
                       data_chik$RESUL_NS1 != "2" |
                       data_chik$RESUL_VI_N != "2" |
                       data_chik$RESUL_PCR_ != "2")
                     
#filtrar zika não preenchido resultado
data_zika <- filter( data_zika,
                     data_zika$CLASSI_FIN != 1 ||
                     data_zika$CLASSI_FIN != 2)


#criação de ID_Pessoa dengue
id_pessoa_dengue <- paste(data_dengue$NM_PACIENT,
                          data_dengue$DT_NASC,
                          data_dengue$cod_ibge)

#criação de ID_Pessoa zika
id_pessoa_zika <- paste(data_zika$NM_PACIENT,
                          data_zika$DT_NASC,
                          data_zika$cod_ibge)

#criação de ID_Pessoa dengue
id_pessoa_chik <- paste(data_chik$NM_PACIENT,
                        data_chik$DT_NASC,
                        data_chik$cod_ibge)


#criação de ID_Pessoa gal
id_pessoa_gal <-  paste(data_gal$Paciente,
                        data_gal$`Data de Nascimento`,
                        data_gal$`IBGE Município de Residência`)


#acrescentar coluna com ID_Pessoa
data_dengue <- mutate(data_dengue, id_pessoa_dengue)
data_zika <- mutate(data_zika, id_pessoa_zika)
data_chik <- mutate(data_chik, id_pessoa_chik)
data_gal_dengue <- mutate(data_gal, id_pessoa_gal)
data_gal_chik <- mutate(data_gal, id_pessoa_gal)
data_gal_zika <- mutate(data_gal, id_pessoa_gal)


# separar duplicidades
#dengue
DT_data_dengue <- Sys.Date() - data_dengue$DT_NOTIFIC
sum(duplicated(data_dengue$id_pessoa_dengue))

duplicidade_dengue <- data_dengue[duplicated(data_dengue$id_pessoa_dengue), ]
duplicidade_dengue <- subset(duplicidade_dengue, 
                            select = -c(id_pessoa_dengue)) 
duplicidade_dengue <- subset(duplicidade_dengue, 
                            select = -c(DT_data_dengue))

#chik
DT_data_chik <- Sys.Date() - data_chik$DT_NOTIFIC
sum(duplicated(data_chik$id_pessoa_chik))

duplicidade_chik<- data_chik[duplicated(data_chik$id_pessoa_chik), ]
duplicidade_chik <- subset(duplicidade_chik, 
                             select = -c(id_pessoa_chik)) 
duplicidade_chik <- subset(duplicidade_chik, 
                             select = -c(DT_data_chik))

#zika
DT_data_zika <- Sys.Date() - data_zika$DT_NOTIFIC
sum(duplicated(data_zika$id_pessoa_zika))

duplicidade_zika <- data_zika[duplicated(data_zika$id_pessoa_zika), ]
duplicidade_zika <- subset(duplicidade_zika, 
                           select = -c(id_pessoa_zika)) 
duplicidade_zika <- subset(duplicidade_zika, 
                           select = -c(DT_data_zika))

#renomear colunas para fazer join
data_dengue <- data_dengue %>% rename("id_pessoa_gal" = "id_pessoa_dengue")
data_zika <- data_zika %>% rename("id_pessoa_gal" = "id_pessoa_zika")
data_chik <- data_chik %>% rename("id_pessoa_gal" = "id_pessoa_chik")

#separar gal que não está notificado
#dengue
gal_dengue_notificar <- anti_join(data_gal_dengue, data_dengue,
                                 by = "id_pessoa_gal")

gal_dengue_notificar <- gal_dengue_notificar[!duplicated(gal_dengue_notificar$id_pessoa_gal), ]
gal_dengue_notificar <- select (gal_dengue_notificar,
                                Requisição,
                                Paciente,
                                'Data de Nascimento',
                                'Municipio de Residência',
                                'Data da Liberação',
                                '1º Campo Resultado',
                                '2º Campo Resultado')

#chik
gal_chik_notificar <- anti_join(data_gal_chik, data_chik,
                                  by = "id_pessoa_gal")
gal_chik_notificar <- gal_chik_notificar[!duplicated(gal_chik_notificar$id_pessoa_gal), ]

gal_chik_notificar <- select (gal_chik_notificar,
                                Requisição,
                                Paciente,
                                'Data de Nascimento',
                                'Municipio de Residência',
                                'Data da Liberação',
                                '1º Campo Resultado',
                                '2º Campo Resultado')

#zika
gal_zika_notificar <- anti_join(data_gal_zika, data_zika,
                                by = "id_pessoa_gal")
gal_zika_notificar <- gal_zika_notificar[!duplicated(gal_zika_notificar$id_pessoa_gal), ]
gal_zika_notificar <- select (gal_zika_notificar,
                              Requisição,
                              Paciente,
                              'Data de Nascimento',
                              'Municipio de Residência',
                              'Data da Liberação',
                              '1º Campo Resultado',
                              '2º Campo Resultado')

#foi para o sinan, mas não foi para o gal
#dengue
sinan_dengue_sem_gal <- anti_join(data_dengue, data_gal_dengue,
                               by = "id_pessoa_gal")

sinan_dengue_sem_gal <- subset(sinan_dengue_sem_gal, 
                            select = -c(id_pessoa_gal))
sinan_dengue_sem_gal <- sinan_dengue_sem_gal[!duplicated(sinan_dengue_sem_gal$id_pessoa_gal), ]

#chik
sinan_chik_sem_gal <- anti_join(data_chik, data_gal_chik,
                                  by = "id_pessoa_gal")

sinan_chik_sem_gal <- subset(sinan_chik_sem_gal, 
                               select = -c(id_pessoa_gal))
sinan_chik_sem_gal <- sinan_chik_sem_gal[!duplicated(sinan_chik_sem_gal$id_pessoa_gal), ]

#zika
sinan_zika_sem_gal <- anti_join(data_zika, data_gal_zika,
                                by = "id_pessoa_gal")

sinan_zika_sem_gal <- subset(sinan_zika_sem_gal, 
                             select = -c(id_pessoa_gal))
sinan_zika_sem_gal <- sinan_zika_sem_gal[!duplicated(sinan_zika_sem_gal$id_pessoa_gal), ]

#separar notificação que tem resultado, porém não está preenchido
#dengue
data_dengue_sem_resul0 <- data_dengue %>% filter(is.na(data_dengue$RES_CHIKS1))
data_dengue_sem_resul1 <- data_dengue %>% filter(is.na(data_dengue$RES_CHIKS2))
data_dengue_sem_resul2 <- data_dengue %>% filter(is.na(data_dengue$RESUL_PRNT))
data_dengue_sem_resul3 <- data_dengue %>% filter(is.na(data_dengue$RESUL_SORO))
data_dengue_sem_resul4 <- data_dengue %>% filter(is.na(data_dengue$RESUL_NS1))
data_dengue_sem_resul5 <- data_dengue %>% filter(is.na(data_dengue$RESUL_PCR_))

data_dengue_sem_resultado <- bind_rows(data_dengue_sem_resul0, data_dengue_sem_resul1,
                                      data_dengue_sem_resul2, data_dengue_sem_resul3,
                                      data_dengue_sem_resul4, data_dengue_sem_resul5)

data_dengue_inserir_resultado_gal <- left_join(data_gal_dengue, data_dengue_sem_resultado, by = "id_pessoa_gal")
data_dengue_inserir_resultado_gal <- select (data_dengue_inserir_resultado_gal,
                                             Requisição,
                                             Paciente,
                                             "Data de Nascimento",
                                             "Municipio de Residência",
                                             "Data da Liberação",
                                             "Exame",
                                             "1º Campo Resultado",
                                             "2º Campo Resultado",
                                             NU_NOTIFIC,
                                             DT_SIN_PRI,
                                             NM_PACIENT,
                                             DT_NASC,
                                             nm_municipios_PB)

data_dengue_inserir_resultado_gal <- data_dengue_inserir_resultado_gal[!duplicated(data_dengue_inserir_resultado_gal), ]

#chik
data_chik_sem_resul0 <- data_chik %>% filter(is.na(data_chik$RES_CHIKS1))
data_chik_sem_resul1 <- data_chik %>% filter(is.na(data_chik$RES_CHIKS2))
data_chik_sem_resul2 <- data_chik %>% filter(is.na(data_chik$RESUL_PRNT))
data_chik_sem_resul3 <- data_chik %>% filter(is.na(data_chik$RESUL_SORO))
data_chik_sem_resul4 <- data_chik %>% filter(is.na(data_chik$RESUL_NS1))
data_chik_sem_resul5 <- data_chik %>% filter(is.na(data_chik$RESUL_PCR_))

data_chik_sem_resultado <- bind_rows(data_chik_sem_resul0, data_chik_sem_resul1,
                                     data_chik_sem_resul2, data_chik_sem_resul3,
                                     data_chik_sem_resul4, data_chik_sem_resul5)

data_chik_inserir_resultado_gal <- left_join(data_gal_chik, data_chik_sem_resultado, by = "id_pessoa_gal")
data_chik_inserir_resultado_gal <- select (data_chik_inserir_resultado_gal,
                                           Requisição,
                                           Paciente,
                                           "Data de Nascimento",
                                           "Municipio de Residência",
                                           "Data da Liberação",
                                           "Exame",
                                           "1º Campo Resultado",
                                           "2º Campo Resultado",
                                           NU_NOTIFIC,
                                           DT_SIN_PRI,
                                           NM_PACIENT,
                                           DT_NASC,
                                           nm_municipios_PB)

data_chik_inserir_resultado_gal <- data_chik_inserir_resultado_gal[!duplicated(data_chik_inserir_resultado_gal), ]

#zika
data_zika_sem_resul <- data_zika %>% filter(is.na(data_zika$CLASSI_FIN))

data_zika_inserir_resultado_gal <- left_join(data_gal_zika, data_zika_sem_resul, by = "id_pessoa_gal")
data_zika_inserir_resultado_gal <- select (data_zika_inserir_resultado_gal,
                                           Requisição,
                                           Paciente,
                                           "Data de Nascimento",
                                           "Municipio de Residência",
                                           "Data da Liberação",
                                           "Exame",
                                           "1º Campo Resultado",
                                           "2º Campo Resultado",
                                           NU_NOTIFIC,
                                           DT_SIN_PRI,
                                           NM_PACIENT,
                                           DT_NASC,
                                           nm_municipios_PB)

data_zika_inserir_resultado_gal <- data_zika_inserir_resultado_gal[!duplicated(data_zika_inserir_resultado_gal), ]

#separar inconsistência de evolução
#dengue
data_dengue <- mutate(data_dengue, DT_data_dengue)

data_dengue_evo_vazia <- data_dengue %>% filter(is.na(data_dengue$EVOLUCAO))
data_dengue_corrigir_evolucao1 <- filter(data_dengue,
                                        data_dengue$EVOLUCAO != 1,
                                        data_dengue$EVOLUCAO != 2,
                                        data_dengue$EVOLUCAO != 3)
data_dengue_corrigir_evolucao2 <- filter(data_dengue,
                                        data_dengue$EVOLUCAO == 9)

data_dengue_corrigir_evolucao3 <- filter(data_dengue,
                                         data_dengue$DT_data_dengue > '46 days',
                                         data_dengue$EVOLUCAO != 1,
                                         data_dengue$EVOLUCAO != 2,
                                         data_dengue$EVOLUCAO != 3)

data_dengue_corrigir_evolucao <-  bind_rows(data_dengue_evo_vazia, 
                                           data_dengue_corrigir_evolucao1,
                                           data_dengue_corrigir_evolucao2,
                                           data_dengue_corrigir_evolucao3)

data_dengue_corrigir_evolucao <- data_dengue_corrigir_evolucao[!duplicated(data_dengue_corrigir_evolucao), ]

#chik
data_chik <- mutate(data_chik, DT_data_chik)

data_chik_evo_vazia <- data_chik %>% filter(is.na(data_chik$EVOLUCAO))
data_chik_corrigir_evolucao1 <- filter(data_chik,
                                         data_chik$EVOLUCAO != 1,
                                         data_chik$EVOLUCAO != 2,
                                         data_chik$EVOLUCAO != 3)
data_chik_corrigir_evolucao2 <- filter(data_chik,
                                         data_chik$EVOLUCAO == 9)

data_chik_corrigir_evolucao3 <- filter(data_chik,
                                         data_chik$DT_data_chik > '46 days',
                                         data_chik$EVOLUCAO != 1,
                                         data_chik$EVOLUCAO != 2,
                                         data_chik$EVOLUCAO != 3)

data_chik_corrigir_evolucao <-  bind_rows(data_chik_evo_vazia, 
                                            data_chik_corrigir_evolucao1,
                                            data_chik_corrigir_evolucao2,
                                            data_chik_corrigir_evolucao3)

data_chik_corrigir_evolucao <- data_chik_corrigir_evolucao[!duplicated(data_chik_corrigir_evolucao), ]

#zika
data_zika <- mutate(data_zika, DT_data_zika)

data_zika_evo_vazia <- data_zika %>% filter(is.na(data_zika$EVOLUCAO))
data_zika_corrigir_evolucao1 <- filter(data_zika,
                                       data_zika$EVOLUCAO != 1,
                                       data_zika$EVOLUCAO != 2,
                                       data_zika$EVOLUCAO != 3)
data_zika_corrigir_evolucao2 <- filter(data_zika,
                                       data_zika$EVOLUCAO == 9)

data_zika_corrigir_evolucao3 <- filter(data_zika,
                                       data_zika$DT_data_zika > '46 days',
                                       data_zika$EVOLUCAO != 1,
                                       data_zika$EVOLUCAO != 2,
                                       data_zika$EVOLUCAO != 3)

data_zika_corrigir_evolucao <-  bind_rows(data_zika_evo_vazia, 
                                          data_zika_corrigir_evolucao1,
                                          data_zika_corrigir_evolucao2,
                                          data_zika_corrigir_evolucao3)

data_zika_corrigir_evolucao <- data_zika_corrigir_evolucao[!duplicated(data_zika_corrigir_evolucao), ]

#separar óbito sem classificação
#dengue
data_dengue_obito_sem_class <- data_dengue %>% filter(is.na(data_dengue$CLASSI_FIN) & 
                                                        data_dengue$EVOLUCAO == 2)
data_dengue_obito_sem_class <- subset(data_dengue_obito_sem_class, 
                                     select = -c(id_pessoa_gal)) 
data_dengue_obito_sem_class <- subset(data_dengue_obito_sem_class, 
                                     select = -c(DT_data_dengue))

data_outro_dengue_obito_sem_class <- data_dengue %>% filter(is.na(data_dengue$CLASSI_FIN) & 
                                                             data_dengue$EVOLUCAO == 3)
data_outro_dengue_obito_sem_class <- subset(data_outro_dengue_obito_sem_class, 
                                     select = -c(id_pessoa_gal)) 
data_outro_dengue_obito_sem_class <- subset(data_outro_dengue_obito_sem_class, 
                                     select = -c(DT_data_dengue))

#chik
data_chik_obito_sem_class <- data_chik %>% filter(is.na(data_chik$CLASSI_FIN) & 
                                                    data_chik$EVOLUCAO == 2)
data_chik_obito_sem_class <- subset(data_chik_obito_sem_class, 
                                      select = -c(id_pessoa_gal)) 
data_chik_obito_sem_class <- subset(data_chik_obito_sem_class, 
                                      select = -c(DT_data_chik))

data_outro_chik_obito_sem_class <- data_chik %>% filter(is.na(data_chik$CLASSI_FIN) & 
                                                              data_chik$EVOLUCAO == 3)
data_outro_chik_obito_sem_class <- subset(data_outro_chik_obito_sem_class, 
                                            select = -c(id_pessoa_gal)) 
data_outro_chik_obito_sem_class <- subset(data_outro_chik_obito_sem_class, 
                                            select = -c(DT_data_chik))

#chik
data_zika_obito_sem_class <- data_zika %>% filter(is.na(data_zika$CLASSI_FIN) & 
                                                    data_zika$EVOLUCAO == 2)
data_zika_obito_sem_class <- subset(data_zika_obito_sem_class, 
                                    select = -c(id_pessoa_gal)) 
data_zika_obito_sem_class <- subset(data_zika_obito_sem_class, 
                                    select = -c(DT_data_zika))

data_outro_zika_obito_sem_class <- data_zika %>% filter(is.na(data_zika$CLASSI_FIN) & 
                                                          data_zika$EVOLUCAO == 3)
data_outro_zika_obito_sem_class <- subset(data_outro_zika_obito_sem_class, 
                                          select = -c(id_pessoa_gal)) 
data_outro_zika_obito_sem_class <- subset(data_outro_zika_obito_sem_class, 
                                          select = -c(DT_data_zika))


#olhar a parte do if para o sorotipo (preencher sorotipo)

##saidas #nome da aba = #nome do objeto
export(list(duplicidade_dengue = duplicidade_dengue,
            duplicidade_chik = duplicidade_chik, 
            duplicidade_zika = duplicidade_zika,
            gal_dengue_notificar = gal_dengue_notificar,
            gal_chik_notificar = gal_chik_notificar,
            gal_zika_notificar = gal_zika_notificar,
            sinan_dengue_sem_gal = sinan_dengue_sem_gal,
            sinan_chik_sem_gal = sinan_chik_sem_gal,
            sinan_zika_sem_gal = sinan_zika_sem_gal,
            dengue_inserir_resul_gal = data_dengue_inserir_resultado_gal,
            chik_inserir_resul_gal = data_chik_inserir_resultado_gal,
            zika_inserir_resul_gal = data_zika_inserir_resultado_gal,
            dengue_corrigir_evo = data_dengue_corrigir_evolucao,
            chik_corrigir_evo = data_chik_corrigir_evolucao,
            zika_corrigir_evo = data_zika_corrigir_evolucao,
            dengue_obito_sem_class = data_dengue_obito_sem_class,
            chik_obito_sem_class = data_chik_obito_sem_class,
            zika_obito_sem_class = data_zika_obito_sem_class,
            obito_outrod_sem_class = data_outro_dengue_obito_sem_class,
            obito_outroc_sem_class = data_outro_chik_obito_sem_class,
            obito_outroz_sem_class = data_outro_zika_obito_sem_class),
            file = "Qualificar_ARBO.xlsx")
#####FIM
