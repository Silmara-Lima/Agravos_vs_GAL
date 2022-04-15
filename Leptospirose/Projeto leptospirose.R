library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")
library("rio")

#abrir arquivos
data_lepto <- read.dbf("C:/Users/NDTA/Desktop/Projeto Leptospirose/entradas/LEPTONET.dbf")
data_gal_lepto <- read.csv("C:/Users/NDTA/Desktop/Projeto Leptospirose/entradas/data.csv", header = TRUE, sep = ";")

#ajustar datas
data_gal_lepto$Data.de.Nascimento <- dmy(data_gal_lepto$Data.de.Nascimento)
data_gal_lepto$Data.do.1º.Sintomas <- dmy(data_gal_lepto$Data.do.1º.Sintomas)

#ajustar datas
data_lepto$DT_SIN_PRI <- as.Date(data_lepto$DT_SIN_PRI, format="%d-%m-%Y")
data_lepto$DT_NASC <- as.Date(data_lepto$DT_NASC, format="%d-%m-%Y")

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

RemoveAcentos(data_lepto$NM_PACIENT)
RemoveAcentos(data_gal_lepto$Paciente)



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
data_lepto <- rename(data_lepto, cod_ibge = ID_MN_RESI)
data_lepto <- left_join(data_lepto, municipios_PB, by = "cod_ibge")

#criar chave
id_pessoa_lepto <- paste(data_lepto$NM_PACIENT,
                         data_lepto$DT_NASC,
                         data_lepto$cod_ibge)

id_pessoa_gal_lepto <- paste(data_gal_lepto$Paciente,
                             data_gal_lepto$Data.de.Nascimento,
                             data_gal_lepto$IBGE.Município.de.Residência)

#acrescentar coluna com ID_Pessoa
data_lepto <- mutate(data_lepto, id_pessoa_lepto)
data_gal_lepto <- mutate(data_gal_lepto, id_pessoa_gal_lepto)

# separar duplicidades
DT_data_lepto <- Sys.Date() - data_lepto$DT_SIN_PRI
sum(duplicated(data_lepto$id_pessoa_lepto))

duplicidade_lepto <- data_lepto[duplicated(data_lepto$id_pessoa_lepto), ]
duplicidade_lepto <- subset(duplicidade_lepto, 
                            select = -c(id_pessoa_lepto)) 
duplicidade_lepto <- subset(duplicidade_lepto, 
                            select = -c(DT_data_lepto))

#separar gal que não está notificado
data_gal_lepto <- rename(data_gal_lepto, id_pessoa_lepto = id_pessoa_gal_lepto)
gal_notificar_sinan <- anti_join(data_gal_lepto, data_lepto,
                                 by = "id_pessoa_lepto")
gal_notificar_sinan <- select (gal_notificar_sinan,
                               Requisição,
                               Data.do.1º.Sintomas,
                               Paciente,
                               Data.de.Nascimento,
                               Municipio.de.Residência,
                               Data.da.Liberação,
                               Resultado)

#foi para o sinan, mas não foi para o gal
sinan_mas_sem_gal <- anti_join(data_lepto, data_gal_lepto,
                               by = "id_pessoa_lepto")

sinan_mas_sem_gal <- subset(sinan_mas_sem_gal, 
                            select = -c(id_pessoa_lepto))


#separar notificação que tem resultado, porém não está preenchido
data_lepto_sem_resul0 <- data_lepto %>% filter(is.na(data_lepto$LAB_ELIS_1))
data_lepto_sem_resul1 <- data_lepto %>% filter(is.na(data_lepto$LAB_ELIS_2))
data_lepto_sem_resul2 <- data_lepto %>% filter(is.na(data_lepto$LAB_MICR_1))
data_lepto_sem_resul3 <- data_lepto %>% filter(is.na(data_lepto$LAB_MICR_2))
data_lepto_sem_resul4 <- data_lepto %>% filter(is.na(data_lepto$MICRO2_S1))
data_lepto_sem_resul5 <- data_lepto %>% filter(is.na(data_lepto$MICRO2_S_2))
data_lepto_sem_resul6 <- data_lepto %>% filter(is.na(data_lepto$RES_ISOL))
data_lepto_sem_resul7 <- data_lepto %>% filter(is.na(data_lepto$RES_IMUNO))
data_lepto_sem_resul8 <- data_lepto %>% filter(is.na(data_lepto$RES_PCR))

data_lepto_sem_resultado <- bind_rows(data_lepto_sem_resul0, data_lepto_sem_resul1,
                                      data_lepto_sem_resul2, data_lepto_sem_resul3,
                                      data_lepto_sem_resul4, data_lepto_sem_resul5,
                                      data_lepto_sem_resul6, data_lepto_sem_resul7,
                                      data_lepto_sem_resul8)

data_lepto_sem_resultado <- unique(data_lepto_sem_resultado)
data_lepto_sem_resultado <- data.frame(data_lepto_sem_resultado)

data_lepto_inserir_resultado_gal <- left_join(data_gal_lepto, data_lepto_sem_resultado, by = "id_pessoa_lepto")
data_lepto_inserir_resultado_gal <- select ( data_lepto_vs_gal,
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

#separar inconsistência de evolução
data_lepto <- mutate(data_lepto, DT_data_lepto)

data_lepto_evo_vazia <- data_lepto %>% filter(is.na(data_lepto$EVOLUCAO))
data_lepto_corrigir_evolucao1 <- filter(data_lepto,
                                        data_lepto$EVOLUCAO != 1,
                                        data_lepto$EVOLUCAO != 2,
                                        data_lepto$EVOLUCAO != 3)
data_lepto_corrigir_evolucao2 <- filter(data_lepto,
                                        data_lepto$EVOLUCAO == 9)

data_lepto_corrigir_evolucao3 <- filter(data_lepto,
                                        data_lepto$DT_data_lepto > '46 days',
                                        data_lepto$EVOLUCAO != 1,
                                        data_lepto$EVOLUCAO != 2,
                                        data_lepto$EVOLUCAO != 3)

data_lepto_corrigir_evolucao <-  bind_rows(data_lepto_evo_vazia, 
                                           data_lepto_corrigir_evolucao1,
                                           data_lepto_corrigir_evolucao2,
                                           data_lepto_corrigir_evolucao3)

data_lepto_corrigir_evolucao <- unique(data_lepto_corrigir_evolucao)
data_lepto_corrigir_evolucao <- data.frame(data_lepto_corrigir_evolucao)                                           

##################fazer o select das colunas

#separar óbito sem classificação

data_lepto_obito_sem_class <- data_lepto %>% filter(is.na(data_lepto$CLASSI_FIN) & 
                                                      data_lepto$EVOLUCAO == 2)
data_lepto_obito_sem_class <- subset(data_lepto_obito_sem_class, 
                                     select = -c(id_pessoa_lepto)) 
data_lepto_obito_sem_class <- subset(data_lepto_obito_sem_class, 
                                     select = -c(DT_data_lepto))


data_outro_obito_sem_class <- data_lepto %>% filter(is.na(data_lepto$CLASSI_FIN) & 
                                                      data_lepto$EVOLUCAO == 3)
data_outro_obito_sem_class <- subset(data_outro_obito_sem_class, 
                                     select = -c(id_pessoa_lepto)) 
data_outro_obito_sem_class <- subset(data_outro_obito_sem_class, 
                                     select = -c(DT_data_lepto))

##saidas #nome da aba = #nome do objeto
export(list(duplicidade_lepto = duplicidade_lepto,
            gal_notificar_sinan = gal_notificar_sinan, 
            sinan_mas_sem_gal = sinan_mas_sem_gal,
            inserir_resultado_gal = data_lepto_inserir_resultado_gal,
            data_lepto_corrigir_evolucao = data_lepto_corrigir_evolucao,
            data_lepto_obito_sem_class = data_lepto_obito_sem_class,
            data_outro_obito_sem_class = data_outro_obito_sem_class),
       file = "Qualificar_LEPTO.xlsx")

########