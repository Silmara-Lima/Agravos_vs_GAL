install.packages("lubridate")
install.packages("stringr")
install.packages("tidyverse")
install.packages("foreign")
install.packages("writexl")
install.packages("dplyr")
library("lubridate")
library("stringr")
library("tidyverse")
library("foreign")
library("writexl")
library("dplyr")

#abrir arquivos
data_srag <- read.dbf("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/entrada/sivepsrag.dbf")
data_gal_covid <- read.csv("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/entrada/lacen_covid.csv", header = TRUE, sep = ";")
data_gal_flu <- read.csv("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/entrada/lacen_flu.csv", header = TRUE, sep = ";")


#função remover acentos
RemoveAcentos <- function(textoComAcentos) {
  if(!is.character(textoComAcentos)){
    on.exit()
  }
  letrasComAcentos <- "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨"
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "
  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  ) 
  return(textoSemAcentos)
}

RemoveAcentos(data_srag$NM_PACIENT)
RemoveAcentos(data_gal_covid$Paciente)
RemoveAcentos(data_gal_flu$Paciente)



#ajustar datas
data_srag$DT_SIN_PRI <- dmy(data_srag$DT_SIN_PRI)
data_srag$DT_NASC <- dmy(data_srag$DT_NASC)
data_gal_covid$Data.de.Nascimento <- dmy(data_gal_covid$Data.de.Nascimento)
data_gal_covid$Data.do.1º.Sintomas <- dmy(data_gal_covid$Data.do.1º.Sintomas)
data_gal_flu$Data.de.Nascimento <- dmy(data_gal_flu$Data.de.Nascimento)
data_gal_flu$Data.do.1º.Sintomas <- dmy(data_gal_flu$Data.do.1º.Sintomas)

#ajustar números
data_srag$REQUI_GAL <- as.numeric(levels(data_srag$REQUI_GAL))[data_srag$REQUI_GAL]
#data_gal$Requisição <- as.numeric(levels(data_gal$Requisição))[data_gal$Requisição]
data_srag$PCR_RESUL <- as.numeric(levels(data_srag$PCR_RESUL))[data_srag$PCR_RESUL]

#filtrar srag não preenchido covid
srag_n_covid <- filter( data_srag,
                        data_srag$PCR_RESUL != 1, 
                        data_srag$OBSERVA != "transferido",
                        data_srag$OBSERVA != "transferida",
                        data_srag$OBSERVA != "regulado para",
                        data_srag$OBSERVA != "regulada para",
                        data_srag$OBSERVA != "regulado pra",
                        data_srag$OBSERVA != "regulada pra")

#criação de ID_Pessoa srag
id_pessoa_srag <- paste(srag_n_covid$NM_PACIENT,
                        srag_n_covid$DT_NASC,
                        srag_n_covid$CO_MUN_RES,
                        srag_n_covid$DT_SIN_PRI)


#criação de ID_Pessoa gal
id_pessoa_gal_covid <-  paste(data_gal_covid$Paciente,
                        data_gal_covid$Data.de.Nascimento,
                        data_gal_covid$IBGE.Município.de.Residência, 
                        data_gal_covid$Data.do.1º.Sintomas)

id_pessoa_gal_flu <-  paste(data_gal_flu$Paciente,
                            data_gal_flu$Data.de.Nascimento,
                            data_gal_flu$IBGE.Município.de.Residência, 
                            data_gal_flu$Data.do.1º.Sintomas)


#acrescentar coluna com ID_Pessoa
srag_n_covid <- mutate(srag_n_covid, id_pessoa_srag)
data_gal_covid <- mutate(data_gal_covid, id_pessoa_gal_covid)
data_gal_flu <- mutate(data_gal_flu, id_pessoa_gal_flu)

#join srag com gal (encontrados)
data_srag_gal_covid0 <- merge(srag_n_covid, data_gal_covid, by.x = "REQUI_GAL", by.y = "Requisição")
data_srag_gal_covid1 <- merge(srag_n_covid, data_gal_covid, by.x = "id_pessoa_srag", by.y = "id_pessoa_gal_covid")
data_srag_gal_flu0 <- merge(srag_n_covid, data_gal_flu, by.x = "REQUI_GAL", by.y = "Requisição")
data_srag_gal_flu1 <- merge(srag_n_covid, data_gal_flu, by.x = "id_pessoa_srag", by.y = "id_pessoa_gal_flu")

#join srag com gal (encontrados)
srag_covid_gal <- bind_rows(data_srag_gal_covid0, data_srag_gal_covid1)
srag_covid_gal <- srag_covid_gal %>% filter (! duplicated(srag_covid_gal$id_pessoa_srag))
srag_flu_gal <- bind_rows(data_srag_gal_flu0, data_srag_gal_flu1)
srag_flu_gal <- srag_flu_gal %>% filter (! duplicated(srag_flu_gal$id_pessoa_srag))

#encontrar os dados que sobraram no srag_n_covid (que não foi encontrado no cruzamento com gal) com o anti join
srag_n_covid_NA0 <- anti_join(srag_n_covid, data_srag_gal_covid0, by.x = "REQUI_GAL", by.y = "Requisição")
srag_n_covid_NA1 <- anti_join(srag_n_covid, data_srag_gal_covid1, by.x = "id_pessoa_srag", by.y = "id_pessoa_gal_covid")
srag_n_flu_NA0 <- anti_join(srag_n_covid, data_srag_gal_flu0, by.x = "REQUI_GAL", by.y = "Requisição")
srag_n_flu_NA1 <- anti_join(srag_n_covid, data_srag_gal_flu1, by.x = "id_pessoa_srag", by.y = "id_pessoa_gal_flu")

#união dos que não foram encontrados no cruzamento com o gal
srag_n_covid_NA <- bind_rows(srag_n_covid_NA0, srag_n_covid_NA1)
srag_n_covid_NA <- srag_n_covid_NA %>% filter (! duplicated(srag_n_covid_NA$id_pessoa_srag))
srag_n_flu_NA <- bind_rows(srag_n_flu_NA0, srag_n_flu_NA1)
srag_n_flu_NA <- srag_n_flu_NA %>% filter (! duplicated(srag_n_flu_NA$id_pessoa_srag))

#filtrar as saídas
#covid detectável, não detectável e não econtrado
#flu detectável, não detectável e não econtrado
srag_covid_gal_detec1 <- filter(srag_covid_gal, srag_covid_gal$Resultado == "Detectável") 
srag_covid_gal_detec <- select ( srag_covid_gal_detec1,
                                 NU_NOTIFIC, DT_SIN_PRI, 
                                 NM_PACIENT, ID_MN_RESI, 
                                 DT_INTERNA, NM_UN_INTE,
                                 REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                                 CLASSI_FIN, EVOLUCAO, 
                                 Resultado, Data.da.Liberação, 
                                 RES_AN, OBSERVA)

srag_covid_gal_ND1 <- filter(srag_covid_gal, srag_covid_gal$Resultado == "Não Detectável") 
srag_covid_gal_ND <- select ( srag_covid_gal_ND1,
                                 NU_NOTIFIC, DT_SIN_PRI, 
                                 NM_PACIENT, ID_MN_RESI, 
                                 DT_INTERNA, NM_UN_INTE,
                                 REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                                 CLASSI_FIN, EVOLUCAO, 
                                 Resultado, Data.da.Liberação, 
                                 RES_AN, OBSERVA)


srag_covid_gal_NA1 <- srag_covid_gal %>% filter(is.na(srag_covid_gal$Resultado)|srag_covid_gal$Resultado == "Inconclusivo")
srag_covid_gal_NA2 <- srag_covid_gal %>% filter(srag_covid_gal$Resultado == "Inconclusivo")
srag_covid_gal_NA <- bind_rows(srag_covid_gal_NA1, srag_covid_gal_NA2)

srag_covid_gal_NA <- select ( srag_covid_gal_NA1,
                              NU_NOTIFIC, DT_SIN_PRI, 
                              NM_PACIENT, ID_MN_RESI, 
                              DT_INTERNA, NM_UN_INTE,
                              REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                              CLASSI_FIN, EVOLUCAO, 
                              Resultado, Data.da.Liberação, 
                              RES_AN, OBSERVA)

srag_flu_gal_detec1 <- filter(srag_flu_gal, srag_flu_gal$Resultado == "Detectável") 
srag_flu_gal_detec <- select ( srag_flu_gal_detec1,
                                 NU_NOTIFIC, DT_SIN_PRI, 
                                 NM_PACIENT, ID_MN_RESI, 
                                 DT_INTERNA, NM_UN_INTE,
                                 REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                                 CLASSI_FIN, EVOLUCAO, 
                                 Resultado, Data.da.Liberação, 
                                 RES_AN, OBSERVA)

srag_flu_gal_ND1 <- filter(srag_flu_gal, srag_flu_gal$Resultado == "Não Detectável") 
srag_flu_gal_ND <- select ( srag_flu_gal_ND1,
                               NU_NOTIFIC, DT_SIN_PRI, 
                               NM_PACIENT, ID_MN_RESI, 
                               DT_INTERNA, NM_UN_INTE,
                               REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                               CLASSI_FIN, EVOLUCAO, 
                               Resultado, Data.da.Liberação, 
                               RES_AN, OBSERVA)

srag_flu_gal_NA1 <- srag_flu_gal %>% filter(is.na(srag_flu_gal$Resultado))
srag_flu_gal_NA <- select ( srag_flu_gal_NA1,
                               NU_NOTIFIC, DT_SIN_PRI, 
                               NM_PACIENT, ID_MN_RESI, 
                               DT_INTERNA, NM_UN_INTE,
                               REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                               CLASSI_FIN, EVOLUCAO, 
                               Resultado, Data.da.Liberação, 
                               RES_AN, OBSERVA)


#covid ND para rodar influenza
rodar_flu <- bind_rows(srag_covid_gal_ND1, srag_covid_gal_NA1)
rodar_flu$Documento.Paciente.1 <- as.numeric(rodar_flu$Documento.Paciente.1)
id_pessoa_srag1 <- paste(rodar_flu$NM_PACIENT,
                         rodar_flu$DT_NASC,
                         rodar_flu$CO_MUN_RES,
                         rodar_flu$DT_SIN_PRI)
rodar_flu <- mutate(rodar_flu, id_pessoa_srag1)


rodar_flu <- dplyr::rename(rodar_flu, id_pessoa_gal_flu = id_pessoa_srag1)
rodar_flu= rodar_flu %>% anti_join(data_gal_flu,by="id_pessoa_gal_flu")
rodar_flu <- select (rodar_flu,
                     NU_NOTIFIC, DT_SIN_PRI, 
                     NM_PACIENT, DT_NASC, 
                     NM_MAE_PAC, ID_MN_RESI, 
                     DT_INTERNA, NM_UN_INTE,
                     REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                     CLASSI_FIN, EVOLUCAO, 
                     Resultado, Data.da.Liberação, 
                     OBSERVA)

#objeto para vê covid e flu olhar direitinho essa parte
Covid1 <- filter(data_srag, data_srag$RES_AN == 1)
Covid2 <- filter(data_srag, data_srag$PCR_RESUL == 1 | data_srag$CLASSI_FIN == 5)
data_srag_covid <- bind_rows(Covid1, Covid2, srag_covid_gal_detec1)
Flu1 <- filter(data_srag, data_srag$POS_AN_FLU == 1)
Flu2 <- filter(data_srag, data_srag$POS_PCRFLU == 1 | data_srag$CLASSI_FIN == 1)
data_srag_flu <- bind_rows(Flu1, Flu2, srag_flu_gal_detec1)

#unir covid e flu
data_srag_covid$Documento.Paciente.1 <- as.numeric(data_srag_covid$Documento.Paciente.1)

data_covid_e_flu <- bind_rows(data_srag_covid, data_srag_flu)

data_covid_e_flu0 <- filter(data_covid_e_flu, data_covid_e_flu$POS_PCRFLU == 1 &
                            data_covid_e_flu$RES_AN == 1)

data_covid_e_flu1 <- filter(data_covid_e_flu, data_covid_e_flu$POS_PCRFLU == 1 &
                             data_covid_e_flu$CLASSI_FIN == 5)

data_covid_e_flu2 <- filter(data_covid_e_flu, data_covid_e_flu$POS_AN_FLU == 1 &
                              data_covid_e_flu$RES_AN == 1)


data_covid_e_flu__ <- bind_rows(data_covid_e_flu0, data_covid_e_flu1, data_covid_e_flu2)
data_covid_e_flu__ <- data_covid_e_flu__ %>% filter (! duplicated(data_covid_e_flu__$id_pessoa_srag))
data_covid_e_flu__ <- select ( data_covid_e_flu__,
                            NU_NOTIFIC, DT_SIN_PRI, 
                            NM_PACIENT, DT_NASC, 
                            NM_MAE_PAC, ID_MN_RESI, 
                            DT_INTERNA, NM_UN_INTE,
                            REQUI_GAL,  PCR_RESUL, POS_PCRFLU,
                            RES_AN, POS_AN_FLU,
                            CLASSI_FIN, EVOLUCAO, 
                            OBSERVA)


##saidas 
write.table(srag_covid_gal_detec, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_covid/covidvsgal_Detec.csv", sep = ";", row.names = FALSE)
write.table(srag_covid_gal_ND, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_covid/covidvsgal_ND.csv", sep = ";", row.names = FALSE)
write.table(srag_covid_gal_NA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_covid/covidvsgal_NA.csv", sep = ";", row.names = FALSE)
write.table(srag_flu_gal_detec, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_flu/flusgal_Detec.csv", sep = ";", row.names = FALSE)
write.table(srag_flu_gal_ND, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_flu/fluvsgal_ND.csv", sep = ";", row.names = FALSE)
write.table(srag_flu_gal_NA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/sivep_gal_flu/flusgal_NA.csv", sep = ";", row.names = FALSE)
write.table(rodar_flu, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/rodar influenza.csv", sep = ";", row.names = FALSE)
write.table(data_covid_e_flu__, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Projeto SRAG vs GAL/Projeto teste_srag_gal_covid_flu/saidas/Covid_e_Flu.csv", sep = ";", row.names = FALSE)
#####FIM
