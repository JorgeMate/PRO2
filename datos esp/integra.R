getwd()
setwd("~/UPV/PR2/datos/datos esp")

library(tidyverse)


###################################################################################################
df.mort.hosp.uci <- read.csv('MortHospUCI.csv', header= TRUE, sep = ';',dec='.',encoding= 'utf-8' )

df.mort.hosp.uci[,1] <- toupper(df.mort.hosp.uci[,1])
df.mort.hosp.uci.Ordenado <- df.mort.hosp.uci %>% arrange(CCAA)
names(df.mort.hosp.uci.Ordenado)[1] <- 'comunidad'

df.mort.hosp.uci.Ordenado$ISO = ''

df1 = df.mort.hosp.uci.Ordenado


# ISO 3166-2:ES

library(data.table)
df1$ISO[df1$comunidad %like% "ANDA"] <- 'ES-AN'
df1$ISO[df1$comunidad %like% "ANDA"] <- 'ES-AN'
df1$ISO[df1$comunidad %like% "ARAG"] <- 'ES-AR'
df1$ISO[df1$comunidad %like% "ASTU"] <- 'ES-AS'

df1$ISO[df1$comunidad %like% "BALE"] <- 'ES-IB'
df1$ISO[df1$comunidad %like% "CANA"] <- 'ES-CN'

df1$ISO[df1$comunidad %like% "CANT"] <- 'ES-CB'

df1$ISO[df1$comunidad %like% "MANC"] <- 'ES-CM'
df1$ISO[df1$comunidad %like% "LEON"] <- 'ES-CL'

df1$ISO[df1$comunidad %like% "VALE"] <- 'ES-VC'
df1$ISO[df1$comunidad %like% "CATA"] <- 'ES-CT'

df1$ISO[df1$comunidad %like% "EXTR"] <- 'ES-EX'
df1$ISO[df1$comunidad %like% "GALI"] <- 'ES-GA'
df1$ISO[df1$comunidad %like% "RIOJ"] <- 'ES-RI'

df1$ISO[df1$comunidad %like% "MADR"] <- 'ES-MD'
df1$ISO[df1$comunidad %like% "MURC"] <- 'ES-MC'

df1$ISO[df1$comunidad %like% "NAVA"] <- 'ES-NC'
df1$ISO[df1$comunidad %like% "VASC"] <- 'ES-PV'

## (Excluimos ceuta y melilla). 
df1 <-subset(df1, !(df1$ISO == '')) 

df1.Ordenado <- df1 %>% arrange(ISO)


library("readxl")
#####################################################################
df2 <- read_excel("Datos_Economicos_España.xlsx", sheet = 3, skip = 4)

df2.2019 <-  df2 %>% select(`Comunidad Autónoma`, `2019 (A)`)

df2.2019Limpio <- na.omit(df2.2019)

names(df2.2019Limpio)[1] <- 'comunidad'
names(df2.2019Limpio)[2] <- 'pib_capita'

df2.Ordenado <- df2.2019Limpio %>% arrange(comunidad)

df2 = df2.Ordenado

df2$ISO = ''

df2$ISO[df2$comunidad %like% "ANDA"] <- 'ES-AN'
df2$ISO[df2$comunidad %like% "ARAG"] <- 'ES-AR'
df2$ISO[df2$comunidad %like% "ASTU"] <- 'ES-AS'

df2$ISO[df2$comunidad %like% "BALE"] <- 'ES-IB'
df2$ISO[df2$comunidad %like% "CANA"] <- 'ES-CN'

df2$ISO[df2$comunidad %like% "CANT"] <- 'ES-CB'

df2$ISO[df2$comunidad %like% "MANC"] <- 'ES-CM'
df2$ISO[df2$comunidad %like% "LEÓN"] <- 'ES-CL'  ## Ojo acento

df2$ISO[df2$comunidad %like% "VALE"] <- 'ES-VC'
df2$ISO[df2$comunidad %like% "CATA"] <- 'ES-CT'

df2$ISO[df2$comunidad %like% "EXTR"] <- 'ES-EX'
df2$ISO[df2$comunidad %like% "GALI"] <- 'ES-GA'
df2$ISO[df2$comunidad %like% "RIOJ"] <- 'ES-RI'

df2$ISO[df2$comunidad %like% "MADR"] <- 'ES-MD'
df2$ISO[df2$comunidad %like% "MURC"] <- 'ES-MC'

df2$ISO[df2$comunidad %like% "NAVA"] <- 'ES-NC'
df2$ISO[df2$comunidad %like% "VASC"] <- 'ES-PV'

## (Excluimos ceuta y melilla). 
df2 <-subset(df2, !(df2$ISO == '')) 

df2.Ordenado <- df2 %>% arrange(ISO)

#------------------------------------------------------------------------------
# Quitamos la comunidad de este df
df2.Ordenado <- df2.Ordenado[, names(df2.Ordenado) %in% c("ISO", "pib_capita")]

dfESP <- merge(df1.Ordenado,df2.Ordenado)



####################################################################################################
df3 <- read.csv('tasa_por_100.000_habitant.csv', header= TRUE, sep = ',',dec='.',encoding= 'utf-8' )

df3 <- df3[,3:4]

names(df3)[1] <- 'comunidad'
names(df3)[2] <- 'medicos_activos'

df3[,1] <- toupper(df3[,1])

df3$ISO = ''

df3$ISO[df3$comunidad %like% "ANDA"] <- 'ES-AN'
df3$ISO[df3$comunidad %like% "ARAG"] <- 'ES-AR'
df3$ISO[df3$comunidad %like% "ASTU"] <- 'ES-AS'

df3$ISO[df3$comunidad %like% "BALE"] <- 'ES-IB'
df3$ISO[df3$comunidad %like% "CANA"] <- 'ES-CN'

df3$ISO[df3$comunidad %like% "CANT"] <- 'ES-CB'

df3$ISO[df3$comunidad %like% "C-LM"] <- 'ES-CM'
df3$ISO[df3$comunidad %like% "CYL"] <- 'ES-CL'

df3$ISO[df3$comunidad %like% "VALE"] <- 'ES-VC'
df3$ISO[df3$comunidad %like% "CATA"] <- 'ES-CT'

df3$ISO[df3$comunidad %like% "EXTR"] <- 'ES-EX'
df3$ISO[df3$comunidad %like% "GALI"] <- 'ES-GA'
df3$ISO[df3$comunidad %like% "RIOJ"] <- 'ES-RI'

df3$ISO[df3$comunidad %like% "MADR"] <- 'ES-MD'
df3$ISO[df3$comunidad %like% "MURC"] <- 'ES-MC'

df3$ISO[df3$comunidad %like% "NAVA"] <- 'ES-NC'
df3$ISO[df3$comunidad %like% "VASC"] <- 'ES-PV'

df3 <-subset(df3, !(df3$ISO == '')) 

df3.Ordenado <- df3 %>% arrange(ISO)

# Quitamos la comunidad de este df
df3.Ordenado <- df3.Ordenado[, names(df3.Ordenado) %in% c("ISO", "medicos_activos")]

dfESP <- merge(dfESP,df3.Ordenado)


merge de MortHospUCI.csv | Datos_Economicos_España.xlsx | tasa_por_100.000_habitant.csv


