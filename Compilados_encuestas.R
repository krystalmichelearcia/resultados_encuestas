#----------------------------------------------------------------
#       Código para compilar datos de encuestas de opinión
#---------------------------------------------------------------
#--------------------------------

#Con Semana y REC (Completo)
rm(list = ls())

# Direccionar
#setwd("C:/Users/KrystalMicheleOcampo/Documents/Apoyo a tu seguimiento modular")

# ---- Cargar las librerías necesarias ----

if(!require(pacman)){
  install.packages('pacman')
};
library(pacman)
#library(readr)
pacman::p_load(here,tidyverse, openxlsx, readr,dplyr, stringr,tidyr, purrr, rsconnect, magrittr, rlang, DT,shinythemes)

# ---- Crear directorios ----
##crea los directorios para base de datos y productos
#directorio de insumos
dir_insumos<- paste0(here(),'/INSUMOS')
if (!dir.exists(dir_insumos)) {
  dir.create(dir_insumos)
}

dir_insumos_SAME<- paste0(here(),'/INSUMOS_SAME')
if (!dir.exists(dir_insumos_SAME)) {
  dir.create(dir_insumos_SAME)
}
#directorio productos
dir_productos<- paste0(here(),'/PRODUCTOS')
if (!dir.exists(dir_productos)) {
  dir.create(dir_productos)
}
#Analiza los archivos en el directorio y los clasifica segun su tipo
#DF de archivos
archivos<-data.frame(archivos = list.files(here()))

for (archivo in archivos[,1]) { #Diferenciar entre archivos de Asignación General e importables
  if (grepl('\\.(xlsx)$', archivo,ignore.case = TRUE)) {
    
        file.rename(paste0(here(), "/", archivo), paste0(dir_insumos, "/", archivo))
        #print(paste0(dir_insumos, "/", archivo))
      
  }
}

bases <- data.frame(nombre = list.files(dir_insumos))  

#Modular
#Ordinarios
MOD_22_23_ORD <- data.frame()
MOD_17_21_ORD <- data.frame()
MOD_1_16_ORD <- data.frame()
#Recursamiento
MOD_22_23_REC <- data.frame()
MOD_17_21_REC <- data.frame()
MOD_1_16_REC <- data.frame()
#AV
#Ordinarios
AV_22_23_ORD <- data.frame()
AV_1_21_ORD <- data.frame()
#Recursamiento
AV_22_23_REC <- data.frame()
AV_1_21_REC <- data.frame()

for (base in bases[,1]) {
  if (grepl("Compilado", base, ignore.case = TRUE) && (grepl('\\.(xlsx)$',base, ignore.case = TRUE)) {
    nombre<-paste0(dir_insumos,'/', base)
    base1<-readxl::read_xlsx(paste0(nombre)) 
    #base1<-read_csv(paste0(nombre), locale = locale(encoding = "Latin1"))
    if (grepl('AV', nombre,ignore.case = TRUE)) {
      if (grepl('ORD', nombre,ignore.case = TRUE)) {
        if (grepl('23', nombre,ignore.case = TRUE)) {
          
        } else if (grepl('REC', nombre,ignore.case = TRUE)){
          
        }
      } else if (grepl('REC', nombre,ignore.case = TRUE)){
        
      }
    } else if (grepl('Mod', nombre,ignore.case = TRUE)){
      if (grepl('ORD', nombre,ignore.case = TRUE)) {
        
      } else if (grepl('REC', nombre,ignore.case = TRUE)){
        
      }
    }
    #assign(paste0(base),base1)
    Concentrado_de_puntajes <- rbind(Concentrado_de_puntajes, base1)  # Combina los datos #bind_rows(M, base1)
  };
}



for (base in bases[,1]) {
  if (grepl("Compilado", base, ignore.case = TRUE) && (grepl('\\.(xlsx)$',base, ignore.case = TRUE)) {
    nombre<-paste0(dir_insumos,'/', base)
    base1<-readxl::read_xlsx(paste0(nombre)) 
    if (grepl('AV', nombre,ignore.case = TRUE))  && (grepl('ORD', nombre,ignore.case = TRUE)) {
      if (grepl('22_23', nombre,ignore.case = TRUE)) {
        
        AV_22_23_ORD <- rbind(AV_22_23_ORD, base1)  # Combina los datos #bind_rows(M, base1)
      
      } else if (grepl('1_21', nombre,ignore.case = TRUE)) {
        
        AV_1_21_ORD <- rbind(AV_1_21_ORD, base1)  # Combina los datos #bind_rows(M, base1)
      
      }
    } else if (grepl('AV', nombre,ignore.case = TRUE))  && (grepl('REC', nombre,ignore.case = TRUE)) {
      if (grepl('22_23', nombre,ignore.case = TRUE)) {
        
        AV_22_23_REC <- rbind(AV_22_23_REC, base1)  # Combina los datos #bind_rows(M, base1)
        
      } else if (grepl('1_21', nombre,ignore.case = TRUE)) {
        
        AV_1_21_REC <- rbind(AV_1_21_REC, base1)  # Combina los datos #bind_rows(M, base1)
        
      }
    } else if (grepl('MOD', nombre,ignore.case = TRUE))  && (grepl('ORD', nombre,ignore.case = TRUE)) {
      if (grepl('22_23', nombre,ignore.case = TRUE)) {
        
        MOD_22_23_ORD <- rbind(MOD_22_23_ORD, base1)  # Combina los datos #bind_rows(M, base1)
        
      } else if (grepl('17_21', nombre,ignore.case = TRUE)) {
        
        MOD_17_21_ORD <- rbind(MOD_17_21_ORD, base1)  # Combina los datos #bind_rows(M, base1)
        
      } else if (grepl('1_16', nombre,ignore.case = TRUE)) {
        
        MOD_1_16_ORD <- rbind(MOD_1_16_ORD, base1)  # Combina los datos #bind_rows(M, base1)
        
      }
    } else if (grepl('MOD', nombre,ignore.case = TRUE))  && (grepl('REC', nombre,ignore.case = TRUE)) {
      if (grepl('22_23', nombre,ignore.case = TRUE)) {
       
         MOD_22_23_REC <- rbind(MOD_22_23_REC, base1)  # Combina los datos #bind_rows(M, base1)
         
      } else if (grepl('17_21', nombre,ignore.case = TRUE)) {
        
        MOD_17_21_REC <- rbind(MOD_17_21_REC, base1)  # Combina los datos #bind_rows(M, base1)
        
      } else if (grepl('1_16', nombre,ignore.case = TRUE)) {
        
        MOD_1_16_REC <- rbind(MOD_1_16_REC, base1)  # Combina los datos #bind_rows(M, base1)
        
      }
    }
}

archivos_to_save <- list(`MOD_22-23_ORD` = MOD_22_23_ORD, 
                         `MOD_17-21_ORD` = MOD_17_21_ORD,
                         `MOD_1-16_ORD` = MOD_1_16_ORD,
                         `Compilado_final-MOD-REC22-23-DIC24-JUL25` = MOD_22_23_REC, 
                         `Compilado_final-MOD-REC17-21-DIC24-JUL25` = MOD_17_21_REC,
                         `Compilado_final-MOD_REC1-16-DIC24-JUL25` = MOD_1_16_REC,
                         `Compilado_AV_ORD_22Y23` = AV_22_23_REC,
                         `Compilado_AV_ORD_1_21` = AV_1_21_REC,
                         `Compilado_AV_REC22Y23` = AV_22_23_REC,
                         `Compilado_AV_REC_1_21` = AV_1_21_REC)

for (archivo_to_save in names(archivos_to_save)) {
  #write.csv(archivos_to_save[[archivo_to_save]], here(dir_productos,"/",nuevo_nombre), row.names = FALSE, fileEncoding = "Latin1")
  #write.csv(archivos_to_save[[archivo_to_save]], here(dir_productos, paste0(archivo_to_save, ".csv")), row.names = FALSE, fileEncoding = "Latin1")
  write.xlsx(archivos_to_save[[archivo_to_save]], here(dir_productos, paste0(archivo_to_save, ".xlsx")))
}

