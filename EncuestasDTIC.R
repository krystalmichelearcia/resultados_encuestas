#----------------------------------------------------------------
#       Código para limpiar y organizar resultados de encuestas
#           "Opina sobre tu AV" y "Opina sobre tu Modulo"
#---------------------------------------------------------------
#Usar "Latin1" en lugar de UTF-8 tanto para leer como guardar
rm(list = ls())

# Direccionar
#setwd("C:/Users/KrystalMicheleOcampo/Documents/Encuestas DTIC - copia/2024/Diciembre/Encuestas 12-12-2024") #ir cambiando

# ---- Cargar las librerías necesarias ----
if(!require(pacman)){install.packages('pacman')}
pacman::p_load(tidyverse, openxlsx, readr, here, stats, writexl, Rcpp, stringr)


# ---- Crear directorios ----
##crea los directorios para base de datos y productos
#directorio de insumos
dir_productos<- paste0(here(),'/PRODUCTOS')
if (!dir.exists(dir_productos)) {
  dir.create(dir_productos)
}
#directorio productos
dir_productos_av<- paste0(dir_productos,'/ASESOR')
if (!dir.exists(dir_productos_av)) {
  dir.create(dir_productos_av)
}

dir_productos_mod<- paste0(dir_productos,'/MODULOS')
if (!dir.exists(dir_productos_mod)) {
  dir.create(dir_productos_mod)
}
#Analiza los archivos en el directorio y los clasifica segun su tipo
#DF de archivos
archivos <- data.frame(archivos = list.files(path = getwd(), recursive = TRUE))
archivos <- archivos %>% #Cargar arcivos de encuestas de opinión
  filter(grepl("Opina", archivos, ignore.case = TRUE))

# Loop para renombrar archivos y quitar columnas de más
for (archivo in archivos[,1]) {
  # Extraer el nombre del archivo sin la ruta
  nombre_actual <- basename(archivo)
  
  campus <- sub(".*(..)\\.[^.]*$", "\\1", nombre_actual) #últimos dos caracteres
  modulo <- sub(".*_(.*?)\\..*$", "\\1", nombre_actual)# últimos caracteres después del último guión bajo y antes del punto
  
  #if (grepl("Opina", nombre_actual, ignore.case = TRUE)) { # Tabajar con archivos "Opina"
  
  if (grepl("Asesor", nombre_actual, ignore.case = TRUE)) { # AV
    if (grepl("REC", dirname(archivo), ignore.case = TRUE)) { # REC
      nuevo_nombre <- file.path(paste0(gsub(" ", "",dirname(archivo)),modulo, "_AV.csv"))
    } 
    else if (!grepl("REC", dirname(archivo), ignore.case = TRUE) && (!grepl("C", campus, ignore.case = TRUE))){ # Ordinarios de 1 sólo campus 
      nuevo_nombre <- file.path(paste0(dirname(archivo),"C1_AV.csv"))
    }
    else { # Ordinarios
      nuevo_nombre <- file.path(paste0(dirname(archivo),campus, "_AV.csv")) # Formato G#M#C#_AV
    }
  }
  else if (grepl("tu_M", nombre_actual, ignore.case = TRUE)) { # Trabajar con archivos MOD
    if (grepl("REC", dirname(archivo), ignore.case = TRUE)) { # REC
      nuevo_nombre <- file.path(paste0(gsub(" ", "",dirname(archivo)),modulo, "_MOD.csv"))
    } 
    else if (!grepl("REC", dirname(archivo), ignore.case = TRUE) && (!grepl("C", campus, ignore.case = TRUE))){ # Ordinarios de 1 sólo campus
      nuevo_nombre <- file.path(paste0(dirname(archivo),"C1_MOD.csv"))
    }
    else { # Ordinarios
      nuevo_nombre <- file.path(paste0(dirname(archivo),campus, "_MOD.csv")) # Formato G#M#C#_MOD
    }
  }
  
  #base<-readr::read_csv(paste0(archivo), locale = locale(encoding = "UTF-8")) %>%
  base<-read_csv(paste0(archivo), locale = locale(encoding = "Latin1")) %>%
    select(-c(1:3,5)) # Quitar columnas
  assign(nuevo_nombre, base)
  
  print(paste0("DE:", here(), "/", archivo))
  if (grepl("Asesor", nombre_actual, ignore.case = TRUE)) {
    print(paste0("A:", dir_productos_av,"/",nuevo_nombre))
    # Usar write_csv() de readr para guardar el archivo con la codificación adecuada
    #readr::write_csv(base, here(dir_productos_av, "/", nuevo_nombre))
    # Usar writexl para guardar el archivo en formato Excel
    #writexl::write_xlsx(base, here(dir_productos_av, "/", nuevo_nombre))
    #writexl::write_xlsx(path =  paste0(gsub(x = here(dir_productos_av,"/", nuevo_nombre),replacement = '.csv',pattern = '\\.(csv|CSV)$',)), x = base)
    
    write.csv(base, here(dir_productos_av,"/",nuevo_nombre), row.names = FALSE, fileEncoding = "Latin1")
    # Guardar el archivo con codificación UTF-8
    #write.csv(base, here(dir_productos_av,"/",nuevo_nombre), row.names = FALSE, fileEncoding = "latin1", na = "")
    
    #file.rename(paste0(here(), "/", archivo), paste0(dir_productos_av,"/",nuevo_nombre))
  } else {
    print(paste0("A:", dir_productos_mod,"/",nuevo_nombre))
    # Usar write_csv() de readr para guardar el archivo con la codificación adecuada
    #readr::write_csv(base, here(dir_productos_mod,"/",nuevo_nombre))
    #writexl::write_xlsx(path =  paste0(gsub(x = here(dir_productos_mod,"/", nuevo_nombre),replacement = '.csv',pattern = '\\.(csv|CSV)$',)), x = base)
    
    # Usar writexl para guardar el archivo en formato Excel
    #writexl::write_xlsx(base, here(dir_productos_mod,"/",nuevo_nombre))
    #write.csv(base, here(dir_productos_mod,"/",nuevo_nombre), row.names = FALSE, fileEncoding = "latin1", na = "")
    write.csv(base, here(dir_productos_mod,"/",nuevo_nombre), row.names = FALSE, fileEncoding = "Latin1")
    #file.rename(paste0(here(), "/", archivo), paste0(dir_productos_mod,"/",nuevo_nombre))
    
  }
} 

#save.image(file = "mi_entorno.RData")
#load("mi_entorno.RData")
# Renombrar el archivo .Rproj
#file.rename("Encuestas 12-12-2024.Rproj", "BasesAV_MOD.Rproj")

