#' @title Gestiona las climatizadoras frente a un problema en las sondas de Tº
#'
#' @description Gestiona las climatizadoras frente a un problema en las sondas de Tº
#'
#' @param nombre_PLC, num_climas
#'
#' @return json
#'
#' @examples  control_climas_fallo_sondas("PLC P7",3)
#'
#' @import httr
#' jsonlite
#'
#' @export

control_climas_fallo_sondas <- function(nombre_PLC, num_climas){

  nombre_PLC <- as.character(nombre_PLC)
  num_climas <- as.numeric(num_climas)
  if(num_climas == 2){
    num_climas <- 1
  }

  # ==============================================================================
  # PETICIÓN TOKENs THB
  # ==============================================================================

  auth_chirpstack <- "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhcGlfa2V5X2lkIjoiODU3YzdjNzctYWE2NS00NDkwLWJjOGMtYjNlZTY3Y2YwOGRjIiwiYXVkIjoiYXMiLCJpc3MiOiJhcyIsIm5iZiI6MTY1MjI4Njg3Miwic3ViIjoiYXBpX2tleSJ9.vkUFvGuwteUfo2nAMHBI9p8rqUb7lFQeur9-AWxf7XE"

  cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
  post <- httr::POST(url = "http://88.99.184.239:30951/api/auth/login",
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)

  # ==============================================================================
  # VECTORES ACTIVOS E IDs -- INPUTS
  # ==============================================================================

  ids <- c("2569f620-e9f6-11eb-a661-35f176fab723", "334bed60-e9f7-11eb-9b5b-b36690b9a0e5", "1b5aca10-1acf-11ec-9dbd-2dda84d1e438", "38f7f420-e9f7-11eb-9b5b-b36690b9a0e5",
           "3e5bf560-e9f7-11eb-9b5b-b36690b9a0e5", "43ce7590-e9f7-11eb-9b5b-b36690b9a0e5", "48a57910-e9f7-11eb-9b5b-b36690b9a0e5", "4d305860-e9f7-11eb-9b5b-b36690b9a0e5",
           "52853e70-e9f7-11eb-9b5b-b36690b9a0e5", "57c08200-e9f7-11eb-9b5b-b36690b9a0e5","26b2c7d0-ea0c-11eb-97c1-35f176fab723","2b343140-ea0c-11eb-97c1-35f176fab723",
           "16b901b0-1e90-11ec-a3bf-39d69cec15b9","21e9d820-1e90-11ec-a3bf-39d69cec15b9")
  names(ids) <- c("PLC P8","PLC P7","PLC P6_2","PLC P6_1","PLC P5","PLC P4","PLC P3","PLC P2","PLC P1","PLC PB","PLC Metro Plaza","PLC Metro P4","PLC Metro Avenida","PLC Metro Interior")


  num_climas <- num_climas
  id_planta <- ids[nombre_PLC]

  # ==============================================================================
  # PETICIÓN DATOS
  # ==============================================================================

  fecha_1 <- Sys.time() - 60*60*3
  fecha_2 <- Sys.time()
  # Paso a timestamp
  fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
  fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)

  if(num_climas == 1){
    keys <- URLencode(c("temperatura_ambiente_1"))
  }else if(num_climas == 2){
    keys <- URLencode(c("temperatura_ambiente_1,temperatura_ambiente_2"))
  }else if(num_climas == 3){
    keys <- URLencode(c("temperatura_ambiente_1,temperatura_ambiente_2,temperatura_ambiente_3"))
  }

  url_thb_fechas <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))

  # Evitar diferencia de filas
  num_min_filas <- 5000
  #Obtencion del numero minimo de filas
  for(i in 1:length(df)){
    if(min(nrow(df[[i]])) < num_min_filas){
      num_min_filas <- min(nrow(df[[i]]))
    }
  }

  #Eliminar primera fila a los que superen el numero de filas minimo
  for(i in 1:length(df)){
    if(nrow(df[[i]]) > num_min_filas){
      df[[i]] <- df[[i]][-1,]
    }
  }

  if(num_climas == 1){
    df_datos <- data.frame(format(df$temperatura_ambiente_1$ts,scientific=FALSE),df$temperatura_ambiente_1$value,stringsAsFactors = FALSE)
  }else if(num_climas == 2){
    df_datos <- data.frame(format(df$temperatura_ambiente_1$ts,scientific=FALSE),df$temperatura_ambiente_1$value,df$temperatura_ambiente_2$value,stringsAsFactors = FALSE)
  }else if(num_climas == 3){
    df_datos <- data.frame(format(df$temperatura_ambiente_1$ts,scientific=FALSE),df$temperatura_ambiente_1$value,df$temperatura_ambiente_2$value,df$temperatura_ambiente_3$value,stringsAsFactors = FALSE)
  }

  colnames(df_datos) <- c("ts",names(df))
  df_datos$fecha_time <- as.POSIXct(as.numeric(df_datos$ts)/1000, origin = "1970-01-01")
  # Orden por timestamp
  df_datos <- df_datos[order(df_datos$ts, decreasing = TRUE),]
  df_datos_ultima <- df_datos[1,]
  if(ncol(df_datos_ultima) > 3){
    df_datos_ultima <- df_datos_ultima[,-c(1,ncol(df_datos_ultima))]
  }else{
    df_datos_ultima <- df_datos_ultima[,-3]
  }

  # ==============================================================================
  # IDENTIFICACIÓN DE FALLOS TEMPERATURAS
  # ==============================================================================
  ceros <- df_datos_ultima[1,] == 0
  posicion_fallos <- grep(TRUE,ceros)
  nombres_fallos <- colnames(df_datos_ultima)[posicion_fallos]
  numero_fallos <- as.numeric(gsub(".*?([0-9]+).*", "\\1", nombres_fallos))

  if(identical(numero_fallos,numeric(0))){  # No hay errores
    return(1)
  }

  # Atributos de temperatura que dan fallos

  # CONSIGNAS
  url_thb_fechas <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/attributes/SERVER_SCOPE",sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_atr <- jsonlite::fromJSON(rawToChar(peticion$content))
  df_atributos_seleccion_parcial <- data.frame()
  df_atributos_seleccion <- data.frame()
  for(i in 1:length(numero_fallos)){
    df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Consigna Tº ",numero_fallos[i],sep = ""), paste("Consigna Tº ",numero_fallos[i]," frio",sep = ""))),]
    df_atributos_seleccion <- rbind(df_atributos_seleccion,df_atributos_seleccion_parcial)
  }
  df_consignas <- df_atributos_seleccion


  # RELACIONES PLANTA DISPOSITIVOS PARA SACAR DISPOSITIVOS QUE DEN DATO TEMP
  url_relaciones <- paste("http://88.99.184.239:30951/api/relations?fromId=",id_planta,"&fromType=ASSET",sep = "")
  peticion <- GET(url_relaciones, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_relaciones <- jsonlite::fromJSON(rawToChar(peticion$content))


  # LISTA DE DISPOSITIVOS RELACIONADOS
  ids <- paste(df_relaciones$to$id, collapse = ",")
  url_disp <- paste("http://88.99.184.239:30951/api/devices?deviceIds=",ids,sep = "")
  peticion <- GET(url_disp, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_disp <- jsonlite::fromJSON(rawToChar(peticion$content))

  df_disp_temp <- df_disp[df_disp$type %in% c("Sensor de temperatura, humedad relativa y presencia","Sensores CO2"),]
  ids <- df_disp_temp[,1][,2]
  df_disp_temp <- df_disp_temp[,-1]
  df_disp_temp <- df_disp_temp[,c("type","name")]
  df_disp_temp$id <- ids

  if(num_climas > 1){
    # Ajuste e caso de P5
    if(nombre_PLC == "PLC P5"){  # Tiene + de 3 sensores de temperatura y de estos, algunos no están asociados a la climatizadora
      df_disp_temp <- df_disp_temp[-c(1,2,3,4),]
    }
    df_disp_temp <- df_disp_temp[c(1,2,3),]
  }


  # ==============================================================================
  # GET TEMPERATURA ZONAS FALLOS
  # ==============================================================================
  keys <- URLencode(c("temperatura"))
  for(i in 1:length(numero_fallos)){  # Bucle actuación por sonda que da fallo

    sensor <- numero_fallos[i]
    url_thb_temps <- paste("http://88.99.184.239:30951/api/plugins/telemetry/DEVICE/",df_disp_temp$id[sensor],"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
    peticion <- GET(url_thb_temps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df_temperatura <- data.frame(format(df$temperatura$ts,scientific=FALSE),df$temperatura$value,stringsAsFactors = FALSE)
    colnames(df_temperatura) <- c("ts",names(df))
    df_temperatura <- df_temperatura[order(df_temperatura$ts, decreasing = TRUE),]
    df_temperatura <- df_temperatura[1,]

    df_consignas_seleccion <- df_consignas[c((i*2)-1,i*2),]  #Selección consignas
    if(as.numeric(df_temperatura$temperatura) > df_consignas_seleccion$value[2][[1]] & as.numeric(df_temperatura$temperatura) - df_consignas_seleccion$value[2][[1]] > 1){  # HACE CALOR. PUESTA EN MANUAL Y ABRIR FRIO

      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',sensor,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',sensor,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Abrir válvula frío al 100%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',sensor,'":', 99,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
      # Cerra válvula calor al 1%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',sensor,'":', 1,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
    }else if(as.numeric(df_temperatura$temperatura) < df_consignas_seleccion$value[1][[1]] & df_consignas_seleccion$value[1][[1]] - as.numeric(df_temperatura$temperatura) > 1){  # HACE FRÍO PUESTA EN MANUAL Y ABRIR CALOR
      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',sensor,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',sensor,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Abrir válvula calor al 100%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',sensor,'":', 99,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
      # Cerra válvula frio al 1%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',sensor,'":', 1,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
    }else{

      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',sensor,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',sensor,'":', '"false"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Abrir válvula calor al 10%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',sensor,'":', 10,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
      # Cerra válvula frio al 1o%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',sensor,'":', 10,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )



    }
  }

  print("------------ OK ------------------")

  return(json_envio_plataforma)

}
