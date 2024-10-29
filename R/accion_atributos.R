#' @title Refuerza el envio de atributos cuando estos son multiples
#'
#' @description Refuerza el envio de atributos cuando estos son multiples
#'
#' @param nombre_PLC, num_climas, variable, numero
#'
#' @return json
#'
#' @examples  accion_atributos("PLC_LOPY_RC1_7",3,"clima","1,2")
#'
#' @import httr
#' jsonlite
#' lubridate
#'
#' @export

accion_atributos <- function(nombre_PLC, num_climas){

  nombre_PLC <- as.character(nombre_PLC)
  num_climas <- as.numeric(num_climas)
  variable <- as.character(variable)
  numero <- as.numeric(unlist(strsplit(numero, ",")))
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
  names(ids) <- c("PLC_LOPY_RC1_8","PLC_LOPY_RC1_7","PLC_LOPY_RC1_62","PLC_LOPY_RC1_61","PLC_LOPY_RC1_5","PLC_LOPY_RC1_4","PLC_LOPY_RC1_3","PLC_LOPY_RC1_2","PLC_LOPY_RC1_1","PLC_LOPY_RC1_PB","PLC_LOPY_METRO_PLAZA","PLC_LOPY_METRO_P4","PLC_LOPY_METRO_AVENIDA","PLC_LOPY_METRO_INTERIOR")


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


  # CONSIGNAS
  url_thb_fechas <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/attributes/SERVER_SCOPE",sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_atr <- jsonlite::fromJSON(rawToChar(peticion$content))
  df_atributos_seleccion_parcial <- data.frame()
  df_atributos_seleccion <- data.frame()
  df_atributos_control_usuario_parcial <- data.frame()
  df_atributos_control_usuario <- data.frame()
  for(i in numero){
    if(variable == "clima"){
      df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Climatizadora OFF/ON ",i,sep = ""))),]
    }else if(variable == "electrovalvula"){
      df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Grado apertura EV_calor ",i,sep = ""), paste("Grado apertura EV_frio ",i,sep = ""))),]
    }else if(variable == "alumbrado"){
      df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Alumbrado OFF/ON ",i,sep = ""))),]
    }
    df_atributos_seleccion <- rbind(df_atributos_seleccion,df_atributos_seleccion_parcial)
    df_atributos_control_usuario_parcial <- df_atr[which(df_atr$key %in% c(paste("control_usuario_",i,sep = ""))),]
    df_atributos_control_usuario <- rbind(df_atributos_control_usuario,df_atributos_control_usuario_parcial)
  }

  for(i in numero){

    if(variable == "clima"){
      Sys.sleep(10)
      valor <- as.logical(df_atributos_seleccion$value[df_atributos_seleccion$key == paste("Climatizadora OFF/ON ",i,sep = "")])
      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      #json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',i,'":"', tolower(as.character(valor)),'"','}',sep = "")
      json_envio_plataforma <- toJSON(setNames(list(valor), paste("Climatizadora OFF/ON ",i,sep = "")), auto_unbox = TRUE)
      json_envio_plataforma <- gsub("\\[","",json_envio_plataforma)
      json_envio_plataforma <- gsub("\\]","",json_envio_plataforma)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(5)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
    }

    if(variable == "electrovalvula"){
      Sys.sleep(10)
      valor <- df_atributos_seleccion$value[df_atributos_seleccion$key == paste("Grado apertura EV_calor ",i,sep = "")]
      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',i,'":', valor,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
      valor <- df_atributos_seleccion$value[df_atributos_seleccion$key == paste("Grado apertura EV_frio ",i,sep = "")]
      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',i,'":', valor,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

    }

    if(variable == "alumbrado"){
      Sys.sleep(10)
      valor <- as.logical(df_atributos_seleccion$value[df_atributos_seleccion$key == paste("Alumbrado OFF/ON ",i,sep = "")])
      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- toJSON(setNames(list(valor), paste("Climatizadora OFF/ON ",i,sep = "")), auto_unbox = TRUE)
      json_envio_plataforma <- gsub("\\[","",json_envio_plataforma)
      json_envio_plataforma <- gsub("\\]","",json_envio_plataforma)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(5)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
    }


  }

  return(json_envio_plataforma)

}
