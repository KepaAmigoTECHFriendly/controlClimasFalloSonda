#' @title Gestiona la climatización (climas y electroválvulas) en base a temperatura de ERS
#'
#' @description Gestiona la climatización (climas y electroválvulas) en base a temperatura de ERS
#'
#' @param nombre_PLC, num_climas
#'
#' @return json
#'
#' @examples  control_climas_temperatura("PLC P7",3)
#'
#' @import httr
#' jsonlite
#' lubridate
#'
#' @export

control_climas_temperatura <- function(nombre_PLC, num_climas){

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


  # CONSIGNAS
  url_thb_fechas <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/attributes/SERVER_SCOPE",sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_atr <- jsonlite::fromJSON(rawToChar(peticion$content))
  df_atributos_seleccion_parcial <- data.frame()
  df_atributos_seleccion <- data.frame()
  for(i in 1:num_climas){
    df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Consigna Tº ",i,sep = ""), paste("Consigna Tº ",i," frio",sep = ""))),]
    df_atributos_seleccion <- rbind(df_atributos_seleccion,df_atributos_seleccion_parcial)
  }
  df_consignas <- df_atributos_seleccion


  # RELACIONES PLANTA DISPOSITIVOS PARA SACAR DISPOSITIVOS QUE DEN DATO TEMP
  url_relaciones <- paste("http://88.99.184.239:30951/api/relations?fromId=",id_planta,"&fromType=ASSET",sep = "")
  peticion <- GET(url_relaciones, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_relaciones <- jsonlite::fromJSON(rawToChar(peticion$content))

  if(nombre_PLC != "PLC P6_1"){


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
        df_disp_temp <- df_disp_temp[c(6,7,8),]
      }else{
        if(any(grepl("Este",df_disp_temp$name))){
          pos_norte <- grep("Norte", df_disp_temp$name)
          pos_este <- grep("Este", df_disp_temp$name)
          pos_diaf <- grep("CO2", df_disp_temp$name)
          if(identical(pos_diaf,integer(0))){
            pos_diaf <- grep("C02", df_disp_temp$name)
          }

          df_disp_temp <- df_disp_temp[c(pos_norte,pos_este,pos_diaf),]
        }else{
          df_disp_temp <- df_disp_temp[c(1,2,3),]
        }
      }
    }
  }else{ # Cierre si dispositivo != PLC P6_1

    keys <- URLencode(c("temperatura_ambiente_1,temperatura_ambiente_2,temperatura_ambiente_3"))
    url_thb_temps <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/timeseries?limit=10000&keys=",keys,sep = "")
    peticion <- GET(url_thb_temps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df <- do.call(rbind.data.frame, df)

    tiempo_entre_fechas <- as.numeric(abs(difftime(as_datetime(df$ts[1]/1000), Sys.time(), units="hours")))
    if(tiempo_entre_fechas > 1){
      return(0)
    }

    for(i in 1:nrow(df)){
      if(df$value[i] == 0){
        return(0)
      }
    }

  }


  # ==============================================================================
  # GET TEMPERATURA ZONAS FALLOS
  # ==============================================================================
  keys <- URLencode(c("temperatura"))
  for(i in 1:num_climas){  # Bucle actuación por sonda que da fallo

    if(nombre_PLC != "PLC P6_1"){

      url_thb_temps <- paste("http://88.99.184.239:30951/api/plugins/telemetry/DEVICE/",df_disp_temp$id[i],"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
      peticion <- GET(url_thb_temps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

      # Tratamiento datos. De raw a dataframe
      df <- jsonlite::fromJSON(rawToChar(peticion$content))

      if(length(df) == 0){ # No hay datos del sensor demandado. Se pone automático.
        if(num_climas > 1 & nrow(df_disp_temp) > 1){

          # Puesta en auto
          url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
          json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"false"','}',sep = "")
          post <- httr::POST(url = url,
                             add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                             body = json_envio_plataforma,
                             verify= FALSE,
                             encode = "json",verbose()
          )
          Sys.sleep(10)
          return(json_envio_plataforma)

        }else{# Intento recoger la temperatura del otro sensor

          url_thb_temps <- paste("http://88.99.184.239:30951/api/plugins/telemetry/DEVICE/",df_disp_temp$id[i+1],"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
          peticion <- GET(url_thb_temps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

          # Tratamiento datos. De raw a dataframe
          df <- jsonlite::fromJSON(rawToChar(peticion$content))

          if(length(df) == 0){
            # Puesta en auto
            url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
            json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"false"','}',sep = "")
            post <- httr::POST(url = url,
                               add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                               body = json_envio_plataforma,
                               verify= FALSE,
                               encode = "json",verbose()
            )
            Sys.sleep(10)
            return(json_envio_plataforma)
          }
        }
      }

      df_temperatura <- data.frame(format(df$temperatura$ts,scientific=FALSE),df$temperatura$value,stringsAsFactors = FALSE)
      colnames(df_temperatura) <- c("ts",names(df))
      df_temperatura <- df_temperatura[order(df_temperatura$ts, decreasing = TRUE),]
      df_temperatura <- df_temperatura[1,]

      df_consignas_seleccion <- df_consignas[c((i*2)-1,i*2),]  #Selección consignas

    }else{

      df_temperatura <- df[i,]
      colnames(df_temperatura)[2] <- "temperatura"
      df_consignas_seleccion <- df_consignas[c((i*2)-1,i*2),]  #Selección consignas

    }

    pos_frio <- grep(" frio", df_consignas_seleccion$key)
    pos_calor <- 1 + (2-pos_frio)


    if(as.numeric(df_temperatura$temperatura) > df_consignas_seleccion$value[pos_frio][[1]] & as.numeric(df_temperatura$temperatura) - df_consignas_seleccion$value[pos_frio][[1]] > 0.1){  # HACE CALOR. PUESTA EN MANUAL Y ABRIR FRIO

      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',i,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)


      # Abrir válvula frío al 100%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',i,'":', 99,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)
      # Cerra válvula calor al 1%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',i,'":', 1,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)
    }else if(as.numeric(df_temperatura$temperatura) < df_consignas_seleccion$value[pos_calor][[1]] & df_consignas_seleccion$value[pos_calor][[1]] - as.numeric(df_temperatura$temperatura) > 0.1){  # HACE FRÍO PUESTA EN MANUAL Y ABRIR CALOR
      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)

      # Encendido climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',i,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)

      # Abrir válvula calor al 100%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',i,'":', 99,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)
      # Cerra válvula frio al 1%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',i,'":', 1,'}',sep = "")
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
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"true"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)

      # Apagado climatizadora
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Climatizadora OFF/ON ',i,'":', '"false"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)

      # Abrir válvula calor al 10%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_calor ',i,'":', 10,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)

      # Cerra válvula frio al 10%
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Grado apertura EV_frio ',i,'":', 10,'}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )

      Sys.sleep(10)
      # Puesta en auto
      #url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      #json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',i,'":', '"false"','}',sep = "")
      #post <- httr::POST(url = url,
      #                   add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
      #                   body = json_envio_plataforma,
      #                   verify= FALSE,
      #                   encode = "json",verbose()
      #)

    }
  }

  print("------------ OK ------------------")

  return(json_envio_plataforma)
}
