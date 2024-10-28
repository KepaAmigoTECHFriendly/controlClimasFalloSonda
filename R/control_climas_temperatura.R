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
  df_atributos_control_usuario_parcial <- data.frame()
  df_atributos_control_usuario <- data.frame()
  for(i in 1:num_climas){
    df_atributos_seleccion_parcial <- df_atr[which(df_atr$key %in% c(paste("Consigna Tº ",i,sep = ""), paste("Consigna Tº ",i," frio",sep = ""))),]
    df_atributos_seleccion <- rbind(df_atributos_seleccion,df_atributos_seleccion_parcial)
    df_atributos_control_usuario_parcial <- df_atr[which(df_atr$key %in% c(paste("control_usuario_",i,sep = ""))),]
    df_atributos_control_usuario <- rbind(df_atributos_control_usuario,df_atributos_control_usuario_parcial)
  }
  df_consignas <- df_atributos_seleccion

  # PARO LA EJECUCIÓN SI SOLO HAY UNA CLIMA Y EL MODO ES MANUAL
  if(num_climas == 1){
    if(df_atributos_control_usuario$value[1] == "Manual"){
      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',1,'":', '"true"','}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(2)

      return(json_envio_plataforma) # Termino el programa

    }else if(df_atributos_control_usuario$value[1] == "Local"){
      # Puesta en Automático
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',1,'":', '"false"','}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(5)

      CTc <- as.numeric(df_consignas$value[df_consignas$key == "Consigna Tº 1"])
      CTf <- as.numeric(df_consignas$value[df_consignas$key == "Consigna Tº 1 frio"])

      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Consigna Tº ',1,'":', CTc,'}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      Sys.sleep(5)

      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Consigna Tº ',1,' frio":', CTf,'}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      Sys.sleep(5)

      return(json_envio_plataforma) # Termino el programa
    }
  }


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
      df_disp_temp <- df_disp_temp[c(6,7,8),]
    }else if(nombre_PLC == "PLC P6_1"){ # Tiene + de 3 sensores de temperatura y de estos, algunos no están asociados a la climatizadora
      df_disp_temp <- df_disp_temp[grep("Despacho",df_disp_temp$name),]
    }else if(nombre_PLC == "PLC P6_2"){ # Hay que ordenar los sensores en base a las climas que comandan
      df_disp_temp <- df_disp_temp[c(3,1,2),]
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







  #---------------------------------------------------------------
  # GET REGISTROS DE AUDITORIA PARA DECIDIR SI PONGO EN MANUAL O AUTO

  # Registros auditoria
  # flag_registro <- 1
  # fecha_base <- as.numeric(as.POSIXct(Sys.Date()-5))*1000 # Timestamp en ms
  # url_thb_fechas <- paste("http://88.99.184.239:30951/api/audit/logs/user/c53af270-f104-11eb-abdb-6ff341d2a022?pageSize=10000&page=0&startTime=",fecha_base,"&actionTypes=ATTRIBUTES_UPDATED",sep = "")
  # peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
  # df <- jsonlite::fromJSON(rawToChar(peticion$content))
  # df <- df$data
  # clase <- class(df)
  # if(clase == "list"){
  #   if(length(df) == 0){  # Si hoy no hay acciones, pongo en manual como hasta ahora.
  #     print("No hay acciones por parte del mantenedor. Continuo con rutina automática")
  #     flag_registro <- 0
  #   }
  # }else{
  #   if(nrow(df) == 0){  # Si hoy no hay acciones, pongo en manual como hasta ahora.
  #     print("No hay acciones por parte del mantenedor. Continuo con rutina automática")
  #     flag_registro <- 0
  #   }else{
  #     df$createdTime <- as.Date(as.POSIXct(df$createdTime/1000, origin="1970-01-01"))
  #     df <- df[order(df$createdTime, decreasing = TRUE),]
  #     df <- df[df$createdTime == Sys.Date(),]  # Filtro acciones de hoy
  #     if(nrow(df) == 0){  # Si hoy no hay acciones, pongo en manual como hasta ahora.
  #       print("No hay acciones por parte del mantenedor. Continuo con rutina automática")
  #       flag_registro <- 0
  #     }else{
  #       df <- df[1,]
  #       id_activo <- df$entityId$id
  #       nombre_activo <- df$entityName
  #
  #       acciones <- df$actionData$attributes
  #       acciones_cambio_auto_manual <- acciones[,grep("Modo",colnames(acciones))] # Solo acciones de cambio a auto o manual
  #       acciones_cambio_auto_manual$id_activo <- id_activo
  #       acciones_cambio_auto_manual$nombre_activo <- nombre_activo
  #
  #       nombre_plantas <- c("RC1_P8","RC1_P7","RC1_P6_Z2","RC1_P6_Z1","RC1_P5","RC1_P4","RC1_P3","RC1_P2","RC1_P1","RC1_PB","Metro_Plaza","Metro_P4","Metro_Avenida","	Metro_Interior")
  #       names(nombre_plantas) <- c("PLC P8","PLC P7","PLC P6_2","PLC P6_1","PLC P5","PLC P4","PLC P3","PLC P2","PLC P1","PLC PB","PLC Metro Plaza","PLC Metro P4","PLC Metro Avenida","PLC Metro Interior")
  #
  #       # FILTRO POR PLANTA ACTUACIÓN
  #       nombre_planta_filtro <- nombre_plantas[nombre_PLC]
  #       acciones_cambio_auto_manual <- as.data.frame(acciones_cambio_auto_manual)
  #       acciones_cambio_auto_manual <- acciones_cambio_auto_manual[acciones_cambio_auto_manual$nombre_activo == nombre_planta_filtro,]
  #       numeros_clima_acciones <- as.numeric(substr(colnames(acciones_cambio_auto_manual), nchar(colnames(acciones_cambio_auto_manual)[1]),nchar(colnames(acciones_cambio_auto_manual)[1])))
  #
  #     }
  #   }
  # }

  #---------------------------------------------------------------


  # ==============================================================================
  # GET TEMPERATURA ZONAS FALLOS
  # ==============================================================================
  keys <- URLencode(c("temperatura"))
  for(i in 1:num_climas){  # Bucle actuación por sonda que da fallo

    if(df_atributos_control_usuario$value[i] == "Manual"){
      # Puesta en manual
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',1,'":', '"true"','}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      Sys.sleep(10)

      next # Avanzo la ejecucion

    }else if(df_atributos_control_usuario$value[i] == "Local"){
      # Puesta en Automático
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',1,'":', '"false"','}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      CTc <- as.numeric(df_consignas$value[df_consignas$key == paste("Consigna Tº ",i,sep = "")])
      CTf <- as.numeric(df_consignas$value[df_consignas$key == paste("Consigna Tº ",i," frio",sep = "")])

      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Consigna Tº ',i,'":', CTc,'}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      Sys.sleep(5)

      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Consigna Tº ',i,' frio":', CTf,'}',sep = "")
      print(json_envio_plataforma)
      post <- httr::POST(url = url,
                        add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                        body = json_envio_plataforma,
                        verify= FALSE,
                        encode = "json",verbose()
      )
      Sys.sleep(5)

      next # Avanzo la ejecucion
    }else{
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

          # Notificación puesta en automático a través de atributo servidor, puesta en local.
          url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
          json_envio_plataforma <- paste('{"control_usuario_',i,'":', '"Local"','}',sep = "")
          print(json_envio_plataforma)
          post <- httr::POST(url = url,
                            add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                            body = json_envio_plataforma,
                            verify= FALSE,
                            encode = "json",verbose()
          )

          next # Avanzo ejecucion

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

            # Notificación puesta en automático a través de atributo servidor, puesta en local.
            url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
            json_envio_plataforma <- paste('{"control_usuario_',i,'":', '"Local"','}',sep = "")
            print(json_envio_plataforma)
            post <- httr::POST(url = url,
                              add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                              body = json_envio_plataforma,
                              verify= FALSE,
                              encode = "json",verbose()
            )

            next # Avanzo ejecucion
          }
        }
      }

      df_temperatura <- data.frame(format(df$temperatura$ts,scientific=FALSE),df$temperatura$value,stringsAsFactors = FALSE)
      colnames(df_temperatura) <- c("ts",names(df))
      df_temperatura <- df_temperatura[order(df_temperatura$ts, decreasing = TRUE),]
      df_temperatura <- df_temperatura[1,]

      df_consignas_seleccion <- df_consignas[c((i*2)-1,i*2),]  #Selección consignas

      pos_frio <- grep(" frio", df_consignas_seleccion$key)
      pos_calor <- 1 + (2-pos_frio)


      # ------
      # Reviso si tengo que poner en manual o no en base al comando del usuario mantenedor
      # if(flag_registro == 1){
      #   if(!is.na(numeros_clima_acciones[i])){
      #     if(numeros_clima_acciones[i] == i){
      #       valor_mantenedor <- acciones_cambio_auto_manual[,i]
      #       if(valor_mantenedor == "false"){  # Salto la iteración, aquí tiene preferencia la opción del mantenedor.
      #         next
      #       }
      #     }
      #   }
      # }
      # ------

      if(as.numeric(df_temperatura$temperatura) > as.numeric(df_consignas_seleccion$value[pos_frio][[1]]) & as.numeric(df_temperatura$temperatura) - as.numeric(df_consignas_seleccion$value[pos_frio][[1]]) > 0.1){  # HACE CALOR. PUESTA EN MANUAL Y ABRIR FRIO

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
      }else if(as.numeric(df_temperatura$temperatura) < as.numeric(df_consignas_seleccion$value[pos_calor][[1]]) & as.numeric(df_consignas_seleccion$value[pos_calor][[1]]) - as.numeric(df_temperatura$temperatura) > 0.1){  # HACE FRÍO PUESTA EN MANUAL Y ABRIR CALOR
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
  }

  print("------------ OK ------------------")

  return(json_envio_plataforma)
}
