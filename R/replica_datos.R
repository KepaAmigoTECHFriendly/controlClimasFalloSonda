#' @title Replica datos
#'
#' @description Replica datos
#'
#' @param nombre_PLC
#'
#' @return json
#'
#' @examples  replica_datos("PLC P6_1")
#'
#' @import httr
#' jsonlite
#' lubridate
#'
#' @export

replica_datos <- function(nombre_PLC){

  if(nombre_PLC != "PLC Metro Interior"){
    return(0)
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

  id_planta <- ids[nombre_PLC]

  # ==============================================================================
  # PETICIÓN DATOS
  # ==============================================================================

  fecha_1 <- Sys.time() - 60*60*3
  fecha_2 <- Sys.time()
  # Paso a timestamp
  fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
  fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)


  keys <- URLencode(c("temperatura_ambiente_1,temperatura_ambiente_2,temperatura_ambiente_3,climatizadora_1,climatizadora_2,climatizadora_3,estado_electroválvula_1_calor,estado_electroválvula_2_calor,estado_electroválvula_3_calor,estado_electroválvula_1_frío,estado_electroválvula_2_frío,estado_electroválvula_3_frío,temperatura_impulsión_1,temperatura_impulsión_2,temperatura_impulsión_3"))
  url_thb_temps <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/timeseries?limit=10000&keys=",keys,sep = "")
  peticion <- GET(url_thb_temps, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df_2 <- do.call(rbind.data.frame, df)
  df <- as.data.frame(df)
  df <- df[,-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)]
  colnames(df) <- rownames(df_2)
  df <- df[,c(1,4,7,10,13)]
  #df$temperatura_ambiente_2 <- 0
  #df$temperatura_ambiente_3 <- 0
  #df$climatizadora_2 <- 0
  #df$climatizadora_3 <- 0
  #df$estado_electroválvula_2_calor <- 0
  #df$estado_electroválvula_3_calor <- 0
  #df$estado_electroválvula_2_frío <- 0
  #df$estado_electroválvula_3_frio <- 0
  #df$temperatura_impulsión_2 <- 0
  #df$temperatura_impulsión_3 <- 0

  json <- toJSON(df)
  json <- gsub("\\[","",json)
  json <- gsub("\\]","",json)

  id <- "4d305860-e9f7-11eb-9b5b-b36690b9a0e5"

  url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id,"/timeseries/ANY",sep = "")
  json_envio_plataforma <- json
  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                     body = json_envio_plataforma,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  return(1)



}
