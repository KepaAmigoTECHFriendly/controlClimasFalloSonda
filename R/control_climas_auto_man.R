#' @title Gestiona auto manual climas
#'
#' @description Gestiona auto manual climas
#'
#' @param nombre_PLC
#'
#' @return json
#'
#' @examples  control_climas_fallo_sondas("PLC P7")
#'
#' @import httr
#' jsonlite
#'
#' @export

control_climas_auto_man <- function(nombre_PLC){

  nombre_PLC <- as.character(nombre_PLC)

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


  # ATR
  url_thb_fechas <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/values/attributes/SERVER_SCOPE",sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_atr <- jsonlite::fromJSON(rawToChar(peticion$content))

  df_atr <- df_atr[grep("Modo trabajo climatizadora",df_atr$key),]

  if(any(df_atr$value)){
    pos <- grep("true",df_atr$value,ignore.case = TRUE)

    for(i in 1:length(pos)){
      # Puesta en auto
      sensor <- pos[i]
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_planta,"/SERVER_SCOPE",sep = "")
      json_envio_plataforma <- paste('{"Modo trabajo climatizadora (auto/man) ',sensor,'":', '"false"','}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(10)
    }

  }

  print("------------ OK ------------------")

  return(1)
}
