
# Returns the time that lasts a year (365, 366 or 365.25) per year in the same units
# (secs, hours, min, days) as coord

time_year <- function(coord, Coordenate_option, year){

  unit_value <- units(coord)
  if(Coordenate_option == "y"){
    year_value <- year
  } else {
    year_value <- ifelse(year%%4L == 0L, 366L, 365L)
  }

  if (unit_value == "secs"){
    time_value <- year_value * 24 * 60 * 60
  } else if (unit_value == "mins") {
    time_value <- year_value * 24 * 60
  } else if (unit_value == "hours") {
    time_value <- year_value * 24
  } else {
    time_value <- year_value
  }
  return(time_value)
}

# Calcula la longitud media del anyo entre dos fechas teniendo en cuenta bisiestos
length_year <- function(date1, date2){
   suma.time <- 0L
   suma.weights <- 0L
   year.birth <- as.numeric(format(date1, "%Y"))
   end.year <-  as.POSIXct(paste0(year.birth, "-12-31", " 23:59:59"), tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
   year.event <- as.numeric(format(date2, "%Y"))
   start.year <- as.POSIXct(paste0(year.event, "-01-01", " 00:00:00"), tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")

   # Anyo de nacimiento
   tiempo <- ifelse(year.birth%%4L == 0L, 366L, 365L)
   peso <- end.year - date1
   unidades <- units(peso)
   if (unidades == "secs"){
     time_value <- 24 * 60 * 60
   } else if (unidades == "mins") {
     time_value <-  24 * 60
   } else if (unidades == "hours") {
     time_value <- 24
   } else {
     time_value <- 1
   }
   peso <- (peso/time_value)/tiempo
   suma.weights <- suma.weights + as.numeric(peso)
   suma.time <- suma.time + as.numeric(tiempo*peso)

   # Anyos intermedios
   n.anyos <- as.numeric(format(date2, "%Y")) - as.numeric(format(date1, "%Y")) - 1L
   n29f <- min(as.numeric(format(date1, "%Y"))):max(as.numeric(format(date2, "%Y")))
   n29f <- n29f[n29f%%4L == 0L]
   n29f <- n29f[!(n29f%%100 == 0L & n29f%%400 != 0L)] # Se excluyen aparentes bisiestos como 1900 ó 2100
   if (length(n29f != 0L)){
     n29f <- as.POSIXct(paste0(n29f, "-02-29", " 12:00:00"), tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
     min.condicion <- function(d1) sum(n29f > d1)
     n29f.min <- sapply(X = end.year, FUN = min.condicion)
     max.condicion <- function(d2) sum(n29f < d2)
     n29f.max <- sapply(X = start.year, FUN = max.condicion)
     n29f.r <- n29f.min + n29f.max - length(n29f)
   } else {
     n29f.r <- 0L
   }
   n.anyos[n.anyos < 1L] <- 0L
   n29f.r[n29f.r < 1L] <- 0L
   suma.weights <- suma.weights + n.anyos
   suma.time <- suma.time + ((n.anyos - n29f.r) * 365L + n29f.r * 366L)

   # Anyo del evento
   tiempo <- ifelse(year.event%%4L == 0L, 366L, 365L)
   peso <- date2 - start.year
   unidades <- units(peso)
   if (unidades == "secs"){
     time_value <- 24 * 60 * 60
   } else if (unidades == "mins") {
     time_value <-  24 * 60
   } else if (unidades == "hours") {
     time_value <- 24
   } else {
     time_value <- 1
   }
   peso <- (peso/time_value)/tiempo
   suma.weights <- suma.weights + as.numeric(peso)
   suma.time <- suma.time + as.numeric(tiempo*peso)

   # Longitud media
   ave.length.year <- suma.time / suma.weights

   return(ave.length.year)
}

# Simula instantes dentro del dia cuando nacimiento y evento se producen en el mismo dia
same_day <- function(birth, event, random.b, random.e){
  if(random.b & !random.e){
    h.str <- (as.numeric(format(event, "%H")) +
              as.numeric(format(event, "%M"))/60 +
              as.numeric(format(event, "%S"))/3600)/24
    h.str <- stats::runif(length(h.str), 0, h.str)/365.25
  }
  if(!random.b & random.e){
    h.str <- (as.numeric(format(birth, "%H")) +
              as.numeric(format(birth, "%M"))/60 +
              as.numeric(format(birth, "%S"))/3600)/24
    h.str <- (1L - stats::runif(length(h.str), h.str, 1L))/365.25
  }
  return(h.str)
}

# Simula dentro de un mismo dia un instante posterior a uno dado
simula_post <- function(fijo){
  h.str <- as.numeric(format(fijo, "%H"))*3600 +
           as.numeric(format(fijo, "%M"))*60 +
           as.numeric(format(fijo, "%S"))
  h.str <- 86400 - h.str
  salida <- fijo + stats::runif(length(fijo), 0, h.str)
  return(salida)
}

# Simula dentro de un mismo dia un instante anterior a uno dado
simula_ant <- function(fijo){
  h.str <- as.numeric(format(fijo, "%H"))*3600 +
    as.numeric(format(fijo, "%M"))*60 +
    as.numeric(format(fijo, "%S"))
  salida <- fijo - stats::runif(length(fijo), 0, h.str)
  return(salida)
}

# Asigna a cada sujeto que le ha ocurrido un evento de muerte o emigracion en quarter.age, quarter.season, triangle
# la clave que le corresponde a utilizar en la funcion time_exposed_x_out para calcular
# su tiempo de exposicion durante el trimestre quarter.age.0, quarter.season.0 de la edad x
key_assignment_x_out <- function(quarter.age, quarter.season, triangle, quarter.age.0, quarter.season.0){

  #quarter.age <- datos.p$quarter.age
  #quarter.season <- datos.p$quarter.season
  #triangle <- datos.p$triangle
  #quarter.age.0 <- j
  #quarter.season.0 <- i

  df.x.out <- structure(list(quarter.age = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,
                                             4L, 4L, 4L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L,
                                             3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 2L, 2L, 2L,
                                             2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
                                             4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                                             2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
                                             4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
                             quarter.season = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
                                                4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
                                                4L, 4L, 4L),
                             triangle = c("low", "upp", "low", "low", "upp",
                                          "low", "low", "upp", "low", "low", "upp", "low", "upp", "upp",
                                          "low", "low", "low", "upp", "upp", "upp", "low", "low", "low",
                                          "low", "upp", "upp", "upp", "low", "low", "low", "low", "upp",
                                          "upp", "upp", "low", "upp", "upp", "low", "low", "low", "upp",
                                          "upp", "upp", "upp", "low", "low", "low", "low", "low", "upp",
                                          "upp", "upp", "upp", "upp", "low", "low", "low", "low", "low",
                                          "low", "upp", "upp", "upp", "upp", "upp", "low", "upp", "upp",
                                          "low", "low", "low", "upp", "upp", "upp", "upp", "low", "low",
                                          "low", "low", "low", "upp", "upp", "upp", "upp", "upp", "upp",
                                          "low", "low", "low", "low", "low", "low", "low", "upp", "upp",
                                          "upp", "upp", "upp", "upp", "upp"),
                             quarter.age.0 = c(1L, 1L,
                                               2L, 1L, 2L, 3L, 2L, 3L, 4L, 3L, 4L, 1L, 1L, 1L, 2L, 1L, 1L, 2L,
                                               2L, 1L, 3L, 2L, 2L, 1L, 3L, 3L, 2L, 4L, 3L, 3L, 2L, 4L, 4L, 3L,
                                               1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 3L, 2L, 2L, 1L, 1L, 3L,
                                               3L, 2L, 2L, 1L, 4L, 3L, 3L, 2L, 2L, 1L, 4L, 4L, 3L, 3L, 2L, 1L,
                                               1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 3L, 2L, 2L, 1L, 1L, 3L, 3L,
                                               2L, 2L, 1L, 1L, 4L, 3L, 3L, 2L, 2L, 1L, 1L, 4L, 4L, 3L, 3L, 2L,
                                               2L, 1L),
                             quarter.season.0 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                  1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L,
                                                  2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 3L, 3L, 2L, 3L, 3L, 2L,
                                                  3L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 1L, 3L, 2L, 2L, 1L, 1L, 3L, 3L,
                                                  2L, 2L, 1L, 1L, 3L, 2L, 2L, 1L, 1L, 4L, 4L, 3L, 4L, 4L, 3L, 4L,
                                                  3L, 3L, 2L, 4L, 4L, 3L, 3L, 2L, 4L, 3L, 3L, 2L, 2L, 1L, 4L, 4L,
                                                  3L, 3L, 2L, 2L, 1L, 4L, 3L, 3L, 2L, 2L, 1L, 1L),
                             key = c(10L, 11L, 12L,
                                     6L, 11L, 13L, 17L, 11L, 14L, 22L, 11L, 10L, 15L, 4L, 12L, 1L,
                                     4L, 15L, 7L, 6L, 13L, 6L, 7L, 6L, 15L, 16L, 17L, 14L, 17L, 16L,
                                     17L, 15L, 21L, 22L, 10L, 18L, 5L, 12L, 2L, 5L, 18L, 4L, 1L, 4L,
                                     13L, 1L, 4L, 1L, 4L, 18L, 7L, 6L, 7L, 6L, 14L, 6L, 7L, 6L, 7L,
                                     6L, 18L, 16L, 17L, 16L, 17L, 10L, 19L, 8L, 12L, 3L, 8L, 19L,
                                     5L, 2L, 5L, 13L, 2L, 5L, 2L, 5L, 19L, 4L, 1L, 4L, 1L, 4L, 14L,
                                     1L, 4L, 1L, 4L, 1L, 4L, 19L, 7L, 6L, 7L, 6L, 7L, 6L),
                             ID = c("11low11",
                                    "11upp11", "21low21", "21low11", "21upp21", "31low31", "31low21",
                                    "31upp31", "41low41", "41low31", "41upp41", "12low12", "12upp12",
                                    "12upp11", "22low22", "22low12", "22low11", "22upp22", "22upp21",
                                    "22upp11", "32low32", "32low22", "32low21", "32low11", "32upp32",
                                    "32upp31", "32upp21", "42low42", "42low32", "42low31", "42low21",
                                    "42upp42", "42upp41", "42upp31", "13low13", "13upp13", "13upp12",
                                    "23low23", "23low13", "23low12", "23upp23", "23upp22", "23upp12",
                                    "23upp11", "33low33", "33low23", "33low22", "33low12", "33low11",
                                    "33upp33", "33upp32", "33upp22", "33upp21", "33upp11", "43low43",
                                    "43low33", "43low32", "43low22", "43low21", "43low11", "43upp43",
                                    "43upp42", "43upp32", "43upp31", "43upp21", "14low14", "14upp14",
                                    "14upp13", "24low24", "24low14", "24low13", "24upp24", "24upp23",
                                    "24upp13", "24upp12", "34low34", "34low24", "34low23", "34low13",
                                    "34low12", "34upp34", "34upp33", "34upp23", "34upp22", "34upp12",
                                    "34upp11", "44low44", "44low34", "44low33", "44low23", "44low22",
                                    "44low12", "44low11", "44upp44", "44upp43", "44upp33", "44upp32",
                                    "44upp22", "44upp21", "44upp11")), class = "data.frame",
                        row.names = c(NA, -100L))

  df1 <- as.data.frame(paste0(quarter.age,
                              quarter.season,
                              triangle,
                              quarter.age.0,
                              quarter.season.0))

  colnames(df1) <- c("ID")

  df1$contador  <- 1L:nrow(df1)
  output <- base::merge(df1, df.x.out, by = "ID", all.x = TRUE)
  output <- output[base::order(output$contador), ]

  output <- output[ ,-c(1L:2L)]

  return(output)
}

# Asigna a cada sujeto que le ha ocurrido un evento de muerte o emigracion en quarter.age, quarter.season, triangle
# la clave que le corresponde a utilizar en la funcion time_exposed_x_out para calcular
# su tiempo de exposicion durante el trimestre quarter.age.0, quarter.season.0 de la edad x-1
key_assignment_x_1_out <- function(quarter.age, quarter.season, triangle , quarter.age.0, quarter.season.0){

  df.x.1.out <- structure(list(quarter.age = c(1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
                                 3L, 3L, 3L, 4L),
                 quarter.season = c(1L, 2L, 2L, 2L, 2L, 2L, 3L,
                                    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L,
                                    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
                                    4L, 4L, 4L, 4L, 4L),
                 triangle = c("low", "low", "low", "low",
                               "upp", "low", "low", "low", "low", "low", "low", "upp", "upp",
                               "upp", "low", "low", "low", "upp", "low", "low", "low", "low",
                               "low", "low", "low", "low", "upp", "upp", "upp", "upp", "upp",
                               "low", "low", "low", "low", "low", "upp", "upp", "upp", "low",
                               "low", "low", "upp", "low"),
                 quarter.age.0 = c(4L, 4L, 4L, 3L,
                                   4L, 4L, 4L, 4L, 3L, 3L, 2L, 4L, 4L, 3L, 4L, 4L, 3L, 4L, 4L, 4L,
                                   4L, 3L, 3L, 2L, 2L, 1L, 4L, 4L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 2L,
                                   4L, 4L, 3L, 4L, 4L, 3L, 4L, 4L),
                 quarter.season.0 = c(1L, 2L,
                                      1L, 1L, 1L, 1L, 3L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L,
                                      1L, 4L, 3L, 3L, 2L, 2L, 1L, 1L, 3L, 2L, 2L, 1L, 1L, 3L, 2L, 2L,
                                      1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L),
                 key = c(1L, 2L, 5L,
                         2L, 1L, 1L, 3L, 8L, 3L, 8L, 3L, 2L, 5L, 2L, 2L, 5L, 2L, 1L, 1L,
                         9L, 20L, 9L, 20L, 9L, 20L, 9L, 3L, 8L, 3L, 8L, 3L, 3L, 8L, 3L,
                         8L, 3L, 2L, 5L, 2L, 2L, 5L, 2L, 1L, 1L),
                 ID = c("11low41", "12low42",
                        "12low41", "12low31", "12upp41", "22low41", "13low43", "13low42",
                        "13low32", "13low31", "13low21", "13upp42", "13upp41", "13upp31",
                        "23low42", "23low41", "23low31", "23upp41", "33low41", "14low44",
                        "14low43", "14low33", "14low32", "14low22", "14low21", "14low11",
                        "14upp43", "14upp42", "14upp32", "14upp31", "14upp21", "24low43",
                        "24low42", "24low32", "24low31", "24low21", "24upp42", "24upp41",
                        "24upp31", "34low42", "34low41", "34low31", "34upp41", "44low41"
                         )
                 ),
            class = "data.frame",
            row.names = c(NA, -44L))

  df1 <- as.data.frame(paste0(quarter.age,
                              quarter.season,
                              triangle,
                              quarter.age.0,
                              quarter.season.0))
  colnames(df1) <- c("ID")

  df1$contador  <- 1L:nrow(df1)
  output <- base::merge(df1, df.x.1.out, by = "ID", all.x = TRUE)
  output <- output[base::order(output$contador), ]

  output <- output[, -c(1L:2L)]

  return(output)
}


# Data unas coordenadas de ocurrencia de evento de muerte o emigracion, calcula el tiempo de exposicion
# que el sujeto ha estado expuesto al riesgo con edad x para la key (el escenario) de calculo
time_exposed_x_out <- function (coord.time, coord.age, key, year){


  #coord.time <- x$coord.time
  #dec.x <- x$dec.x
  #key <- x$key.x
  # Reescalamos con el year. scale. Directamente

  #dec.x <- dec.x * ifelse(year%%4 == 0, 366/365.25, 365/365.25)

  ######### En el excel
  # Cambiar "year por *age"
  #

  time.exposed <- ifelse(key == 1L, coord.time - coord.age,
                         ifelse(key == 2L, coord.time - coord.age - 0.25,
                         ifelse(key == 3L, coord.time - coord.age - 0.5,
                         ifelse(key == 4L, coord.age - coord.time  + 0.25,
                         ifelse(key == 5L, coord.age - coord.time  + 0.5,
                         ifelse(key == 6L, coord.time - coord.age + 0.25,
                         ifelse(key == 7L, coord.age - coord.time,
                         ifelse(key == 8L, coord.age - coord.time + 0.75,
                         ifelse(key == 9L, coord.time - coord.age - 0.75,
                         ifelse(key == 10L, coord.age,
                         ifelse(key == 11L, coord.time,
                         ifelse(key == 12L, coord.age - 0.25,
                         ifelse(key == 13L, coord.age - 0.5,
                         ifelse(key == 14L, coord.age - 0.75,
                         ifelse(key == 15L, coord.time - 0.25,
                         ifelse(key == 16L, coord.age - coord.time - 0.25,
                         ifelse(key == 17L, coord.time - coord.age + 0.5,
                         ifelse(key == 18L, coord.time - 0.5,
                         ifelse(key == 19L, coord.time - 0.75,
                         ifelse(key == 20L, coord.age - coord.time + 1L,
                         ifelse(key == 21L, coord.age - coord.time - 0.5,
                         ifelse(key == 22L, coord.time - coord.age + 0.75,
                                NA))))))))))))))))))))))

  return(time.exposed)

}

# Data unas coordenadas de ocurrencia de evento de muerte o emigracion, calcula el tiempo de exposicion
# que el sujeto ha estado expuesto al riesgo con edad x-1 para la key (el escenario) de calculo
time_exposed_x_1_out <- function (coord.time, coord.age, key, year){

  # coord.age <- coord.age * ifelse(year%%4 == 0, 366/365.25, 365/365.25)

  time.exposed <- ifelse(key == 1L, coord.time - coord.age,
                         ifelse(key == 2L, coord.time - coord.age - 0.25,
                         ifelse(key == 3L, coord.time - coord.age - 0.5,
                         ifelse(key == 5L, coord.age - coord.time + 0.5,
                         ifelse(key == 8L, coord.age - coord.time + 0.75,
                         ifelse(key == 9L, coord.time - coord.age - 0.75,
                         ifelse(key == 20L, coord.age - coord.time + 1L,
                                NA)))))))

  return(time.exposed)

}


key_assignment_x_in <- function(quarter.age, quarter.season, triangle, quarter.age.0, quarter.season.0){

  df.x.in <- structure(list(quarter.age = c(4L, 4L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L,
                                            3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 4L, 4L, 3L,
                                            3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L,
                                            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L,
                                            2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                            1L, 1L, 1L, 1L, 1L),
                            quarter.season = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                               3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L,
                                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                               1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                               1L, 1L, 1L, 1L, 1L),
                            triangle = c("upp", "low", "upp", "upp", "low", "upp", "upp", "low", "upp", "upp", "low", "upp",
                                         "low", "low", "upp", "upp", "upp", "low", "low", "low", "upp", "upp", "upp", "upp",
                                         "low", "low", "low", "upp", "upp", "upp", "upp", "low", "low", "low", "upp", "low",
                                         "low", "upp", "upp", "upp", "low", "low", "low", "low", "upp", "upp", "upp", "upp",
                                         "upp", "low", "low", "low", "low", "low", "upp", "upp", "upp", "upp", "upp", "upp",
                                         "low", "low", "low", "low", "low", "upp", "low", "low", "upp", "upp", "upp", "low",
                                         "low", "low", "low", "upp", "upp", "upp", "upp", "upp", "low", "low", "low", "low",
                                         "low", "low", "upp", "upp", "upp", "upp", "upp", "upp", "upp", "low", "low", "low",
                                         "low", "low", "low", "low"),
                            quarter.age.0 = c(4L, 4L, 3L, 4L, 3L, 2L, 3L, 2L, 1L, 2L, 1L, 4L, 4L, 4L, 3L, 4L, 4L, 3L, 3L, 4L,
                                              2L, 3L, 3L, 4L, 2L, 2L, 3L, 1L, 2L, 2L, 3L, 1L, 1L, 2L, 4L, 4L, 4L, 3L, 4L, 4L,
                                              3L, 3L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 2L, 2L, 3L, 3L, 4L, 1L, 2L, 2L, 3L, 3L, 4L,
                                              1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L, 3L, 4L, 4L, 3L, 3L, 4L, 4L, 2L, 3L, 3L, 4L, 4L,
                                              2L, 2L, 3L, 3L, 4L, 4L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L),
                            quarter.season.0 = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 4L, 3L, 3L, 4L, 3L, 4L,
                                                 4L, 3L, 3L, 4L, 4L, 3L, 4L, 4L, 3L, 3L, 4L, 4L, 3L, 4L, 4L, 2L, 2L, 3L, 2L,
                                                 2L, 3L, 2L, 3L, 3L, 4L, 2L, 2L, 3L, 3L, 4L, 2L, 3L, 3L, 4L, 4L, 2L, 2L, 3L,
                                                 3L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 3L, 1L,
                                                 1L, 2L, 2L, 3L, 1L, 2L, 2L, 3L, 3L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 1L, 2L,
                                                 2L, 3L, 3L, 4L, 4L),
                            key = c(10L, 11L, 12L, 4L, 11L, 13L, 9L, 11L, 16L, 21L, 11L, 10L, 14L, 3L, 12L, 1L, 3L, 14L,
                                    7L, 4L, 13L, 4L, 7L, 4L, 14L, 17L, 9L, 16L, 9L, 17L, 9L, 14L, 22L, 21L, 10L, 19L, 5L,
                                    12L, 2L, 5L, 19L, 3L, 1L, 3L, 13L, 1L, 3L, 1L, 3L, 19L, 7L, 4L, 7L, 4L, 16L, 4L, 7L,
                                    4L, 7L, 4L, 19L, 17L, 9L, 17L, 9L, 10L, 18L, 8L, 12L, 6L, 8L, 18L, 5L, 2L, 5L, 13L, 2L, 5L,
                                    2L, 5L, 18L, 3L, 1L, 3L, 1L, 3L, 16L, 1L, 3L, 1L, 3L, 1L, 3L, 18L, 7L, 4L, 7L, 4L, 7L, 4L),
                            ID = c("44upp44", "44low44", "34upp34", "34upp44", "34low34", "24upp24", "24upp34", "24low24",
                                   "14upp14", "14upp24", "14low14", "43upp43", "43low43", "43low44", "33upp33", "33upp43",
                                   "33upp44", "33low33", "33low34", "33low44", "23upp23", "23upp33", "23upp34", "23upp44",
                                   "23low23", "23low24", "23low34", "13upp13", "13upp23", "13upp24", "13upp34", "13low13",
                                   "13low14", "13low24", "42upp42", "42low42", "42low43", "32upp32", "32upp42", "32upp43",
                                   "32low32", "32low33", "32low43", "32low44", "22upp22", "22upp32", "22upp33", "22upp43",
                                   "22upp44", "22low22", "22low23", "22low33", "22low34", "22low44", "12upp12", "12upp22",
                                   "12upp23", "12upp33", "12upp34", "12upp44", "12low12", "12low13", "12low23", "12low24",
                                   "12low34", "41upp41", "41low41", "41low42", "31upp31", "31upp41", "31upp42", "31low31",
                                   "31low32", "31low42", "31low43", "21upp21", "21upp31", "21upp32", "21upp42", "21upp43",
                                   "21low21", "21low22", "21low32", "21low33", "21low43", "21low44", "11upp11", "11upp21",
                                   "11upp22", "11upp32", "11upp33", "11upp43", "11upp44", "11low11", "11low12", "11low22",
                                   "11low23", "11low33", "11low34", "11low44")),
                       class = "data.frame",row.names = c(NA, -100L))

  df1 <- as.data.frame(paste0(quarter.age,
                              quarter.season,
                              triangle,
                              quarter.age.0,
                              quarter.season.0))

  colnames(df1) <- c("ID")

  df1$contador  <- 1L:nrow(df1)
  output <- base::merge(df1, df.x.in, by = "ID", all.x = TRUE)
  output <- output[base::order(output$contador), ]

  output <- output[ ,-c(1L:2L)]

  return(output)
}

key_assignment_x_1_in <- function(quarter.age, quarter.season, triangle , quarter.age.0, quarter.season.0){

  df.x.1.in <- structure(list(quarter.age = c(4L, 4L, 4L, 4L, 4L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L,
                                              4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L,
                                              2L, 2L, 2L, 1L),
                              quarter.season = c(4L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                 1L, 1L, 1L, 1L, 1L, 1L),
                              triangle = c("upp", "upp", "upp", "upp", "low", "upp", "upp", "upp", "upp", "upp", "upp", "low",
                                           "low", "low", "upp", "upp", "upp", "low", "upp", "upp", "upp", "upp", "upp", "upp",
                                           "upp", "upp", "low", "low", "low", "low", "low", "upp", "upp", "upp", "upp", "upp",
                                           "low", "low", "low", "upp", "upp", "upp", "low", "upp"),
                              quarter.age.0 = c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L,
                                                1L, 2L, 2L, 3L, 3L, 4L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 2L, 1L,
                                                1L, 2L, 1L, 1L),
                              quarter.season.0 = c(4L, 3L, 4L, 4L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 3L, 4L, 4L, 3L, 4L, 4L, 4L, 4L,
                                                   1L, 2L, 2L, 3L, 3L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 3L, 4L,
                                                   4L, 3L, 4L, 4L, 4L, 4L),
                              key = c(1L, 2L, 5L, 2L, 1L, 1L, 6L, 8L, 6L, 8L, 6L, 2L, 5L, 2L, 2L, 5L, 2L, 1L, 1L, 15L, 20L, 15L, 20L, 15L,
                                      20L, 15L, 6L, 8L, 6L, 8L, 6L, 6L, 8L, 6L, 8L, 6L, 2L, 5L, 2L, 2L, 5L, 2L, 1L, 1L),
                              ID = c("44upp14", "43upp13", "43upp14", "43upp24", "43low14", "33upp14", "42upp12", "42upp13",
                                     "42upp23", "42upp24", "42upp34", "42low13", "42low14", "42low24", "32upp13", "32upp14",
                                     "32upp24", "32low14", "22upp14", "41upp11", "41upp12", "41upp22", "41upp23", "41upp33",
                                     "41upp34", "41upp44", "41low12", "41low13", "41low23", "41low24", "41low34", "31upp12",
                                     "31upp13", "31upp23", "31upp24", "31upp34", "31low13", "31low14", "31low24", "21upp13",
                                     "21upp14", "21upp24", "21low14", "11upp14")),
                         class = "data.frame", row.names = c(NA, -44L))

  df1 <- as.data.frame(paste0(quarter.age,
                              quarter.season,
                              triangle,
                              quarter.age.0,
                              quarter.season.0))
  colnames(df1) <- c("ID")

  df1$contador  <- 1L:nrow(df1)
  output <- base::merge(df1, df.x.1.in, by = "ID", all.x = TRUE)
  output <- output[base::order(output$contador), ]

  output <- output[, -c(1L:2L)]

  return(output)
}


time_exposed_x_in <- function (coord.time, coord.age, key, year){

  time.exposed <- ifelse(key == 1L, coord.age - coord.time ,
                         ifelse(key == 2L, coord.age - coord.time  - 0.25,
                         ifelse(key == 3L, coord.time - coord.age + 0.25,
                         ifelse(key == 4L, coord.age - coord.time  + 0.25,
                         ifelse(key == 5L, coord.time - coord.age  + 0.5,
                         ifelse(key == 6L, coord.age - coord.time - 0.5,
                         ifelse(key == 7L, coord.time - coord.age,
                         ifelse(key == 8L, coord.time - coord.age + 0.75,
                         ifelse(key == 9L, coord.age - coord.time + 0.5,
                         ifelse(key == 10L, 1L - coord.age,
                         ifelse(key == 11L, 1L - coord.time,
                         ifelse(key == 12L, 0.75 - coord.age,
                         ifelse(key == 13L, 0.5 - coord.age,
                         ifelse(key == 14L, 0.75 - coord.time,
                         ifelse(key == 15L, coord.age - coord.time - 0.75,
                         ifelse(key == 16L, 0.25 - coord.age,
                         ifelse(key == 17L, coord.time - coord.age - 0.25,
                         ifelse(key == 18L, 0.25 - coord.time,
                         ifelse(key == 19L, 0.5 - coord.time,
                         ifelse(key == 20L, coord.time - coord.age + 1L,
                         ifelse(key == 21L, coord.age - coord.time + 0.75,
                         ifelse(key == 22L, coord.time - coord.age - 0.5,
                                NA))))))))))))))))))))))

  return(time.exposed)

}

time_exposed_x_1_in <- function (coord.time, coord.age, key, year){

  time.exposed <- ifelse(key == 1L, coord.age - coord.time ,
                         ifelse(key == 2L, coord.age - coord.time - 0.25,
                         ifelse(key == 5L, coord.time - coord.age + 0.5,
                         ifelse(key == 6L, coord.age - coord.time - 0.5,
                         ifelse(key == 8L, coord.time - coord.age + 0.75,
                         ifelse(key == 15L, coord.age - coord.time  - 0.75,
                         ifelse(key == 20L, coord.time - coord.age + 1,
                                NA)))))))

  return(time.exposed)

}


