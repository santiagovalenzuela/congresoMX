curules_rp <- function(datos, nulos, no_registrados, independientes) {
  #' Calcular curules de representación proporcional (RP)
  #'
  #' Esta función permite calcular las curules de representación proporcional
  #' que corresponden a cada partido en la Cámara de Diputados de México.
  #'
  #' @param datos Debe ser un dataframe con tres columnas: los partidos políticos, la votación total emitida que obtuvieron, y las curules de mayoría relativa alcanzadas.
  #' @param nulos Vector que indica qué fila(s) debe(n) ser consideradas como votos nulos
  #' @param no_registrados Vector que indica qué fila(s) debe(n) ser consideradas como candidatos no registrados
  #' @param independientes Vector que indica qué fila(s) debe(n) ser consideradas como candidatos independientes
  #' @examples curules_rp(diputados_RP_2018, "VOTOS_NULOS", "CANDIDATURAS_NO_REGISTRADAS",  c("CandidaturaIndependiente_1", "CandidaturaIndependiente_2"))

  #Renombramos las columnas
  names(datos)[1] <-"PARTIDO_POLITICO"
  names(datos)[2] <-"VOTACION_TOTAL_EMITIDA"
  names(datos)[3] <-"CURULES_MR"

  #Quitamos las filas innecesarias
  datos <- subset(datos, !(PARTIDO_POLITICO %in% "TOTAL"))
  datos$PARTIDO_POLITICO <-as.character(datos$PARTIDO_POLITICO)


  #Obtenemos la Votación Total Emitida (VTE)
  vte <- sum(datos$VOTACION_TOTAL_EMITIDA)

  # Calculamos los porcentajes de la VTE
  datos$pc_vte <- round((datos$VOTACION_TOTAL_EMITIDA / vte*100),
                        digits = 4)

  # Restamos los votos nulos y a candidatos no registrados para obtener la
  # votación válida total emitida (VVE)

  vve_df <- subset(datos,
                   subset = !(PARTIDO_POLITICO %in% c(nulos, no_registrados)))

  vve <- sum(vve_df$VOTACION_TOTAL_EMITIDA)

  #Calculamos el porcentaje de la VVE
  vve_df$pc_vve <-round((vve_df$VOTACION_TOTAL_EMITIDA/vve)*100,
                        digits = 4)

  # Calculamos la Votación Nacional Emitida, quitando los votos a partidos que no
  # alcanzaron el 3% de la VTE, a Candidatos Independientes, y votos nulos

  vne <- sum(subset(datos, !(PARTIDO_POLITICO %in% c(independientes, nulos) | pc_vte < 3),
                    select= VOTACION_TOTAL_EMITIDA))

  vve_df$pc_vne <- round((vve_df$VOTACION_TOTAL_EMITIDA/vne)*100,
                         digits = 4)

  #Dividimos este número entre 200 para obtener el COCIENTE NATURAL
  cociente_nat <- vne/200

  #Creamos un nvo dataframe con los partidos que entran a RP:
  pp_rp <- subset(vve_df, !(PARTIDO_POLITICO %in% c(independientes, nulos) | pc_vte < 3))

  #Se divide la votación a cada partido entre ese cociente
  pp_rp$dips_cociente <- trunc((pp_rp$VOTACION_TOTAL_EMITIDA)/cociente_nat)

  #Calculamos las curules no asignadas por cociente
  no_asignadas <- 200-sum(pp_rp$dips_cociente)

  #Calculamos votos no utilizados y asignamos dips por resto mayor
  pp_rp$resto_mayor <-(pp_rp$VOTACION_TOTAL_EMITIDA - pp_rp$dips_cociente*cociente_nat)

  #Obtenemos los partidos a los que asignaremos curules por resto mayor
  pp_resto <- pp_rp[order(pp_rp$resto_mayor, decreasing = TRUE),]$PARTIDO_POLITICO[1:no_asignadas]

  pp_rp$dips_resto <-0
  pp_rp$dips_resto[pp_rp$PARTIDO_POLITICO %in% pp_resto] <-1

  no_asignadas <- no_asignadas - sum(pp_rp$dips_resto)

  # Sumamos las curules asignados a cada partido hasta este momento
  pp_rp$curules_RP <- pp_rp$dips_resto + pp_rp$dips_cociente
  pp_rp$dips_totales <- pp_rp$curules_RP + pp_rp$CURULES_MR

  # Calculamos el número max de curules que podría obtener cada partido sin
  # sobrepasar los límites legales

  pp_rp$max_dips <- trunc(((pp_rp$pc_vne + 8)/100)*500)
  pp_rp$max_dips[pp_rp$dips_totales > 300] <-300

  #Obtenemos los nombre de los partidos que exceden el número de curules permitidas:
  pp_excedentes <-pp_rp$PARTIDO_POLITICO[(pp_rp$dips_totales >300) |(pp_rp$dips_totales > pp_rp$max_dips)]
  #Para obtener las curules excedentes:
  excedentes <- sum(pp_rp$dips_totales[(pp_rp$dips_totales - pp_rp$max_dips > 0 ) == T] -
                      pp_rp$max_dips[(pp_rp$dips_totales - pp_rp$max_dips >0) == T])

  #Restamos las curules excedentes:
  pp_rp$curules_RP[(pp_rp$dips_totales > pp_rp$max_dips)] <-
    pmax(c(pp_rp$curules_RP[(pp_rp$dips_totales > pp_rp$max_dips)] - excedentes), 0) #Usamos pmax para no obtener valores negativos

  #Obtenemos la Votación Nacional Efectiva (VNEf)
  vnef <- sum(pp_rp$VOTACION_TOTAL_EMITIDA[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)])
  nvo_cociente <- vnef/(200-pp_rp$curules_RP[pp_rp$PARTIDO_POLITICO %in% pp_excedentes])

  pp_rp$dips_nvo_cociente <-0

  #Asignamos los diputados de los partidos que exceden los límites
  pp_rp$dips_nvo_cociente[pp_rp$PARTIDO_POLITICO %in% pp_excedentes] <-
    pp_rp$curules_RP[pp_rp$PARTIDO_POLITICO %in% pp_excedentes]

  #Se divide la votación a cada partido entre ese cociente
  pp_rp$dips_nvo_cociente[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)] <-
    trunc((pp_rp$VOTACION_TOTAL_EMITIDA)[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)]/ nvo_cociente)

  #Calculamos las curules no asignadas por cociente
  no_asignadas <- 200- sum(pp_rp$dips_nvo_cociente)

  #Calculamos votos no utilizados y asignamos diputados restantes por resto mayor
  pp_rp$nvo_resto_mayor <-0

  pp_rp$nvo_resto_mayor[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)] <-
    (pp_rp$VOTACION_TOTAL_EMITIDA[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)] -
       pp_rp$dips_nvo_cociente[!(pp_rp$PARTIDO_POLITICO %in% pp_excedentes)] * nvo_cociente)

  pp_rp$dips_nvo_resto <- 0
  pp_nvo_resto <- pp_rp[order(pp_rp$nvo_resto_mayor, decreasing = TRUE),]$PARTIDO_POLITICO[1:no_asignadas]
  pp_rp$dips_nvo_resto[pp_rp$PARTIDO_POLITICO %in% pp_nvo_resto] <-1
  pp_rp$CURULES_RP_finales <- pp_rp$dips_nvo_resto + pp_rp$dips_nvo_cociente

  datos$CURULES_RP <-0
  datos$CURULES_RP[datos$PARTIDO_POLITICO %in% pp_rp$PARTIDO_POLITICO] <- pp_rp$CURULES_RP_finales[pp_rp$PARTIDO_POLITICO %in% datos$PARTIDO_POLITICO]
  datos$CURULES_TOTALES <- datos$CURULES_MR + datos$CURULES_RP
  return(datos)
}
