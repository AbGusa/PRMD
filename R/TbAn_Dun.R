#' Tabla ANOVA del Modelo DUNCAN.
#'
#' Obtiene la Tabla de Análisis de Varianza (ANOVA) para la Prueba de Rangos Multiples DUNCAN (PRMD).
#'
#' @param Respuesta (vector) datos de la variable respuesta.
#' @param Tratamiento (vector) datos de la variable que representan el tratamientos.
#' @param Sig (string) variable que representa la significancia del modelo.
#' @param data (\code{data.frame}) Tabla de datos en formato largo con los datos de los tratamientos y de la variable respuesta.
#' @return Devuelve una tabla en formato \code{data.frame} con los cálculos correspondientes al análisis de varianza que compete a la Prueba de DUNCAN.
#' @export
TabAnoDUN <- function(Respuesta = RESPUESTA, Tratamiento = TRATAMIENTOS, Sig = 0.05, data) {

##Defino a mis variables como factores

y <- data [, Respuesta]
Trat <- factor(data[,Tratamiento])
a <- nlevels(Trat) #numero de observaciones por tratamiento

## Calcular el numero de respuestas totales

n_resp <- length(y)

## Calcular el numero de respuestas por tratamiento

n_respxtrat <- tapply(y, INDEX = Trat, FUN = length)

n_respxtrattot <- (n_respxtrat [1])

## Suma de respuestas por tratamiento

sum_respxtrat <- tapply(y, INDEX = Trat, FUN = sum)

sum_totxtrat <- sum(sum_respxtrat)

## Calcular el promedio de los totales

prom_Tots <- ((sum_totxtrat)/(n_resp))

## Calcular el promedio de las respuestas por tratamiento

prom_Yi <- tapply(y, INDEX = Trat, FUN = mean)

## Suma de el promedio de las repsuestas por tartamiento

sumprom_respxtrat <- sum(prom_Yi)

## Calcular el promedio del promedio de las respuestas por tratatmiento

prom_prom <- ((sumprom_respxtrat)/(a))

### Paso 2: Construir la tabla Anova respecto al modelo de DUNCAN

## Calcular la suma de cuadrados

FC <- (((sum_totxtrat)^2)/((n_respxtrattot)*(a)))

## Suma de cuadrados total

SC_tot <- ((sum((y)^2))-FC)

## Suma dde cuadrados por tratamiento

SC_trat <- (((sum((sum_respxtrat)^2))/(n_respxtrattot))-FC)

## Suma de cuadrados del error

SC_error <- ((SC_tot)-(SC_trat))

SC_sum <-(SC_trat + SC_error)

## Calcular los grados de libertad

g <- 1

glet <- ((a)-(g))

gldt_error <- (((a)*((n_respxtrattot)-(g))))

gl_totL <- (((a)*(n_respxtrattot))-(g))

CMET <- ((SC_trat)/(glet))

CME <- ((SC_error)/(gldt_error))

Fc <- ((CMET)/(CME))

## Grados de libertad

Ft <- (qf(1-Sig, glet, gldt_error))

## Calcular el coeficiente de variación

CV <- (((sqrt(CME))/(prom_prom))*100)

CV_porciento <- paste(CV, "%")


tablanov <- data.frame(FV = c("TRATAMIENTOS", "ERROR", "TOTAL", "CV"),
                       SC = c(SC_trat, SC_error, SC_sum, NA),
                       GI = c(glet, gldt_error, gl_totL, NA),
                       CM = c(CMET,CME, NA, CV_porciento),
                       Fc = c(Fc,NA, NA, NA),
                       Ft = c(Ft, NA, NA, NA),
                       check.names = FALSE)

rownames(tablanov) <- NULL
AnoDUN <- format(tablanov)
AnoDUN[is.na(tablanov)] <- ""

return(AnoDUN)

}

