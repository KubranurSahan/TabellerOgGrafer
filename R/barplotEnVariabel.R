#' Barplot af en variabel fra et datasæt
#' @encoding UTF-8
#'
#' @param datasaet en data.frame, Datasættet med de variable som skal bruges til plottet.
#' @param xVariablen en character string, Navnet på x-variablen i datasaet
#' @param yVariablen en character string, Navnet på y-variablen i datasaet
#' @param overskrift en character vector af en maks. længde på 4 eller NA, Overskriften på plottet. Hvis NA så vil overskriften sættes på ud fra xVariablen og yVariablen
#' @param navnPaaXAksen en character string eller NA, Navnet på x-aksen
#' @param navnPaaYAksen en character string eller NA, Navnet på y-aksen
#' @param inputTilLegend en chracter vector, ekstra informationer/tekst som man ønsker at have på som legend. Hvis NA, vil der ikke komme legend på grafen.
#'
#' @details yVariablen i datasaet skal være af typen numeric/integer.
#'
#' @return En barplot med eventuelt legend.
#' @export
#'
#' @examples
#' antalPatienterDat <- data.frame("maaned" = month.name[1:12], "antal" = sample(100:150, 12))
#' barplotEnVariabel(datasaet = antalPatienterDat,
#'                   xVariablen = "maaned",
#'                   yVariablen = "antal",
#'                   overskrift = "Antal patienter med x sygdom i 2018" ,
#'                   navnPaaXAksen = "Maaned for besoeg paa afdelingen",
#'                   navnPaaYAksen = "Antal patienter",
#'                   inputTilLegend = c(paste0("Total antal pat. i 2018 = ",
#'                                      sum(antalPatienterDat$antal)),
#'                                      paste0("Gnst. antal pat. pr maaned = ",
#'                                      round(mean(antalPatienterDat$antal),1))))
#'
barplotEnVariabel <- function(datasaet,
                              xVariablen,
                              yVariablen,
                              overskrift = NA,
                              navnPaaXAksen = NA,
                              navnPaaYAksen = NA,
                              inputTilLegend = NA){

  # Tjekker om inputvariablene har den rigtige objecttype
  if(!("data.frame" %in% class(datasaet)) |
     class(xVariablen) != "character" | length(xVariablen) != 1 |
     class(yVariablen) != "character" | length(yVariablen) != 1 |
     (class(overskrift) != "character" & all(!is.na(overskrift))) |
     (class(navnPaaXAksen) != "character" & !is.na(navnPaaXAksen) & length(navnPaaXAksen) != 1) |
     (class(navnPaaYAksen) != "character" & !is.na(navnPaaYAksen) & length(navnPaaYAksen) != 1) |
     (class(inputTilLegend) != "character" & all(!is.na(inputTilLegend)))){

    stop("En eller flere af de angivne variabler har ikke den rigtige objekttype.
    Følgende skal gælde for input til denne funktion:
    - datasaet skal være en data.frame.
    - xVariablen og yVariablen skal være en character string af længden 1.
    - Resten skal enten være character eller NA.")

  }

  # Tjekker om variablene findes i datasættet
  if(any(!(c(xVariablen, yVariablen) %in% colnames(datasaet)))){

    stop("Variablen/variablene ", paste0(c(xVariablen, yVariablen)[!(c(xVariablen, yVariablen) %in% colnames(datasaet))], collapse = " og "), " findes ikke i datasættet ", deparse(substitute(datasaet)), ".")

  }

  # Tjekker om overskrift overholder grænserne
  if(any(!is.na(overskrift)) & (any(grepl("\n", overskrift, fixed = TRUE)) | length(overskrift) > 4 | max(nchar(overskrift)) > 80)){

    stop("overskrift overholder ikke begrænsningerne. En eller flere af følgende begrænsninger er ikke overholdt:
    - overskrift er en character vector med en længde på maks. 4 (antal entries)
    - hvert entry består af maks. 80 tegn
    - indeholder ikke newline '\\n'
    Begrænsninger er sat for at grafen skal kunne aflæses på en 10x5 inch ramme.")

  }


  # Tjekker om inputTilLegend overholder grænserne
  if(any(!is.na(inputTilLegend)) & (any(grepl("\n", inputTilLegend, fixed = TRUE)) | length(inputTilLegend) > 7 | max(nchar(inputTilLegend)) > 35)){

    stop("inputTilLegend overholder ikke begrænsningerne. En eller flere af følgende begrænsninger er ikke overholdt:
    - inputTilLegend er en character vector med en længde på maks. 7 (antal entries)
    - hvert entry består af maks. 35 tegn
    - indeholder ikke newline '\\n'
    Begrænsninger er sat for at grafen skal kunne aflæses på en 10x5 inch ramme.")

  }

  # Definerer overskrift og teksten på akserne, hvis ikke de er angivet
  if(all(is.na(overskrift))){

    overskrift <- paste0(yVariablen, " mod ", xVariablen)

  }

  if(is.na(navnPaaXAksen)){

    navnPaaXAksen <- xVariablen

  }

  if(is.na(navnPaaYAksen)){

    navnPaaYAksen <- yVariablen

  }

  # Ændrer navnet på variablene i datasættet for en nemmere tilgang
  colnames(datasaet)[colnames(datasaet) == xVariablen] <- "xVariablen"
  colnames(datasaet)[colnames(datasaet) == yVariablen] <- "yVariablen"

  # Laver en vektor af variablene
  vektorGraf <- as.vector(datasaet$yVariablen)
  names(vektorGraf) <- as.vector(paste0(datasaet$xVariablen))

  tabelGraf <- as.table(vektorGraf)

  # Plotter grafen
  plot = barplot(tabelGraf, las = 2, col = voresFarver[1],
                 ylim = c(0, max(tabelGraf) + max(tabelGraf) * 0.50), main = overskrift,
                 cex.names = 0.7, xaxs = "r", yaxs = "r")

  # Plotter titlen på y-aksen
  title(ylab = navnPaaYAksen, font.lab = 2, line = 3.2)

  # Plotter x-aksen
  axis(side = 1, at =  seq(from = 0.125, to = (1.2 * nrow(tabelGraf) + 0.25), by = 1.2), labels = FALSE)

  # Plotter dataværdierne
  tabelGrafA <- tabelGraf
  tabelGrafA[tabelGrafA == 0] <- ""
  text(plot, (tabelGraf + max(tabelGraf) * 0.03), tabelGrafA, cex = 0.5, font = 1)
  title(xlab = navnPaaXAksen, line = 3.5, font.lab = 2)

  # Hvis der er angivet input til legend kommer denne også å
  if(all(!is.na(inputTilLegend))){

    legend("topright", inputTilLegend, cex = 0.7, bty = "n", inset = c(0.03, 0), y.intersp = 0.85, x.intersp = -1, text.width = max(strwidth(inputTilLegend))/2 )

  }

}
