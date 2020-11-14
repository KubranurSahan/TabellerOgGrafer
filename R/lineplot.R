# Funktion til at sætte titel på plot
#' Kom titel på en plot
#' @encoding UTF-8
#'
#' @description Funktion til at sætte titel på en plot.
#'     Funktionen sørger for at få delt titlen op i linjer efter opdelingen i string-vector og/eller newline-tegn.
#'     Funktionen melder fejl, hvis titlen er på mere end tre linjer.
#'     Funktionen tager udgangspunkt i default par-værdier.
#'
#' @param titel en character vector, Teksten som titlen skal bestå af. Se details for mere.
#' @param goerTekstenMindrePrLinje TRUE/FALSE, Hvis TRUE (by defualt) bliver størrelsen på titlen mindre og mindre per linje.
#'
#' @details Titlen må maks være på tre linjer. Teksten opdeles i linjer efter vectoren (hvert entry i titel svarer til en linje) og/eller med new-line '\\n'.
#'
#' @return ingen return. Titlen bliver sat på plottet i den aktive graphics device.
#' @export
#'
#' @examples
#' plot(c(1:12), sample(100:150, 12), type = "l", xlab = "maaned nr.", ylab = "antal patienter", font.lab = 2)
#' komTitelPaaPlot(titel = "Antal patienter med x sygdom \n paa afdeling y\n i perioden jan. 2018 - dec. 18")
#'
komTitelPaaPlot <- function(titel, goerTekstenMindrePrLinje = TRUE){

  if(class(titel) != "character"){

    stop("titel skal være af typen character og må ikke være NA.")

  }

  titelOpdelt <- unlist(strsplit(titel, "\n"))

  if(length(titelOpdelt) > 3){

    stop("titel består af for mange linjer.")

  }

  if(!(goerTekstenMindrePrLinje %in% c(TRUE, FALSE))){

    stop("goerTekstenMindrePrLinje skal enten have den logiske værdi TRUE eller FALSE.")

  }

  tekstStr <- 1.5

  for(i in 1:length(titelOpdelt)){


    if(goerTekstenMindrePrLinje & i != 1){

      tekstStr <- tekstStr - 0.25

    }

    title(main = titelOpdelt[i], cex.main = tekstStr, line = length(titelOpdelt) - (i - 1), font.main = 2)

  }
}



# Funktion til at dele en tekst op i linjer ved mellemrum efter den angivne maksimal længde som teksten må have pr linje.
#' Opdeling af tekst efter en bestemt bredde for teksten
#' @encoding UTF-8
#'
#' @description En funktion til at dele en lang tekst op i flere linjer, hvor hver linje overholder en maksimallængde angivet i inches.
#'
#'
#' @param teksten en character string, En tekst, som man ønsker skal have en bestemst længde pr linje. Tekten må ikke indeholde newline'\\n'.
#' @param maksLaengdeInch numeric/integer, Den maksimale længde teksten må have pr linje i inches
#' @param cexPaaTeksten numeric/integer, Tekststørrelsen på teksten
#'
#' @details Funktionen deler teksten op efter mellemrum. Hvis der er for langt mellem mellemrum og teksten eller dele af denne ikke kan deles op, så vil funktionen melde fejl.
#'
#' @return Teksten opdelt i linjer med newline ved mellemrum og med maksimal længde pr linje svarende til den angivne maksLaengdeInch.
#' @export
#'
#' @examples
#' tekstenOpdelt <- delTekstOpEfterInchLaengde(teksten = "Teksten maa ikke indeholde newline", maksLaengdeInch = 0.75)
#' cat(tekstenOpdelt)
#'
delTekstOpEfterInchLaengde <- function(teksten, maksLaengdeInch, cexPaaTeksten = 1){

  if(class(teksten) != "character" | !(class(maksLaengdeInch) %in% c("numeric","integer")) | !(class(cexPaaTeksten) %in% c("numeric","integer")) |
     length(teksten) != 1 | length(maksLaengdeInch) != 1 | length(cexPaaTeksten) != 1){

    stop("Mindst et af de inputs der er givet har ikke den rigtige objekttype.
         Sørg for at 'teksten' er af en character string og resten er numeric/integer.
         Alle objekter skal have længden en.")

  }
  if(any(grepl("\n", teksten, fixed = TRUE))){

    stop("Teksten må ikke indeholde newline '\\n'")

  }

  mdl <- data.frame( "tekstenOpdelt" = unlist(strsplit(teksten, " ")), stringsAsFactors = FALSE)
  mdl$laengdeInch <- strwidth( mdl$tekstenOpdelt, units = "inches", cex = cexPaaTeksten) + strwidth( " ", units = "inches", cex = cexPaaTeksten)
  mdl$cumsumLaengde <- cumsum(mdl$laengdeInch) %/% maksLaengdeInch + 1

  mdlSamlet <- aggregate(tekstenOpdelt ~ cumsumLaengde, data = mdl, FUN = paste0, collapse = " ")
  resultat <- paste0(unlist(mdlSamlet$tekstenOpdelt), collapse = "\n")

  laengdeInch <- strwidth(resultat, units = "inches", cex = cexPaaTeksten)
  if(any(laengdeInch > maksLaengdeInch)){

    stop("Dele af teksten kan ikke deles op efter den ønskede maksimale længde. Der findes for lange sammenhængende tekst uden mellemrum.")

  }


  return(resultat)

}


#' Kurve plot af en variabel fra et datasæt
#' @encoding UTF-8
#'
#' @param datasaet en data.frame, Datasættet med de variable som skal bruges til plottet.
#' @param xVariablen en character string, Navnet på x-variablen i datasaet
#' @param yVariablen en character string, Navnet på y-variablen i datasaet
#' @param overskrift en character vector eller NA, Overskriften på plottet. Hvis NA så vil overskriften sættes på ud fra xVariablen og yVariablen
#' @param navnPaaXAksen en character string eller NA, Navnet på x-aksen
#' @param navnPaaYAksen en character string eller NA, Navnet på y-aksen
#' @param beregnMiddelOgGraenseVaerdier TRUE/FALSE værdi, Hvis TRUE beregnes og plottes middelværdi og grænseværdierne.
#' @param middelVariabel en character string eller NA, Navnet på middelværdi-variablen. Hvis NA plottes der ingen middelværdi.
#' @param graenseVariabler en character vector af længden to eller NA. Navnet på grænsevariablerne. Hvis NA plottes der ingen grænseværdier.
#'
#' @details Hvis x-variablen er en character-variabel i datasaet, så vil variablen laves om til en factor med levels svarende til rækkefølgen værdierne optræder i datasaet.
#'     Bemærk venligst at grænseværdierne skal beregnes på forhånd.
#'     De angivne grænseværdier vil blive defineret som øvre- og nedre grænse i plottet uafhængig af, hvilken type grænseværdier, der er angivet.
#'     Overskriften sættes på via. funktionen komTitelPaaPlot, der vil hermed gælde samme begrænsninger for overskrift som i denne funktion.
#'
#' @return En kurve plot med eventuelt grænseværdier.
#' @export
#' @importFrom graphics axis barplot legend lines par plot strwidth text title
#'
#' @seealso \code{\link{komTitelPaaPlot}}
#'
#' @examples
#' antalPatienterDat <- data.frame("maaned" = rep(month.name[1:12], 2),
#'                                 "antal" = sample(100:150, 24),
#'                                 stringsAsFactors = FALSE)
#'
#' enkeltLinePlot(datasaet = antalPatienterDat,
#'                xVariablen = "maaned",
#'                yVariablen = "antal",
#'                overskrift = "Antal patienter med x sygdom i 2018",
#'                navnPaaXAksen = "Maaned for besoeg paa afdelingen",
#'                navnPaaYAksen = "Antal patienter",
#'                beregnMiddelOgGraenseVaerdier = TRUE)
#'
#'
#' antalPatienterDat <- data.frame("maaned" = month.name[1:12],
#'                                 "antal" = sample(100:150, 12),
#'                                 stringsAsFactors = FALSE)
#'
#' antalPatienterDat$nedreGraense <- mean(antalPatienterDat$antal) - 3 * sd(antalPatienterDat$antal)
#' antalPatienterDat$oevreGraense <- mean(antalPatienterDat$antal) + 3 * sd(antalPatienterDat$antal)
#'
#' enkeltLinePlot(datasaet = antalPatienterDat,
#'                xVariablen = "maaned",
#'                yVariablen = "antal",
#'                overskrift = "Antal patienter med x sygdom i 2018",
#'                navnPaaXAksen = "Maaned for besoeg paa afdelingen",
#'                navnPaaYAksen = "Antal patienter",
#'                graenseVariabler = c("nedreGraense", "oevreGraense"))
#'
enkeltLinePlot <- function(datasaet,
                           xVariablen,
                           yVariablen,
                           overskrift = NA,
                           navnPaaXAksen = NA,
                           navnPaaYAksen = NA,
                           beregnMiddelOgGraenseVaerdier = FALSE,
                           middelVariabel = NA,
                           graenseVariabler = NA){

  # Tjekker om inputvariablene har den rigtige objecttype
  if(!("data.frame" %in% class(datasaet)) |
     class(xVariablen) != "character" | length(xVariablen) != 1 |
     class(yVariablen) != "character" | length(yVariablen) != 1 |
     ((class(overskrift) != "character" & all(!is.na(overskrift))) | (class(overskrift) == "character" & length(unlist(strsplit(as.character(overskrift), "\n"))) > 3 )|
      (class(navnPaaXAksen) != "character" & all(!is.na(navnPaaXAksen)) & length(unlist(strsplit(as.character(navnPaaXAksen), "\n"))) != 1) |
      (class(navnPaaYAksen) != "character" & all(!is.na(navnPaaYAksen)) & length(unlist(strsplit(as.character(navnPaaYAksen), "\n"))) != 1) |
      !(beregnMiddelOgGraenseVaerdier %in% c(TRUE, FALSE)) |
      (class(middelVariabel) != "character" & all(!is.na(middelVariabel)) & length(middelVariabel) != 1) |
      ((class(graenseVariabler) != "character" & all(!is.na(graenseVariabler))) | (class(graenseVariabler) == "character" & length(graenseVariabler) != 2)))){

    stop("En eller flere af de angivne variabler har ikke den rigtige objekttype.
    Følgende skal gælde for input til denne funktion:
    - datasaet skal være en data.frame.
    - xVariablen og yVariablen skal være en character string af længden en.
    - overskriften skal være en character vektor af en maks længde på tre eller Na.
    - beregnMiddelOgGraenseVaerdier skal være TRUE/FALSE.
    - grænseVariabler skal være en character vektor af længden to eller Na.
    - Resten skal enten være character eller NA.")

  }


  # Tjekker om variablene findes i datasættet
  if(any(!(c(xVariablen, yVariablen) %in% colnames(datasaet)))){

    stop("Variablen/variablene ", paste0(c(xVariablen, yVariablen)[!(c(xVariablen, yVariablen) %in% colnames(datasaet))], collapse = " og "), " findes ikke i datasættet ", deparse(substitute(datasaet)), ".")

  }

  # Tjekker om grænsevariablene findes i datasættet
  if(!is.na(middelVariabel) & !beregnMiddelOgGraenseVaerdier){

    if(!(middelVariabel %in% colnames(datasaet))){

      stop("Den valgte 'middelVariabel' findes ikke i datasættet ", deparse(substitute(datasaet)), ".")

    }

  }

  # Tjekker om grænsevariablene findes i datasættet
  if(all(!is.na(graenseVariabler)) & !beregnMiddelOgGraenseVaerdier){

    if(any(!(graenseVariabler %in% colnames(datasaet)))){

      stop("Variablen/variablene ", paste0(graenseVariabler[!(graenseVariabler %in% colnames(datasaet))], collapse = " og "), " findes ikke i datasættet ", deparse(substitute(datasaet)), ".")

    }

  }

  # Definerer overskriften og teksten på akserne, hvis ikke de er angivet
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

  # Laver x-variablen til en factor, hvis ikke det er en factor
  if(!(class(datasaet$xVariablen) %in% c("numeric", "integer", "factor", "POSIXct", "POSIXlt", "Date"))){

    datasaet$xVariablen <- factor(datasaet$xVariablen, levels = unique(datasaet$xVariablen))

  }else if(class(datasaet$xVariablen) %in% c("numeric", "integer", "POSIXct", "POSIXlt", "Date")){

    datasaet$xVariablen <- as.factor(datasaet$xVariablen)

  }

  if(beregnMiddelOgGraenseVaerdier){
    # Beregner y-værdien pr x-værdi
    datasaet <- aggregate(yVariablen ~ xVariablen, data = datasaet, FUN = sum) %>%
      arrange(xVariablen)

    # Beregner middel- og grænseværdierne
    datasaet <- datasaet %>%
      mutate(middelvaerdien = mean(yVariablen),
             standarafvigelse = sd(yVariablen),
             nedreGraense = middelvaerdien - 3 * standarafvigelse,
             oevreGraense = middelvaerdien + 3 * standarafvigelse)

    # Definerer variabelnavnene så samme kode som til middel- og grænseværdier angivet i input kan bruges
    middelVariabel <- "middelvaerdien"
    graenseVariabler <- c("nedreGraense", "oevreGraense")

  }


  # Finder x-limit værdierne
  xlimMdl <- c(min(as.numeric(datasaet$xVariablen)), max(as.numeric(datasaet$xVariablen)))

  # Finder levels til x-variablen som skal plottes på grafens x-akse
  xLevels <- levels(datasaet$xVariablen)


  # Gemmer default værdierne for margen for at nulstille dem, hvis det bliver ændret
  marDefault <- par("mar") # skal med

  # Ser på hvor lang værdierne i x aksen er. Hvis de er mindre end eller lig med en inch men større end 6 inch så sættes to ekstra linjer ind i margin.
  if(max(strwidth(xLevels, units = "inches", cex = 0.75)) > 1){ #convertX(unit(5, "lines"), "inch")

    stop("Der findes for lange labels blandt de labels, som skal sættes på x aksen.")

  }else if(max(strwidth(xLevels, units = "inches", cex = 0.75)) > 0.6){ #convertX(unit(3, "lines"), "inch")

    # Sætter to ekstra linjer i margin for nedre del af plottet
    par("mar" = c(7,4,4,2) + 0.1)


  }


  # Finder y-limit værdierne
  if(all(!is.na(graenseVariabler))){

    ylimMdl <- c(ifelse((min(datasaet[graenseVariabler], datasaet$yVariablen) - 1) < 0,
                        round((min(datasaet[graenseVariabler], datasaet$yVariablen) - 1) * 1.25),
                        round((min(datasaet[graenseVariabler], datasaet$yVariablen) - 1) * 0.75)),
                 ifelse((max(datasaet[graenseVariabler], datasaet$yVariablen) - 1) < 0,
                        round((max(datasaet[graenseVariabler], datasaet$yVariablen) + 1) * 0.75),
                        round((max(datasaet[graenseVariabler], datasaet$yVariablen) + 1) * 1.25)))

  }else{

    ylimMdl <- c(ifelse((min(datasaet$yVariablen) - 1) < 0,
                        round((min(datasaet$yVariablen) - 1) * 1.25),
                        round((min(datasaet$yVariablen) - 1) * 0.75)),
                 ifelse((max(datasaet$yVariablen) - 1) < 0,
                        round((max(datasaet$yVariablen) + 1) * 0.75),
                        round((max(datasaet$yVariablen) + 1) * 1.25)))

  }


  # Plotter en tom plot
  plot(NULL, type = "n", xaxt = "n", xlim = xlimMdl, ylim = ylimMdl, xlab = "", ylab = navnPaaYAksen, font.lab = 2, cex.axis = 0.75)

  # Sætter titlen på x-aksen enten på næste linje efter længste xlevel eller så den sidste linje indenfor margin
  title(xlab = navnPaaXAksen, line = min(ceiling(max(strwidth(xLevels, units = "inches", cex = 0.75)) / 0.2) + 1, floor(par("mar")[1]) - 1), font.lab = 2)

  # Sætter Værdierne for x-aksen
  axis(side = 1, at = datasaet$xVariablen, labels = xLevels[xLevels %in% datasaet$xVariablen], las = 2, cex.axis = 0.75)

  # Sætter titlen på
  komTitelPaaPlot(overskrift)


  # Sætter x linjen på grafen
  lines(as.numeric(datasaet$xVariablen), datasaet$yVariablen, type = "l",  col = voresFarver[1], lty = 1, lwd = 2, pch = 19)

  # Starter med at gør inputTilLegend klar
  inputTilLegend <- data.frame("tekst" = navnPaaYAksen, "lwd" = 2, col = voresFarver[1], lty = 1, stringsAsFactors = FALSE)

  # Sætter middelVariabel på
  if(!is.na(middelVariabel)){

    lines(as.numeric(datasaet$xVariablen), unlist(c(datasaet[middelVariabel], use.names = FALSE)), type = "l", col = "red", lty = 1, lwd = 2, pch = 1)

    inputTilLegend <- rbind(inputTilLegend,
                            data.frame("tekst" = ifelse(length(unique(datasaet[middelVariabel])) == 1, paste0("middelværdien ", round(unique(datasaet[middelVariabel]), 2)), "middelværdien"), "lwd" = 2, col = "red", lty = 1, stringsAsFactors = FALSE))

  }

  # Sætter øvre og nedre grænserne på
  if(all(!is.na(graenseVariabler))){

    lines(as.numeric(datasaet$xVariablen), unlist(c(datasaet[graenseVariabler[1]], use.names = FALSE)), type = "l", col = "red", lty = 2, lwd = 2, pch = 1)
    lines(as.numeric(datasaet$xVariablen), unlist(c(datasaet[graenseVariabler[2]], use.names = FALSE)),  type = "l",col = "red", lty = 2, lwd = 2, pch = 1)

    inputTilLegend <- rbind(inputTilLegend,
                            data.frame("tekst" = "øvre og nedre grænse", "lwd" = 2, col = "red", lty = 2, stringsAsFactors = FALSE))
  }

  # Sætter legend på
  if(nrow(inputTilLegend) > 1){

    legend("topright", inputTilLegend$tekst, lwd = inputTilLegend$lwd, col = inputTilLegend$col, lty = inputTilLegend$lty, cex = 0.7)

  }

  # Sætter margin tilbage til default værdi
  par("mar" = marDefault)

}


#' Kurve plot af flere variable fra det samme datasæt
#' @encoding UTF-8
#'
#' @param datasaet en data.frame, Datasættet med de variable som skal bruges til plottet
#' @param xVariablen en character string, Navnet på x-variablen i datasaet
#' @param yVariablerne en character vektor, Navnene på de y-variable der skal plottes
#' @param yVariablerneNavneTilLegend  en character vektor af samme længde som yVariablerne, Navnene på y-variablene, som man ønsker dem til at stå i legend.
#'     yVariablerneNavneTilLegend skal have samme rækkefølge som yVariablerne. Hvis NA så vil yVariablerne bruges i legend.
#' @param overskrift en character vector eller NA, Overskriften på plottet. Hvis NA så vil der ikke komme titel på plottet.
#' @param navnPaaXAksen en character string eller NA, Navnet på x-aksen
#' @param navnPaaYAksen en character string eller NA, Navnet på Y-aksen
#'
#' @details Hvis x-variablen er en character-variabel i datasaet, så vil variablen laves om til en factor med levels svarende til rækkefølgen værdierne optræder i datasaet.
#'     Overskriften sættes på via. funktionen komTitelPaaPlot, der vil hermed gælde samme begrænsninger for overskrift som i denne funktion.
#'     Funktionen bruger funktionen delTekstOpEfterInchLængde til at dele legend teksterne op for at hele teksten kan ses på grafen.
#'     Hvis legend er på flere end tre linjer vil den placeres udenfor på højre side af grafen.
#'
#' @return En kurve plot
#' @export
#'
#' @seealso \code{\link{komTitelPaaPlot}} og \code{\link{delTekstOpEfterInchLaengde}}
#' @examples
#' antalPatienterDat <- data.frame("maaned" = month.name[1:12],
#'                           "afdelingA" = sample(100:150, 12),
#'                           "afdelingB" = sample(50:90, 12),
#'                           "afdelingC" = sample(90:120, 12), stringsAsFactors = FALSE)
#' antalPatienterDat$samletAntal <- apply(antalPatienterDat[c("afdelingA","afdelingB","afdelingC")], 1, FUN = sum)
#'
#' flereLinePlot(datasaet = antalPatienterDat,
#'                xVariablen = "maaned",
#'                yVariablerne = c("afdelingA", "afdelingB", "afdelingC"),
#'                yVariablerneNavneTilLegend = c("Afdeling A", "Afdeling B", "Afdeling C"),
#'                overskrift = "Antal patienter med x sygdom i 2018\n fordelt paa afdelinger",
#'                navnPaaXAksen = "Maaned for besoeg paa afdelingen",
#'                navnPaaYAksen = "Antal patienter")
#'
#' flereLinePlot(datasaet = antalPatienterDat,
#'               xVariablen = "maaned",
#'               yVariablerne = c("afdelingA", "afdelingB", "afdelingC", "samletAntal"),
#'               yVariablerneNavneTilLegend = c("Afdeling A", "Afdeling B", "Afdeling C", "Antal pat. i alt"),
#'               overskrift = "Antal patienter med x sygdom i 2018\n fordelt paa afdelinger",
#'               navnPaaXAksen = "Maaned for besoeg paa afdelingen",
#'               navnPaaYAksen = "Antal patienter")
#'
flereLinePlot <- function(datasaet,
                          xVariablen,
                          yVariablerne,
                          yVariablerneNavneTilLegend = NA,
                          overskrift = NA,
                          navnPaaXAksen = NA,
                          navnPaaYAksen = NA){

  # Tjekker om inputvariablene har den rigtige objecttype
  if(!("data.frame" %in% class(datasaet)) |
     class(xVariablen) != "character" | length(xVariablen) != 1 |
     class(yVariablerne) != "character" |
     (class(yVariablerneNavneTilLegend) != "character" & all(!is.na(yVariablerneNavneTilLegend))) | (class(yVariablerneNavneTilLegend) == "character" & length(yVariablerneNavneTilLegend) != length(yVariablerne)) |
     ((class(overskrift) != "character" & all(!is.na(overskrift))) | (class(overskrift) == "character" & length(unlist(strsplit(as.character(overskrift), "\n"))) > 3 )|
      (class(navnPaaXAksen) != "character" & all(!is.na(navnPaaXAksen)) & length(unlist(strsplit(as.character(navnPaaXAksen), "\n"))) != 1) |
      (class(navnPaaYAksen) != "character" & all(!is.na(navnPaaYAksen)) & length(unlist(strsplit(as.character(navnPaaYAksen), "\n"))) != 1))){

    stop("En eller flere af de angivne variabler har ikke den rigtige objekttype.
    Følgende skal gælde for input til denne funktion:
    - datasaet skal være en data.frame.
    - xVariablen skal være en character string af længden en
    - yVariablerne skal være en character vector af længden x.
    - yVariablerneNavneTilLegend skal være en character vector af samme længde som yVariablerne eller NA.
    - overskriften skal være en character vektor af en maks længde på tre eller Na.
    - Resten skal enten være character eller NA.")

  }

  # # Sørger for at yVariablerne kun består af unikke variabelnavne
  # yVariablerne <- unique(yVariablerne)


  # Tjekker om variablene findes i datasættet
  if(any(!(c(xVariablen, yVariablerne) %in% colnames(datasaet)))){

    stop("Variablen/variablene ", paste0(c(xVariablen, yVariablerne)[!(c(xVariablen, yVariablerne) %in% colnames(datasaet))], collapse = " og "), " findes ikke i datasættet ", deparse(substitute(datasaet)), ".")

  }

  # Beholder kun de variable i datasættet som skal bruges til at generere grafen
  datasaet <-  datasaet[c(xVariablen, yVariablerne)]

  # Ændrer navnet på variablene i datasættet for en nemmere tilgang
  colnames(datasaet)[colnames(datasaet) == xVariablen] <- "xVariablen"

  # Laver x-variablen til en factor hvis ikke det er en factor
  if(!(class(datasaet$xVariablen) %in% c("numeric", "integer", "factor", "POSIXct", "POSIXlt", "Date"))){

    datasaet$xVariablen <- factor(datasaet$xVariablen, levels = unique(datasaet$xVariablen))

  }else if(class(datasaet$xVariablen) %in% c("numeric", "integer", "POSIXct", "POSIXlt", "Date")){

    datasaet$xVariablen <- as.factor(datasaet$xVariablen)

  }

  # Finder x-limit værdierne
  xlimMdl <- c(min(as.numeric(datasaet$xVariablen)), max(as.numeric(datasaet$xVariablen)))

  # Finder levels til x-variablen som skal plottes på grafens x-akse
  xLevels <- levels(datasaet$xVariablen)


  # Gemmer default værdierne for margen for at nulstille dem hvis det bliver ændret
  marDefault <- par("mar") # skal med
  par("mar" = c(5,4,4,2) + 0.1)

  # Ser på hvor lang værdierne i x aksen er. Hvis de er mindre end eller lig med en inch men større end 6 inch så sættes to ekstra linjer ind i margin.
  if(max(strwidth(xLevels, units = "inches", cex = 0.75)) > 1){ #convertX(unit(5, "lines"), "inch")

    stop("Der findes for lange labels i for x aksen")

  }else if(max(strwidth(xLevels, units = "inches", cex = 0.75)) > 0.6){ #convertX(unit(3, "lines"), "inch")

    # Sætter to ekstra linjer i margin for nedre del af plottet
    par("mar" = par("mar") + c(2, 0, 0, 0))


  }

  # Finder y-limit værdierne
  ylimMdl <- c(ifelse((min(datasaet[c(yVariablerne)]) - 1) < 0,
                      round((min(datasaet[c(yVariablerne)]) - 1) * 1.25),
                      round((min(datasaet[c(yVariablerne)]) - 1) * 0.75)),
               ifelse((max(datasaet[c(yVariablerne)]) - 1) < 0,
                      round((max(datasaet[c(yVariablerne)]) + 1) * 0.75),
                      round((max(datasaet[c(yVariablerne)]) + 1) * 1.25)))



  # Ser på om legend skal sættes ude på siden eller øverst til højre i grafen
  # Starter med at angive yVariablerneNavneTilLegend, hvis ikke der er angivet noget ved input
  if(all(is.na(yVariablerneNavneTilLegend))){

    yVariablerneNavneTilLegend <- yVariablerne

  }

  legendOpdelt <- sapply(yVariablerneNavneTilLegend, FUN = delTekstOpEfterInchLaengde, maksLaengdeInch = 0.75, cexPaaTeksten = 0.70)

  if(any(sapply(legendOpdelt, function(x) sum(attr(gregexpr("\n", x, fixed = TRUE)[[1]], "match.length"))) > 2)){

    stop("Teksten er opdelt mere end to gange.")

  }

  if(length(unlist(strsplit(legendOpdelt, "\n"))) > 15){

    stop("Legend er for lang.")

  }

  if(length(unlist(strsplit(legendOpdelt, "\n"))) > 3){

    flytLegendUd <- TRUE
    par("mar" = par("mar") + c(0, 0, 0, 5))

  }else{

    flytLegendUd <- FALSE

  }

  # Plotter en tom plot
  plot(NULL, type = "n", xaxt = "n", xlim = xlimMdl, ylim = ylimMdl, xlab = "", ylab = navnPaaYAksen, font.lab = 2, cex.axis = 0.75)

  # Sætter titlen på x-aksen enten på næste linje efter længste xlevel eller så den sidste linje indenfor margin
  title(xlab = navnPaaXAksen, line = min(ceiling(max(strwidth(xLevels, units = "inches", cex = 0.75)) / 0.2) + 1, floor(par("mar")[1]) - 1), font.lab = 2)

  # Sætter Værdierne for x-aksen
  axis(side = 1, at = datasaet$xVariablen, labels = xLevels[xLevels %in% datasaet$xVariablen], las = 2, cex.axis = 0.75)

  # Sætter titlen på
  komTitelPaaPlot(overskrift)


  for(i in 1:length(yVariablerne)){

    # Sætter x linjen på grafen
    lines(as.numeric(datasaet$xVariablen), unlist(datasaet[yVariablerne[i]]), type = "l",  col = voresFarver[i], lty = 1, lwd = 2, pch = 19)

  }

  # Sætter legend på
  if(flytLegendUd){

    legend("left", inset = c(1,0), xpd = TRUE, legendOpdelt, lwd = 2, col = voresFarver[1:length(yVariablerne)], lty = 1, cex = 0.70, bty = "n")

  }else{

    legend("topright", legendOpdelt, lwd = 2, col = voresFarver[1:length(yVariablerne)], lty = 1, cex = 0.70)


  }
  # Sætter margin tilbage til default værdi
  par("mar" = marDefault)

}
