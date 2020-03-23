# gridExtra::tableGrob()
# gridExtra::arrangeGrob()
# grid::textGrob()
# grid::gpar()
# grid::grid.newpage()
# grod::grid.draw()
### Funktion til at udskrive tabeller til pdf-fil ###
#' Udskriv tabel til pdf
#' @encoding UTF-8
#'
#' @param datasaet en data.frame
#' @param filSti en chracter string, Stien til filen som der skal udskrives til. Strien skal ende med '.pdf' .
#' @param titelTekst en chracter string, Titlen på tabellen
#' @param antalRaekker en numeric/integer værdi, Maksimale antal rækker der skal på hver side. Default værdi er den maksimale værdi af antal rækker.
#'     Man må ikke sætte antalRaekker over default værdien.
#' @param tema en liste, Tema til tabellen. Må ikke ændres med mindre man ved, hvad man gør.
#' @param maksAntalTegnPrKol en numeric/integer værdi, Den maksimale antal tegn pr. kolonne. Bruges i tilfælde af, at det viser sig, at tabellen er for bred til at være på siden.
#'     Hvis tabellen er for bred, vil funktionen begrænse hvert eneste kolonne til at bestå af den makimalt antal tegn, som er angivet her.
#'     Hvis tabellen stadig er for bred, melder funktionen fejl.
#'
#' @return En pdf.-fil med tabellen.
#' @export
#'
#' @import grid
#' @import gridExtra
#' @importFrom grDevices dev.cur dev.off pdf
#' @importFrom stats aggregate
#'
#' @examples
#' \dontrun{antalPatienterDat <- data.frame("maaned" = month.name[1:12],
#'                                "afdelingA" = sample(100:150, 12),
#'                                 "afdelingB" = sample(50:90, 12),
#'                                 "afdelingC" = sample(90:120, 12), stringsAsFactors = FALSE)
#' antalPatienterDat$samletAntal <- apply(antalPatienterDat[c("afdelingA","afdelingB","afdelingC")], 1, FUN = sum)
#'
#' udskrivTabelTilPdf(datasaet = antalPatienterDat,
#'                    filSti = "antal patienter paa de tre udvalgte afdelinger.pdf",
#'                    titelTekst = "Antal patienter paa de udvalgte afdelinger")}
udskrivTabelTilPdf <- function(datasaet,
                               filSti,
                               titelTekst,
                               antalRaekker = 33,
                               tema = tTema,
                               maksAntalTegnPrKol = 40){

  # Tjekker om filSti er angivet korrekt
  if(substr(filSti, nchar(filSti) - 3, nchar(filSti)) != ".pdf"){

    stop("filSti skal ende med '.pdf'.")

  }

  # Tjekker om antalRaekker er sat til at være større end 33, som er den maksimale tilladte værdi
  if(antalRaekker > 33){

    stop("antalRaekker må ikke være over 33.")

  }

  # Beregner antallet af sider tabellen fylder
  antalsider <- ceiling(nrow(datasaet) / antalRaekker)

  # Starter med at se på, hvor stor tabellen er
  tabelFuld <- tableGrob(datasaet, rows = NULL, theme = tema)

  # # Melder fejl hvis den maksimale tykkelse på hver række overskrider det tilladte
  # if(max(convertUnit(tabelFuld$heights, "cm", valueOnly = TRUE)) > 0.78){# oprindeligt 0.7704167
  #
  #   stop("Tabellen indeholder rækker der er tykkere/højere end tilladt.")
  #
  # }

  # Laver en dataframe af højderne på hvert række
  tabelFuldHoejde <- data.frame("hoejde" = convertUnit(tabelFuld$heights, "cm", valueOnly = TRUE))

  # Grupperer højderne efter sider på nær den første som er højden på kolonnenavnene som kommer på hver side
  tabelFuldHoejde$gruppe <-  c(0, rep(1:antalsider, each = antalRaekker))[1:nrow(tabelFuldHoejde)]

  # Ser på tabellernes højde pr side
  tabelFuldHoejdeRes <- aggregate(hoejde ~ gruppe, data = tabelFuldHoejde, FUN = sum)

  # Melder fejl hvis den den maksimale tykkelse overskrides med udgangspunkt i den maksimale tykkelse pr række 0.78
  if(any(tabelFuldHoejdeRes$hoejde > 26.5 - tabelFuldHoejdeRes[tabelFuldHoejdeRes$gruppe == 0,]$hoejde)){ # oprindeligt 25.684 istedet for 26.5

    stop("Tabellen er for lang/højere end tilladt selvom den er delt op på flere sider.
         Sæt antalRaekker ned og prøv igen.")

  }


  # Ser på om tabellen kan være på siden uden at skære noget væk fra kolonnerne
  if(sum(convertUnit(tabelFuld$widths, "cm", valueOnly = TRUE)) > 19){

    # Sørger for at alle variable indeholder maksimalt 40 tegn eller x tegn hvis maksAntalTegnPrKol er sat til noget andet
    datasaet <- as.data.frame(apply(datasaet, 2, function(x) substring(x, 1, maksAntalTegnPrKol)))

    tabelFuld <- tableGrob(datasaet, rows = NULL, theme = tema)

    # Hvis stadig bredere end siden kan rumme
    if(sum(convertUnit(tabelFuld$widths, "cm", valueOnly = TRUE)) > 19){

      stop("Tabellen er for bred til at være på siden. Sæt eventuelt maksAntalTegnPrKol ned.")

    }

  }

  if(dev.cur() == 2){invisible(dev.off())} #convertUnit funktionerne åbner en grafics device selvom valueOnly = TRUE burde sørge for at dette ikke sker

  # Åbner pdf-filen
  pdf(filSti, width = 210/25.4, height = 297/25.4)


  for(i in 1:antalsider){

    # Deler tabellen op
    if(i == 1){

      tabellen <- tableGrob(datasaet[1:min(antalRaekker, nrow(datasaet)), ], rows = NULL, theme = tema)

    }else if(i == antalsider){

      tabellen <- tableGrob(datasaet[((i - 1) * antalRaekker + 1) : nrow(datasaet), ], rows = NULL, theme = tema)

    }else{

      tabellen <- tableGrob(datasaet[((i - 1) * antalRaekker + 1) : (i * antalRaekker), ], rows = NULL, theme = tema)

    }

    # Gør titlen klar efter om det er første eller en af de efterfølgende sider
    if(i == 1){

      tabelTitel <- textGrob(titelTekst,
                             y = unit(1.5, "lines"), vjust = 0,
                             gp = gpar(fontsize = 12, fontface = "bold"))

    }else{

      tabelTitel <- textGrob(paste0(titelTekst, "- fortsat"),
                             y = unit(1.5, "lines"), vjust = 0,
                             gp = gpar(fontsize = 12, fontface = "bold"))

    }

    # Gruppere titel og tabel sammen
    tilUdskrift <- arrangeGrob(tabellen, top = tabelTitel, padding = unit(1.5, "cm")) # padding ca. 3 lines

    grid.newpage()

    # Udksriver tabellen samt titlen
    grid.draw(tilUdskrift)

  }

  # Lukker pdf-filen
  if(dev.cur() == 2){invisible(dev.off())}

}



#' Plot tabel
#'
#' @param tabel en data.frame, Tabellen der skal plottes
#' @param tabelTitel en character string, Titlen på tabellen
#' @param tema en liste, Tema til tabellen. Må ikke ændres med mindre man ved, hvad man gør.
#'
#' @return tabel i en plot
#' @export
#'
#' @examples
#' antalPatienterDat <- data.frame("afdeling" = c("afdelingA", "afdelingB", "afdelingC"),
#'                           "antalPatienter" = sample(50:150, 3))
#' plotTabel(tabel = antalPatienterDat, tabelTitel = "Oversigt over antal patienter med x sygdom")
#'
plotTabel <- function(tabel, tabelTitel, tema = tTema){

  # Som med andre plotfunktioner bør der sættes begræsninger på størrelsen af tabellen og titlen som varierer med rammestørrelsen (10X5 eller en A4)

  # Udskriver tabellen til pdf-filen
  tabellen <- tableGrob(tabel, rows = NULL, theme = tema)

  grid.newpage()

  # Definerer højde på tabellen for at finde ud at hvor teksten skal placeres
  hoejde <- grobHeight(tabellen)
  # bredde <- grobWidth(tabellen)


  tabelTitelA <- textGrob(tabelTitel,
                          y = unit(0.5, "npc") + 1 * hoejde, vjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))

  # Sætter tabellen og titlen sammen
  gt <- gTree(children = gList(tabellen, tabelTitelA))

  # Plotter tabellen og titlen
  grid.draw(gt)

}


# 210/25.4 inch × 297/25.4 inch - A4 størrelse
# (3*2)/2.54 fra top/bund og (2*2)/2.54 fra siderne

