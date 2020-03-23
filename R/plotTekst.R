#' Plot tekst
#' @encoding UTF-8
#'
#' @param overskrift en character string, Overskriften til teksten. Overskriften skal være en chracter string af længden en og på maks tre linjer opdelt med newline '\\n'.
#' @param tekst en character string, Teksten der skal plottes. Teksten skal være en chracter string af længen en og på makst 20 linjer opdelt med newline'\\n'.
#'
#' @return En plot med den ønskede tekst og overskrift.
#' @export
#' @examples
#' plotTekst(overskrift = "Overskrift til teksten skrives her\n Eventuelt paa flere linjer\n men den maa ikke vaere paa over tre linjer.",
#'           tekst = paste0("Teksten kan vaere paa maks 20 linjer og med maks 100 tegn pr linje. \nOverskriften har ogsaa en begraensning paa ", 80, " tegn pr linje.",
#'                   "\nFunktionen melder fejl hvis ikke disse begraensninger overholdes."))
#'
plotTekst <- function(overskrift = NA, tekst){

  if(class(overskrift) != "character" & all(!is.na(overskrift)) | length(overskrift) != 1|
     class(tekst) != "character" | length(tekst) != 1){

    stop("Mindst et af de inputs der er givet har ikke den rigtige objekttype.
         Sørg for at 'tekst' er en character string af længden en og overskrift enten er en character string af længden en eller NA.")

  }

  # Starter med at tjekke at overskrift og tekst er for lange til at det kan plottes ordentligt
  # Tager udgagnspunkt i, hvordan det vil se ud i en 10X5 inch pdf fil.

  # Danner en variabel for at indsamle fejl
  fejl <- c()

  if(!is.na(overskrift)){

    if(gregexpr("\n", overskrift, fixed = TRUE)[[1]][1] == -1){

      antalLinjer <- 0

      # Rykker overskriften lidt ned for at der ikke skal være meget tomt mellem overskrift og teksten
      overskrift <- paste0("\n", overskrift)

      if(nchar(overskrift) > 80){

        fejl <- c(fejl, "Overskriften må maks have 80 tegn i hvert linje. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

      }

    }else{

      antalLinjer <- length(gregexpr("\n", overskrift, fixed = TRUE)[[1]])

      if(antalLinjer > 2){

        fejl <- c(fejl, "Overskriften må maks være på tre linjer. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

      }

      if(max(c(gregexpr("\n", overskrift, fixed = TRUE)[[1]], nchar(overskrift)) - c(0, gregexpr("\n", overskrift, fixed = TRUE)[[1]]) - c(rep(1, antalLinjer), 0)) > 80){

        fejl <- c(fejl, "Overskriften må maks have 80 tegn i hvert linje. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

      }

    }

  }

  if(gregexpr("\n", tekst, fixed = TRUE)[[1]][1] == -1){

    antalLinjerT <- 0

    if(nchar(tekst) > 100){

      fejl <- c(fejl, "Teksten må maks have 100 tegn i hvert linje. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

    }

  }else{

    antalLinjerT <- length(gregexpr("\n", tekst, fixed = TRUE)[[1]])

    if(antalLinjerT > 19){

      fejl <- c(fejl, "Teksten må maks være på 20 linjer. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

    }

    if(max(c(gregexpr("\n", tekst, fixed = TRUE)[[1]], nchar(tekst)) - c(0, gregexpr("\n", tekst, fixed = TRUE)[[1]]) - c(rep(1, antalLinjerT), 0))  > 100){

      fejl <- c(fejl, "Teksten må maks have 100 tegn i hvert linje. Ellers vil teksten ikke kunne læses i en 10X5 inch pdf.")

    }

  }

  # Melder fejl hvis der er indsamlet fejl
  if(!is.null(fejl)){

    stop(paste0(fejl, collapse = "\n"))

  }


  # Gemmer default parværdier
  parDefault <- par(no.readonly = TRUE)

  # Fjerner margener
  par( mar = c(0, 0, 0, 0))

  # Åbner "rammen" som teksten skal stå i, dvs. danner en tom plot og fjerner akser ol.
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

  if(!is.na(overskrift)){

    # Plotter overskriften
    text(x = 0.5, y = 1, paste0(paste0(rep("\n", antalLinjer), collapse = ""), overskrift), cex = 1.2, col = "black", font = 2)

  }

  # Plotter teksten
  text(x = 0.01, y = 0.83, paste0(paste0(rep("\n", antalLinjerT), collapse = ""), tekst), cex = 1, col = "black", adj = 0)

  # Gendanner til default parværdier
  par(parDefault)

}



