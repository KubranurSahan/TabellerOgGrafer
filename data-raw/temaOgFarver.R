# # library(usethis)
# # library(grid)
# # library(gridExtra)
# #
# # gridExtra::ttheme_default()
# # grid::unit
# #Tema til pdf tabeller
# tTema <- ttheme_default(
#
#   core = list(fg_params = list(parse = TRUE, hjust = 0, x = 0.05, cex = 0.8)),
#   colhead = list(fg_params = list(hjust = 0, x = 0.05, cex = 0.9)),
#   padding = unit(c(4, 4), "mm")
#   # rowhead = list(fg_params = list(hjust = 0, x = 0))
#
# )
#
#
# voresFarver <- c("royalblue4", "slategray3", "dimgray", "gray60", "lightpink4", "navajowhite4", "wheat1")
#
# # I funktionerne skal man sætte palette() lig med vores farver, hvis man vil angive farver automatisk isttedet for manuelt
# # Først skal paletteDef <- palette() for at nulstille efter funktion.
# use_data(tTema, voresFarver, internal = TRUE) # data gemmes som internal data
