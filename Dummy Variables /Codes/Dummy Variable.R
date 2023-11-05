###############################################################################
#######                                                                 #######
#                          UNIVERSIDA DEL QUINDÍO                             #
#                           ASESORIA DUMMY ECO I                              #
#######                                                                 #######
###############################################################################
#31 de octubre de 2023


#By: Nicolás García Peñaloza
#+57 3122852823
#nicolasgp0109@gmail.com

library(skimr)
library(tidyverse)


B = read.csv("C:/Users/nicol.NICOLAS_GP/Downloads/SALMUJER-FICTI.csv")

B |> str()


B$CASADO |> table(useNA = "always")

table(B$VIVECTAL , useNA = "always")


summary(B$AGE )

table(is.na(B$VIVECTAL ))


B |> group_by(CASADO) |> summarise( PRO_EDAD = mean(AGE))

B |> group_by(VIVECTAL) |> summarise( PRO_EDAD = mean(AGE))


REG = lm( LNSAL ~ AGE  + CASADO , data = B )

REG |> summary()
