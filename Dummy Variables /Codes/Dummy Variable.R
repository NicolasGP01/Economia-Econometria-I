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




EMICRON = EMICRON |> mutate( N_EMPLEADOS = P3091 - 1 ) |> mutate(
 DUMMY = case_when(
    N_EMPLEADOS == 0 ~ 0 , 
    N_EMPLEADOS %in% seq(1,16,1) ~ 1 ))

c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
                           
EMICRON$DUMMY |> table()     
                         

EMICRON = EMICRON |> mutate(DUMMY_2 = DUMMY * P35)

EMICRON$DUMMY_2 |> table( useNA =  )


Copy_of_RP2017_1_ = read_excel("C:/Users/nicol.NICOLAS_GP/OneDrive/Escritorio/Portafolio/My Work/Armenia/My First Jobs_2022_1/Observatorio Fiscal/Copy of RP2017(1).xlsx", 
                               sheet = "RP2017")


copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol = ifelse(str_detect( Observación , "CONTRATO"), 1 , 0))

table(Copy_of_RP2017_1_$newCol)


Copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol_PROF = ifelse(str_detect( Observación , "CONTRATO DE PRESTACION DE SERVICIOS PROFESIONALES"), 0 , 1))

table(Copy_of_RP2017_1_$newCol_PROF) 


Copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol_DEF = newCol * newCol_PROF  )


table(Copy_of_RP2017_1_$newCol_DEF) 




