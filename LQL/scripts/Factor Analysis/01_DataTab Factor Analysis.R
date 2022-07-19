###################################################################################################
#  Creador: José Eduardo Martínez Téllez
#  Fecha: Julio-2022
#   Titulo: Fctr Analysis
#  Descr:
###################################################################################################

pacman::p_load(tidyverse, magrittr, janitor,fastDummies, readxl,
               psych, GPArotation)



# read_data ---------------------------------------------------------------

dt <- readxl::read_xlsx("LQL/Apuntes/Bases/Datos Factor Analysis.xlsx")



# Calculando número de factores -------------------------------------------

data <- as.data.frame(dt[,2:7])
ev <- eigen(cor(data))


# scree-plot --------------------------------------------------------------

dta_scree <- data.frame(  number = seq(1:6), eigen_values = ev$values)



dta_scree %>% 
  ggplot(aes(x= number, y = eigen_values))+
  geom_point(shape = 1)+
  geom_line() +
  geom_hline(yintercept = 1, color = "red")+
  labs(x = "Number of factors", y = "Eigen values",
       title = "Screeplot")+
  theme_minimal()




# parallel analysis -------------------------------------------------------

parallel <- fa.parallel(data)




## Principal Axis Factoring


fa_1 <-   fa(
  data,
  nfactors = 3,
  fm = "pa",
  max.iter = 100,
  rotate = "oblimin"
)


fa_2 <-   fa(
  data,
  nfactors = 3,
  fm = "pa",
  max.iter = 100,
  rotate = "varimax"
)


fa.diagram(fa_1)
fa.diagram(fa_2)

