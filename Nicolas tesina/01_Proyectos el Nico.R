###################################################################################################
#  Creador: José Eduardo Martínez Téllez
#  Fecha: Julio-2022
#   Titulo: Tesina Nico
#  Descr:
###################################################################################################

pacman::p_load(tidyverse, magrittr, janitor,fastDummies, readxl,
               psych, GPArotation, tidyverse,
               tidymodels, vip, vcd,gam,car,statmod)

# data --------------------------------------------------------------------



data <- 
  read_xlsx("Nicolas tesina/Bienestar.xlsx")
cat <- 
  read_xlsx("Nicolas tesina/cat_seccion.xlsx")


bd<-data.frame(c(data[,c(200,
                             35,36,37,38,39,40,
                             89,90,135,141,148,165,167,
                             142,143,144,145,146,
                             130,131,
                             138,139,140,150,151,152,153,154,177,178)])) %>% 
  drop_na() %>% 
  mutate(across(everything(), ~as.factor(.)))



# desciptivo --------------------------------------------------------------

bd %>% 
  describe()

bd %>% 
  summary()

bd %>% skimr::skim()


# selección de variables --------------------------------------------------



c <- matrix(1:60, nrow = 30)
for (i in 1:30) {
  table1 <- table(bd[[1]], bd[[i+1]]) 
  chiq <- chisq.test(table1)
  c[i,]<- c(chiq$statistic,chiq$p.value)
}

chi_squar <- data.frame(c %>% 
             as.data.frame(), bd %>% 
             names() %>% 
             as.data.frame() %>% 
             row_to_names(row_number = 1)) %>% 
  rename(X_square= V1, p_value = V2) %>% 
  left_join(cat)

# Modelo 1-------------------------------------------------------------------

best_chi <- chi_squar %>% 
  group_by(grupo) %>% 
  filter(p_value!=0) %>% 
  mutate(flg_selection = ifelse(X_square == max(X_square), 1, 0)) %>% 
  filter(flg_selection==1) %>% 
  select(-flg_selection)



variables_chi <- as.vector(best_chi$gr_satis)

data_1 <- 
  bd %>% 
  select(gr_satis, variables_chi) %>% 
  mutate(across(everything(), ~as.numeric(.)))

# Modelo 1: presta_1 ------------------------------------------------------


gr_satis_presta_1 <- xtabs( ~ gr_satis + presta_1, data = data_1)
stdres.gr_satis_presta_1 <- chisq.test(gr_satis_presta_1)$stdres

# mosaico plot
mosaic(
  gr_satis_presta_1,
  gp = shading_Friendly,
  residuals = stdres.gr_satis_presta_1,
  residuals_type = "Std\nresiduals",
  labeling = labeling_residuals
)

# Modelo 1: fuerte ------------------------------------------------------


gr_satis_fuerte <- xtabs( ~ gr_satis + fuerte, data = data_1)
stdres.gr_satis_fuerte <- chisq.test(gr_satis_fuerte_1)$stdres

# mosaico plot
mosaic(
  gr_satis_fuerte,
  gp = shading_Friendly,
  residuals = stdres.gr_satis_fuerte,
  residuals_type = "Std\nresiduals",
  labeling = labeling_residuals
)
# Modelo 1: nivel_h ------------------------------------------------------


gr_satis_nivel_h <- xtabs( ~ gr_satis + nivel_h, data = data_1)
stdres.gr_satis_nivel_h <- chisq.test(gr_satis_nivel_h)$stdres

# mosaico plot
mosaic(
  gr_satis_nivel_h,
  gp = shading_Friendly,
  residuals = stdres.gr_satis_nivel_h,
  residuals_type = "Std\nresiduals",
  labeling = labeling_residuals
)
# Modelo 1: advers ------------------------------------------------------

## Cuidado, probablemente no sea significativos en el modelos
gr_satis_advers <- xtabs( ~ gr_satis + advers, data = data_1)
stdres.gr_satis_advers <- chisq.test(gr_satis_advers)$stdres

# mosaico plot
mosaic(
  gr_satis_advers,
  gp = shading_Friendly,
  residuals = stdres.gr_satis_advers,
  residuals_type = "Std\nresiduals",
  labeling = labeling_residuals
)

# Modelo 1: satis_6 ------------------------------------------------------

## Checar preguntas

gr_satis_satis_6 <- xtabs( ~ gr_satis + satis_6, data = data_1)
stdres.gr_satis_satis_6 <- chisq.test(gr_satis_satis_6)$stdres

# mosaico plot
mosaic(
  gr_satis_satis_6,
  gp = shading_Friendly,
  residuals = gr_satis_satis_6,
  residuals_type = "Std\nresiduals",
  labeling = labeling_residuals
)


# Modelo 1: Preparación ------------------------------------------------------------

set.seed(2004)

prep_data_1 <- 
  initial_split(data_1, prop = .70, strata = gr_satis)

## Base de entrenamiento
train_mdl.1 <- 
  prep_data_1 %>% training()
## base de prueba
test_mdl.1 <- 
  prep_data_1 %>% testing()

# Modelo 1: Seleccionar modelo --------------------------------------------

## Empezaremos con un modelo lineal sencichitoo

lm_mdl.1 <- 
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

mod.1 <- 
  lm_mdl.1 %>% 
  fit(gr_satis~., data = data_1)


# Modelo 1: Interpretación ------------------------------------------------

## Advers no es signficativa de cero, por lo que se podría retirar
## el modelo tiene una R ajustada de .18(pésimo).
summary(mod.1$fit)
glance(mod.1)

## assumptions

par(mfrow=c(2,2))
plot(mod.1$fit, 
     pch = 16, 
     col = '#006EA1')



## variables por importancia
vip(mod.1) + 
  theme_classic()+
  labs(title = "Variables por Importancia")


# Modelo 1: Testing -------------------------------------------------------


## Conclusión, un modelo lineal para variables categoricas no sirve




# Modelo 2 ----------------------------------------------------------------

## Probaremos un modelo generalizado 
# tabla <-
#   xtabs(~gr_satis + presta_1 + fuerte + nivel_h + advers + satis_6, 
#         data = data_1)

train_mdl.1 <- 
  train_mdl.1 %>% 
  mutate(across(-gr_satis,  ~as.factor(.)))


mod.2 <- 
  glm( gr_satis~ presta_1 + fuerte + nivel_h + advers + satis_6,
         , family=poisson(link=log), data=train_mdl.1)


summary(mod.2)

confint(mod.2)

Anova(mod.2)

mod.0 <- 
  glm( gr_satis~ 1,
       , family=poisson(link=log), data=train_mdl.1)


glm.scoretest(mod.0, train_mdl.1$gr_satis)^2



# Modelo 3 ----------------------------------------------------------------


mod3 <- 
  glm( gr_satis~ presta_1 + fuerte + satis_6, 
       family=poisson(link=log), data=train_mdl.1)


summary(mod3)


