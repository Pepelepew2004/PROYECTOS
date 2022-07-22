#Análisis exploratorio de los datos
archivo <- "Growth.xlsx"
datos<- read_xlsx(archivo)
attach(datos)
variables<-datos[,-1]
summary(variables)
diag(var(variables))
g1<-ggplot(variables,aes(growth)) + 
  geom_density(fill="red",colour = "red")
g2<-ggplot(variables,aes(rgdp60)) + 
  geom_density(fill="red",colour = "red")
g3<-ggplot(variables,aes(tradeshare)) + 
  geom_density(fill="red",colour = "red")
g4<-ggplot(variables,aes(yearsschool)) + 
  geom_density(fill="red",colour = "red")
g5<-ggplot(variables,aes(rev_coups)) + 
  geom_density(fill="red",colour = "red")
g6<-ggplot(variables,aes(assasinations)) + 
  geom_density(fill="red",colour = "red")

(g1 | g2 | g3) / 
  (g4 | g5 | g6)

ggplot(data = melt(datos),aes(x=variable,y=value)) +
  geom_boxplot(aes(fill = variable))

seleccion<-datos[,-4]
ggplot(data = melt(seleccion),aes(x=variable,y=value)) +
  geom_boxplot(aes(fill = variable))

# Correlaciones

ggplot(data = datos, aes(x = rgdp60, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y PIB en 1960",
    subtitle = "(Muestra completa)",
    caption = "Elaboración propia",
    x="PIB en 1960",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = datos, aes(x = tradeshare, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y PIB en 1960",
    subtitle = "(Muestra completa)",
    caption = "Elaboración propia",
    x="PIB en 1960",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = datos, aes(x = yearsschool, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y PIB en 1960",
    subtitle = "(Muestra completa)",
    caption = "Elaboración propia",
    x="PIB en 1960",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = datos, aes(x = rev_coups, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y PIB en 1960",
    subtitle = "(Muestra completa)",
    caption = "Elaboración propia",
    x="PIB en 1960",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = datos, aes(x = assasinations, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y PIB en 1960",
    subtitle = "(Muestra completa)",
    caption = "Elaboración propia",
    x="PIB en 1960",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

sel<-datos[-65,]
attach(sel)
ggplot(data = sel, aes(x = yearsschool, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= sel$country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y Años de escolaridad",
    subtitle = "(Excluyendo Malta)",
    caption = "Elaboración propia",
    x="Años de escolaridad",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = sel, aes(x = tradeshare, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= sel$country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y Años de escolaridad",
    subtitle = "(Excluyendo Malta)",
    caption = "Elaboración propia",
    x="Años de escolaridad",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = sel, aes(x = yearsschool, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= sel$country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y Años de escolaridad",
    subtitle = "(Excluyendo Malta)",
    caption = "Elaboración propia",
    x="Años de escolaridad",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = sel, aes(x = rev_coups, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= sel$country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y Años de escolaridad",
    subtitle = "(Excluyendo Malta)",
    caption = "Elaboración propia",
    x="Años de escolaridad",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

ggplot(data = sel, aes(x = assasinations, y = growth)) +
  geom_point(color = 'red') +
  geom_text(label= sel$country_name, size=3, check_overlap = TRUE,
            vjust = 1.5, hjust = 0.6) +
  labs(
    title ="Crecimiento y Años de escolaridad",
    subtitle = "(Excluyendo Malta)",
    caption = "Elaboración propia",
    x="Años de escolaridad",
    y="Tasa de crecimiento"
  ) +
  geom_smooth()

vars<-data.frame(growth,rgdp60,log(rgdp60),tradeshare,tradeshare^2,tradeshare^3,
                 yearsschool,log(yearsschool),yearsschool^2,rev_coups,
                 assasinations)
matcor<-cor(vars)
corrplot(matcor, method="number",type="upper")

modelo<- lm(growth~log(rgdp60) + tradeshare+I(tradeshare^2) + I(tradeshare^3)
            + log(yearsschool) + rev_coups + assasinations)
modelo
summary(modelo)

stargazer(modelo, type="text")

extract_eq(modelo)

modelo0<-lm(growth~log(rgdp60)+log(yearsschool) + rev_coups + assasinations)
modelo1<-lm(growth~log(rgdp60)+tradeshare + log(yearsschool) + rev_coups 
            + assasinations)
modelo2<-lm(growth~log(rgdp60)+tradeshare + I(tradeshare^2)+log(yearsschool) 
            + rev_coups + assasinations)
modelo3<-lm(growth~log(rgdp60)+tradeshare + I(tradeshare^2)+ I(tradeshare^3)
            +log(yearsschool) + rev_coups + assasinations)
stargazer(modelo0, modelo1, modelo2, modelo3, type="text")

linearHypothesis(modelo3,c("tradeshare=0","I(tradeshare^2)=0", "I(tradeshare^3)=0"))

regresoras<-data.frame(growth,log(rgdp60),log(yearsschool),rev_coups,assasinations)
seleccion<-regresoras[-65,]
attach(seleccion)
model <- lm(growth ~., data = seleccion)
k <- ols_step_all_possible(model)
k
plot(k)
ols_step_best_subset(model)
stargazer(model,type="text")
resettest(model, power = 2, type ="fitted", data = seleccion)
