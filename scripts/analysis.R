# Desdobramentos - Estatística experimental - Testes Post Hoc
# João Pedro Martins Oliveira - 149170

install.packages("tidyverse")
install.packages("GGally")
install.packages("stargazer")

library(MASS)
library(tidyverse)
library(GGally)
library(stargazer)
library(car)

theme_set(theme_classic())

# Importandos os dados
car_data <- read.table("data/Auto MPG/auto-mpg.data", header=FALSE)

# Adicionando os nomes às variáveis
names(car_data) <- c("mpg", "cylinders", "displacement", "horsepower", 
                 "weight", "acceleration", "model year", "origin", 
                    "car name")
head(car_data)
glimpse(car_data)
# Precisamos trasnformar o tipo de algumas variáveis
car_data$horsepower <- as.numeric(car_data$horsepower)
car_data$origin <- as.factor(car_data$origin)
car_data$cylinders <- as.factor(car_data$cylinders)

# Retirando possíveis NAs
car_data <- car_data %>% 
                filter(car_data$horsepower != "?" ) %>% 
                select(c(-`model year`, -`car name`))

# Verificando informações básicas
glimpse(car_data)
View(car_data)

ggpairs(car_data, 
            title = "Análise dois a dois", 
            mapping = aes(color = cylinders),
            legend = 1
        )

# Modelo da RLM
model <- lm(mpg ~ displacement + horsepower + weight + acceleration + cylinders + origin,
            data = car_data)
stargazer::stargazer(model, type = "text")
anova(model)
shapiro.test(model$residuals)

# Note que acceleration não é significativo, então retiramos do modelo
model_2 <- lm(mpg ~ displacement + horsepower + weight + cylinders + origin,
              data = car_data)
stargazer::stargazer(model_2, type = "text")
anova(model_2)

backward <- stepAIC(model_2, direcion="backward", trace=FALSE)
anova(backward)

vif(backward)

shapiro.test(backward$residuals)


model_3 <- lm(mpg ~ horsepower + cylinders + origin,
              data = car_data)
anova(model_3)
vif(model_3)
summary(model_3)
summary(backward)

model_4 <- lm(mpg ~ horsepower + weight + origin,
              data = car_data)
anova(model_4)
vif(model_4)

shapiro.test(model_4$residuals)

model_5 <- lm(mpg ~ ., 
              data=car_data)
forward <- stepAIC(model_5, direction="forward", trace=FALSE)

anova(forward)

f_2 <- stepAIC(forward, direction="forward", trace = TRUE)

f_2 <- update(f_2,  . ~ . - acceleration)
f_2 <- update(f_2,  . ~ . - displacement)

anova(f_2)
vif(f_2)


model_6 <- lm(mpg ~ horsepower + weight + acceleration,
              data=car_data)

anova(model_6)
vif(model_6)

model_7 <- lm(mpg ~ log(horsepower) + log(weight),
              data=car_data)
anova(model_7)
vif(model_7)

shapiro.test(model_7$residuals)


reg<-lm(mpg~displacement + horsepower + weight + acceleration + cylinders + origin + cylinders*origin, data = car_data)
anova(reg)
summary(reg)
plot(car_data$mpg)










# Premissas a serem verificadas
residuals_vs_fitted <- model %>% 
    ggplot(aes(fitted(model), resid(model))) +
    geom_point() +
    geom_smooth(colour = 'red') +
    labs(title = "Residuals vs fitted",
         x = "Fitted", 
         y = "Residuals")

scale_location <- model %>%
    ggplot(aes(fitted(model), sqrt(abs(resid(model))))) +
    geom_point() +
    geom_smooth(colour = 'red') +
    labs(title = "Scale-Location",
         x = "Fitted Values",
         y = "Square Root of Standardized Residuals")

residuals_vs_fitted

shapiro.test(model)

shapiro.test(residuals(model))
shapiro.test(model$residuals)

qqplot(residuals(model))

box_cox_model <- boxcox(model_3)
