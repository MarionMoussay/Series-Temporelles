library(zoo)
library(xts)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(gridExtra)


data <- read.csv("hungary_chickenpox.csv")
data$Date <- dmy(data$Date)
mean(colMeans(data[-1])) # Moyenne générale de cas de varicelle de 38.84282
colMeans(data[-1]) # Budapest bien supérieure à la moyenne, top 1 (logique car capitale)

budapest <- data[,1:2]
colnames(budapest) <- c("date", "nb")
budapest %>% ggplot() + aes(x=date, y=nb) + geom_line() + ggtitle("Nombre de cas hebdomadaires de varicelles à Budapest")+theme_minimal()+ xlab("Date")+ylab("Nombre de cas")


temp.ts <- ts(budapest$nb, start=c(2005,03,01), frequency=52)
mod_stl_add <- stl(temp.ts, s.window = "periodic")

donnee <- cbind(budapest,as.data.frame(mod_stl_add$time.series))

donnee %>% ggplot() + 
  geom_line(aes(x = date, y=nb, color="Xt")) +
  geom_line(aes(x=date, y=trend+seasonal, color="mt+st")) + geom_line(aes(x=date, y=trend, color="mt")) +
  scale_color_manual(values = c("red", "purple", "black")) +
  theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") +
  labs(colour = "Modele") + ggtitle("stl() modèle additif") +
  xlab("Temps") + ylab("Temperature") + 
  theme(
    legend.background = element_rect(fill = "darkgray"),
    legend.text = element_text(size = 13) #+ theme_economist()
  )

donnee %>% ggplot() + aes(x=date, remainder) + geom_line() +  xlab("") + ylab("") + ggtitle("Résidus du modèle")


arima <- auto.arima(donnee$remainder)

data <- donnee %>% mutate(res2 = arima$residuals)

data %>% ggplot() + aes(x=date,y=res2) + geom_line()


checkresiduals(arima, lag.max=20)

mean(data$res2)

var(data$res2)

ggAcf(data$res2)

gglagplot(data$res2, do.lines = FALSE, set.lags = 1:20, colour = FALSE)

data %>% ggplot() + aes(x=res2) + geom_density()

ggPacf(data$res2)

Box.test(data$res2, lag = 20, type = "Box-Pierce", fitdf = 2)
Box.test(data$res2, lag = 20, type = "Ljung-Box", fitdf = 2)
