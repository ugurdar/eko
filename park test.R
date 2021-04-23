# Park Testi

library(readxl)
data <- read_excel("C:/Users/gr/Desktop/EKONOMETRİ - I/park.xlsx")
x <- data$x
y <- data$y
lmfit <- lm(y~x,data=data)
summary(lmfit)

ei  <- lmfit$residuals
ei2 <- ei^2

dt <- data.frame(ei2 = log(ei2),x = log(x))

lmfit1 <- lm(ei2~x,data=dt)

summary(lmfit1)

# 0.05 anlamlılık düzeyinde
qt(0.025,8,lower.tail=F)



