# White testi

data <- tablo2
x1 <- data$EGITIM
x2 <- data$DENEYIM
y <- data$UCRET
data <- cbind(y,x1,x2)
data <- as.data.frame(data)
dt <- data.frame(y,x1,x2)
k <- 2
N <- length(x)
model1 <- lm(y~.,data=dt)
res <- model1$residuals
res2 <- res^2

dt2 <- as.data.frame(cbind(res2,x1,x2,x1^2,x2^2,x1*x2))
colnames(dt2) <- c("res2","x1","x2","x1x1","x2x2","x1x2")
model2 <- lm(res2~.,data=dt2)
summ <- summary(model2)
r2 <- summ$r.squared
nr2 <- N*r2
#0.05 anlalımlık seviyesinde
if(qchisq(1-0.05,k) < nr2) "Sabit varyans reddedilir" else "Sabitvaryans reddedilemez"
