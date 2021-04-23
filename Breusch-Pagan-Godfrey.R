Breusch-Pagan-Godfrey Testinin Algoritması
x <- c(80,100,85,110,120,115,130,140,
       125,90,105,160,150,165,145,
       180,225,200,240,185,220,
       210,245,260,190,205,265,270,
       230,250)

y <- c(55,65,70,80,79,84,98,95,90,75,
       74,110,113,125,108,115,140,
       120,145,130,152,144,175,180,
       135,140,178,191,137,189)


dt <- data.frame(y,x)

# Adım-1 Regresyon modelini tahmin ediniz. Artıkları bulunuz. 
lmfit <- lm(y~.,data=dt)
summary(lmfit)

plot(x,y)
res <- lmfit$residuals
rss <- sum(res^2)

# Adım-2  sigma karenin MLE Tahmicisi
sigmasapka <- rss/length(x)

# Adım-3 pi değerinin hesaplanması
p <- res^2/sigmasapka
dt2 <- data.frame(p,x)

# Adım-4 pi~. regresyon modelinin kuurlması
lmfit2 <- lm(p~.,data=dt2)
summary(lmfit2)

# Adım-5 
essfit2 <- sum((lmfit2$fitted.values-mean(p))^2)
Theta <- essfit2/2

# alfa=0.05 için 1 serbestlik dereceli ki-kare
# tablo değeri 3.8414. Sabit varyansı rededebiliriz.
if(qchisq(1-0.05,1) < Theta) "Sabit varyans reddedilir" else "Sabitvaryans reddedilemez"
