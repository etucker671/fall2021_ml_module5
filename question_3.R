#clear env and load data
rm(list=ls())
wage_data <- read.csv("Wage.csv", header = TRUE)

#3a Fit wage~age with cubic regression spline, plot curve, and predict wage for age = 30
library(splines)
fit_cubreg <- lm(wage ~ bs(age, knots = c(30,50,60)), data = wage_data)
age_range <- 20:80
preds_cubreg <- predict(fit_cubreg, newdata = list(age = age_range), se = T)
plot(wage_data$age,wage_data$wage, col = "gray", pch=1, cex = 0.5, xlab = "Age", ylab = "Wage (USD x10^3)")
lines(age_range,preds_cubreg$fit, lwd = 2.5, col = "blue")
lines(age_range,preds_cubreg$fit + 2*preds_cubreg$se.fit, lty = "dashed", col = "dodgerblue2")
lines(age_range,preds_cubreg$fit - 2*preds_cubreg$se.fit, lty = "dashed", col = "dodgerblue2")
as.numeric(preds_cubreg$fit[which(age_range == 30)]) -> pred.30.cubreg
cat("Cubic regression spline predicts a wage of $",pred.30.cubreg,"k at 30 years old.","\n")

#3b Fit wage~age with smoothing spline, plot curve, and predict wage for age = 30
fit_smooth <- smooth.spline(wage_data$age,wage_data$wage,cv=TRUE)
plot(wage_data$age,wage_data$wage, col = "gray", pch=1, cex = 0.5, xlab = "Age", ylab = "Wage (USD x10^3)")
lines(fit_smooth, col = "blue", lwd = 2.5)
fit_smooth$y[which(fit_smooth$x == 30)] -> pred.30.smooth
cat("Smoothing spline predicts a wage of $",pred.30.smooth,"k at 30 years old.","\n")

#3c Fit wage ~ age + maritl using GAM, df = 4, and a separate constant for each maritl category
wage_data_2 <- wage_data
wage_data_2$maritl <- factor(wage_data_2$maritl, levels = c("2. Married", "1. Never Married", "3. Widowed", "4. Divorced", "5. Separated"))
fit_gam <- lm(wage ~ ns(age,4) + maritl, data = wage_data_2)

#coefficients produced for age polynomials and marital statuses
#married is default status
fit_gam$coefficients

#3c plot curve - general gam output
#plot 1 = wage~age holding marital status constant
#plot 2 = level changes for marital status
library(gam)
plot.Gam(fit_gam, se = TRUE, col = "red")

#3c plot all curves together
plot(wage_data$age,wage_data$wage, col = "gray", pch=1, cex = 0.5, ylim = c(50,130), xlab = "Age", ylab = "Wage (USD x10^3)")
preds_gam_married <- predict(fit_gam, newdata = list(age = 18:80, maritl = rep("2. Married", length(18:80))))
lines(x = 18:80, y = preds_gam_married, lwd = 2, col = "blue")
preds_gam_nevermarried <- predict(fit_gam, newdata = list(age = 18:80, maritl = rep("1. Never Married", length(18:80))))
lines(x = 18:80, y = preds_gam_nevermarried, lwd = 2, col = "purple")
preds_gam_divorced <- predict(fit_gam, newdata = list(age = 18:80, maritl = rep("4. Divorced", length(18:80))))
lines(x = 18:80, y = preds_gam_divorced, lwd = 2, col = "dodgerblue3")
preds_gam_separated <- predict(fit_gam, newdata = list(age = 18:80, maritl = rep("5. Separated", length(18:80))))
lines(x = 18:80, y = preds_gam_separated, lwd = 2, col = "red")
preds_gam_widowed <- predict(fit_gam, newdata = list(age = 18:80, maritl = rep("3. Widowed", length(18:80))))
lines(x = 18:80, y = preds_gam_widowed, lwd = 2, col = "green")
legend("bottomright", legend = c("Married", "Never Married", "Divorced", "Separated", "Widowed"), col = c("blue","purple","dodgerblue3","red","green"), lty = 1, lwd = 2, cex = 0.5)

#3c predict wage for a married 30 year old
pred.30.married.gam <- as.numeric(predict(fit_gam, newdata = list(age = 30, maritl = "2. Married")))
cat("GAM predicts a wage of $",pred.30.married.gam,"k for a married 30 year old.","\n")
