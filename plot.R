rm(list = ls(all.names = TRUE))
cat("\014")
windowsFonts(A = windowsFont("Times New Roman"))
plot.error <- function(x, y, y_U, y_L, len = 1, col = "black") {
  len <- len * 0.03
  arrows(x0 = x, y0 = y, x1 = x, y1 = y_U, col = col, angle = 90, length = len)
  arrows(x0 = x, y0 = y, x1 = x, y1 = y_L, col = col, angle = 90, length = len)
}
par(family = "A")
data<-read.csv("C:/Users/22592/Documents/R/209395_Frail status impact HRQoL.csv", header=T, sep=",", encoding = "UTF-8")
dim(data)
labs <- c("Total", "Robust", "Pre.frail", "Frail")
# total
plot(data$Week, data$Total, family="A", xlab = "", ylab = "EQ-5D",cex.lab=1.1,cex.axis = 1.1, pch = 16, xlim = c(0, 67), ylim = c(0.4, 1), las = 1)
lines(spline(data$Week, data$Total, n=10000, method = "natural"), col = "red", lwd = 2)
plot.error(data$Week, data$Total, y_U = data$Total_U, y_L = data$Total_L, col = "red")
points(data$Week, data$Total, pch = 16, col = "black")
# Robust
lines(spline(data$Week, data$Robust, n=10000, method = "natural"), col = "seagreen", lwd = 2)
plot.error(data$Week, data$Robust, y_U = data$Robust_U, y_L = data$Robust_L, col = "seagreen")
points(data$Week, data$Robust, pch = 15, col = "black")
# Pre.frail
lines(spline(data$Week, data$Pre.frail, n=10000, method = "natural"), col = "blue", lwd = 2)
plot.error(data$Week, data$Pre.frail, y_U = data$Pre.frail_U, y_L = data$Pre.frail_L, col = "blue")
points(data$Week, data$Pre.frail, pch = 17, col = "black")
# Frail
lines(spline(data$Week, data$Frail, n=10000, method = "natural"), col = "orange", lwd = 2)
plot.error(data$Week, data$Frail, y_U = data$Frail_U, y_L = data$Frail_L, col = "orange")
points(data$Week, data$Frail, pch = 18, col = "black")
# legend
legend("topright", legend = labs, cex = 1.1,pt.cex = 1, lty = 1, pch = c(16, 15, 17, 18), col = c("red", "seagreen", "blue", "orange"), inset = 0.01, box.col = "gray")
# Baseline
abline(h=0.87,lty=2)
text(x=25, y=0.88,family="A",label="Baseline pre-operation EQ-5D",cex=1.1)

axis(1,data$Week,family="A",labels=c("Pre", "1M", "3M", "6M", "1Y"),line=-2.5, cex.axis=1.1)