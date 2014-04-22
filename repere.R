repere<-function(Titre)
{
  #x<-seq(0, 100)
  x<-seq(0, 26)
  #y<-seq(-3,3,0.06)
  #y<-seq(-1.2,1.2,0.09)
  y<-seq(-0.2,3.2,0.13)
  plot(x, y, main = Titre, xlab = "Times", ylab = "Levels", type = "l", col = "white", pch = 3, lab = c(10, 5, 0))
  #title(Titre, cex.main = 0.8)
}