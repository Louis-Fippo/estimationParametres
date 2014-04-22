#Auteur: Fippo Fitime Louis
#Date:06-11-2013
#But: fonction permettant de tracer les états des sortes intéressantes

drawAll<-function(Donnees,born)
{
  #on redirige la sortie de l'image
  pdf("testredirection.pdf")
  source("repere.R");
  source("drawsignal.R");
  par(mfrow=c(2,2));
  noms=row.names(Donnees);
  
  #on fixe les bornes
  Min=born; Max=born+3;
  for(i in Min:Max)
  {    
    DonneesInt=Donnees[i,];
    DonneesInt=DonneesInt[-28];
    DonneesNumeric=as.numeric(DonneesInt);
    head(DonneesNumeric)
    repere(noms[i]);
    drawsignal(DonneesNumeric);
  }
  
  dev.off()
}
