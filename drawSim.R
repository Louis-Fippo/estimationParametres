#Auteur: Fippo Fitime Louis
#Date:06-11-2013
#But: fonction permettant de tracer toutes les simulations effectuées

drawSim<-function(JeuxDonnees)
{
  #inclusion des fichiers sources
  source("drawAll.R");
  source("aSurveiller.R");
  
  
  #lecture du fichier et simulation
  #args <- commandArgs(TRUE); #on recupère l'argument donné au script
  #JeuxDonne=read.table(args[1], h=T);
 
  #JeuxDonneAS=aSurveiller(JeuxDonnees);
  JeuxDonneesAS=JeuxDonnees[c("p45.pts","p25.pts","p31.pts","p32.pts","p33.pts","p42.pts","p43.pts","p44.pts","p45.pts","p52.pts","p65.pts","p78.pts","p111.pts","p114.pts","p115.pts","p116.pts"),];
  
 
  borne=c(1,5,9,13);
  for(i in borne)
  { 
    print(i)
    drawAll(JeuxDonneAS,i);
  }
  dm=dim(JeuxDonneesAS)
  print(dm)
  
}