#Auteur: Fippo Fitime Louis
#Date:06-11-2013
#But: fonction permettant de tracer un état 
drawState<-function(PState, NState, Temps)
{
  #coordonnees du point de depart
  XDepart=Temps-1;
  YDepart=PState;
  
  #on teste si l etat n'a pas change
  if(PState==NState)
  {
    #l'etat n'as pas changé on calcul les coodonnées du point d'arrivé
    XArrive=Temps;
    YArrive=NState;
    
    #On trace le segment 
    segments(XDepart,YDepart,XArrive,YArrive);
  }
  else  #on a changé d'état
   {
  #on se met dans le bon etat
  #on calcul les coordonnées intermédiaire
  XInt=Temps-1;
  YInt=NState;
  #on change d'état
  segments(XDepart,YDepart,XInt,YInt);
  
  
  #on trace le nouvel état
  #on calcul les coordonnées du point d'arrivé
  XArrive=Temps;
  YArrive=NState;
  
  #on trace l'etat
  segments(XInt,YInt,XArrive,YArrive);
  
}
}