#Auteur: Fippo Fitime Louis
#Date:02-12-2013
#But: fonction permettant de dicretiser les données d'expression continu de gène


#fonction permettant de déterminer le type de front que nous avons
NatureFront<-function(NextLevel, PreviousLevel)
{
  if((NextLevel-PreviousLevel)>0)
  {
    return (1);
  }
  else
    {
      return (0);  
    }
  
}

#fonction pour déterminer la significativité du FrontMontant
significativiteFM<-function(NextLevel, PreviousLevel, etat, seuil1, seuil2)
{
  if(etat==0)
  {
    if(NextLevel<seuil1)
    {
      significativite<-0;
      
    }
    else if(NextLevel<seuil2)
    {
      significativite<-1;
      etat<-1;
    }
    else
    {
      significativite<-1;
      etat<-2;
    }
  }
  else if(etat==1)
  {
    if(NextLevel<seuil2)
    {
      significativite<-0;
      etat<-1;
    }
    else
    {
      significativite<-1;
      etat<-2;
    }
  }
  else
  {
    significativite<-0;
    etat<-2;
  }

  
  return (c(significativite,etat));
  
}


#fonction pour déterminer la significativité du FrontDescendant
significativiteFD<-function(NextLevel, PreviousLevel, etat, seuil1, seuil2)
{
  if(etat==2)
  {
    if(NextLevel>seuil2)
    {
      significativite<-0;
      etatRetour<-2;
      
    }
    else if(NextLevel>seuil1)
    {
      significativite<-1;
      etatRetour<-1;
    }
    else
    {
      significativite<-1;
      etatRetour<-0;
    }
  }
  else if(etat==1)
  {
    if(NextLevel>seuil1)
    {
      significativite<-0;
      etatRetour<-1;
    }
    else
    {
      significativite<-1;
      etatRetour<-0;
    }
  }
  else
  {
    significativite<-0;
    etatRetour<-0;
  }
  
  return (c(significativite,etatRetour));
  
}



#fonction permettant de calculer les seuils pour chaque expression d'un gène
seuilExpression<-function(X)
{
  #on extrait le minimum et le maximum du vecteur
  Max<-max(X);
  Min<-min(X);
  
  #on calcul la longueur de l'interval
  ecart<-Max-Min;
  
  
  #on divise l'écart en trois
  #ecart<-ecart/3;
  
  #on calcul les différents seuils
  seuil1<-Min+(ecart/3);
  seuil2<-Min+2*(ecart/3);
  vecteurSeuil<-c(seuil1,seuil2);
  return(vecteurSeuil);
  
}

InitialisationDonnees<-function(vecteurSeuil,donnees)
{
  #dimDonnees<-dim(donnees);
  
    if(0<vecteurSeuil[1])
    {
      donnees<-0;
    }
    else if(0>vecteurSeuil[2])
    {
      print("cas 2")
      donnees<-2;
    }
    else
    {
      donnees<-1;
    }
 return(donnees);
}


#fonction de discretisation qui va faire appel à d'autres fonctions
discretisation<-function()

{
  #lecture du fichier des données
  genexpr=read.csv("sig_genes_result_temp.csv",sep="\t",h=T);
  donneesDiscrete=read.csv("sig_genes_result_temp_disFS3.csv", sep="\t", h=T);
  
  #construction des noms des colonnes
  vecteurNom1=names(genexpr);
  vecteurNom2=names(donneesDiscrete);
  #on enlève les deux premières colonnes
  vecteurNom1=vecteurNom1[-1];
  vecteurNom1=vecteurNom1[-1];
  vecteurNom2=vecteurNom2[-1];
  vecteurNom2=vecteurNom2[-1];
  
  #initialisation du tableau discret en appelant la fonction InitialisationDonnees
  donneesDepart=genexpr[,-1];
  donneesDepart=donneesDepart[,-1];
  dimTab=dim(donneesDepart)
  
  for(i in 1:dimTab[1])
  {
    seuilcourant=seuilExpression(donneesDepart[i,]);print(seuilcourant);
    donneesDiscrete[i,3]=InitialisationDonnees(seuilcourant,donneesDiscrete[i,3]);
    
    
    #indexCourant=as.character(i);
    vecteurCourantNom1=vecteurNom1[-1];
    vecteurCourantNom2=vecteurNom2[-1];
    #initialisation du pindex
    pindex="X0h";
    
    #pour chaque ligne(gene) on va discretisert
    #on va parcourir toutes les colones 
    for(j in vecteurCourantNom1)
    {
      print(j);
      #on teste si c'est un front montant
      #pindex est l'index précedent
      if(NatureFront(donneesDepart[i,j],donneesDepart[i,pindex]))
      {
        signif=significativiteFM(donneesDepart[i,j],donneesDepart[i,pindex],donneesDiscrete[i,pindex],seuilcourant[1],seuilcourant[2]);
        
        #si c'est un FMS on affecte les etats dans le tableau des donnees à discretiser
        for(k in vecteurCourantNom2)
        {
          print(k);
          
          donneesDiscrete[i,k]=signif[2];
          
          vecteurCourantNom2=vecteurCourantNom2[-1];
          if(k==j)break;
        }
        
      }else
      {
       
        signif=significativiteFD(donneesDepart[i,j],donneesDepart[i,pindex],donneesDiscrete[i,pindex],seuilcourant[1],seuilcourant[2]);
        
        #si c'est un FDS on affecte les etats dans le tableau des donnees à discretiser
        for(k in vecteurCourantNom2)
        {
          print(k);
          
          donneesDiscrete[i,k]=signif[2];
          
          vecteurCourantNom2=vecteurCourantNom2[-1];
          if(k==j)break;
        }
        
      }
      
      
      
      pindex=j;
      vecteurCourantNom1=vecteurCourantNom1[-1];
    }
    
  }
  
  
  
  #print(donneesDiscrete); pour écrire dans un fichier 
 write.table(donneesDiscrete, "sig_genes_result_temp_disFS3.csv", sep = "\t", quote = FALSE, col.names = NA);
 write.table(donneesDiscrete, "sig_genes_result_temp_disFS3.txt", sep = "\t", quote = FALSE, col.names = NA);

 return (donneesDiscrete);
 
 
}
