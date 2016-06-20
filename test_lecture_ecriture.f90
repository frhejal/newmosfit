program test_lecture_ecriture
  use precision
  use lecture
  use ecriture
  use variablesFixes
  use options
  use variablesAjustables
  implicit none
  call lecture_titre
  call lecture_options(CN,NMAX,NS,NS1,NS2,HBRUIT,GRASS)
  call ecriture_nommer_fichier_de_sortie('test.out')
  call ecriture_titre(0)
  call ecriture_options(CN,NMAX,NS,NS1,NS2)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call lecture_param0(DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA,THETA,PTHETA,GAMA,BETA,ALPHA,MONOC,NB)
  call variablesAjustables_super(1,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call variablesAjustables_super(2,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  call variablesAjustables_super(3,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,THETA,GAMA,BETA,ALPHA,MONOC,NB)
  print *,CH, PCH

end program test_lecture_ecriture
