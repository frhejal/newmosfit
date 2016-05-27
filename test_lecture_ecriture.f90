program test_lecture_ecriture
  use precision
  use lecture
  use ecriture
  use hyperfins
  implicit none
  integer::NMAX,NS,NS1,NS2,IOGV
  real(dp)::CN,HBRUIT
  real(dp)::GRASS(10)
!~   character(len=*)::fichier_sortie='test.out'
  call lecture_options(CN,NMAX,NS,NS1,NS2,HBRUIT,GRASS)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV)
  call ecriture_nommer_fichier_de_sortie('test.out')
  call ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call lecture_param( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call lecture_param0(DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA,TETA,PTETA,GAMA,BETA,ALFA,MONOC,NB)
  call hyperfins_super(1,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call hyperfins_super(2,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  call hyperfins_super(3,1)
  call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
  print *,CH, PCH

end program test_lecture_ecriture
