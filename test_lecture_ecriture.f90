program test_lecture_ecriture
  use precision
  use lecture
  use ecriture
  integer::NMAX,NS,NS1,NS2,IOPT,MONOC
  real(dp)::CN,HBRUIT, DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA
  real(dp)::GRASS(10)
!~   character(len=*)::fichier_sortie='test.out'
  call lecture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,GRASS)
  call lecture_spectre( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA)
  call ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,'test.out')
  
end program test_lecture_ecriture
