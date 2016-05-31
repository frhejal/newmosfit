program test_cegren
  use precision
  use algebre
  use old_cegren
  implicit none
  complex(dp)::A(10),Aold(10),V(16),Vold(16)
  integer::k,n,mv
  n=4
  mv=0
  A=(0.0_dp,0.0_dp)
  V=(0.0_dp,0.0_dp)
  Vold=(0.0_dp,0.0_dp)
!~   call cegren(A,V,n,mv)
!~   write(6,*) "A"
!~   call afficher_triangle(A,n)
!~   write(6,*) "V"
!~   call afficherVecMat(V,n)
  !creation d'une matrice complexe
!~   A=...
  do k=1,10
    A(k)=cmplx(k*1.0_dp, (11-k)*1.0_dp,dp)
  enddo
  Aold=A
!~   call afficher_triangle(A,n)
!~   call afficher_triangle(Aold,n)
  !appel de l'ancienne version de CEGREN 
  call cegren77(Aold,Vold,n,mv)
  !appel de la nouvelle version de cegren
  call cegren(A,V,n,mv)
  call afficherVecMat(Vold,n)
  call afficherVecMat(V,n)
  
  call afficher_triangle(A,n)
  call afficher_triangle(Aold,n)
  contains
  subroutine afficher_triangle(A,n)
    complex(dp),intent(in)::A(10)
    integer,intent(in)::n
    integer::i,k
    k=1
    write(6,*) "partie reelle"
    do i=1,n
      write(6,*)real(A(k:k+i-1))
      k=k+i
    enddo
      write(6,*) "partie complexe"
    k=1
    do i=1,n
      write(6,*)imag(A(k:k+i-1))
      k=k+i
    enddo
  end subroutine afficher_triangle
  subroutine afficherVecMat(A,n)
    complex(dp),intent(in)::A(16)
    integer,intent(in)::n
    integer::i,imin,imax
    write(6,*) "partie reelle"

    do i=1,n
      imin=(i-1)*n+1
      imax=i*n
      write(6,*)real(A(imin:imax))
    enddo
    write(6,*) "partie complexe"
    do i=1,n
      write(6,*)imag(A(i:i+n-1))
    enddo
  end subroutine afficherVecMat
  
end program test_cegren
