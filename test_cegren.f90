program test_cegren
  use precision
  use algebre
  implicit none
  complex(dp)::A(10),V(16)
  integer::n,mv
  n=4
  mv=0
  A=(0.0_dp,0.0_dp)
  V=(0.0_dp,0.0_dp)
  call cegren(A,V,n,mv)
  write(6,*) "A"
  call afficher_triangle(A,n)
  write(6,*) "V"
  call afficherVecMat(V,n)
  contains
  subroutine afficher_triangle(A,n)
    complex(dp),intent(in)::A(10)
    integer,intent(in)::n
    integer::i,k
    k=0
    write(6,*) "partie reelle"
    do i=1,n
      write(6,*)real(A(k+1:k+i))
      k=k+1
    enddo
      write(6,*) "partie complexe"
    k=0
    do i=1,n
      write(6,*)imag(A(k+1:k+i))
      k=k+1
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
