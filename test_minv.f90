program test_minv
!program de test de la routine minv du module algebre, pour l'inversion de matrice carree.
use precision
use algebre
implicit none
integer::i,j,N=3
real(dp)::A(3,3), AA(9), InvATrue(3,3)
real(dp)::D
! Matrice inversible
A= reshape((/2.0_dp, -1.0_dp, 0.0_dp, -1.0_dp,2.0_dp, -1.0_dp, 0.0_dp, -1.0_dp, 2.0_dp/), shape(A))
InvATrue = reshape((/0.75_dp, 0.5_dp, 0.25_dp, 0.5_dp ,1.0_dp, 0.5_dp, 0.25_dp , 0.5_dp, 0.75_dp/), shape(InvATrue))
write(6,*) "Premier test : matrice inversible"
call afficherMatrice(A,N)
write(6,*) "Inverse attendue :"
call afficherMatrice(InvATrue,N)
call mat2Vect(A,AA,N)
write(6,*) "****CALCUL****"
call algebre_inverser_matrice(AA,N,d)
write(6,*) "Matrice apres inversion :"
call vect2mat(AA,A,N)
call afficherMatrice(A,N)
write(6,*) "Plus grand ecart avec theorie : ", maxval(abs(A-InvATrue))
write(6,*) "Determinant :", D
!Matrice singuliere
A= reshape((/-3.0_dp, 9.0_dp, 18.0_dp, -5.0_dp, 14.0_dp, 29.0_dp, 1.0_dp, 1.0_dp, -2.0_dp/), shape(A))
!~ InvATrue = reshape((/0.75_dp, 0.5_dp, 0.25_dp, 0.5_dp ,1.0_dp, 0.5_dp, 0.25_dp , 0.5_dp, 0.75_dp/), shape(InvATrue))
write(6,*) "-----------------------------------------------"
write(6,*) "Second test : matrice singulière:"
call afficherMatrice(A,N)
!~ write(6,*) "Inverse attendue :"
!~ call afficherMatrice(InvATrue,N)
call mat2Vect(A,AA,N)
write(6,*) "****CALCUL****"
call algebre_inverser_matrice(AA,N,d)
write(6,*) "Determinant :", D
if(abs(D)<1D-11) write(6,*) "LA MATRICE EST SINGULIERE ! (youpi)"

!~ write(6,*) "Plus grande difference avec theorie : ", maxval(abs(A-InvATrue))




contains
  ! afficherMatrice : affiche une matrice A(N,N) dans la sortie standard
  subroutine afficherMatrice(A,N)
    integer,intent(in)::N
    real(dp),intent(in)::A(N,N)
    integer ::i,j
    do i=1,N
      write(6,'(a)',advance='no') "|"
      do j=1,N
        write(6,'(f9.4)',advance='no') A(i,j)
      enddo
      write(6,*) "|"
    enddo
  end subroutine afficherMatrice
  ! mat2Vect : range une matrice A(N,N) dans un vecteur V(N*N), colonne par colonne
  ! chemin inverse: vect2Mat
  subroutine mat2Vect(A,V,N)
    integer,intent(in)::N
    real(dp),intent(in)::A(N,N)
    real(dp),intent(out)::V(N*N)
    integer ::i,j
    do j=1,N
      V( (j-1)*N+1 :  N*j ) =  A(1:N,j)
    enddo
  end subroutine mat2Vect
  ! vect2mat : construit une une matrice A(N,N) à partir d'un vecteur V(N*N), supposé rangé colonne par colonne
  ! chemin inverse: mat2Vect
  subroutine vect2mat(V,A,N)
    integer,intent(in)::N
    real(dp),intent(out)::A(N,N)
    real(dp),intent(in)::V(N*N)
    integer ::i,j
    do j=1,N
      A(1:N,j) = V( (j-1)*N+1 :  N*j )
    enddo
  end subroutine vect2mat
end program
