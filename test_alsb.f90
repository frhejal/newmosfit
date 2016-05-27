program test_alsb
!program de test de la routine aslb du module algebre, pour la resolution de systemes lineaires.
use precision
use algebre
implicit none
integer::i,j,N=3,erreur
integer,dimension(100)::K
real(dp)::A(3,3),V(3),  AA(3,4),solution(3), true_solution(3)
real(dp)::D
A= reshape((/1.0_dp, 3.0_dp, 4.0_dp, 3.0_dp, 5.0_dp, 7.0_dp, 4.0_dp, -4.0_dp, -2.0_dp/), shape(A))
V=(/50.0_dp,2.0_dp ,31.0_dp/)
write(6,*) "Premier test : systeme à une solution"
call afficherMatrice(A,N,N)
call assembler(A,V,N,N,AA)
write(6,*) "matrice A + second membre"
call afficherMatrice(AA,N,N+1)
call alsb(AA,3,3,1,K,erreur)
write(6,*) "inverse de A  + solutions"
call afficherMatrice(AA,N,N+1)
write(6,*) "indice d'erreur :", erreur
contains
  ! afficherMatrice : affiche une matrice A(N,N) dans la sortie standard
  subroutine afficherMatrice(A,Ni,Nj)
    integer,intent(in)::Ni,Nj
    real(dp),intent(in),dimension(Ni,Nj)::A
    integer ::i,j
    do i=1,Ni
      write(6,'(a)',advance='no') "|"
      do j=1,Nj
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
  ! vect2mat : construit une matrice A(N,N) à partir d'un vecteur V(N*N), supposé rangé colonne par colonne
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
  subroutine assembler(A,V,Ni,Nj,AA)
    integer,intent(in)::Ni,Nj
    real(dp),dimension(Ni,Nj)::A
    real(dp),dimension(Ni)::V
    real(dp),dimension(Ni,*),intent(inout)::AA
    integer::j
    do j=1,Nj
      AA(:,j)=A(:,j)
    enddo
    AA(:,Nj+1)=V(:)
  end subroutine assembler
end program test_alsb
