program test_alsb
!program de test de la routine aslb du module algebre, pour la resolution de systemes lineaires.
use precision
use algebre
implicit none
integer::N=3,erreur
integer,dimension(100)::K
real(DP)::A(3,3),V(3),  AA(3,4)
A= reshape((/1.0_DP, 3.0_DP, 4.0_DP, 3.0_DP, 5.0_DP, 7.0_DP, 4.0_DP, -4.0_DP, -2.0_DP/), shape(A))
V=(/50.0_DP,2.0_DP ,31.0_DP/)
write(6,*) "Premier test : systeme à une solution"
call afficherMatrice(A,N,N)
call assembler(A,V,N,N,AA)
write(6,*) "matrice A + second membre"
call afficherMatrice(AA,N,N+1)
call algebre_resoudre_systeme(AA,3,3,1,K,erreur)
write(6,*) "inverse de A  + solutions"
call afficherMatrice(AA,N,N+1)
write(6,*) "indice d'erreur :", erreur
contains
  ! afficherMatrice : affiche une matrice A(N,N) dans la sortie standard
  subroutine afficherMatrice(A,Ni,Nj)
    integer,intent(in)::Ni,Nj
    real(DP),intent(in),dimension(Ni,Nj)::A
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
    real(DP),intent(in)::A(N,N)
    real(DP),intent(out)::V(N*N)
    integer ::j
    do j=1,N
      V( (j-1)*N+1 :  N*j ) =  A(1:N,j)
    enddo
  end subroutine mat2Vect
  ! vect2mat : construit une matrice A(N,N) à partir d'un vecteur V(N*N), supposé rangé colonne par colonne
  ! chemin inverse: mat2Vect
  subroutine vect2mat(V,A,N)
    integer,intent(in)::N
    real(DP),intent(out)::A(N,N)
    real(DP),intent(in)::V(N*N)
    integer ::j
    do j=1,N
      A(1:N,j) = V( (j-1)*N+1 :  N*j )
    enddo
  end subroutine vect2mat
  subroutine assembler(A,V,Ni,Nj,AA)
    integer,intent(in)::Ni,Nj
    real(DP),dimension(Ni,Nj)::A
    real(DP),dimension(Ni)::V
    real(DP),dimension(Ni,*),intent(inout)::AA
    integer::j
    do j=1,Nj
      AA(:,j)=A(:,j)
    enddo
    AA(:,Nj+1)=V(:)
  end subroutine assembler
end program test_alsb
