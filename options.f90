!>@file
!***********************************************************************
!                        MODULE OPTION
!***********************************************************************
!>@brief Variables de controle des options de Mosfit.
!>@version juin 2016
!>@details Les options principales sont specifiéée par le contenu du tableau IO.
!!    Numéro  |valeur|  Option
!!    --------|-----:|---------
!!      IO(1) |N | Ajout de N millions au spectre,
!!      IO(2) |1 | Option obsolete : changement d'echelle du tracé du spectre,
!!      IO(3) |0 | Choix de l'élement Fe57,
!!      .     |1 | Choix de l'élement Sn119,
!!      IO(4) |0 | Hbruit=0,
!!      .     |1 | Hbruit non ajustable,
!!      .     |2 | Hbruit ajustable,
!!      IO(5) |N | Choix de connexion entre parametre (module CONNEX),
!!      IO(6) |1 | Ecrire Ycalc-Yexp,
!!      IO(7) |1 | Ecrire Ycalc,
!!      IO(8) |1 | Ecrire des position, largeur et hauteur de raie,
!!      IO(9) |1 | BETA=TETA ; ALPHA=GAMMA,
!!      IO(10)|0 | Lecture du spectre experimental,
!!      .     |1 | Pas de spectre expérimental,
!!      .     |2 | Même spectre expérimental que cas précédent ( +IO(1) millions),
!!      IO(11)|1 | Tracer Yexp-Ycalc,
!!      IO(12)|1 | Ecrire du résume dans un fichier RESULTAT.doc, écriture de Yexp et Ycalc dans un fichier .dat exploitable par gnuplot,
!!      IO(13)|1 | Lissage de tout les sous-spectres, et des moyennes des sous-spectres
!!            |2 | Lissage de la distribution uniquement
!!            |3 | Lissage d'une plage de sous-spectres spécifiée par PLAGEL
!!      IO(14)|1 | Tracer le diagramme du lissage
!!      IO(15)|1 | Utilisation de la formulation du champ hyperfin pour des cycloïdes (Gamma est remplacé par Wm)
!!      IO(16)|N | Convolution Gauss*Lorentz pour les enveloppes des raies (N=nbre de sous canaux),
!!      IO(17)|1 | Tracer les sous-spectres tels que definis par GRASS (si IO(12)==1),
!!      IO(18)|1 | Adapter les noms des fichiers de sortie au nom du fichier d'entrée
!!      IO(19)|  |     OPTION VACANTE
!!      IO(20)|1 | Horizontalisation du fond continu.
!! Si non précisé dans le tableau ci-dessus, la mise à zéro d'une option correspond à une absence d'effet. 
!**********************************************************************
module options
  use precision
  implicit none
  integer,save::IOPT   !< indique si des options IO sont spécifiées.
  integer,save::IO(20) !< liste des options
  integer,save::IZZ    !< indique si il y a des canaux à ignorer
  integer,save::IZ(10) !< plages de canaux a ignorer
  character(len=256),save::TITRE !< en-tete du fichier .coo (date, echantillon, temperature, etc...)

  contains
  !>@brief initialisation des variables d'options
  subroutine options_raz
    IZ=0
    IO=0
    IOPT=0
    IZZ=0
    TITRE='Sans Titre'
  end subroutine options_raz
end module options
