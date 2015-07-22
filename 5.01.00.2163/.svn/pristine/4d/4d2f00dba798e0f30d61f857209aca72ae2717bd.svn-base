      subroutine part08 ( lun    , nodye  , nocont , ictmax , amassd ,  &
                          ictime , amassc , aconc  , tmass  , tmassc ,  &
                          nosubs , ndprt  , tmassu , ftime  , linear ,  &
                          substi , nmdyer , nmconr )

!     CALCULATES TOTAL RELEASED MASS FOR DYE AND CONTINOUS RELEASES
!              (per time step)

!     system administration : m. zeeuw

!     created      : february 1990, by l. postma

!     modified     : july 1992 by r.j. vos
!                    - for 8 substances with identical trans-
!                      port but different decay
!                      june 1993 by m. zeeuw
!                    - initialized array tmassu with zero's

!     modified     : january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     note      : none.

!     logical unit numbers  : lun - output log file

      use precision    ! single/double precision

      implicit none    ! force explicit typing

!     parameters

!     kind          function         name                          Descriptipon

      integer (ip), intent(in   ) :: lun                         !< output unit number
      integer (ip), intent(in   ) :: nodye                       !< number of dye releases
      integer (ip), intent(in   ) :: nocont                      !< number of continuous releases
      integer (ip), intent(in   ) :: nosubs                      !< number of substances
      integer (ip), intent(in   ) :: ictmax(nocont)              !< number of breakpoints per continuous discharge
      real    (rp), intent(in   ) :: amassd(nosubs,nodye)        !< masses of dye releases (per subst)
      integer (ip), intent(in   ) :: ictime(nocont, * )          !< breakpoint times per continuous load
      real    (rp), intent(in   ) :: amassc(nocont,nosubs,*)     !< rates of continuous releases at breakpoints per substances
      real    (rp), intent(  out) :: aconc (nodye+nocont,nosubs) !< mass per particle for each load and substance
      real    (rp), intent(  out) :: tmass (nosubs)              !< total mass per substance
      real    (rp), intent(  out) :: tmassc(nocont,nosubs)       !< total mass continuous loads per substance
      integer (ip), intent(in   ) :: ndprt (nodye+nocont)        !< number of particles per load
      real    (rp), intent(  out) :: tmassu(nocont)              !< total unit mass continuous loads
      real    (rp), intent(in   ) :: ftime (nocont,*)            !< unit release rate per breakpoint
      integer (ip), intent(in   ) :: linear(nocont)              !< 1 = block; 2 = linear
      character(*), intent(in   ) :: substi(nosubs)              !< substance names
      character(*), intent(in   ) :: nmdyer(nodye )              !< dye release names
      character(*), intent(in   ) :: nmconr(nocont)              !< continuous release names
      return
!
      end subroutine
