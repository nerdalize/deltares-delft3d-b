      subroutine dlwqbl ( lunin  , lunout , itime  , idtime , itime1 ,    &
     &                    itime2 , ihdel  , nftot  , nrtot  , array1 ,    &
     &                    result , ipnt   , luntxt , isflag , ifflag ,    &
     &                    update )

!     Deltares Software Centre

!>/File
!>            Steps along in a dataset with blockwave property (flows)
!>
!>            This routine distinguishes from flwqtd that also supports
!>            blockwaves in that it only needs one array.\n
!>            Because of support of active-only files this have become
!>            two arrays, would otherwise need 3 arrays.\n
!>            The price paid is that:
!>            - the series need to be equidistant
!>            - the last record before rewind should contain only zeros
!>            probably an additional array and use of dlwqtd would be simpler

!     system administration : Antoon Koster

!     created               : September 1996 by Robert Vos

!     modified              : June      2011 by Leo Postma : support for active only hydrodynamics
!                             January   2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unitnumbers   : lunin  - input unit number hydrodynamic file
!                             lunout - monitor file

!     subroutines called    : srstop   , stops execution

      use precision         ! single/double precision
      use timers

      implicit none

!     Arguments           :

!     kind           function         name               description

      integer  (ip), intent(in   ) :: lunin            !< unit number intermediate file
      integer  (ip), intent(in   ) :: lunout           !< unit number report file
      integer  (ip), intent(in   ) :: itime            !< current time in the model
      integer  (ip), intent(inout) :: idtime           !< time offset: > 0 after rewind
      integer  (ip), intent(inout) :: itime1           !< lower time in file
      integer  (ip), intent(inout) :: itime2           !< higher time in file
      integer  (ip), intent(in   ) :: ihdel            !< time step size in file
      integer  (ip), intent(in   ) :: nftot            !< array size in the file
      integer  (ip), intent(in   ) :: nrtot            !< array size to be delivered
      real     (sp), intent(inout) :: array1(nftot)    !< record at lower time in file
      real     (sp), intent(inout) :: result(nrtot)    !< record as delivered to Delpar
      integer  (ip), intent(in   ) :: ipnt  (nftot)    !< pointer from nftot to nrtot
      character( *), intent(in   ) :: luntxt           !< text with this unit number
      integer  (ip), intent(in   ) :: isflag           !< if 1 then 'dddhhmmss' format
      integer  (ip), intent(in   ) :: ifflag           !< if 1 then this is first invokation
      logical      , intent(  out) :: update           !< true if record is updated

      return

      end subroutine
