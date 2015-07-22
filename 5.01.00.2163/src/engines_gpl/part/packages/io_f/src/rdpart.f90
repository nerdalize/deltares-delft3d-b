      subroutine rdpart ( lun1   , lun2   , lnam1  )

!       Deltares Software Centre

!>\file
!>                          Reads the delpar input file

!       Created           : July      2011 by Leo Postma

!       Modified          : January   2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!       Subroutines called:

!       Functions called  :

!       Logical units     : lun1 = delpar input file
!                           lun2 = delpar report file

      use precision       ! flexible size definition

      implicit none

!     Arguments

!     kind            function         name           description

      integer  ( ip), intent(in   ) :: lun1          !< unit number input file
      integer  ( ip), intent(in   ) :: lun2          !< unit number report file
      character(256), intent(in   ) :: lnam1         !< name of the input file

      return
      end    