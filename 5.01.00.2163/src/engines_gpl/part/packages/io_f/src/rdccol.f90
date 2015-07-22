      subroutine rdccol ( nmax   , mmax   , lun    , fnam   , ftype  ,  &
                          lgrid  , xbott  , ybott  , lun2   )

!     READING CURVILINEAR CCO FILE
!          (initially)

!     system administration : m. zeeuw


!     created               : juli 1989, by m.e. sileon



!     modified              : june 1993, by m. zeeuw
!                             - implemented error numbers
!                             nov 1997: uses openfl
!                             dec 1997: read also layt form first record!!
!                                       and 9 times xdummy
!                             january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

      use precision      ! single and double precision

!     parameters

!     kind           function         name                Descriptipon

      integer  (ip), intent(in   ) :: nmax              !< first dimension of the grid
      integer  (ip), intent(in   ) :: mmax              !< second dimension of the grid
      integer  (ip), intent(in   ) :: lun               !< unit number cco file
      character( *), intent(in   ) :: fnam              !< name of cco file
      character( *), intent(in   ) :: ftype(*)          !< type of cco file
      integer  (ip), intent(in   ) :: lgrid(nmax,mmax)  !< grid table
      real     (sp), intent(  out) :: xbott(*)          !< x-values in the grid
      real     (sp), intent(  out) :: ybott(*)          !< y-values in the grid
      integer  (ip), intent(in   ) :: lun2              !< unit number log-file

      return

      end subroutine
