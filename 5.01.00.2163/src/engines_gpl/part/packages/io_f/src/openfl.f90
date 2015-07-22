module openfl_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision              ! single and double precision
      use timers
!
!  module procedure(s)
!
use delete_file_mod        ! explicit interface
!
implicit none              ! force explicit typing
!
contains
      subroutine openfl ( lun, finam, ftype, iopt)
!
!
!     Deltares (former: Deltares)
!
!     version: 2.40      : windows 95 version
!
!     created            : oktober '95  by  robert vos
!
!     modified           : november '97 by robert vos
!                          delete file first when iopt = 1
!                          for iopt=0 file is 'old'
!
!     subroutines called : delete_file
!                          srstop
!
!     function           : open file dependent on ftype
!
!     note               : possible types
!                          'unformatted' (all platforms)
!                          'transparent' (pc lahey     )
!                          'binary'      (pc microsoft )
!
!*********************************************************************
!     system dependent routine
!     configuration
!
!     unix systems
!*********************************************************************
!
!     logical units      : -
!
!     parameters    :
!
!     name    kind       length      funct.  description
!     ---------------------------------------------------------
!     finam   character*80 1         input   file name
!     ftype   character*20 1         input   file type
!     ierror  integer      1         local   error in delete_file
!     iopt    integer      1         input   if 1 delete existing file
!                                            with same name
!     lun     integer      1         input   unit number file
!     ---------------------------------------------------------
!
      implicit none

      character(len=20)  :: ftype
      character(len=256) :: finam
!
!     local scalars
!
      integer(ip) :: iopt, ierror
      integer(ip) :: lun
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "openfl", ithndl )

!
      select case ( iopt )
         case ( 1 )
            call delete_file ( finam, ierror )
            if(ftype=='unformatted') then
               open ( lun, file = finam, form = ftype, err = 99)
            elseif (ftype=='binary') then
               open ( lun, file = finam, form = ftype, err = 99)
            else
               write(*,*) ' Error when opening file with name: '
               write(*,*) finam
               write(*,*) ' of type: ', ftype
               call error(' Filetype not known by openfl ')
            endif
         case ( 0 )
            if(ftype=='unformatted') then
               open ( lun, file = finam, form = ftype, status ='old', err = 99)
            elseif (ftype=='binary') then
               open ( lun, file = finam, form = ftype, status = 'old', err = 99)
            else
               write(*,'(//a,a40)') ' Error on opening file: ',finam
               write(*,'(  a,a  )') ' Expected file type   : ',ftype
               write(*,'(  a    )') ' Please check if file exists'
               write(*,'(  a    )') ' Please check correct file type'
               call error(' filetype not known by openfl ')
            endif
         case ( 3 )
            if(ftype=='unformatted') then
               open ( lun, file = finam, form = ftype, status ='old', err = 10)
            elseif (ftype=='binary') then
               open ( lun, file = finam, form = ftype, status = 'old', err = 10)
            else
               write(*,'(//a,a40)') ' Error on opening file: ',finam
               write(*,'(  a,a  )') ' Expected file type   : ',ftype
               write(*,'(  a    )') ' Please check if file exists'
               write(*,'(  a    )') ' Please check correct file type'
               call error(' filetype not known by openfl ')
            endif
      end select
!
      if ( timon ) call timstop ( ithndl )
      return

   10 write(*,'(/a,a40/)') ' Warning, file does not exist: ',finam
      lun = 0
      if ( timon ) call timstop ( ithndl )
      return
!
 99   write(*,'(//a,a40)') ' Error on opening file: ',finam
      write(*,'(  a,a  )') ' Expected file type   : ',ftype
      write(*,'(  a    )') ' Please check if file exists'
      write(*,'(  a    )') ' Please check correct file type'
      call srstop(1)
!
      end subroutine
end module
