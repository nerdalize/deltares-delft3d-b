      subroutine hsurf    ( nosys  , notot  , noseg  , nopa   , paname ,
     &                      param  , nosfun , sfname , segfun , surface,
     &                      lun)
!     Deltares Software Centre

!>\File
!>           Set values of horizontal surface array.

!     Created             :    September 2012 by Christophe Thiange

!     Logical unitnumbers : lun     = number of monitoring file

!     Subroutines called  : none

      use timers
      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      integer   (4), intent(in   ) :: nopa                    !< number of parameters
      character(20), intent(in   ) :: paname(nopa  )          !< names of the parameters
      real      (4), intent(in   ) :: param (nopa  ,noseg)    !< parameter values
      integer   (4), intent(in   ) :: nosfun                  !< number of segment functions
      character(20), intent(in   ) :: sfname(nosfun)          !< names of the segment functions
      real      (4), intent(in   ) :: segfun(noseg ,nosfun)   !< segment function values
      real      (4), intent(inout) :: surface(noseg)          !< horizontal surface
      integer   (4), intent(in   ) :: lun                     !< logical unit number monitoring file


!     local variables

      logical   , save :: first = .true.  ! true if first time step
      integer(4), save :: indx            ! index of the surf variable in the array
      integer(4), save :: mode            ! -1 segment functions, +1 parameters, 0 none
      integer(4), save :: ithandl         ! timer handle
      data       ithandl /0/
      if ( timon ) call timstrt ( "hsurf", ithandl )

!         see if the surface is available

      if ( nosys .ne. notot ) then                          ! if there are no bed-substances
         if ( first ) then
            first = .false.
            call zoek20 ( 'SURF      ', nopa  , paname , 10 , indx )
            if ( indx .gt. 0 ) then                           ! SURF is found
               mode = 1
               surface(:) = param(indx,1:noseg)
            else
               call zoek20 ( 'SURF      ', nosfun, sfname, 10, indx )
               if ( indx .gt. 0 ) then
                  mode = -1
               else
                 surface = 1.0
                 write(lun, 2000)
               endif
            endif
         endif
         if ( mode .eq.  -1 ) then
            surface(:) = segfun(1:noseg,indx)
         endif
      endif

      if ( timon ) call timstop ( ithandl )
      return
 2000 format ( ' ERROR  : surface is not available')
      end
