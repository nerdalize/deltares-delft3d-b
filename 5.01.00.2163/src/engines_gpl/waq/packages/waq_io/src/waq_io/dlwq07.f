!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine dlwq07 ( lun    , lchar    , filtype, noseg  , nocons ,
     &                    nopa   , nofun    , nosfun , itfact , dtflg2 ,
     &                    dtflg3 , iwidth   , novec  , vrsion , ioutpt ,
     &                    nothrd , constants, ierr   , iwar   )

!     Deltares Software Centre

!>\file
!>                          Reads all model constants, variables and distributed variables/functions - old style
!>\par  Description:
!>                          This routine reads:
!>                             - the process constants
!>                             - the process time functions
!>                             - the process distributed constants
!>                             - the process distributed time functions
!>                             .
!>                          This old style routine uses rigorous input processing

!     Global declarations

      use dlwq_data      ! for definition and storage of data
      use rd_token       ! tokenized reading
      use partmem        ! for the interface with Delpar (Tau and VertDisp)
      use timers         ! performance timers
      implicit none

!     declaration of arguments

      integer               , intent(in)    :: lun(*)       !< unit numbers used
      character(len=*)      , intent(inout) :: lchar(*)     !< filenames
      integer  ( 4)         , intent(inout) :: filtype(*)   !< type of binary file
      integer               , intent(in)    :: noseg        !< number of segments
      integer               , intent(out)   :: nocons       !< number of constants
      integer               , intent(out)   :: nopa         !< number of constants
      integer               , intent(out)   :: nofun        !< number of constants
      integer               , intent(out)   :: nosfun       !< number of constants
      integer               , intent(in)    :: itfact       !< factor between timescales
      logical               , intent(in)    :: dtflg2       !< option "date"-format 2nd time scale
      logical               , intent(in)    :: dtflg3       !< 'date'-format (F;ddmmhhss,T;yydddhh)
      integer               , intent(in)    :: iwidth       !< width of output file
      integer               , intent(inout) :: novec        !< number of fastsolv vectors
      real                  , intent(in)    :: vrsion       !< program version number
      integer               , intent(in)    :: ioutpt       !< output file option
      integer  ( 4)         , intent(  out) :: nothrd       !< nr of threads to be used for paralell processing
      type(t_dlwq_item)     , intent(inout) :: constants    !< delwaq constants list
      integer  ( 4)         , intent(inout) :: ierr         !< Cumulative error count
      integer  ( 4)         , intent(inout) :: iwar         !< cumulative warning count

!     Local declaration

      integer                               :: subject                    ! subject of input(boundary,waste,function..)
      type(t_dlwqdata)                      :: dlwqdata                   ! one data block
      type(t_dlwqdatacoll)                  :: proc_pars                  ! all the process parameters data from file
      real                , allocatable     :: fscale(:)                  ! scale facotrs
      character(len=20)   , allocatable     :: funame(:)                  ! function names
      character(len=20)   , allocatable     :: sfname(:)                  ! segment function names
      character(len=255)                    :: cdummy                     ! dummy, not always, opt1 so take care
      real                                  :: adummy                     ! dummy
      integer                               :: idummy                     ! dummy
      integer                               :: idum                       ! dummy
      integer                               :: iardum(1)                  ! dummy integer work array
      logical                               :: ldummy                     ! dummy
      character(len=20)                     :: ch20, chulp                ! help variable
      integer                               :: irmax2                     ! length of help array
      integer                               :: inovec                     ! index of the 'NOVEC' constant
      integer                               :: inothr                     ! index of the 'NOTHREADS' constant
      integer                               :: itype                      ! type to be read
      integer                               :: icopt1                     ! file option constants
      integer                               :: ipopt1                     ! file option parameters
      integer                               :: ipopt2                     ! second option parameters
      integer                               :: ifopt1                     ! file option functions
      integer                               :: isopt1                     ! file option segment functions
      integer                               :: ifop                       ! file option for single segment function
      integer                               :: i, ipa, iseg, isfun        ! loop indexes
      integer                               :: idata                      ! index
      integer                               :: ioerr                      ! local error io
      integer                               :: ierr2                      ! local error
      integer                               :: ierr_alloc                 ! local error allocating
      logical                                 taupart              ! is tau present?
      logical                                 vdfpart              ! is vertical diffusion present
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq07", ithndl )

      ! some initialisation

      taupart = .false.
      vdfpart = .false.
      proc_pars%maxsize = 0
      proc_pars%cursize = 0
      ierr2 = dlwq_init(constants)

      ! read constants

      iposr  = 0
      inovec = 0
      inothr = 0
      lunut  = lun(29)
      ierr2  = 0
      itype  = 2
      call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *              iposr  , npos   , cdummy , nocons , adummy ,
     *                                         itype  , ierr2  )
      if ( ierr2 .gt. 0 ) goto 80

      if ( nocons .lt. 0 ) then

         ! nocons also in external file

         icopt1 = -1
         idum = 0
         write ( lunut , 2150 ) icopt1
C                call with record length 0 => ICOPT1 of -4 not allowed
         call opt1 ( icopt1  , lun     , idum    , lchar   , filtype ,
     &               ldummy  , dtflg3  , 0       , ierr2   , iwar    )
         if ( ierr2 .gt. 0 ) goto 80
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , nocons , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
      endif

      write ( lunut , 2000 ) nocons

      if ( nocons .lt. 0 ) then
         write ( lunut , 2110 )
         ierr = ierr + 1
      elseif ( nocons .gt. 0 ) then

         ! read and write constant names.

         dlwqdata%subject         = SUBJECT_CONSTANT
         dlwqdata%no_param        = nocons
         dlwqdata%no_loc          = 1
         dlwqdata%no_brk          = 1
         dlwqdata%functype        = FUNCTYPE_CONSTANT
         dlwqdata%igrid           = 1
         dlwqdata%extern          = .false.
         dlwqdata%filetype        = FILE_NONE
         dlwqdata%filename        = ' '
         dlwqdata%lun             = 0
         dlwqdata%iorder          = ORDER_PARAM_LOC
         dlwqdata%param_named     = .true.
         dlwqdata%loc_named       = .false.
         dlwqdata%param_pointered = .false.
         dlwqdata%loc_defaults    = .false.
         dlwqdata%loc_pointered   = .false.
         dlwqdata%scaled          = .false.
         dlwqdata%param_scaled    = .false.
         dlwqdata%loc_scaled      = .false.
         allocate(dlwqdata%param_name(nocons))
         allocate(dlwqdata%values(nocons,1,1))
         if ( ioutpt .lt. 2 ) write ( lunut , 2155 )
         do i = 1, nocons
            itype = 1
            call rdtok1 ( lunut , ilun   , lch   , lstack , cchar  ,
     *                    iposr , npos   , chulp , idummy , adummy ,
     *                                             itype  , ierr2  )
            if ( ierr2 .gt. 0 ) goto 80
            if ( chulp .eq. ' ' ) then
               write ( chulp, 2170 ) i
            endif
            dlwqdata%param_name(i) = chulp
         enddo
         if ( ioutpt .ge. 2 ) then
            write ( lunut, 2160 ) (i,dlwqdata%param_name(i),i=1,nocons)
            write ( lunut, *    )
         endif
         write ( lun(2) ) (dlwqdata%param_name(i),i=1,nocons)

         ! special values, this is very tricky to get the value

         ch20 = 'NOVEC'
         call zoek( ch20 , nocons , dlwqdata%param_name, 20 , inovec)
         ch20 = 'NOTHREADS'
         call zoek( ch20 , nocons , dlwqdata%param_name, 20 , inothr)

         ! read constant values.

         call read_opt2 ( 1      , dlwqdata%values , 1      , nocons , nocons ,
     &                    iwidth , 0               , ioutpt , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         if ( inovec .gt. 0 ) then
            novec = nint(dlwqdata%values(inovec,1,1))
            write(lunut,2240)
            write(lunut,2250) novec
         endif
         if ( inothr .gt. 0 ) then
            nothrd = nint(dlwqdata%values(inothr,1,1))
            write(lunut,2310)
            write(lunut,2320) nothrd
         else
            nothrd = 1
         endif

         ! fill the constants structure for use in dlwqp1

         ierr2 = dlwq_init(constants)
         ierr2 = dlwq_resize(constants,nocons)
         constants%no_item = nocons
         constants%name(1:nocons) = dlwqdata%param_name(1:nocons)
         constants%constant(1:nocons) = dlwqdata%values(1:nocons,1,1)

         ! add the constant block to the collection

         idata = dlwqdataCollAdd( proc_pars, dlwqdata )

      endif

      ! read parameters

      itype = 2
      call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *              iposr  , npos   , cdummy , nopa   , adummy ,
     *                                         itype  , ierr2  )
      if ( ierr2 .gt. 0 ) goto 80
      if ( nopa  .gt. 0 ) then
         write ( lunut , 2010 ) nopa
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , ipopt1 , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
      elseif ( nopa  .lt. 0 ) then
         ipopt1 = -1
      else
         write ( lunut , 2010 ) nopa
         goto 40
      endif

      write ( lunut , 2020 ) ipopt1
C                call with record length 0 => IPOPT1 of -4 not allowed
      call opt1 ( ipopt1  , lun     , 33      , lchar   , filtype ,
     &            ldummy  , dtflg3  , 0       , ierr2   , iwar    )
      if ( ierr2 .gt. 0 ) goto 80

      if ( nopa  .lt. 0 ) then
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , nopa   , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         write ( lunut , 2010 ) nopa
         if ( nopa  .eq. 0 ) goto 40
         if ( nopa  .lt. 0 ) then
            ierr = ierr + 1
            write ( lunut , 2120 )
            goto 40
         endif
      endif

      ! read and write parameter names.

      dlwqdata%subject         = SUBJECT_PARAMETER
      dlwqdata%no_param        = nopa
      dlwqdata%no_loc          = noseg
      dlwqdata%no_brk          = 1
      dlwqdata%functype        = FUNCTYPE_CONSTANT
      dlwqdata%igrid           = 1
      dlwqdata%extern          = .false.
      dlwqdata%filetype        = FILE_NONE
      dlwqdata%filename        = ' '
      dlwqdata%lun             = 0
      dlwqdata%iorder          = ORDER_PARAM_LOC
      dlwqdata%param_named     = .true.
      dlwqdata%loc_named       = .false.
      dlwqdata%param_pointered = .false.
      dlwqdata%loc_defaults    = .false.
      dlwqdata%loc_pointered   = .false.
      dlwqdata%scaled          = .false.
      dlwqdata%param_scaled    = .false.
      dlwqdata%loc_scaled      = .false.
      allocate(dlwqdata%param_name(nopa))
      allocate(dlwqdata%values(nopa,noseg,1))
      if ( ioutpt .lt. 2 ) write ( lunut , 2155 )
      do i = 1, nopa
         itype = 1
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , chulp  , idummy , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         if ( chulp .eq. ' ' ) then
            write ( chulp, 2180 ) i
         endif
         dlwqdata%param_name(i) = chulp
      enddo
      if ( ioutpt .ge. 2 ) then
         write ( lunut, 2160 ) (i,dlwqdata%param_name(i),i=1,nopa)
         write ( lunut, *    )
      endif
      write ( lun(2) )      (  dlwqdata%param_name(i),i=1,nopa)

      if ( ipopt1 .eq. 0 ) then

         call dhopnf  ( lun(33) , lchar(33) , 33     , 2     , ioerr )
         if ( ioerr .ne. 0 ) then
            write (lunut,2230) 33,lun(33),lchar(33)
            ierr = ierr + 1
         else
            read (lun(33),end=90,err=100) idummy,((dlwqdata%values(ipa,iseg,1),ipa=1,nopa),iseg=1,noseg)
         endif
         goto 40
      endif

      itype = 2
      call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *              iposr  , npos   , cdummy , ipopt2 , adummy ,
     *                                         itype  , ierr2  )
      if ( ierr2 .gt. 0 ) goto 80
      write ( lunut , 2040 ) ipopt2

      if ( ipopt2 .eq. 1  .or. ipopt2 .eq. 2 ) then
         call opt2 ( ipopt2 , dlwqdata%values , noseg  , nopa   , nopa   ,
     &               iwidth , 0               , ioutpt , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
      else
         write ( lunut , 2030 )
         ierr = ierr+1
      endif

      ! add the parameter block to the collection

   40 continue
      if ( nopa .gt. 0 ) then
         idata = dlwqdataCollAdd( proc_pars, dlwqdata )
      endif

      ! read functions

      itype = 2
      call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *              iposr  , npos   , cdummy , nofun  , adummy ,
     *                                         itype  , ierr2  )

      if ( nofun .eq. 0 ) then           ! there are no functions
         write ( lunut , 2050 ) nofun
         goto 60
      endif
      if ( nofun .gt. 0 ) then           ! there are functions
         write ( lunut , 2050 ) nofun
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , ifopt1 , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
      endif
      if ( nofun .lt. 0 ) then           ! whether there are functions
         ifopt1 = -1                     ! comes from another ascii file
      endif

      ! attention, temporarily use IS = 17

      write ( lunut , 2060 ) ifopt1
C                call with record length NOFUN => IFOPT1 of -4 allowed
      call opt1 ( ifopt1  , lun     , 17      , lchar   , filtype ,
     &            dtflg2  , dtflg3  , nofun   , ierr2   , iwar    )
      if ( ierr2 .gt. 0 ) goto 80
      if ( nofun .lt. 0 ) then           ! now we read then number of
         itype = 2                       ! functions from the other file
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , nofun  , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         write ( lunut , 2050 ) nofun
         if ( nofun .eq. 0 ) goto 60
         if ( nofun .lt. 0 ) then
            ierr = ierr + 1
            write ( lunut , 2130 )
            goto 60
         endif
      endif

      !  read and write function names.

      allocate(funame(nofun))

      if ( ioutpt .lt. 2 ) write ( lunut , 2155 )
      do i = 1, nofun
         itype = 1
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , chulp  , idummy , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         if ( chulp .eq. ' ' ) then
            write ( chulp, 2190 ) i
         endif
         funame(i) = chulp
      enddo
      if ( ioutpt .ge. 2 ) then
         write ( lunut, 2160 ) (i,funame(i),i=1,nofun)
         write ( lunut, *    )
      endif
      write ( lun(2) ) (funame(i),i=1,nofun)
      deallocate(funame)

      if ( ifopt1 .eq. -2 .or. ifopt1 .eq. 0 ) then

         ! external file

         dlwqdata%subject         = SUBJECT_FUNCTION
         dlwqdata%no_param        = nofun
         dlwqdata%no_loc          = 1
         dlwqdata%no_brk          = 0
         if ( ifopt1 .eq. -2 ) then
            dlwqdata%functype     = FUNCTYPE_BLOCK
         else
            dlwqdata%functype     = FUNCTYPE_ALLDT
         endif
         dlwqdata%igrid           = 1
         dlwqdata%extern          = .true.
         dlwqdata%filetype        = FILE_BINARY
         dlwqdata%filename        = lchar(17)
         dlwqdata%lun             = 0
         dlwqdata%iorder          = ORDER_PARAM_LOC
         dlwqdata%param_named     = .false.
         dlwqdata%loc_named       = .false.
         dlwqdata%param_pointered = .false.
         dlwqdata%loc_defaults    = .false.
         dlwqdata%loc_pointered   = .false.
         dlwqdata%scaled          = .false.
         dlwqdata%param_scaled    = .false.
         dlwqdata%loc_scaled      = .false.

         ! add to the collection

         idata = dlwqdataCollAdd( proc_pars, dlwqdata )

      elseif ( ifopt1 .eq. -1 .or. ifopt1 .eq. 1 ) then

         ! read time dependent data old style delwaq input

         subject = SUBJECT_FUNCTION
         call read_opt3 ( lun      , lchar    , nofun    , 0        , 1        ,
     &                    itfact   , dtflg2   , iwidth   , dtflg3   , vrsion   ,
     &                    ioutpt   , subject  , proc_pars, ierr2    )
         if ( ierr2 .gt. 0 ) goto 80

      else

         ! file option not valid

         write ( lunut , 2030 )
         ierr = ierr+1

      endif
   60 continue

      ! read segment functions

      itype = 2
      call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *              iposr  , npos   , cdummy , nosfun , adummy ,
     *                                         itype  , ierr2  )
      if ( ierr2 .gt. 0 ) goto 80

      ! handle option to interchange file option nr and nr of segment functions

      if ( nosfun .gt. 0 ) then

         ! normal course of the program

         write ( lunut , 2070 ) nosfun
         if ( nosfun .eq. 0 ) goto 70
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , isopt1 , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
      elseif ( nosfun .lt. 0 ) then

         ! interchanged file option

         isopt1 = -1
      else

         ! no segment functions

         write ( lunut , 2070 ) nosfun
         goto 70
      endif

      ! handle external file specifier

      write ( lunut , 2080 ) isopt1
      if ( isopt1 .le. -3 ) then
         ifop = isopt1
         if ( isopt1 .eq. -3 ) ifop = isopt1+1 ! -3 becomes -2 but per substance, -4 should remain -4 per substance
         dlwqdata%subject         = SUBJECT_SEGFUNC
         dlwqdata%no_param        = 1
         dlwqdata%no_loc          = noseg
         dlwqdata%no_brk          = 0
         dlwqdata%functype        = FUNCTYPE_BLOCK
         dlwqdata%igrid           = 1
         dlwqdata%extern          = .true.
         dlwqdata%filetype        = FILE_BINARY
         dlwqdata%filename        = lchar(17)
         dlwqdata%lun             = 0
         dlwqdata%iorder          = ORDER_LOC_PARAM
         dlwqdata%param_named     = .false.
         dlwqdata%loc_named       = .false.
         dlwqdata%param_pointered = .true.
         dlwqdata%loc_defaults    = .false.
         dlwqdata%loc_pointered   = .false.
         dlwqdata%scaled          = .false.
         dlwqdata%param_scaled    = .false.
         dlwqdata%loc_scaled      = .false.
         do isfun = 1 , nosfun
C                call with record length NOSEG => IFOP of -4 allowed
            call opt1 ( ifop   , lun    , 17     , lchar  , filtype,
     &                  dtflg2 , dtflg3 , noseg  , ierr2  , iwar   )
            if ( ierr2 .gt. 0 ) goto 80

            ! add to the collection

            allocate(dlwqdata%param_pointers(1))
            dlwqdata%param_pointers  = isfun
            dlwqdata%filename        = lchar(17)
            idata = dlwqdatacolladd( proc_pars, dlwqdata )
            nullify(dlwqdata%param_pointers)

         enddo
      else

         ! call with record length 0 => ISOPT1 of -4 not allowed

         call opt1 ( isopt1  , lun     , 17      , lchar   , filtype ,
     &               dtflg2  , dtflg3  , 0       , ierr2   , iwar    )
         if ( ierr2 .gt. 0 ) goto 80
      endif

      ! option to interchange file option nr and nr of segment functions

      if ( nosfun .lt. 0 ) then
         itype = 2
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , cdummy , nosfun , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         write ( lunut , 2070 ) nosfun
         if ( nosfun .lt. 0 ) then
            ierr = ierr + 1
            write ( lunut , 2140 )
            goto 80
         endif
         if ( nosfun .le. 0 ) goto 70
      endif

      ! read and write segment function names.

      allocate(sfname(nosfun))
      if ( ioutpt .lt. 2 ) write ( lunut , 2155 )
      do i = 1 , nosfun
         itype = 1
         call rdtok1 ( lunut  , ilun   , lch    , lstack , cchar  ,
     *                 iposr  , npos   , chulp  , idummy , adummy ,
     *                                            itype  , ierr2  )
         if ( ierr2 .gt. 0 ) goto 80
         if ( chulp .eq. ' ' ) then
            write ( chulp, 2200 ) i
         endif
         sfname(i) = chulp
         if ( chulp .eq. 'TAU'        ) taupart = .true.
         if ( chulp .eq. 'VERTDISPER' ) vdfpart = .true.
      enddo
      if ( ioutpt .ge. 2 ) then
         write ( lunut, 2160 ) (i,sfname(i),i=1,nosfun)
         write ( lunut, *    )
      endif
      write ( lun(2) ) (sfname(i),i=1,nosfun)

      if ( isopt1 .eq. -3 .or. isopt1 .eq. -4) then

         ! already handled

      elseif ( isopt1 .eq. -2 .or. isopt1 .eq. 0 ) then

         ! external file

         dlwqdata%subject         = SUBJECT_SEGFUNC
         dlwqdata%no_param        = nosfun
         dlwqdata%no_loc          = noseg
         dlwqdata%no_brk          = 0
         if ( isopt1 .eq. -2 ) then
            dlwqdata%functype     = FUNCTYPE_BLOCK
         else
            dlwqdata%functype     = FUNCTYPE_ALLDT
         endif
         dlwqdata%igrid           = 1
         dlwqdata%extern          = .true.
         dlwqdata%filetype        = FILE_BINARY
         dlwqdata%filename        = lchar(17)
         dlwqdata%lun             = 0
         dlwqdata%iorder          = ORDER_LOC_PARAM
         dlwqdata%param_named     = .false.
         dlwqdata%loc_named       = .false.
         dlwqdata%param_pointered = .false.
         dlwqdata%loc_defaults    = .false.
         dlwqdata%loc_pointered   = .false.
         dlwqdata%scaled          = .false.
         dlwqdata%param_scaled    = .false.
         dlwqdata%loc_scaled      = .false.

         ! add to the collection

         idata = dlwqdataCollAdd( proc_pars, dlwqdata )

      elseif ( isopt1 .eq. -1 .or. isopt1 .eq. 1 ) then

         ! read time dependent data old style delwaq input

         subject = SUBJECT_SEGFUNC
         call read_opt3 ( lun      , lchar    , nosfun   , noseg    , 1        ,
     &                    itfact   , dtflg2   , iwidth   , dtflg3   , vrsion   ,
     &                    ioutpt   , subject  , proc_pars, ierr2    )
         if ( ierr2 .gt. 0 ) goto 80

      else

         ! file option not valid

         write ( lunut , 2030 )
         ierr = ierr+1

      endif
   70 continue

      if ( .not. alone ) then              ! Delwaq runs with Delpar
         if ( lsettl .or. layt .gt. 1 ) then
            if ( taupart ) then
               write ( lunut, 2330 )
            else
               write ( lunut, 2340 )
               iwar = iwar + 1
            endif
            if ( layt .gt. 1 ) then
               if ( vdfpart ) then
                  write ( lunut, 2350 )
               else
                  write ( lunut, 2360 )
                  iwar = iwar + 1
               endif
            endif
         endif
      endif

      ! write to output and report files

      call dhopnf  ( lun(16) , lchar(16) , 16    , 1     , ioerr )
      write(lun(16)) ' 5.000PROCES'
      write(lun(16)) proc_pars%cursize
      do i = 1, proc_pars%cursize
         ioerr = dlwqdataWrite( lun(16), proc_pars%dlwqdata(i) )
      enddo
      close ( lun(16) )

      ! clean proc_pars, also in case of errors?

   80 continue

      if ( ierr2 .gt. 0 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      goto 110

      ! error processing

   90 write ( lunut , 2090 ) lun(33),lchar(33)
      ierr = ierr+1
      goto 110

  100 write ( lunut , 2100 ) lun(33),lchar(33)
      ierr = ierr+1

  110 call check  ( cdummy , iwidth , 7      , ierr2  , ierr   )
      if ( timon ) call timstop( ithndl )
      return

      ! output formats

 2000 FORMAT ( //,' Number of constants            :',I4 )
 2010 FORMAT (  /,' Number of parameters           :',I4 )
 2020 FORMAT (    ' First  parameters option       :',I4 )
 2030 FORMAT (  /,' ERROR, option not implemented')
 2040 FORMAT (    ' Second parameters option       :',I4 )
 2050 FORMAT (  /,' Number of functions            :',I4 )
 2060 FORMAT (    ' Option selected for functions  :',I4 )
 2070 FORMAT (  /,' Number of segment-functions    :',I4 )
 2080 FORMAT (    ' First selected option          :',I4 )
 2090 FORMAT (  /,' ERROR, end of file on unit:',I3,
     *          /,' Filename is: ',A20 )
 2100 FORMAT (  /,' ERROR, reading file on unit:',I3,
     *          /,' Filename is: ',A20 )
 2110 FORMAT (    ' ERROR number of constants is negative ' )
 2120 FORMAT (    ' ERROR number of parameters is negative ' )
 2130 FORMAT (    ' ERROR number of functions is negative ' )
 2140 FORMAT (    ' ERROR number of segment-functions is negative ' )
 2150 FORMAT (    ' First  constants option       :',I4 )
 2155 FORMAT (    ' Names are printed for output option 2 and higher !')
 2160 FORMAT (  /,' Item nr:       names:',/
     &           (I6,10X,A20)                )
 2170 FORMAT (    'Constant ',I4 )
 2180 FORMAT (    'Parameter ',I4 )
 2190 FORMAT (    'Function ',I4 )
 2200 FORMAT (    'Segmentfunction ',I4 )
 2230 FORMAT (   ' ERROR opening file number:',I3,' on unit:',I3  ,
     *         /,' Filename is: ',A80 )
 2240 FORMAT (   ' NOVEC Keyword found')
 2250 FORMAT (   ' Number of fast solver vectors set to :',I6  )
 2310 FORMAT ( /,' NOTHREADS Keyword found')
 2320 FORMAT (   ' Number of threads for parallel processing set to :',I6  )
 2330 FORMAT (   ' TAU from DELWAQ will be used for DELPAR'  )
 2340 FORMAT (   ' WARNING: TAU not found. DELPAR will try to get its own TAU or compute it!' )
 2350 FORMAT (   ' VertDisp from DELWAQ will be used for DELPAR'  )
 2360 FORMAT (   ' WARNING: VertDisp not found. DELPAR will try to get its own VertDisp or compute it!' )

      end
