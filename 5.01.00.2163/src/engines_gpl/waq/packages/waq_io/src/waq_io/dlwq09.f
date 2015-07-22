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

      subroutine dlwq09 ( lun    , lchar  , filtype, car    , iar    ,
     &                    icmax  , iimax  , iwidth , ibflag , vrsion ,
     &                    ioutpt , ioutps , outputs, ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>                        Defines variables for output per available output file
!>\par  Description:
!>                          This routine ... to be expanded ...

!     Created           : May    1993 by Jan van Beek
!                         July   2002 by Leo Postma   : Call to Opt1 changed.
!                         August 2012 by Leo Postma   nototp for particle-substances

!     Subroutine called : DEFOUT, set default output behavior
!                         RDODEF, reads output definition block
!                         OPT1  , handles file options
!                         OUTBOO, calculates boot variables output system
!                         DHOPNF, opens files
!                         RDWRK4, read part of DELWAQ system file
!                         GETOPO, sets pointer to the arrays for output
!                         WRIOUT, write OUTPUT system work file
!                         CHECK , checks for the block delimiter

!     Logical units     : -

      use rd_token     !   for the reading of tokens
      use Output
      use timers       !   performance timers

      implicit none

!     kind           function         name                Description

      integer  ( 4), intent(in   ) :: lun   (*)         !< array with unit numbers
      character( *), intent(inout) :: lchar (*)         !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)        !< type of binary file
      integer  ( 4), intent(in   ) :: icmax             !< size of the character workspace
      character(20), intent(inout) :: car   (icmax)     !< character workspace
      integer  ( 4), intent(inout) :: iar   (*)         !< integer workspace ( dump locations at entrance )
      integer  ( 4), intent(in   ) :: iimax             !< size of the integer workspace
      integer  ( 4), intent(in   ) :: iwidth            !< width of the output file
      integer  ( 4), intent(in   ) :: ibflag            !< mass balance option flag
      real     ( 4), intent(in   ) :: vrsion            !< version number of this input
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      integer  ( 4), intent(  out) :: ioutps(7,noutp)   !< output administration array
      type(OutputColl)                Outputs           !< output collection
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count

      INCLUDE 'sysi.inc'     !   COMMON  /  SYSI  /    Timer characteristics
      INCLUDE 'sysn.inc'     !   COMMON  /  SYSN  /    System dimensions

!     In SYSN.INC:

!     NOSEG   INTEGER  1           INPUT   Number of segments
!     NOSYS   INTEGER  1           INPUT   Number of active substances
!     NOTOT   INTEGER  1           INPUT   Number of substances
!     NODISP  INTEGER  1           INPUT   number of dispersion arrays
!     NOVELO  INTEGER  1           INPUT   number of velocity arrays
!     NOQ     INTEGER  1           INPUT   number of exchanges
!     NODUMP  INTEGER  1           INPUT   Number of monitor point
!     NOBND   INTEGER  1           INPUT   Number of boundaries
!     NOWST   INTEGER  1           INPUT   Number of waste loads
!     NOCONS  INTEGER  1           INPUT   Number of constants
!     NOPA    INTEGER  1           INPUT   Number of parameters
!     NOFUN   INTEGER  1           INPUT   Number of functions
!     NOSFUN  INTEGER  1           INPUT   Number of segment functions
!     NX      INTEGER  1           INPUT   Width of grid
!     NY      INTEGER  1           INPUT   Depth of grid
!     NOUTP   INTEGER  1           INPUT   Number of output files
!     NRVART  INTEGER  1           OUTPUT  Total number of output variables
!     NBUFMX  INTEGER  1           OUTPUT  Length of output buffer needed
!     NCBUFM  INTEGER  1           IN/OUT  Length of character buffer
!     NDMPAR  INTEGER  1           INPUT   number of dump areas
!     NTDMPQ  INTEGER  1           INPUT   total number exchanges in dump area
!     NTDMPS  INTEGER  1           INPUT   total number segments in dump area
!     NORAAI  INTEGER  1     1     INPUT   number of raaien
!     NTRAAQ  INTEGER  1     1     INPUT   total number of exch. in raaien
!     NOBTYP  INTEGER  1           INPUT   nr of boundary types
!     NOWTYP  INTEGER  1           INPUT   nr of wasteload types
!     NOGRID  INTEGER  1           INPUT   nr of grids
!     nototp  integer                      total nr of substances including Delpar

!     Local

      integer                       nrvar (noutp) ! Number of extra output vars
      integer                       iostrt(noutp) ! Output start time (scu)
      integer                       iostop(noutp) ! Output stop time (scu)
      integer                       iostep(noutp) ! Output step time (scu)
      integer                       isrtou(noutp) ! Sort output indication
      integer                       igrdou(noutp) ! Output grid indication
      character(40)                 modid (4)     ! Model and run-ID
      character(20), allocatable :: sysid (:)     ! Systems ID
      character(20), allocatable :: coname(:)     ! Constant names
      character(20), allocatable :: paname(:)     ! Parameter names
      character(20), allocatable :: funame(:)     ! Function names
      character(20), allocatable :: sfname(:)     ! Segment function names
      character(20), allocatable :: diname(:)     ! Dispersion array names
      character(20), allocatable :: vename(:)     ! Velocity array names
      integer                       noqtt         ! all exchanges inclusive of the layered bed
      integer                       nosss         ! all computational cells inclusive of layered bed
      integer                       nrvarm        ! maximum number of variables that fits in array size
      integer                       itype         ! return variable for get_token call
      integer                       ierr2         ! local error variable
      integer                       iopt1         ! input file option
      integer                       nmis          ! number of unresolved variables
      integer                       iv, ip        ! help variables to pointer in array
      integer                       i             ! loop variable
      integer                       ivar          ! cumulative variable counter

      LOGICAL       INFILE, LMOUTP, LDOUTP, LHOUTP
      LOGICAL       LDUMMY
      character*255 lchloc            ! Local character variable for file name
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq09", ithndl )
C
C     Some init
C
      NOQTT  = NOQ   + NOQ4
      NOSSS = NOSEG + NSEG2 ! with or without bottom
      LUNUT = LUN(29)
      IF ( IMSTRT .LE. ITSTOP .AND. IMSTRT .LE. IMSTOP .AND.
     +     IMSTEP .GT. 0      ) THEN
         LMOUTP = .TRUE.
      ELSE
         LMOUTP = .FALSE.
      ENDIF
      IF ( IDSTRT .LE. ITSTOP .AND. IDSTRT .LE. IDSTOP .AND.
     +     IDSTEP .GT. 0      ) THEN
         LDOUTP = .TRUE.
      ELSE
         LDOUTP = .FALSE.
      ENDIF
      IF ( IHSTRT .LE. ITSTOP .AND. IHSTRT .LE. IHSTOP .AND.
     +     IHSTEP .GT. 0      .AND.
     +    (INTSRT .LE. 5      .OR.  INTSRT .GE. 10     )    ) THEN
         LHOUTP = .TRUE.
      ELSE
         LHOUTP = .FALSE.
      ENDIF
C
C     Test voor steady state opties
C
      IF ( (INTSRT .GT. 5     .AND.  INTSRT .LT. 10  ) .OR.
     +     INTSRT .EQ. 17    .OR.    INTSRT .EQ. 18  ) THEN
         LMOUTP = .TRUE.
         LDOUTP = .TRUE.
         LHOUTP = .TRUE.
         IMSTRT = ITSTRT
         IDSTRT = ITSTRT
         IHSTRT = ITSTRT
         IMSTOP = ITSTRT + 1
         IDSTOP = ITSTRT + 1
         IHSTOP = ITSTRT + 1
         IMSTEP = 1
         IDSTEP = 1
         IHSTEP = 1
      ENDIF

!     Determine local maximum

      nrvarm = min(iimax,icmax)/noutp
      allocate ( sysid (nototp) , coname(nocons) , paname(nopa  )  )
      allocate ( funame(nofun ) , sfname(nosfun) , diname(nodisp)  )
      allocate ( vename(novelo) )

!     Set default action

      call defout ( noutp   , nrvar   , iostrt  , iostop  , iostep  ,
     &              isrtou  , igrdou  )

!     Handle file option

      ierr2 = gettoken( lchloc, iopt1, itype, ierr2 )          !  < -1 not
      if ( itype .eq. 2 ) then                                 !    -1 external placed on the stack by opt1
         write ( lunut , 2000 ) iopt1                          !     0 not
         infile = .true.                                       !     1 this file, no action
      else       !        "old" file no block 9                !     2 added here for "no file"
         infile= .false.                                       !  >  2 not
         iopt1 = 2
      endif
      if ( iopt1 .lt. -1 .or. iopt1 .eq. 0 .or. iopt1 .gt. 2 ) then
         write ( lunut , 2010 )        !        option out of range
         ierr2 = 1
         goto 100
      endif
      if ( iopt1 .eq. 2 ) then
         infile = .false.
         write ( lunut , 2020 )
      else                             !        Handle option -1 and 1
         call opt1   ( iopt1  , lun    , 18     , lchar  , filtype,
     &                 ldummy , ldummy , 0      , ierr2  , iwar   )
         if ( ierr2 .gt. 0 ) goto 100
      endif

!        Read output definition block

      call rdodef ( noutp   , nrvar   , nrvarm  , isrtou  , car     ,
     &              infile  , nx      , ny      , nodump  , ibflag  ,
     &              lmoutp  , ldoutp  , lhoutp  , ierr    , igrdou  ,
     &              ndmpar  , vrsion  )

!     Calculate OUTPUT boot variables NVART, NBUFMX

      call outboo ( noutp   , nrvar   , igrdou  , isrtou  , nosss   ,
     &              nodump  , nx      , ny      , nrvart  , nbufmx  ,
     &              ndmpar  , notot   , ncbufm  , noraai  )

!     If extra ouptut parameters requested set the pointers

      if ( nrvart .gt. 0 ) then

!        Only if no previous errors , otherwise the reading will fail

         if ( ierr .eq. 0 ) then

!           Read part of delwaq file

            call dhopnf( lun(2)  , lchar(2), 2       , 2       , ierr2   )
            call rdwrk4( lun(2)  , lunut   , modid   , sysid   , notot   ,
     &                   nodump  , nosys   , nobnd   , nowst   , nocons  ,
     &                   nopa    , noseg   , nseg2   , coname  , paname  ,
     &                   funame  , nofun   , sfname  , nosfun  , nodisp  ,
     &                   novelo  , diname  , vename  , iar     , iar     ,
     &                   ndmpar  , ntdmpq  , ntdmps  , noqtt   , noraai  ,
     &                   ntraaq  , nobtyp  , nowtyp  , nogrid  , iar     ,
     &                   iar     , iar     , nototp  )
            close ( lun(2) )

!           Get output pointers

            call getopo( noutp   , nrvar   , nrvarm  , car     , iar     ,
     &                   nmis    , nototp  , sysid   , nocons  , coname  ,
     &                   nopa    , paname  , nofun   , funame  , nosfun  ,
     &                   sfname  , lunut   )

!           If not all vars found, set error

            if ( nmis .gt. 0 ) then
               write(lunut,*) ' Not all variables available.'
               write(lunut,*) ' Number off missing variables :',nmis
            endif
         else
            write ( lunut , 2040 )
            iwar = iwar + 1
         endif
      endif

!     Write OUTPUT intermediate file

      allocate( Outputs%names(nrvart), Outputs%pointers(nrvart) )
      Outputs%cursize = nrvart
      ivar = 0
      do i = 1 , noutp
         ioutps(1,i) = iostrt(i)
         ioutps(2,i) = iostop(i)
         ioutps(3,i) = iostep(i)
         ioutps(4,i) = nrvar (i)
         ioutps(5,i) = isrtou(i)
         ioutps(6,i) = igrdou(i)
         do iv = 1 , nrvar(i)
            ivar = ivar + 1
            ip = (i-1)*nrvarm + iv
            Outputs%pointers(ivar) = iar(ip)
            Outputs%names   (ivar) = car(ip)
         enddo
      enddo

  100 if ( infile ) then
         call check  ( lchloc , iwidth , 9      , ierr2  , ierr   )
      else
         if ( iwidth .eq. 5 ) then
            write ( lunut , 2060 ) 9
         else
            write ( lunut , 2070 ) 9
         endif
      endif
      if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format ( //,' Option selected for output specification :',I4 )
 2010 format (  /,' ERROR, option not implemented')
 2020 format (  /,' Output not specified, using default output parameters')
 2040 format (  /,' WARNING, Not able to locate extra output variables',
     &          /,'          because of errors in input')
 2050 format (    ' ERROR, integer work array overflow in DLWQ09:',I3)
 2060 format (/1X, 59('*'),' B L O C K -',I2,' ',5('*')/)
 2070 format (/1X,109('*'),' B L O C K -',I2,' ',5('*')/)

      end
