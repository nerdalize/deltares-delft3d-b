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

      SUBROUTINE SRWSHL(ITIME, A, J, C)
      use timers
      use delwaq2_data
      implicit none

      real,             dimension(*) :: a
      integer,          dimension(*) :: j
      character(len=*), dimension(*) :: c

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI   /   System characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Dynamical memory allocation
C
      INCLUDE 'fsm-fix.i'

      integer    itime

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "srwshl", ithandl )
C

      CALL SRWINT ( ITIME   , ITSTRT  , ITSTOP  ,
     j              NOSYS   , NOTOT   , NOSEG   ,
     +              NOBND   , NOWST   , NOCONS  , NOPA    , NOFUN   ,
     +              NOSFUN  , NOQ     ,
     J              J(iwast-1),
     J              J(IXPNT-1),
     +              A(ICONS-1),
     J              A(IPARM-1),
     J              A(IBSET-1),
     J              A(IWSTE-1),
     +              A(IFUNC-1),
     J              A(ISFUN-1),
     J              A(ICONC-1),
     J              C(IBNID-1),
     J              C(ISNAM-1),
     J              C(IWSID-1),
     +              C(ICNAM-1),
     J              C(IPNAM-1),
     J              C(IFNAM-1),
     J              C(ISFNA-1),
     J              C(IMNAM-1) )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
      SUBROUTINE SRWINT ( ITIME , ITSTRT, ITSTOP,
     J                    NOSYS , NOTOT , NOSEG ,
     +                    NOBND , NOWST , NOCONS, NOPA  , NOFUN ,
     +                    NOSFUN, NOQ   , IWASTE, IPOINT, CONS  ,
     +                    PARAM , BNDSET, WASTE , FUNC  , SEGFUN,
     +                    CONC  , BNDNAM, SYSNAM, WANAME, CONAME,
     +                    PANAME, FUNAME, SFNAME, MONAME)
      use timers

C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     FUNCTION           : Interface for SRW communications
C
C                          26-9-2001, WRITE OUTPUT FOR LATERALS
C                                     USE BOUNDARY STREAMS ONLY
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     ITIME   INTEGER    1      INPUT  Simulation time (s)
C     ITSTRT  INTEGER    1      INPUT  Simulation start time (s)
C     NOSYS   INTEGER    1      INPUT  Nr. of active substances
C     NOTOT   INTEGER    1      INPUT  Total nr. of substances
C     NOSEG   INTEGER    1      INPUT  Nr of segments
C     NOBND   INTEGER    1      INPUT  Nr of boundaries
C     NOWST   INTEGER    1      INPUT  Nr of waste loads
C     NOCONS  INTEGER    1      INPUT  Nr of constants
C     NOPA    INTEGER    1      INPUT  Nr of parameters (f(x))
C     NOFUN   INTEGER    1      INPUT  Nr of functions (f(t))
C     NOSFUN  INTEGER    1      INPUT  Nr of segment functions (f(x,t))
C     NOQ     INTEGER    1      INPUT  Nr of exchanges
C     IPOINT  INTEGER    ??,*   INPUT  Definition of exchanges
C     CONS    REAL       *      INPUT  Constants
C     PARAM   REAL       *      INPUT  Parameters
C     BNDSET  REAL       *      INPUT  Boundary concentrations
C     WASTE   REAL       *      INPUT  Wasteloads
C     FUNC    REAL       *      INPUT  Functions
C     SEGFUN  REAL       *      INPUT  Segment functions
C     BNDNAM  CHARACTER  *      INPUT  Boundary names
C     SYSNAM  CHARACTER  *      INPUT  Substances names
C     WANAME  CHARACTER  *      INPUT  Wasteload names
C     CONAME  CHARACTER  *      INPUT  Constant names
C     PANAME  CHARACTER  *      INPUT  Parameter names
C     FUNAME  CHARACTER  *      INPUT  Function names
C     SFNAME  CHARACTER  *      INPUT  Segment function names
C     MONAME  CHARACTER  *      INPUT  Model identification strings

C     Delft-IO for SRW
c     use dio_streams
c     use dio_plt_rw
      include 'dio-plt.inc'

C     DELWAQ variables from argument list
C
      INTEGER      ITIME , ITSTRT, NOSYS , NOTOT , NOSEG ,
     +             NOBND , NOWST , NOCONS, NOPA  , NOFUN ,
     +             NOSFUN, NOQ   , ITSTOP
      INTEGER      IPOINT( 4,NOQ ), IWASTE(NOWST)
      REAL         CONS(*)        , PARAM(NOPA,*)   ,
     +             WASTE(NOTOT+1,*), BNDSET(NOSYS,*),
     +             FUNC(*)        , SEGFUN(NOSEG,*) ,
     +             CONC(NOTOT,*)
      CHARACTER*20 BNDNAM(*)      , SYSNAM(*)       ,
     +             WANAME(*)      , CONAME(*)       ,
     +             PANAME(*)      , FUNAME(*)       ,
     +             SFNAME(*)
      CHARACTER*40 MONAME(4)

c     Local variables

      INTEGER      isys  , iseg  , ibnd  , iwst  , icons ,
     +             ifun  , ipa   , isf   , iloc  , ivar  , ioq

      INTEGER      PROCID, Nr_Times
      character*20 Times(1)
      INTEGER      NrLocBoundIn,  NrVarBoundIn
      INTEGER      NrLocBoundOut, NrVarBoundOut
      character*20, allocatable, save : :
     j             LocBoundIn(:), VarBoundIn(:),
     j             LocBoundOut(:), VarBoundOut(:)
      real, allocatable, save : :
     j             Values(:,:)
      integer, allocatable, save : :
     j             dynbnd(:,:,:), dynwst(:,:,:),
     j             bn2seg(:)

      save         Procid, NrLocBoundIn,  NrVarBoundIn,
     j             NrLocBoundOut, NrVarBoundOut

      real         misval
      parameter   (misval = -9999.999)
      character*20 start_date, actual_date

c     Variables related to DIO calls

      integer srwBoundOutSet,    srwBoundInSet
      integer srwBoundInStream, srwBoundOutStream
cjvb  logical srwStatus
      integer srwStatus

      save srwBoundOutSet, srwBoundInSet

      data procid / 0 /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "online", ithandl )

c     WRITE (*,*) ' SRWSHELL itime = ' , itime

c***********************************************************************
C     System initialisation
c***********************************************************************

      IF ( ITSTRT .EQ. ITIME ) THEN

c         Allocate arrays
          allocate (
     j              LocBoundIn(nobnd+nowst), VarBoundIn(nosys),
     j              LocBoundOut(nobnd+nowst), VarBoundOut(nosys),
     j              Values(nosys,nobnd+nowst),
     j              dynbnd(nosys,nobnd,2), dynwst(nosys,nowst,2),
     j              bn2seg(nobnd)
     j             )

c***********************************************************************
c         Inquire which locations and variables will be input
c
c         In the current version ONLY the active state variables
c         at the model boundaries and at the waste loads are potential
c         input

c         Initialise mapping structure to Delwaq
          do ibnd = 1,nobnd
              do isys = 1,nosys
                  dynbnd(isys,ibnd,1) = 0
                  dynbnd(isys,ibnd,2) = 0
              enddo
          enddo
          do iwst = 1,nowst
              do isys = 1,nosys
                  dynwst(isys,iwst,1) = 0
                  dynwst(isys,iwst,2) = 0
              enddo
          enddo

c         Open Data Stream

c         write (*,*) ' Create BoundInStream'
          srwBoundInStream = dioCreateStreamSynched (
     j                       dio_Binary_stream,
     j                       'BoundToDelwaq',
     j                       'r' )
c         write (*,*) ' Done'

c         Open Data Set and Collect Info
c         write (*,*) ' Open BoundInDataSet'
          srwBoundInSet = dioGetPltDatasetInfo (
     j                       srwBoundInStream,
     j                       'IncomingBoundaries',
     j                       NrVarBoundIn, VarBoundIn,
     j                       NrLocBoundIn, LocBoundIn,
     +                       Nr_Times, Times )
c         write (*,*) ' Done'

          if ( srwBoundInSet .gt. 0 ) then
c             SRW run
              procid = 1

              call map_input (
     j                        NrVarBoundIn, VarBoundIn,
     j                        NrLocBoundIn, LocBoundIn,
     j                        nosys , sysnam, nobnd , bndnam, dynbnd )
              call map_input (
     j                        NrVarBoundIn, VarBoundIn,
     j                        NrLocBoundIn, LocBoundIn,
     j                        nosys , sysnam, nowst , waname, dynwst )
          else
c             Not an SRW run
              procid = 0
              goto 9999 !   return
          endif

c***********************************************************************
c         Define which locations and variables will be output
c
c         In the current version ONLY the active state variables
c         at the model boundaries and laterals are output.

          NrVarBoundOut = nosys
          NrLocBoundOut = nobnd+nowst
          do ibnd = 1,nobnd
              LocBoundOut(ibnd) = bndnam(ibnd)
          enddo
          do iwst = 1,nowst
              LocBoundOut(nobnd+iwst) = waname(iwst)
          enddo
          do isys = 1,nosys
              VarBoundOut(isys) = sysnam(isys)
          enddo

c         Open data stream
c         write (*,*) ' Create BoundOutStream'

          srwBoundOutStream = dioCreateStreamSynched (
     j                          dio_Binary_stream,
     j                          'BoundFromDelwaq',
     j                          'w')
c         write (*,*) ' Done'
c         Create data set
c         write (*,*) ' Open BoundOutSet'

c          write (*,*) ' Locations'
c          do iloc=1,NrLocBoundOut
c              write (*,*) iloc,locBoundOut(iloc)
c          enddo
c          write (*,*) ' Variables'
c          do iloc=1,NrVarBoundOut
c              write (*,*) ivar,VarBoundOut(ivar)
c          enddo

          srwBoundOutSet = dioDefinePltDataset (
     j                          srwBoundOutStream,
     j                          'OutGoingBoundaries',
     j                          Dio_Plt_Real,
     j                          NrVarBoundOut,VarBoundOut,
     j                          NrLocBoundOut,LocBoundOut)
c          do iwst = 1,nowst
c              write (*,*) ' Lateral ',iwst,' segment',iwaste(iwst)
c          enddo
c         write (*,*) ' Done'

C         Map boundaries to segments

          do ioq=1,noq
              if ( ipoint(1,ioq).lt.0 .and. ipoint(2,ioq).gt.0 ) then
                  bn2seg(-ipoint(1,ioq)) = ipoint(2,ioq)
              endif
              if ( ipoint(2,ioq).lt.0 .and. ipoint(1,ioq).gt.0 ) then
                  bn2seg(-ipoint(2,ioq)) = ipoint(1,ioq)
              endif
          enddo

c***********************************************************************
c         Retrieve start date
          start_date = moname(4)(1:20)

c     End of initialisation
      ENDIF

c***********************************************************************
C     Actions within DELWAQ's time loop
c***********************************************************************

      if ( procid .le. 0 ) goto 9999  !    return

c***********************************************************************
c     Fill output array

      do ibnd = 1,nobnd
          iseg = bn2seg(ibnd)
          do isys = 1,nosys
              values(isys,ibnd) = conc(isys,iseg)
c              write (*,*) ' val(',isys,',',ibnd,')=',conc(isys,iseg)
          enddo
      enddo
      do iwst = 1,nowst
          iseg = iwaste(iwst)
          do isys = 1,nosys
              values(isys,nobnd+iwst) = conc(isys,iseg)
c              write (*,*) ' val(',isys,',',nobnd+iwst,')=',
c     j                     conc(isys,iseg)
          enddo
      enddo

c***********************************************************************
c     What time is it?
c     actual_date = find_date_from_start (start_date,(itime-itstrt))

c***********************************************************************
c     Send output
c     write (*,*) ' Send output to SRW ...'
      call DioPutPltDataSetReals (srwBoundOutSet,times(1),
     j               NrVarBoundOut, NrLocBoundOut,  values)
c     write (*,*) ' Done'

c***********************************************************************
c     Collect Input (and implicitly get permission to proceed)
C     Jos van Gils, 27-4-2001, NOT FOR LAST TIME STEP!
c***********************************************************************
c     Store input in Delwaq structures

      if ( itime .lt. itstop ) then

c     write (*,*) ' Get Boundaries from SRW ...'
      srwStatus = dioGetPltDataSetReals (srwBoundInSet,times(1),
     j            NrVarBoundIn,NrLocBoundIn,values)
c     write (*,*) ' Done'

      if ( srwStatus .gt. 0 ) then
          do ibnd = 1,nobnd
              do isys = 1,nosys
                  iloc = dynbnd(isys,ibnd,1)
                  ivar = dynbnd(isys,ibnd,2)
                  if (iloc.gt.0.and.ivar.gt.0) then
                      if ( abs(values(ivar,iloc)-misval) .gt.
     j                     abs(0.001*misval) )
     j                bndset(isys,ibnd) = values(ivar,iloc)
                  endif
              enddo
          enddo
          do iwst = 1,nowst
              do isys = 1,nosys
                  iloc = dynwst(isys,iwst,1)
                  ivar = dynwst(isys,iwst,2)
                  if (iloc.gt.0.and.ivar.gt.0) then
                      if ( abs(values(ivar,iloc)-misval) .gt.
     j                     abs(0.001*misval) )
     j                waste(1+isys,iwst) = values(ivar,iloc)
c     ......... gaat dit goed? praten we niet over g/s ipv g/m3????????
                  endif
              enddo
          enddo
      endif

      endif

 9999 if ( timon ) call timstop ( ithandl )
      RETURN
      END

      subroutine map_input (Nvarin, varin, Nlocin, locin,
     j                      Nvar  , var  , nloc  , loc  , map )
      use timers

      integer           Nvarin, Nlocin, Nvar  , nloc
      integer           map(nvar,nloc,2)
      character*(*)     varin(nvarin), locin(nlocin),
     j                  var(nvar), loc(nloc)

      integer           ilocin, ivarin, iloc   , ivar
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "map_input", ithandl )

      do ilocin = 1,nlocin
c         write ( 11 , * ) ' loc ',Locin(ilocin)
          call zoek (Locin(ilocin),nloc,loc,20,iloc)
          do ivarin = 1,nvarin
c             write ( 11 , * ) ' Var ', varin(ivarin)
              call zoek (Varin(ivarin),nvar,var,20,ivar)
              if ( iloc .ge. 1 .and. ivar .ge. 1 ) then
c                 write (*,*) ' BINGO!'
                  map(ivar,iloc,1) = ilocin
                  map(ivar,iloc,2) = ivarin
              endif
          enddo
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      SUBROUTINE RTCSHL(ITIME, A, J, C)
      use timers
      use delwaq2_data
      implicit none

      real,             dimension(*) :: a
      integer,          dimension(*) :: j
      character(len=*), dimension(*) :: c

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI   /   System characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Dynamical memory allocation
C
      INCLUDE 'fsm-fix.i'

      integer    itime
C
      REAL PROLOC
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "rtcshl", ithandl )

      call rtcint (NOCONS,
     j             NOPA  ,
     j             NOFUN ,
     j             NOSFUN,
     j             NOTOT ,
     J             A(ICONC-1),
     J             A(ISFUN-1),
     +             A(IFUNC-1),
     J             A(IPARM-1),
     +             A(ICONS-1),
     j             IDT   ,
     j             ITIME ,
     +             A(IVOL -1),
     j             NOSEG ,
     j             NOSYS ,
     j             NDMPAR,
     J             J(IPDMP-1),
     +             A(IBOUN-1),
     j             NOLOC ,
     j             PROLOC,
     j             NODEF ,
     +             A(IDEFA-1),
     j             NTDMPQ,
     J             C(IDANA-1),
     J             C(ISNAM-1) )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
      subroutine rtcint (NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NDMPAR, IPDMP ,
     +                   BOUND , NOLOC , PROLOC, NODEF , DEFAUL,
     +                   NTDMPQ, DANAM , SYNAME)
      use timers


      INTEGER    NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , IDT   , ITIME , NOSEG , NOSYS ,
     +           NDMPAR, NOLOC , NODEF , NTDMPQ
      INTEGER    IPDMP(*)
      REAL       CONC(NOTOT,*),
     +           SEGFUN(NOSEG,*), FUNC(*)      ,
     +           PARAM(*)       , CONS(*)      ,
     +           VOLUME(*)      , BOUND(*)     ,
     +           PROLOC(*)      , DEFAUL(*)
      CHARACTER*20 DANAM(*), SYNAME(*)

c     Interface to RTC

c     Writes concentration values for dump areas

C     Call to DELWAQ routine from output subsystem
c     NCOUT  is number of output substances (=0 or NOTOT)
c     NRVAR  is number of extra output variables
c     OUTVAL is a buffer of at least (NOTOT+NRVAR)*NDMPAR reals
c     IOPOIN is not used if NRVAR = 0

      real, allocatable, save : : outval(:)
      integer iopoin, nrvar, ncout, io_rtc, isys, idmp
      logical first, rewine
      character*40 moname(4)
      character*255 filnam
      character*255 inifil
      logical       lfound
      integer       idummy, ierr2
      real          rdummy

      save    first
      save    filnam
      data    first   /.true./
      data    rewine  /.true./
      data    io_rtc /1234/
      data    nrvar  /0/
      data    moname /'Interface from Delwaq to RTC',
     j                'Concentrations for current time step',
     j                ' ',
     j                ' '/
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "rtcint", ithandl )

      if ( first ) then
          allocate ( outval(ndmpar*notot) )
          filnam = ' '
          call getcom ( '-i'  , 3    , lfound, idummy, rdummy,
     +                  inifil, ierr2)
          if ( lfound ) then
             if ( ierr2.ne. 0 ) then
                inifil = ' '
             endif
          else
             inifil = 'delwaq.ini'
          endif
          open(io_rtc,file=inifil,status='old',err=123)
          call gkwini(io_rtc,'SimulationOptions',
     j                       'FilenameRTCOutput',filnam)
          close (io_rtc)
  123     continue
          if ( filnam .eq. ' ' ) filnam = 'wq2rtc.his'
      endif

      ncout =  notot
      CALL FIOSUB       (OUTVAL, IOPOIN, NRVAR , NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NDMPAR, IPDMP ,
     +                   BOUND , NOLOC , PROLOC, NODEF , DEFAUL,
     +                   NCOUT , NTDMPQ, paname, sfname)

      if ( first .or. rewine ) then
          CALL DHOPNF ( IO_RTC, FILNAM, 21    , 1     , IDUM  )

          write ( io_rtc ) moname
          write ( io_rtc ) notot, ndmpar
          write ( io_rtc ) (syname(isys),isys=1,notot)
          write ( io_rtc ) (idmp,danam(idmp),idmp=1,ndmpar)
      endif

      write ( io_rtc ) itime
      write ( io_rtc ) (outval(isys),isys=1,notot*ndmpar)

      if ( rewine ) close ( io_rtc )

      first = .false.

      if ( timon ) call timstop ( ithandl )
      return
      end

