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

      subroutine dlwqn8 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Performs iterative steady state computation
!>                         backward in space (8).

C
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : june 1988 by L. Postma
C
C     LOGICAL UNITS      : LUN(19) , output, monitoring file
C                          LUN(20) , output, formatted dump file
C                          LUN(21) , output, unformatted hist. file
C                          LUN(22) , output, unformatted dump file
C                          LUN(23) , output, unformatted dump file
C
C     SUBROUTINES CALLED : DLWQTR, user transport routine
C                          DLWQWQ, user waterquality routine
C                          DLWQPP, user postprocessing routine
C                          DLWQ10, system monitoring routine
C                          DLWQ11, system dump routine
C                          DLWQ13, system postpro-dump routine
C                          DLWQ41, reads second volume
C                          DLWQ65, computes closure error
C                          DLWQ80, convert water quality processes
C                          DLWQ81, add waste loads
C                          DLWQ82, do the transport
C                          DLWQ83, set iteration step and check
C                          DHOPNF, opens files
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     A       REAL       *      LOCAL  real      workspace array
C     J       INTEGER    *      LOCAL  integer   workspace array
C     C       CHARACTER  *      LOCAL  character workspace array
C     LUN     INTEGER    *      INPUT  array with unit numbers
C     LCHAR   CHARACTER  *      INPUT  filenames
C
C     Declaration of arguments
C
      use grids
      use timers
      use delwaq2_data
      use waqmem          ! module with the more recently added arrays

      DIMENSION       A(*)   , J(*)   , LUN(*)
      CHARACTER*(*)   C(*)
      CHARACTER*(*)   LCHAR(*)

      INTEGER                     :: ACTION
      TYPE(DELWAQ_DATA)           :: DLWQD
      type(GridPointerColl)       :: GridPs               ! collection off all grid definitions

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Local declarations
C
      LOGICAL         IMFLAG , IDFLAG , IHFLAG , CONVER
      LOGICAL         LDUMMY , LSTREC , LREWIN

      INTEGER         ITIME
      INTEGER         ITIMEL
      INTEGER         IAFLAG
      INTEGER         IBFLAG
      INTEGER         ISYS
      INTEGER         ICSYS
      INTEGER         NSYS
      INTEGER         INWTYP
      INTEGER         I
      INTEGER         ISTEP
      INTEGER          :: NOSSS
      INTEGER          :: NOQTT

      REAL            ASTOP

      integer       :: ithandl

      INCLUDE 'state_data.inc'


      !
      ! Distinguishing the actions is superfluous:
      ! there is only one step
      !
      IF ( ACTION == ACTION_INITIALISATION .OR.
     &     ACTION == ACTION_FINALISATION        ) THEN
          RETURN
      ENDIF
C
C          some initialisation
C
      ithandl = 0
      if ( timon ) call timstrt ( "dlwqn8", ithandl )

      ITIMEL = ITSTRT
      ITIME  = ITSTRT + IDT
      IHFLAG = .FALSE.
      IAFLAG = 0
      IBFLAG = 0
      IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
      CALL ZERO   ( A(IMAS2), NOTOT*5 )
      LDUMMY = .FALSE.
      LSTREC = .FALSE.
      nosss  = noseg + nseg2
      NOQTT  = NOQ + NOQ4
      inwtyp = intyp + nobnd

!        Determine the volumes and areas that ran dry,
!        They cannot have explicit processes during this time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )
C
C          make closure error correction
C
      IF ( IDT.EQ.0 ) THEN
         CALL ZERO ( A(IVOL2), NOSEG )
      ELSE IF ( J(INRH2+1).GE.0 .AND. IVFLAG.EQ.0 ) THEN
         CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *                 J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *                 J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *                 LDUMMY  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *                 LSTREC  , LREWIN  , A(IVOLL), MYPART  , dlwqd   )
         CALL DLWQ65 ( A(IVOL2), A(IVOL) , IDT     , NOSEG   )
      ELSE
         CALL ZERO   ( A(IVOL2), NOSEG   )
         WRITE ( LUN(19), 1000 )
      ENDIF
C
      ASTOP  = 1.0
      DO 10 I = 1,IMSTOP
      ASTOP  = ASTOP/10.0
   10 CONTINUE
      CALL ZERO ( A(ITIMR) , NOTOT*NOSEG )
C
C======================= iteration loop =============================
C
C          do the user transport processes
C
      DO  20 ISTEP = 1,ITSTOP
      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITSTRT  ,
     *              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), LDUMMY  , ILFLAG  ,
     *              NPARTp  )
C
C          do the user water quality processes
C
      CALL DLWQWQ ( NOTOT   , NOSYS   , NOSEG   , NOPA    , NOSFUN  ,
     *              A(IVOL) , A(ICONC), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), A(IDERV), ITSTRT  , IDT     , A(ISMAS),
     *              IBFLAG  , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), NODUMP  , J(IDUMP))
      CALL DLWQ80 ( A(IDERV), NOTOT   , NOSEG   , ITFACT  , A(ITIMR),
     *              A(IMASS), A(IMAS2), IAFLAG  , A(IDMPS), INTOPT  ,
     *              J(ISDMP))
C
C          add the waste loads
C
      CALL DLWQ81 ( A(IWSTE), J(IWAST), NOWST   , NOTOT   , A(ICONC),
     *              A(IDERV), A(ITIMR), A(IVOL) , A(IMAS2), IAFLAG  ,
     *              A(IDMPS), NDMPS   , INTOPT  , J(ISDMP), NOSYS   )
C
C          do the transport itself
C
      CALL DLWQ82 ( A(IDISP), A(IDIFF), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVELO), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NODISP  ,
     *              NOVELO  , J(IDPNT), J(IVPNT), A(IDERV), A(ITIMR),
     *              A(IVOL) , INTOPT  , A(IMAS2), IAFLAG  , ILFLAG  ,
     *              A(IDMPQ), NDMPQ   , J(IQDMP))
C
C          set an iteration step and check convergence
C
      CALL DLWQ83 ( A(ICONC), A(IMASS), A(IDERV), A(IVOL) , A(IVOL2),
     *              A(ITIMR), NOSYS   , NOTOT   , NOSEG   , ISTEP   ,
     *                                  ASTOP   , CONVER  , LUN(19) )
C
C          simulation done ?
C
      IF ( CONVER .AND. IHFLAG ) GOTO 30
      IF ( CONVER .OR. ISTEP .EQ. ITSTOP-1 ) THEN
           IAFLAG = 1
           IHFLAG = .TRUE.
      ENDIF
   20 CONTINUE
C
C     Call OUTPUT system
C
   30 CONTINUE
      CALL DLWQO2 ( NOTOT   , NOSEG   , NOPA    , NOSFUN  , ITSTRT  ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , 1       , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IDERV), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), NOQ     , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  )
C
C          close files, except monitor file
C
      call CloseHydroFiles( dlwqd%collcoll )
      call close_files( lun )
C
C          write restart file
C
      CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITSTRT, C(IMNAM) ,
     *              C(ISNAM) , NOTOT , NOSEG    )
C
C          user output routine
C
      CALL DLWQPP ( NOTOT   , NOSYS   , NOSEG   , NOPA    , NOSFUN  ,
     *              ITSTRT  , IMFLAG  , IDFLAG  , IHFLAG  , C(IMNAM),
     *              C(ISNAM), C(IDNAM), C(IWSID), J(IDUMP), NODUMP  ,
     *              J(IWAST), NOWST   , A(ICONC), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IVOL ), A(IWSTE), A(IBOUN),
     *              NOBND   , ITSTRT  , ITSTOP  , NX      , NY      ,
     *              J(IGRID), NODISP  , NOVELO  , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , A(IDISP), A(IVELO), A(ISMAS),
     *              IBFLAG  , NOCONS  , NOFUN   , C(ICNAM), C(IPNAM),
     *              C(IFNAM), C(ISFNA), C(IBNID))
C
C          output formats
C
 1000 FORMAT ( 'No closure error corrections !' )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
