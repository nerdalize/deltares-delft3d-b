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

      SUBROUTINE DLWQT0 ( LUN    , ITIME  , ITIMEL , HARMAT , ARRAY  ,
     *                    IHARM  , NRHARM , NRFTOT , IDT    , VOLUME ,
     *                    DISPER , AREA   , FLOW   , VELO   , ALENG  ,
     *                    WASTES , BOUNDS , CONSTS , PARAM  , FUNCS  ,
     *                    SFUNCS , IPOINT , LUNTXT , LUNTX2 , ftype  ,
     *                    INTSRT , ISFLAG , IFFLAG , IVFLAG , ILFLAG ,
     *                    UPDATE , IKTIM  , IKNMRK , INWSPC , ANWSPC ,
     *                    INWTYP , IWORK  , LSTREC , LREWIN , VOLLST ,
     &                    RDVOLU , GridPs , dlwqd  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april- 8-1988 by L.Postma
C
C     LAST UPDATE: august 1997 by L. Postma,
C                           Merged with DLWQ45 to one routine
C
C     FUNCTION            : Makes values at ITIME for all the time
C                                                   dependent aspects.
C
C     LOGICAL UNITS       : LUN(3), harmonics file
C                           LUN(4), function pointer file
C
C     SUBROUTINES CALLED  : DLWQT1, makes one time function
C                           DLWQTA, make values for const,param,func,sfunc
C                           DLWQTK, make values for kenmerk array
C                           DHOPNF, opens files
      use timers
      use m_couplib
      use delwaq2_data
      use grids
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER       ?     INPUT   array with unit numbers
C     ITIME   INTEGER       1     INPUT   Model timer
C     ITIMEL  INTEGER       1     INPUT   Model timer one step ago
C     HARMAT  REAL          ?     IN/OUT  matrices harmonic components
C     ARRAY   REAL          ?     LOCAL   set of double file buffers
C     IHARM   INTEGER       ?     INPUT   harmonics time space
C     NRHARM  INTEGER       ?     INPUT   set of nrs of harmonic records
C     NRFTOT  INTEGER       ?     INPUT   set of record lengthes
C     IDT     INTEGER       1     OUTPUT  integration time step size
C     VOLUME  REAL       NOSEG    OUTPUT  array of segment volumes
C     DISPER  REAL        NOQ     OUTPUT  array of dispersions
C     AREA    REAL        NOQ     OUTPUT  array of exchange surfaces
C     FLOW    REAL        NOQ     OUTPUT  array of flows
C     VELO    REAL    NOQ*NOVELO  OUTPUT  array of velocities
C     ALENG   REAL       NOQ*2    OUTPUT  array of from and to lengthes
C     WASTES  REAL (NOTOT+1)*NOWST  OUTPUT  array of wasteloads
C     BOUNDS  REAL   NOSYS*NOBND  OUTPUT  array of boundary conditions
C     CONST   REAL      NOCONS    OUTPUT  array of constant values
C     PARAM   REAL   NOPA*NOSEG   OUTPUT  array of parameter values
C     FUNCS   REAL       NOFUN    OUTPUT  array of function values
C     SFUNCS  REAL      NOSFUN    OUTPUT  array of segment functions
C     IPOINT  INTEGER       ?     INPUT   set of pointers to destination
C     LUNTXT  CHAR*(*)      ?     INPUT   text with the unit numbers
C     LUNTX2  CHAR*(*)      ?     INPUT   text with the binary files
      integer, intent(in   ) :: ftype  (*)   !< type of files to be opened
C     INTSRT  INTEGER       1     INPUT   integration option
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C     IVFLAG  INTEGER       1     INPUT   = 1 then computed volumes
C     ILFLAG  INTEGER       1     INPUT   = 0 then constant lengthes
C     UPDATE  LOGICAL       1     OUTPUT  TRUE if update took place
C     IKTIM   INTEGER       *     IN/OUT  Timers in file
C     IKNMRK  INTEGER   NOSEG,*   IN/OUT  Kenmerk array
C     INWSPC  INTEGER       *     IN/OUT  Integer space new time funs
C     ANWSPC  REAL          *     IN/OUT  Real space new time functions
C     INWTYP  INTEGER       *     INPUT   Types of items
C     IWORK   INTEGER       *     LOCAL   Integer workspace
C     LSTREC  LOGICAL       1     INPUT   Switch last record on rewind wanted
C     LREWIN  LOGICAL       1     OUTPUT  Then rewind took place
C     VOLLST  REAL          *     OUTPUT  Last volume record before rewind
C     RDVOLU  LOGICAL       1     INPUT   .TRUE. if volume is expected
      type(delwaq_data), intent(inout) :: dlwqd !< derived type for persistent storage
C
C     DECLARATIONS        :
C
      DIMENSION    LUN   (*) , HARMAT(*) , ARRAY (*) , IHARM (*) ,
     *             NRHARM(*) , NRFTOT(*) , VOLUME(*) , DISPER(*) ,
     *             AREA  (*) , FLOW  (*) , VELO  (*) , ALENG (*) ,
     *             WASTES(*) , BOUNDS(*) , CONSTS(*) , PARAM(*)  ,
     *             FUNCS(*)  , SFUNCS(*) , IPOINT(*) , ADT   (1) ,
     *             IKTIM (*) , IKNMRK(*) , INWSPC(*) , ANWSPC(*) ,
     *             INWTYP(*) , IWORK (*) , VOLLST(*)
      CHARACTER*(*) LUNTXT(*)
      CHARACTER*200 LUNTX2(*)
      LOGICAL      UPDATE    , LSTREC    , LREWIN    , RDVOLU
      type(GridPointerColl), intent(in)    :: GridPs               ! collection off all grid definitions
C
C     COMMON  /  SYSN   /   System dimensions
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYST   /   System time function flags
C
      INCLUDE 'syst.inc'
C
C     Local declarations
C
      REAL         RDUMMY(1) , ADUMMY(1)
      LOGICAL      LSTDUM    , LREDUM
      LOGICAL      LDUM(3)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt0", ithandl )
C
C         open the harmonics and pointer files
C
      IF ( IFFLAG .EQ. 1 ) THEN
         BNDSET = .FALSE.
         WSTSET = .FALSE.
         FUNSET = .FALSE.
         OTHSET = .FALSE.
         IF (MYPART .EQ. 1) THEN
            CALL DHOPNF ( LUN(3), LUNTXT(3), 3    , 2     , IERR  )
            CALL DHOPNF ( LUN(4), LUNTXT(4), 4    , 2     , IERR  )
         ENDIF
      ENDIF
C
C         initialisation
C
      IPH  = 1
      IPF  = 1
      IPA  = 1
      IPI  = 1
      IPNI = 1
      IPNA = 1
      IT   = 1
      ISNUL  = 0
      ISNUL2 = 0
      IDUMMY = 0
      LSTDUM = .FALSE.
      UPDATE = .FALSE.
C
C         integration step size IDT
C
      IF ( NRFTOT( 1) .GT. 0 ) THEN
         IF (MYPART.EQ.1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),ADT    ,1         ,NRHARM( 1) ,
     *           1         ,NRFTOT( 1) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,5      ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     ,LSTDUM     ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd    )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. ) CALL DISTRIBUTE_DATA(MYPART, ADT, 1, IERR)
         IF ( OTHSET ) THEN
            IS = 5
            GOTO 10
         ENDIF
         IDT = ADT(1) +   0.5
      ENDIF
C
C         volumes
C
C     if read anyway or ( read-requested and there is something to read )
      IF ( NRHARM( 2) .GE. 0 ) THEN
         IF   ( RDVOLU ) THEN
C           if .not. computed volumes .or. this is the first time
            IF ( IVFLAG     .EQ. 0 .OR. IFFLAG .EQ. 1 ) then
               IF (MYPART .EQ. 1) THEN
                  CALL DLWQT1 (
     *                 LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *                 ARRAY(IPA),IPOINT(IPI),VOLUME ,1         ,NRHARM( 2) ,
     *                 NOSEG     ,NRFTOT( 2) ,IPA    ,IPH       ,IPF        ,
     *                 IPI       ,LUNTXT     ,7      ,ISFLAG    ,IFFLAG     ,
     *                 UPDATE    ,OTHSET     ,0      ,IWORK     ,LSTREC     ,
     *                 LREWIN    ,VOLLST     ,ftype  ,dlwqd     )
                  LDUM(1) = UPDATE
                  LDUM(2) = OTHSET
                  LDUM(3) = LREWIN
               ENDIF

               CALL DISTRIBUTE_DATA(MYPART, LDUM, 3, IERR)
               UPDATE = LDUM(1)
               OTHSET = LDUM(2)
               LREWIN = LDUM(3)

               IF ( UPDATE .OR. .TRUE. )
     *             CALL DISTRIBUTE_DATA(MYPART, VOLUME, 'noseg','distrib_itf', IERR)
            ENDIF
         ELSE
            IPA = IPA + NRFTOT(2)*2
            IPI = IPI + NOSEG + 3
         ENDIF
         IF ( OTHSET ) THEN
            IS = 7
            GOTO 10
         ENDIF
      ENDIF
C
C         dispersions
C
      IF ( NRHARM( 3) .GE. 0 ) THEN
         IF (MYPART.EQ.1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),DISPER ,NODISP    ,NRHARM( 3) ,
     *           NOQ       ,NRFTOT( 3) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,9      ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     ,LSTDUM     ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, DISPER, NODISP,'noq',1,
     *                           'distrib_itf', IERR)
         IF ( OTHSET ) THEN
            IS = 9
            GOTO 10
         ENDIF
      ENDIF
C
C         area
C
      IF ( NRHARM( 4) .GE. 0 ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),AREA   ,1         ,NRHARM( 4) ,
     *           NOQ       ,NRFTOT( 4) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,10     ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     , LSTDUM    ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, AREA, 'noq', 'distrib_itf', IERR)
         IF ( OTHSET ) THEN
            IS = 10
            GOTO 10
         ENDIF
      ENDIF
C
C         flow
C
      IF ( NRHARM( 5) .GE. 0 ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF), HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),FLOW   ,1         ,NRHARM( 5)  ,
     *           NOQ       ,NRFTOT( 5) ,IPA    ,IPH       ,IPF         ,
     *           IPI       ,LUNTXT     ,11     ,ISFLAG    ,IFFLAG      ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     , LSTDUM     ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, FLOW, 'noq', 'distrib_itf', IERR)
         IF ( OTHSET ) THEN
            IS = 11
            GOTO 10
         ENDIF
      ENDIF
C
C         velocities
C
      IF ( NRHARM( 6) .GE. 0 ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),VELO   ,NOVELO    ,NRHARM( 6) ,
     *           NOQ       ,NRFTOT( 6) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,12     ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     ,LSTDUM     ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART,VELO,NOVELO,'noq',1, 'distrib_itf',IERR)
         IF ( OTHSET ) THEN
            IS = 12
            GOTO 10
         ENDIF
      ENDIF
C
C         'from'- and 'to'-length
C
      IF ( NRHARM( 7) .GE. 0 .AND. ILFLAG .EQ. 1 ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),ALENG  ,2         ,NRHARM( 7) ,
     *           NOQ       ,NRFTOT( 7) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,13     ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,OTHSET     ,0      ,IWORK     , LSTDUM    ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)
         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, ALENG, 2,'noq',1, 'distrib_itf', IERR)
         IF ( OTHSET ) THEN
            IS = 13
            GOTO 10
         ENDIF
      ENDIF
C
C         boundaries
C
      IF ( INTSRT .EQ. 6 .OR. INTSRT .EQ. 7 ) THEN
         NOSUBS = NOTOT
      ELSE
         NOSUBS = NOSYS
      ENDIF
      IF ( NRHARM( 8) .GE. 0 .AND. .NOT. BNDSET ) then
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),BOUNDS ,NOSUBS    ,NRHARM( 8) ,
     *           NOBND     ,NRFTOT( 8) ,IPA    ,IPH       ,IPF        ,
     *           IPI       , LUNTXT    ,14     ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,BNDSET     ,0      ,IWORK     , LSTDUM    ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
            LDUM(3) = BNDSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 3, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)
         BNDSET = LDUM(3)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, BOUNDS, NOSUBS*NOBND, IERR)
      ENDIF

      IF ( BNDSET ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN    , ITIME     ,ITIMEL ,INWSPC(IPNI),ANWSPC(IPNA),
     *           ADUMMY , INWTYP(IT),BOUNDS ,NOSUBS      ,ISNUL2      ,
     *           NOBND  , ISNUL     ,IPNI   ,IPNA        ,IDUMMY      ,
     *           IBNDMX , LUNTXT    ,14     ,ISFLAG      ,IFFLAG      ,
     *           UPDATE , BNDSET    ,0      ,IWORK       ,LSTDUM      ,
     *           LREDUM , RDUMMY    ,ftype  ,dlwqd       )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, BOUNDS, NOSUBS*NOBND, IERR)

         IT     = IT + NOBND
c        ILP1 = 0
c        WRITE ( * , * ) ITIME
c        DO 2 ILPP = 1,NOBND
c           WRITE ( * , * ) ILPP
c           WRITE ( * , '(10F12.2)' ) (BOUNDS(ILP1+K),K=1,NOSYS)
c           ILP1 = ILP1 + NOSYS
c   2    CONTINUE
      ENDIF
C
C         wastes
C
      IF ( NRHARM( 9) .GE. 0 .AND. .NOT. WSTSET ) then
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME      ,ITIMEL ,IHARM(IPF),HARMAT(IPH),
     *           ARRAY(IPA),IPOINT(IPI),WASTES ,NOTOT+1   ,NRHARM( 9) ,
     *           NOWST     ,NRFTOT( 9) ,IPA    ,IPH       ,IPF        ,
     *           IPI       ,LUNTXT     ,15     ,ISFLAG    ,IFFLAG     ,
     *           UPDATE    ,WSTSET     ,1      ,IWORK     , LSTDUM    ,
     *           LREDUM    ,RDUMMY     ,ftype  ,dlwqd     )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
            LDUM(3) = WSTSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 3, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)
         WSTSET = LDUM(3)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, WASTES, (NOTOT+1)*NOWST, IERR)
      ENDIF
      ISNUL = 0
      ISNUL2= 0
      IF ( WSTSET ) THEN
         IF (MYPART .EQ. 1) THEN
            CALL DLWQT1 (
     *           LUN       ,ITIME     ,ITIMEL ,INWSPC(IPNI),ANWSPC(IPNA),
     *           ADUMMY    ,INWTYP(IT),WASTES ,NOTOT+1     ,ISNUL2      ,
     *           NOWST     ,ISNUL     ,IPNI   ,IPNA        ,IDUMMY      ,
     *           IWSTMX    ,LUNTXT    ,15     ,ISFLAG      ,IFFLAG      ,
     *           UPDATE    ,WSTSET    ,1      ,IWORK       ,LSTDUM      ,
     *           LREDUM    ,RDUMMY    ,ftype  ,dlwqd       )
            LDUM(1) = UPDATE
            LDUM(2) = OTHSET
         ENDIF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         OTHSET = LDUM(2)

         IF ( UPDATE .OR. .TRUE. )
     *      CALL DISTRIBUTE_DATA(MYPART, WASTES, (NOTOT+1)*NOWST, IERR)

         IT     = IT + NOWST
c        ILP1 = 0
c        WRITE ( * , * ) ITIME
c        DO 1 ILPP = 1,NOWST
c           WRITE ( * , * ) ILPP
c           WRITE ( * , '(20F6.2)' ) (WASTES(ILP1+K),K=1,NOTOT+1)
c           ILP1 = ILP1 + NOTOT + 1
c   1    CONTINUE
      ENDIF
C
C         functions
C
      NOSSS = NOSEG + NSEG2
      IF ( NRHARM(10) .GE. 0 ) then
         IF (MYPART .EQ. 1) THEN
            call dlwqta ( lun(16), luntxt(16), lun(19), nosss  , nocons ,
     +                    nopa   , nofun     , nosfun , consts , param  ,
     +                    funcs  , sfuncs    , isflag , ifflag , itime  ,
     +                    GridPs , dlwqd     , ierr   )
         ENDIF
         CALL DISTRIBUTE_DATA(MYPART, IFFLAG, 1, IERR)
         IF (IFFLAG .EQ. 1) THEN
            CALL DISTRIBUTE_DATA(MYPART, CONSTS, NOCONS, IERR)
            CALL DISTRIBUTE_DATA(MYPART, PARAM , NOPA,'noseg',1,
     *                           'distrib_itf' , IERR)
         ENDIF
         CALL DISTRIBUTE_DATA(MYPART, FUNCS , NOFUN      , IERR)
         CALL DISTRIBUTE_DATA(MYPART, SFUNCS, NOSSS*NOSFUN , IERR)
      ENDIF

      CALL DISTRIBUTE_DATA(MYPART, NRHARM, 10, IERR)
C
C     kenmerk array
C
      CALL DLWQTK ( LUN    , ITIME  , IKTIM  , IKNMRK , NOSSS  ,
     +              40     , LUNTXT , ISFLAG , IFFLAG , IFIOPK )
C
C         close the harmonics and pointer files
C
   10 IF ( IFFLAG .EQ. 1 ) THEN
         IF (MYPART .EQ. 1 ) THEN
            CLOSE ( LUN( 3) )
            CLOSE ( LUN( 4) )
         endif
         IF ( OTHSET ) THEN
            WRITE ( LUN(19) , * ) ' ERROR, new time series processing',
     *           ' wanted for an unsupported item: ',LUNTXT(IS)
            CALL SRSTOP(1)
         ENDIF
      ENDIF
C
      ITIMEL =  ITIME
      if ( timon ) call timstop ( ithandl )
      RETURN
      END


C     HARMAT  REAL          ?     IN/OUT  matrices harmonic components

c dimensies achterhalen
C     VOLLST  REAL          *     OUTPUT  Last volume record before rewind
cajv nog uizoeken wat hiermee moet gebeuren, evt ook aanpassen in dlwq41

c call naar dlwqtk
C     IKTIM   INTEGER       *     IN/OUT  Timers in file
C     IKNMRK  INTEGER   NOSEG,*   IN/OUT  Kenmerk array


c in dlwqqta
C     INWSPC  INTEGER       *     IN/OUT  Integer space new time funs
C     ANWSPC  REAL          *     IN/OUT  Real space new time functions
