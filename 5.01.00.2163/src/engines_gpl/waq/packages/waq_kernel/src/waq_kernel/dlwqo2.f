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

      SUBROUTINE DLWQO2 ( NOTOT , NOSEG , NOPA  , NOSFUN, ITIME ,
     +                    MONAME, SYNAME, DUNAME, IDUMP , NODUMP,
     +                    CONC  , CONS  , PARAM , FUNC  , SEGFUN,
     +                    VOLUME, NOCONS, NOFUN , IDT   , NOUTP ,
     +                    LCHAR , LUN   , IOUTPS, IOPOIN, RIOBUF,
     +                    OUNAM , NX    , NY    , LGRID , CGRID ,
     +                    NOSYS , BOUND , IP    , AMASS , AMASS2,
     +                    ASMASS, NOFLUX, FLXINT, ISFLAG, IAFLAG,
     +                    IBFLAG, IMSTRT, IMSTOP, IMSTEP, IDSTRT,
     +                    IDSTOP, IDSTEP, IHSTRT, IHSTOP, IHSTEP,
     +                    IMFLAG, IDFLAG, IHFLAG, NOLOC , PROLOC,
     +                    NODEF , DEFAUL, ITSTRT, ITSTOP, NDMPAR,
     +                    DANAM , NDMPQ , NDMPS , IQDMP , ISDMP ,
     +                    IPDMP , DMPQ  , DMPS  , FLXDMP, NTDMPQ,
     +                    NAMBUF, NORAAI, NTRAAQ, IORAAI, NQRAAI,
     +                    IQRAAI, TRRAAI, RANAM , STOCHI, NOGRID,
     +                    NOVAR , VARARR, VARIDX, VARTDA, VARDAG,
     +                    ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET,
     +                    GRDNOS, GRDSEG, A     , NOBND , NOBTYP,
     +                    BNDTYP, INWTYP, CONAME, NOQ   , IPOINT,
     +                    INTOPT, PANAME, FUNAME, SFNAME, DMPBAL,
     +                    NOWST , NOWTYP, WSTTYP, IWASTE, INXTYP,
     +                    WSTDMP, iknmrk, OWNERS, MYPART)
C
C
C     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : january 1993 Jan van Beek
C
C
C     FUNCTION            : Driver output system
C
C     LOGICAL UNITS       : -
C
C     SUBROUTINES CALLED  : BALDMP, fills balance for dump area's
C                           FIOUTV, fills output buffer, single cel grids
C                           FIOSUB, fills output buffer, sub-grids
C                           OUTMON, performs a monitor output step
C                           OUTDMP, performs a grid dump output step
C                           OUTHIS, performs a history output step
C                           OUTHNF, performs a history NEFIS step
C                           OUTMAP, performs a map output step
C                           OUTMNF, performs a map NEFIS step
C                           OUTBAL, performs a balance output step
C                           RAATRA, fills transport for raaien
C                           STEPYN, evaluates timers
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     MONAME  CHAR*40       4     INPUT   Model and run names
C     SYNAME  CHAR*20    NOTOT    INPUT   names of substances
C     DUNAME  CHAR*20    NODUMP   INPUT   names of dump locations
C     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
C     NODUMP  INTEGER       1     INPUT   number of dump locations
C     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
C     CONS    REAL          *     IN/OUT  Model constants
C     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
C     FUNC    REAL          *     IN/OUT  Model functions at ITIME
C     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     IDT     INTEGER       1     INPUT   Simulation timestep
C     NOUTP   INTEGER       1     INPUT   Number of output files
C     LCHAR   CHAR*(*)      *     INPUT   File names
C     LUN     INTEGER       *     INPUT   Uint numbers
C     IOUTPS  INTEGER 7*NOUTP    IN/OUT   Output structure
C                                            index 1 = start time
C                                            index 2 = stop time
C                                            index 3 = time step
C                                            index 4 = number of vars
C                                            index 5 = kind of output
C                                            index 6 = grid of output
C                                            index 7 = initialize flag
C     IOPOIN  INTEGER       *     INPUT   Pointer to DELWAQ array's
C     RIOBUF  REAL          *     LOCAL   Output buffer
C     OUNAM   CHAR*20       *     INPUT   name of output variable
C     NX      INTEGER       1     INPUT   Width of output grid
C     NY      INTEGER       1     INPUT   Depth of output grid
C     LGRID   INTEGER     NX*NY   INPUT   grid-layout
C     CGRID   CHAR*20       *     LOCAL   Char buffer for dmp output
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     BOUND   REAL          *     INPUT   Bounary conditions
C     IP      INTEGER       *     IN/OUT  Paging structure
C     AMASS   REAL       NOTOT,*  INPUT   Mass array
C     AMASS2  REAL       NOTOT,*  IN/OUT  Cummulative balance on whole
C     ASMASS  REAL       NOTOT,*  IN/OUT  Cummulative balance per segment
C     NOFLUX  INTEGER       1     INPUT   Number of fluxes
C     FLXINT  REAL  NOFLUX*NDMPAR IN/OUT  Integrated fluxes at dump segments
C     ISFLAG  INTEGER       1     INPUT   if 1 then dd-hh:mm'ss"
C     IAFLAG  INTEGER       1     OUTPUT  if 1 then accumulate mass bal
C     IBFLAG  INTEGER       1     INPUT   Flag = 1 then balances
C     IMSTRT  INTEGER       1     INPUT   Monitoring start time ( scu )
C     IMSTOP  INTEGER       1     INPUT   Monitoring stop time ( scu )
C     IMSTEP  INTEGER       1     INPUT   Monitoring time step ( scu )
C     IDSTRT  INTEGER       1     INPUT   Dump start time ( scu )
C     IDSTOP  INTEGER       1     INPUT   Dump stop time ( scu )
C     IDSTEP  INTEGER       1     INPUT   Dump time step ( scu )
C     IHSTRT  INTEGER       1     INPUT   History start time ( scu )
C     IHSTOP  INTEGER       1     INPUT   History stop time ( scu )
C     IHSTEP  INTEGER       1     INPUT   History time step ( scu )
C     IMFLAG  LOGICAL       1     OUTPUT  If .T. then monitor step
C     IDFLAG  LOGICAL       1     OUTPUT  If .T. then dump step
C     IHFLAG  LOGICAL       1     OUTPUT  If .T. then history step
C     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
C     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
C     NODEF   INTEGER       1     INPUT   Number of used defaults
C     DEFAUL  REAL          *     INPUT   Default proces parameters
C     ITSTRT  INTEGER     1       INPUT   start time
C     ITSTOP  INTEGER     1       INPUT   stop time
C     NDMPAR  INTEGER     1       INPUT   Number of dump areas
C     DANAM   CHAR*20  NDMPAR     INPUT   Dump area names
C     NDMPQ   INTEGER     1       INPUT   Number of dumped exchanges
C     NDMPS   INTEGER     1       INPUT   Number of dumped segments
C     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
C     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     DMPQ    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
C     DMPS    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
C     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
C     NAMBUF  CHAR*20       *     INPUT   Buffer for names
C     NORAAI  INTEGER       1     INPUT   Number of raaien
C     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
C     IORAAI  INTEGER       *     INPUT   Output option for raai
C     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
C     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
C     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai
C     RANAM   CHAR*20       *     INPUT   Raaien names
C     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
C     INTOPT  INTEGER     1       INPUT   Integration and balance suboptions
C     OWNERS  INTEGER   NOSEG     INPUT   ownership of segments
C     MYPART  INTEGER     1       INPUT   number of current part/subdomain
C     ==================================================================
C
      use timers
      use m_couplib
C
      INTEGER       NOTOT , NOSEG , NOPA  , NOSFUN, ITIME ,
     +              NODUMP, NOCONS, NOFUN , IDT   , NOUTP ,
     +              NX    , NY    , NOSYS , NOFLUX, ISFLAG,
     +              IAFLAG, IBFLAG, IMSTRT, IMSTOP, IMSTEP,
     +              IDSTRT, IDSTOP, IDSTEP, IHSTRT, IHSTOP,
     +              IHSTEP, NOLOC , NODEF , ITSTRT, ITSTOP,
     +              NDMPAR, NDMPQ , NDMPS , NTDMPQ, NORAAI,
     +              NTRAAQ, NOGRID, NOVAR , NOBND , NOBTYP,
     +              NOQ   , MYPART
      INTEGER       IDUMP(*)      , LUN(*)        ,
     +              IOUTPS(7,*)   , IOPOIN(*)     ,
     +              LGRID(*)      , IP(*)         ,
     +              IQDMP(*)      , ISDMP(*)      ,
     +              IPDMP(*)      , IORAAI(*)     ,
     +              NQRAAI(*)     , IQRAAI(*)     ,
     +              VARARR(NOVAR) , VARIDX(NOVAR) ,
     +              VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +              ARRKND(*)     , ARRPOI(*)     ,
     +              ARRDM1(*)     , ARRDM2(*)     ,
     +              VGRSET(NOVAR,*),GRDNOS(NOGRID),
     +              GRDSEG(NOSEG,NOGRID)          ,
     +              INWTYP(*)     , IPOINT( 4,NOQ),
     +              OWNERS(NOSEG)
      integer(4), intent(in   ) :: iknmrk(noseg)      ! Feature array. Bit zero set means active.
      REAL          conc ( notot, noseg ),
     &                              CONS(*)       ,
     &              param( nopa , noseg ),
     &                              FUNC(*)       ,
     &              segfun(noseg, nosfun),
     &                              VOLUME(*)     ,
     +              RIOBUF(*)     , BOUND(*)      ,
     +              amass( notot, noseg ),
     &                              AMASS2(NOTOT,5),
     +              ASMASS(*)     , FLXINT(*)     ,
     +              PROLOC(*)     , DEFAUL(*)     ,
     +              DMPQ(*)       , DMPS(*)       ,
     +              FLXDMP(*)     , TRRAAI(NOSYS,*),
     +              STOCHI(NOTOT,NOFLUX), A(*)
      CHARACTER*20  SYNAME(*)     , DUNAME(*)     ,
     +              OUNAM(*)      , CGRID(*)      ,
     +              DANAM(*)      , NAMBUF(*)     ,
     +              RANAM(*)      , BNDTYP(*)     ,
     +              CONAME(*)     , PANAME(*)     ,
     +              FUNAME(*)     , SFNAME(*)
      CHARACTER*40  MONAME(4)
      CHARACTER*(*) LCHAR (*)
      LOGICAL       IMFLAG, IDFLAG, IHFLAG
      integer                    :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
      integer                    :: nowst                 ! number of wasteloads
      integer                    :: nowtyp                ! number of wasteload types
      character(len=20)          :: wsttyp(nowtyp)        ! wasteload types names
      integer                    :: iwaste(nowst)         ! segment numbers of the wasteloads
      integer                    :: inxtyp(nowst)         ! wasteload type number (index in wsttyp)
      real                       :: wstdmp(notot,nowst,2) ! accumulated wasteloads 1/2 in and out
C
C     Local declarations
C
      PARAMETER   ( IMON = 1 , IMO2 = 2 , IDMP = 3 , IDM2 = 4 ,
     +              IHIS = 5 , IHI2 = 6 , IMAP = 7 , IMA2 = 8 ,
     +              IBAL = 9 , IHNF =10 , IHN2 =11 , IMNF =12 ,
     +              IMN2 =13 , IMO3 =14 , IMO4 =15 , IHI3 =16 ,
     +              IHI4 =17 , IHN3 =18 , IHN4 =19 , IBA2 =20 ,
     +              IBA3 =21 )
      PARAMETER   ( IGSEG = 1, IGMON = 2, IGGRD = 3, IGSUB= 4 )
      PARAMETER   ( LUOFF = 18 )
      PARAMETER   ( LUOFF2= 36 )
      INTEGER       K1    , IOSTRT, IOSTOP, IOSTEP, NRVAR ,
     +              ISRTOU, IGRDOU, INIOUT, LUNOUT, IOUT
      CHARACTER*255 LCHOUT
      CHARACTER*20  NAME
      LOGICAL       LOFLAG, LMFIRS, LDFIRS, LHFIRS, LDUMMY
      LOGICAL       LGET  , LREAD
      REAL, ALLOCATABLE :: SURF(:)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqo2", ithandl )
C
C     Evaluate standard DELWAQ output timers
C
      CALL STEPYN (ITIME , IDT   , IMSTRT, IMSTOP, IMSTEP,
     +             IMFLAG, LMFIRS)
      CALL STEPYN (ITIME , IDT   , IDSTRT, IDSTOP, IDSTEP,
     +             IDFLAG, LDFIRS)
      CALL STEPYN (ITIME , IDT   , IHSTRT, IHSTOP, IHSTEP,
     +             IHFLAG, LHFIRS)
C
C     Fill mass in AMASS2 array by summing AMASS over all segments
C
      IF ( IMFLAG ) THEN
         call collect_data(mypart, amass , notot,'noseg',1,ierr)
         call combine_1d_rdata(amass2, notot*5, CP_SUM, ierr)
         IAFLAG = 1
         if (mypart.eq.1) then
            DO 20 I2 = 1,NOTOT
               AMASS2(I2,1) = 0.0
               DO 10 I1 = 1,NOSEG
                  AMASS2(I2,1) = AMASS2(I2,1) + AMASS(I2,I1)
   10          CONTINUE
   20       CONTINUE
         endif
      ENDIF
C
C     Fill mass in ASMASS array using DMPQ and DMPS
C
      IF ( IMFLAG .OR. ( IHFLAG .AND. NORAAI .GT. 0) ) THEN
         IF ( IBFLAG .EQ. 1 ) THEN
            call collect_data(mypart, flxdmp, noflux,'ndmps',1, ierr)
            call collect_data(mypart, dmps  , notot ,'ndmps',3, ierr)
            call collect_data(mypart, dmpq  , nosys ,'ndmpq',2, ierr)
            if (mypart.eq.1) then
               CALL BALDMP (NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +                      NDMPS , NTDMPQ, IQDMP , ISDMP , IPDMP ,
     +                      DMPQ  , AMASS , DMPS  , FLXDMP, ASMASS,
     +                      FLXINT)
            endif
         ENDIF

         IF ( NORAAI .GT. 0 ) THEN
            IF ( LHFIRS ) THEN
               CALL ZERO   (TRRAAI, NORAAI*NOSYS  )
            ELSE
               call collect_data(mypart, dmpq  , nosys , 'ndmps',2,ierr)
               if (mypart.eq.1) then
                  CALL RAATRA (NOSYS , NDMPQ , NORAAI, NTRAAQ, IORAAI,
     +                         NQRAAI, IQRAAI, IQDMP , DMPQ  , TRRAAI)
               endif
            ENDIF
         ENDIF
C
      ENDIF
C
C     Initialize K1, pointer in IOPOIN and OUNAM
C
      lread = .true.
      K1 = 1
C
C     Loop over the output files
C
      DO 200 IOUT = 1 , NOUTP
C
C        Map output structure to single variables part 1
C
         IOSTRT = IOUTPS(1,IOUT)
         IOSTOP = IOUTPS(2,IOUT)
         IOSTEP = IOUTPS(3,IOUT)
         NRVAR  = IOUTPS(4,IOUT)
C
C        Output required ?
C
         CALL STEPYN (ITIME , IDT   , IOSTRT, IOSTOP, IOSTEP,
     +                LOFLAG, LDUMMY)
C
         IF ( .NOT. LOFLAG ) GOTO 100
C
C        Collect data on master-process
C
         if (lread) then
            call collect_data(mypart, conc  , notot ,'noseg',1, ierr)
            call collect_data(mypart, volume, 1     ,'noseg',1, ierr)
            call collect_data(mypart, proloc, noloc ,'noseg',1, ierr)
            if (ibflag.gt.0)
     +         call accumulate_data(flxint, noflux,'ndmpar',1, 'my_dmpar', ierr)
            lread = .false.
         endif

         if (mypart.eq.1) then
C
C        Map output structure to single variables part 2
C
            ISRTOU = IOUTPS(5,IOUT)
            IGRDOU = IOUTPS(6,IOUT)
            INIOUT = IOUTPS(7,IOUT)
            IF ( IOUT .LE. 4 ) THEN
               IFI = IOUT + LUOFF
            ELSEIF ( IOUT .LE. 7 ) THEN
               IFI = IOUT + LUOFF2 - 4
            ELSE
               IFI = IOUT + LUOFF2 - 2
            ENDIF
            LUNOUT = LUN(IFI)
            LCHOUT = LCHAR(IFI)
C
C        No balance output if they are not active
C
            IF ( ( ISRTOU .EQ. IBAL .OR. ISRTOU .EQ. IBA2 .OR.
     +             ISRTOU .EQ. IBA2) .AND. IBFLAG .NE. 1 ) GOTO 100
C
C        Set all local variables used active on base grid
C
            CALL ACTLOC (IOPOIN, NRVAR , NOCONS, NOPA  , NOFUN ,
     +                   NOSFUN, NOTOT , NOSEG , NOLOC , NOGRID,
     +                   NOVAR , VARARR, VARIDX, VARTDA, VARDAG,
     +                   ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET,
     +                   GRDNOS, GRDSEG, A     )
C
C        Fill output buffer
C
            IF ( ISRTOU .EQ. IBA2 ) THEN
C
               CALL FLXBAL (NOTOT , NOFLUX, NDMPAR, NRVAR , STOCHI,
     +                      FLXINT, ASMASS, RIOBUF)
C
            ELSEIF ( ISRTOU .EQ. IBA3 ) THEN
C     jos doet het zelf
            ELSEIF ( IGRDOU .EQ. IGSUB ) THEN
               IF (ISRTOU .EQ. IMO3 .OR.
     +             ISRTOU .EQ. IHI3 .OR.
     +             ISRTOU .EQ. IHN3     ) THEN
                  NCOUT = NOTOT
               ELSE
                  NCOUT = 0
               ENDIF
               NRVAR2 = NRVAR/2
C
C           For the dump area's
C
               CALL FIOSUB (RIOBUF, IOPOIN(K1), NRVAR2, NOCONS, NOPA  ,
     +                      NOFUN , NOSFUN    , NOTOT , CONC  , SEGFUN,
     +                      FUNC  , PARAM     , CONS  , IDT   , ITIME ,
     +                      VOLUME, NOSEG     , NOSYS , NDMPAR, IPDMP ,
     +                      BOUND , NOLOC     , PROLOC, NODEF , DEFAUL,
     +                      NCOUT , NTDMPQ    , paname, sfname)
C
C           For the raaien
C
               IF ((ISRTOU .EQ. IHI3 .OR.
     +              ISRTOU .EQ. IHN3     ) .AND.
     +              NORAAI .GT. 0               ) THEN
                  NRVAR3 = NOTOT + NRVAR2
                  IP1 = (NCOUT+NRVAR2)*NDMPAR + 1
                  CALL FIORAA (RIOBUF(IP1), NRVAR3, TRRAAI, NORAAI, NOSYS)
               ENDIF
C
            ELSE
               NRVAR2 = NRVAR
               CALL FIOUTV ( RIOBUF, IOPOIN(K1), NRVAR , NOCONS, NOPA  ,
     +                       NOFUN , NOSFUN    , NOTOT , CONC  , SEGFUN,
     +                       FUNC  , PARAM     , CONS  , IDT   , ITIME ,
     +                       VOLUME, NOSEG     , NOSYS , NODUMP, IDUMP ,
     +                       NX    , NY        , LGRID , IGRDOU, BOUND ,
     +                       NOLOC , PROLOC    , NODEF , DEFAUL)
            ENDIF
C
C        Fill character buffer with substance names and output names
C
            IF ( ISRTOU .EQ. IMNF .OR.
     +           ISRTOU .EQ. IHNF .OR.
     +           ISRTOU .EQ. IMO3 .OR.
     +           ISRTOU .EQ. IHI3 .OR.
     +           ISRTOU .EQ. IHN3     ) THEN
               DO 30 I = 1 , NOTOT
                  NAMBUF(I) = SYNAME(I)
   30          CONTINUE
               DO 40 I = 1 , NRVAR2
                  NAMBUF(NOTOT+I) = OUNAM(K1+I-1)
   40          CONTINUE
            ENDIF
C
C        Perform output
C
            IF ( ISRTOU .EQ. IMON ) THEN
C
               CALL OUTMON ( LUNOUT   , IDUMP , CONC  , AMASS2, ITIME ,
     +                       DUNAME   , SYNAME, MONAME, NODUMP, NOTOT ,
     +                       IP       , ISFLAG, ASMASS, IBFLAG, NRVAR ,
     +                       OUNAM(K1), RIOBUF, ITSTRT, ITSTOP, NDMPAR,
     +                       DANAM    )
C
            ELSEIF ( ISRTOU .EQ. IMO2 ) THEN
C
               CALL OUTMON ( LUNOUT   , IDUMP , CONC  , AMASS2, ITIME ,
     +                       DUNAME   , SYNAME, MONAME, NODUMP, 0     ,
     +                       IP       , ISFLAG, ASMASS, IBFLAG, NRVAR ,
     +                       OUNAM(K1), RIOBUF, ITSTRT, ITSTOP, NDMPAR,
     +                       DANAM    )
C
            ELSEIF ( ISRTOU .EQ. IMO3 ) THEN
C
               CALL OUTMO3 ( LUNOUT, AMASS2   , ITIME , SYNAME, MONAME,
     +                       NOTOT , IP       , ISFLAG, ASMASS, IBFLAG,
     +                       NRVAR2, OUNAM(K1), RIOBUF, ITSTRT, ITSTOP,
     +                       NDMPAR, DANAM    )
C
            ELSEIF ( ISRTOU .EQ. IMO4 ) THEN
C
               CALL OUTMO3 ( LUNOUT, AMASS2   , ITIME , SYNAME, MONAME,
     +                       0     , IP       , ISFLAG, ASMASS, IBFLAG,
     +                       NRVAR2, OUNAM(K1), RIOBUF, ITSTRT, ITSTOP,
     +                       NDMPAR, DANAM    )
C
            ELSEIF ( ISRTOU .EQ. IDMP ) THEN
C
               CALL OUTDMP (LUNOUT, LCHOUT, ITIME , MONAME, NX       ,
     +                      NY    , LGRID , CGRID , NOTOT , NOSYS    ,
     +                      SYNAME, CONC  , BOUND , NRVAR , OUNAM(K1),
     +                      RIOBUF, IP(5) , ISFLAG, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IDM2 ) THEN
C
               CALL OUTDMP (LUNOUT, LCHOUT, ITIME , MONAME, NX       ,
     +                      NY    , LGRID , CGRID , 0     , 0        ,
     +                      SYNAME, CONC  , BOUND , NRVAR , OUNAM(K1),
     +                      RIOBUF, IP(5) , ISFLAG, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHIS ) THEN
C
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NODUMP,
     +                      IDUMP , DUNAME   , NOTOT , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHNF ) THEN
C
               IOF = NRVAR*NODUMP + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME , MONAME, NOSEG ,
     +                      NOTOT , CONC       , NAMBUF, NRVAR , RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP, NODUMP, IDUMP ,
     +                      DUNAME, RIOBUF(IOF), INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHI2 ) THEN
C
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NODUMP,
     +                      IDUMP , DUNAME   , 0     , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHN2 ) THEN
C
               IOF = NRVAR*NODUMP + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , OUNAM(K1), NRVAR , RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NODUMP, IDUMP ,
     +                      DUNAME, RIOBUF(IOF), INIOUT   )
C
            ELSEIF ( ISRTOU .EQ. IHI3 ) THEN
C
C           Let op RANAM achter DANAM
C
               NRVAR3 = NOTOT + NRVAR2
               NSEGOU = NDMPAR + NORAAI
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NSEGOU,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR3, NAMBUF   , RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHN3 ) THEN
C
C           Let op RANAM achter DANAM
C
               NRVAR3 = NOTOT + NRVAR2
               NSEGOU = NDMPAR + NORAAI
               IOF = NRVAR3*NSEGOU + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , NAMBUF   , NRVAR3, RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NSEGOU, IDUMP ,
     +                      DANAM , RIOBUF(IOF), INIOUT   )
C
            ELSEIF ( ISRTOU .EQ. IHI4 ) THEN
C
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NDMPAR,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR2, OUNAM(K1), RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IHN4 ) THEN
C
               IOF = NRVAR2*NDMPAR + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , OUNAM(K1), NRVAR2, RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NDMPAR, IDUMP ,
     +                      DANAM , RIOBUF(IOF), INIOUT   )
C
            ELSEIF ( ISRTOU .EQ. IMAP ) THEN
C
               CALL OUTMAP (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG ,
     +                      NOTOT    , CONC  , SYNAME, NRVAR , RIOBUF,
     +                      OUNAM(K1), iknmrk, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IMNF ) THEN
C
               IOF = NRVAR*NOSEG + 1
               CALL OUTMNF (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG      ,
     +                      NOTOT    , CONC  , SYNAME, NRVAR , RIOBUF     ,
     +                      OUNAM(K1), IOSTRT, IOSTOP, IOSTEP, RIOBUF(IOF),
     +                      INIOUT   )
C
            ELSEIF ( ISRTOU .EQ. IMA2 ) THEN
C
               CALL OUTMAP (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG ,
     +                      0        , CONC  , SYNAME, NRVAR , RIOBUF,
     +                      OUNAM(K1), iknmrk, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IMN2 ) THEN
C
               IOF = NRVAR*NOSEG + 1
               CALL OUTMNF (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG      ,
     +                      0        , CONC  , SYNAME, NRVAR , RIOBUF     ,
     +                      OUNAM(K1), IOSTRT, IOSTOP, IOSTEP, RIOBUF(IOF),
     +                      INIOUT   )
C
            ELSEIF ( ISRTOU .EQ. IBAL ) THEN
C
               CALL OUTBAL (LUNOUT, LCHOUT, ITIME , MONAME, NOTOT ,
     +                      NOFLUX, SYNAME, NDMPAR, DANAM , ASMASS,
     +                      FLXINT, NRVAR2, RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IBA2 ) THEN
C
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NDMPAR,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
C
            ELSEIF ( ISRTOU .EQ. IBA3 ) THEN
C
               ALLOCATE(SURF(NOSEG))
               NAME = 'SURF'
               LGET = .TRUE.
               CALL VALUES ( NAME   , NOSEG  , SURF   , NOCONS , NOPA   ,
     +                       NOFUN  , NOSFUN , CONS   , CONAME , PARAM  ,
     +                       PANAME , FUNC   , FUNAME , SEGFUN , SFNAME ,
     +                       LGET   , IERR   )

               CALL SOBBAL ( NOTOT , ITIME , NOSYS , NOFLUX   , NDMPAR,
     J                       NDMPQ , NTDMPQ, ITSTOP, IMSTRT   , IMSTOP,
     J                       IQDMP , IPDMP , ASMASS, FLXINT   , STOCHI,
     J                       SYNAME, DANAM , MONAME, DMPQ     , NOBND ,
     J                       NOBTYP, BNDTYP, INWTYP, NOCONS   , CONAME,
     J                       CONS  , NOQ   , IPOINT, OUNAM(K1), INTOPT,
     J                       VOLUME, SURF  , NOSEG , LUNOUT   , LCHOUT,
     J                       INIOUT, DMPBAL, NOWST , NOWTYP   , WSTTYP,
     J                       IWASTE, INXTYP, WSTDMP)
               DEALLOCATE (SURF)
C
            ENDIF
C
            IOUTPS(7,IOUT) = INIOUT
C
         endif !(mypart.eq.1)
C
  100    CONTINUE
C
C        Update K1, pointer in IOPOIN and OUNAM
C
         K1 = K1 + NRVAR
C
  200 CONTINUE

      if ( timon ) call timstop ( ithandl )
      RETURN
      END

