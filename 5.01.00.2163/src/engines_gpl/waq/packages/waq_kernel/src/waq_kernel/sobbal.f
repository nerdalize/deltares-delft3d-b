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

      SUBROUTINE SOBBAL ( NOTOT , ITIME , NOSYS , NOFLUX, NDMPAR,
     J                    NDMPQ , NTDMPQ, ITSTOP, IMSTRT, IMSTOP,
     J                    IQDMP , IPDMP , ASMASS, FLXINT, STOCHI,
     J                    SYNAME, DANAM , MONAME, DMPQ  , NOBND ,
     J                    NOBTYP, BNDTYP, INBTYP, NOCONS, CONAME,
     J                    CONS  , NOQ   , IPOINT, FLXNAM, INTOPT,
     J                    VOLUME, SURF  , NOSEG , LUNOUT, LCHOUT,
     J                    INIOUT, DMPBAL, NOWST , NOWTYP, WSTTYP,
     J                    IWASTE, INWTYP, WSTDMP)
C
C     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : PROTOTYPE dec. 2001 by Jos van Gils
C
C
C     FUNCTION            : Integrated emissions and processes balance
C
C     LOGICAL UNITS       : -
C
C     SUBROUTINES CALLED  :
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     ITSTOP  INTEGER       1     INPUT   Stop time of the simulation
C     IMSTRT  INTEGER       1     INPUT   Start time of the output
C     IMSTOP  INTEGER       1     INPUT   Stop time of the output
C     MONAME  CHAR*40       4     INPUT   Model and run names
C     SYNAME  CHAR*20    NOTOT    INPUT   names of substances
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     ASMASS  REAL       NOTOT,*  IN/OUT  Cummulative balance per dump area
c                                         1   = mass
c                                         2   = processes
c                                         3/4 = loads in/out
c                                         5/6 = transport in/out
C     NOFLUX  INTEGER       1     INPUT   Number of fluxes
C     FLXINT  REAL          *     IN/OUT  Integrated fluxes in dump areas
C     NDMPAR  INTEGER     1       INPUT   Number of dump areas
C     NTDMPQ  INTEGER     1       INPUT
C     DANAM   CHAR*20  NDMPAR     INPUT   Dump area names
C     NDMPQ   INTEGER     1       INPUT   Number of dumped exchanges
C     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
C     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
C     NOBND   INTEGER     1       INPUT   number of boundaries
C     NOBTYP  INTEGER     1       INPUT   number of boundaries types
C     BNDTYP  CHAR*20  NOBTYP     INPUT   boundary types names
C     INBTYP  INTEGER  NOBND      INPUT   boundary type number (index in BNDTYP)
C     NOCONS  INTEGER     1       INPUT   number of constants
C     CONAME  CHAR*20  NOCONS     INPUT   constants names
C     CONS    REAL     NOCONS     INPUT   constants array
C     NOQ     INTEGER     1       INPUT   number of exchanges
C     IPOINT  INTEGER   4,NOQ     INPUT   pointer array
C     FLXNAM  INTEGER  NOFLUX     INPUT   flux names
C     INTOPT  INTEGER     1       INPUT   Integration and balance suboptions
C     VOLUME  REAL     NOSEG      INPUT   Volume
C     SURF    REAL     NOSEG      INPUT   horizontal surface area
C     DMPBAL  INTEGER  NDMPAR     INPUT   if dump area is included in balance
C     NOWST   INTEGER     1       INPUT   number of wasteloads
C     NOWTYP  INTEGER     1       INPUT   number of wasteload types
C     WSTTYP  CHAR*20  NOWTYP     INPUT   wasteload types names
C     IWASTE  INTEGER  NOWST      INPUT   segment number wasteloads
C     INWTYP  INTEGER  NOWST      INPUT   wasteload type number (index in WSTTYP)
C     WSTDMP  REAL     NOTOT,NOWST,2  I   accumulated wasteloads 1/2 in and out
C     ==================================================================
C
      use timers
      INTEGER       NOTOT , ITIME , NOSYS ,
     j              NOFLUX, NDMPAR, NDMPQ , NTDMPQ,
     j              NOBND , ITSTOP, IMSTOP, IMSTRT,
     J              NOBTYP, NOCONS, NOQ   , INIOUT
      INTEGER       IQDMP(*)      , IPDMP(*)  ,
     +              INBTYP(NOBND) , IPOINT( 4,NOQ )
      REAL          DMPQ(NOSYS,NDMPQ,*),
     +              ASMASS(NOTOT,NDMPAR,*), FLXINT(NOFLUX,*),
     +              STOCHI(NOTOT,NOFLUX)  , CONS(NOCONS)     ,
     +              VOLUME(*)             , SURF(*)
      CHARACTER*20  SYNAME(*)     , DANAM(*),
     +              BNDTYP(NOBTYP), CONAME(NOCONS),
     +              FLXNAM(NOFLUX)
      CHARACTER*40  MONAME(4)
      CHARACTER*255 LCHOUT
      integer                    :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
      integer                    :: nowst                 ! number of wasteloads
      integer                    :: nowtyp                ! number of wasteload types
      character(len=20)          :: wsttyp(nowtyp)        ! wasteload types names
      integer                    :: iwaste(nowst)         ! segment numbers of the wasteloads
      integer                    :: inwtyp(nowst)         ! wasteload type number (index in wsttyp)
      real                       :: wstdmp(notot,nowst,2) ! accumulated wasteloads 1/2 in and out

C     Local declarations
C
C     NAME    KIND     LENGTH     DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSUM   INTEGER     1       Nr of sum parameters
C     SFACTO  REAL   NOSUM,NOTOT  Factor for substance in sum parameters
C     STOCHL  REAL   NOSUM,NOFLUX Local STOCHI for sum parameters
C     NOOUT   INTEGER     1       Nr of balance terms per dump segment
C     IMASSA  INTEGER NOTOT+NOSUM Pointer to accumulation term in balance
C     IEMISS  INTEGER NOTOT+NOSUM Pointer to boundary terms in balance
C     NEMISS  INTEGER     1       Nr of boundary terms in balance
C     ITRANS  INTEGER NOTOT+NOSUM Pointer to int. transport terms in balance
C     IPROCS  INTEGER NOTOT+NOSUM Pointer to processes term(s) in balance
C     NPROCS  INTEGER NOTOT+NOSUM Nr of processes terms in balance
C     BALANS  REAL   NOOUT,*      Mass balances for current time
C     BALTOT  REAL   NOOUT,*      Integrated mass balances
C     OUNAME  C*20      NOOUT     Names of terms in balances
C     FL2BAL  INT   NOTOT+NOSUM,* Pointer to relevant fluxes per substance
C     DANAMP  C*20     NDMPAR+1   Copy of DANAM including sum segment
C     SYNAMP  C*20  NOTOT+NOSUM   Copy of SYNAME including sum parameters

      INTEGER       IOSOBH, IOBALI, ISYS  , IBOUN , NEMISS, IFRAC ,
     J              IFLUX , IDUMP , IPQ   , ISYS2 , ITEL  , IINIT ,
     J              ITEL1 , IP1   , ITEL2 , NQC   , IQC   , IQ    ,
     J              IPOIN , NOOUT , IERR  , IOUT  , ISUM  , NOSUM ,
     J              NSC   , ISC
      PARAMETER    (NOSUM=2)
      real   ,      allocatable : : SFACTO(:,:),
     J                              STOCHL(:,:),
     J                              FLTRAN(:,:),
     J                              BALANS(:,:),
     J                              BALTOT(:,:),
     J                              DMP_SURF(:),
     J                              DMP_VOLU(:)
      integer,      allocatable : : JDUMP(:),
     J                              FL2BAL(:,:),
     J                              IMASSA(:),
     J                              IEMISS(:),
     J                              ITRANS(:),
     J                              IPROCS(:),
     J                              NPROCS(:),
     J                              SEGDMP(:),
     J                              IWDMP(:)
      character*20, allocatable : : OUNAME(:),
     J                              DANAMP(:),
     J                              SYNAMP(:)

      LOGICAL       LUMPEM, LUMPPR, IFIRST, SUPPFT, ONLYSM,
     J              INCLUD, BOUNDA, LUMPTR, B_AREA, B_VOLU
      REAL          RDUM(1)
      REAL          ST    , TFACTO(NOSUM)
      CHARACTER*20  C20   , SYNAMS(NOSUM)
      CHARACTER*40  CDUM
      CHARACTER*255 FILNAM
      character*2   c2
      character*255 inifil
      logical       lfound
      integer       idummy, ierr2
      real          rdummy
      DATA          IOBALI /   85 /
      DATA          LUMPEM /.true./
      DATA          LUMPPR /.true./
      DATA          SUPPFT /.true./
      DATA          ONLYSM /.TRUE./
      DATA          LUMPTR /.FALSE./
      DATA          B_AREA /.FALSE./
      DATA          B_VOLU /.FALSE./
      DATA          SYNAMS /'TotN','TotP'/
c     SAVE          IOBALI, BALTOT, SFACTO, SUPPFT, ONLYSM, STOCHL,
c    J              OUNAME, DANAMP, SYNAMP, JDUMP , LUMPEM, LUMPPR,
c    J              BALANS, FLTRAN, IMASSA, IEMISS, ITRANS, IPROCS,
c    J              NPROCS, FL2BAL, LUMPTR, B_AREA, B_VOLU, SEGDMP
      SAVE
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "sobbal", ithandl )

c**************** INITIALIZATION **************************************

      IF ( INIOUT .EQ. 1 ) THEN
          IFIRST = .TRUE.

c         Process flags

c         from input

          lumppr = .NOT. btest(intopt,8)
          lumpem = .NOT. btest(intopt,9)
          lumptr = .NOT. btest(intopt,10)
          b_area = btest(intopt,11)
          b_volu = btest(intopt,12)
          onlysm = .NOT. btest(intopt,13)
          suppft = .NOT. btest(intopt,14)

c         from ini file

          call getcom ( '-i'  , 3    , lfound, idummy, rdummy,
     +                  inifil, ierr2)
          if ( lfound ) then
             if ( ierr2.ne. 0 ) then
                inifil = ' '
             endif
          else
             inifil = 'delwaq.ini'
          endif
          open ( 1801 , file=inifil , status='old' , err=123 )
          call gkwini ( 1801 ,'Balance Options','LumpProcessesContributions' , c2 )
          if ( c2 .eq. '-1' ) lumppr = .true.
          if ( c2 .eq. '0' ) lumppr = .false.
          call gkwini ( 1801 ,'Balance Options','LumpBoundaryContributions' , c2 )
          if ( c2 .eq. '-1' ) lumpem = .true.
          if ( c2 .eq. '0' ) lumpem = .false.
          call gkwini ( 1801 ,'Balance Options','SumOfMonitoringAreas' , c2 )
          if ( c2 .eq. '-1' ) onlysm = .true.
          if ( c2 .eq. '0' ) onlysm = .false.
          call gkwini ( 1801 ,'Balance Options','SuppressTimeDependentOutput' , c2 )
          if ( c2 .eq. '-1' ) suppft = .true.
          if ( c2 .eq. '0' ) suppft = .false.
          close ( 1801 )
  123     continue

          ! count number of output dump areas

          ndmpar_out = 0
          do idump = 1, ndmpar
             if ( dmpbal(idump) .eq. 1 ) then
                ndmpar_out = ndmpar_out + 1
             endif
          enddo

c         Dimension arrays

          if ( allocated(fltran) ) then
              deallocate(
     j               FLTRAN,
     j               JDUMP,
     j               SFACTO,
     j               DANAMP,
     j               SYNAMP,
     J               IMASSA,
     J               IEMISS,
     J               ITRANS,
     J               IPROCS,
     J               NPROCS,
     J               STOCHL,
     J               FL2BAL)
          endif

          allocate ( FLTRAN(2,NOSYS),
     j               JDUMP(NDMPAR_OUT+1),
     j               SFACTO(NOSUM,NOTOT),
     j               DANAMP(NDMPAR_OUT+1),
     j               SYNAMP(NOTOT+NOSUM),
     J               IMASSA(NOTOT+NOSUM),
     J               IEMISS(NOTOT+NOSUM),
     J               ITRANS(NOTOT+NOSUM),
     J               IPROCS(NOTOT+NOSUM),
     J               NPROCS(NOTOT+NOSUM),
     J               STOCHL(NOSUM,NOFLUX),
     J               FL2BAL(NOTOT+NOSUM,NOFLUX),
     J               STAT = IERR )
          IF ( IERR .GT. 0 ) GOTO 9000
          IF ( .NOT. LUMPTR ) THEN

C             allocate and set SEGDMP, first dump number for each segment (if any)
              if ( allocated(segdmp) ) then
                  deallocate( segdmp )
              endif
              allocate ( SEGDMP(NOSEG),
     J                   STAT = IERR )
              IF ( IERR .GT. 0 ) GOTO 9000
              SEGDMP = 0
              ITEL   = 0
              IDUMP_OUT = 0
              DO IDUMP = 1 , NDMPAR
                  NSC = IPDMP(NDMPAR+NTDMPQ+IDUMP)
                  IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                     IDUMP_OUT = IDUMP_OUT + 1
                     DO ISC = 1 , NSC
                         ITEL  = ITEL + 1
                         ISEG  = IPDMP(NDMPAR+NTDMPQ+NDMPAR+ITEL)
                         IF ( ISEG .GT. 0 ) THEN
                             IF ( SEGDMP(ISEG) .EQ. 0 ) THEN
                                 SEGDMP(ISEG) = IDUMP_OUT
                             ENDIF
                         ENDIF
                     ENDDO
                  ELSE
                     ITEL = ITEL + NSC
                  ENDIF
              ENDDO

          ENDIF
          IF ( .NOT. LUMPEM ) THEN

C             allocate and set IWDMP, first dump number for each wasteload
              if ( allocated(iwdmp) ) then
                  deallocate( iwdmp )
              endif
              allocate ( IWDMP(NOWST),STAT = IERR )
              IF ( IERR .GT. 0 ) GOTO 9000
              IWDMP  = 0
              ITEL   = 0
              IDUMP_OUT = 0
              DO IDUMP = 1 , NDMPAR
                  NSC = IPDMP(NDMPAR+NTDMPQ+IDUMP)
                  IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                     IDUMP_OUT = IDUMP_OUT + 1
                     DO ISC = 1 , NSC
                         ITEL  = ITEL + 1
                         ISEG  = IPDMP(NDMPAR+NTDMPQ+NDMPAR+ITEL)
                         IF ( ISEG .GT. 0 ) THEN
                             DO IW = 1, NOWST
                                IF ( IWASTE(IW) .EQ. ISEG .AND. IWDMP(IW) .EQ. 0 ) THEN
                                   IWDMP(IW) = IDUMP_OUT
                                ENDIF
                             ENDDO
                         ENDIF
                     ENDDO
                  ELSE
                     ITEL = ITEL + NSC
                  ENDIF
              ENDDO

          ENDIF

c         Balances are constructed for all system variables
c         + total N + total P (IF RELEVANT!)
C         Find which state variables contribute to what extent

          CALL COMSUM (NOSUM , TFACTO, NOTOT , SYNAME, SFACTO,
     J                 NOCONS, CONAME, CONS  )
          DO ISYS = 1,NOTOT
              SYNAMP(ISYS) = SYNAME(ISYS)
          ENDDO
          DO ISUM = 1,NOSUM
              SYNAMP(NOTOT+ISUM) = SYNAMS(ISUM)
          ENDDO

C         Count number of balance terms dep. on flags LUMPEM/LUMPPR

c         first term   nr of terms    description
c         ----------   -----------    ---------------------------
c         IMASSA(ISYS) 1              mass/accumulation
c         IEMISS(ISYS) NEMISS         inflow/outflow over boundaries
c                                     if LUMPEM only totals
c                                     if not    per fraction
c         ITRANS(ISYS) 2              inflow/outflow other segments
c         IPROCS(ISYS) NPROCS(ISYS)   contribution from processes
c                                     if LUMPPR only total
c                                     if not    per process
C
          IF ( LUMPEM ) THEN
              NEMISS = 2
          ELSE

c             boundary types and loads as seperate term (all in and out)
              NEMISS = 2*NOBTYP + 2*NOWTYP
          ENDIF

          IF ( LUMPTR ) THEN
              NTRANS = 2
          ELSE

c             internal transport from every dump area possible plus the other term
              NTRANS = 2*NDMPAR_OUT + 2
          ENDIF

          NOOUT  = 0
          DO ISYS = 1,NOTOT+NOSUM
              IF ( ISYS .GT. NOTOT ) THEN
                  IF ( TFACTO(ISYS-NOTOT) .GT. 0.0001 ) THEN
                      INCLUD = .TRUE.
                  ELSE
                      INCLUD = .FALSE.
                      IMASSA(ISYS) = -1
                  ENDIF
              ELSE
                  INCLUD = .TRUE.
              ENDIF
              IF (INCLUD) THEN
                  IMASSA(ISYS) = NOOUT + 1
                  NOOUT = NOOUT + 1
                  IEMISS(ISYS) = NOOUT + 1
                  NOOUT = NOOUT + NEMISS
                  ITRANS(ISYS) = NOOUT + 1
                  NOOUT = NOOUT + NTRANS
                  IPROCS(ISYS) = NOOUT + 1
                  IF ( LUMPPR ) THEN
                      NPROCS(ISYS) = 1
                  ELSE
c                     Find sum STOCHI coefficients for sum parameters
                      IF ( ISYS .GT. NOTOT ) THEN
                          ISUM = ISYS - NOTOT
                          DO IFLUX = 1,NOFLUX
                              STOCHL(ISUM,IFLUX) = 0.0
                              DO ISYS2 = 1,NOTOT
                                  STOCHL(ISUM,IFLUX) =
     J                            STOCHL(ISUM,IFLUX)
     J                                     + STOCHI(ISYS2,IFLUX)
     J                                     * SFACTO(ISUM,ISYS2)
                              ENDDO
                          ENDDO
                      ENDIF

C                     Make sure that irrelevant fluxes are not included
                      NPROCS(ISYS) = 0
                      DO IFLUX = 1,NOFLUX
                          IF ( ISYS .LE. NOTOT ) THEN
                              ST = STOCHI(ISYS,IFLUX)
                          ELSE
                              ST = STOCHL(ISYS-NOTOT,IFLUX)
                          ENDIF
                          IF ( ABS(ST) .GT. 1.E-20 ) THEN
                              NPROCS(ISYS) = NPROCS(ISYS) + 1
                              FL2BAL(ISYS,NPROCS(ISYS)) = IFLUX
                          ENDIF
                      ENDDO
                  ENDIF
                  NOOUT = NOOUT + NPROCS(ISYS)
              ENDIF
          ENDDO

c         Dimension additional arrays

          if ( allocated(balans) ) then
              deallocate( balans, baltot, ouname )
          endif

          allocate ( BALANS(NOOUT,NDMPAR_OUT+1),
     J               BALTOT(NOOUT,NDMPAR_OUT+1),
     j               OUNAME(NOOUT),
     j               stat = ierr )
          if ( ierr .gt. 0 ) goto 9000

c         Set balance term names

          DO ISYS = 1,NOTOT+NOSUM
              IF ( IMASSA(ISYS) .GT. 0 ) THEN
                  C20 = SYNAMP(ISYS)
                  OUNAME(IMASSA(ISYS)) = C20(1:6)//'_Storage'
                  IF ( LUMPEM ) THEN
                      OUNAME(IEMISS(ISYS)  )=C20(1:6)//'_All Bo+Lo_In'
                      OUNAME(IEMISS(ISYS)+1)=C20(1:6)//'_All Bo+Lo_Out'
                  ELSE
                      ITEL2 = IEMISS(ISYS)-1
                      DO IFRAC = 1,NOBTYP
                          ITEL2 = ITEL2 + 1
                          OUNAME(ITEL2) =
     J                    C20(1:6)//'_'//BNDTYP(IFRAC)(1:9)//'_In'
                          ITEL2 = ITEL2 + 1
                          OUNAME(ITEL2) =
     J                    C20(1:6)//'_'//BNDTYP(IFRAC)(1:9)//'_Out'
                      ENDDO
                      DO IFRAC = 1,NOWTYP
                          ITEL2 = ITEL2 + 1
                          OUNAME(ITEL2) =
     J                    C20(1:6)//'_'//WSTTYP(IFRAC)(1:9)//'_In'
                          ITEL2 = ITEL2 + 1
                          OUNAME(ITEL2) =
     J                    C20(1:6)//'_'//WSTTYP(IFRAC)(1:9)//'_Out'
                      ENDDO
                  ENDIF
                  IF ( LUMPTR ) THEN
                      OUNAME(ITRANS(ISYS)) = C20(1:6)//'_Transport In'
                      OUNAME(ITRANS(ISYS)+1) = C20(1:6)//'_Transport Out'
                  ELSE
                      ITEL2 = ITRANS(ISYS)-1
                      ITEL2 = ITEL2 + 1
                      OUNAME(ITEL2) = C20(1:6)//'_'//'Other    '//'_In'
                      ITEL2 = ITEL2 + 1
                      OUNAME(ITEL2) = C20(1:6)//'_'//'Other    '//'_Out'
                      DO IDUMP = 1,NDMPAR
                          IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                             ITEL2 = ITEL2 + 1
                             OUNAME(ITEL2) = C20(1:6)//'_'//DANAM(IDUMP)(1:9)//'_In'
                             ITEL2 = ITEL2 + 1
                             OUNAME(ITEL2) = C20(1:6)//'_'//DANAM(IDUMP)(1:9)//'_Out'
                          ENDIF
                      ENDDO
                  ENDIF
                  IF ( LUMPPR ) THEN
                      OUNAME(IPROCS(ISYS)) = C20(1:6)//'_Processes'
                  ELSE
                      DO ITEL = 1,NPROCS(ISYS)
                          IFLUX = FL2BAL(ISYS,ITEL)
                          OUNAME(IPROCS(ISYS)+ITEL-1) =
     J                    C20(1:6)//'_'//FLXNAM(IFLUX)(1:13)
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO

          idump_out = 0
          DO IDUMP = 1,NDMPAR
             if ( dmpbal(idump) .eq. 1 ) then
                idump_out = idump_out + 1
                DANAMP(IDUMP_out) = DANAM(IDUMP)
                JDUMP(IDUMP_out)  = IDUMP_OUT
             endif
          ENDDO
          DANAMP(NDMPAR_OUT+1) = 'Sum_of_balance_areas'
          JDUMP(NDMPAR_OUT+1) = NDMPAR_OUT+1

          IF ( .NOT. SUPPFT )
     J    CALL DHOPNF ( LUNOUT, LCHOUT, 21    , 1     , IDUM  )

c         Zero output matrices
          DO IOUT  = 1,NOOUT
          DO IDUMP = 1,NDMPAR_OUT+1
              BALTOT(IOUT,IDUMP) = 0.0
              BALANS(IOUT,IDUMP) = 0.0
          ENDDO
          ENDDO
      ELSE
          IFIRST = .FALSE.
      ENDIF

c**** END OF **** INITIALIZATION *************************************

c     Loop over dump areas

      ITEL1 = NDMPAR
      IP1   = NDMPAR + NTDMPQ
      ITEL2 = NDMPAR + NTDMPQ + NDMPAR
      IDUMP_OUT = 0
      DO IDUMP = 1 , NDMPAR

c         ONLY second and following calls !!!!!!
          IF ( .NOT.IFIRST ) THEN

          IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
             IDUMP_OUT = IDUMP_OUT + 1

C            Mass / accumulation term, previous mass already here
C            Subtract current mass
             CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA, IMASSA, 0     ,
     J                     BALANS   , NOSUM , ASMASS(1,IDUMP,1)     ,
     J                     -1.0     , 1     , SFACTO, NOOUT, NOTOT )

c            Process INFLOW/OUTFLOW terms, both boundaries and internal
C            (the IPOINT array is used to distinguish the two)

C            Loop over exchanges relevant for current MON AREA
             NQC = IPDMP(IDUMP)
             DO IQC = 1 , NQC
                 ITEL1 = ITEL1 + 1
                 IQ    = IPDMP(ITEL1)

C                Is it a boundary? Which type/fraction?
                 IPOIN = ABS(IQ)
                 IVAN  = IPOINT(1,IPOIN)
                 INAAR = IPOINT(2,IPOIN)
                 IF ( IVAN .LT. 0 .OR.
     J                INAAR .LT. 0 ) THEN
C                    BOUNDARY!!!
                     BOUNDA = .TRUE.
                     IF ( IVAN .LT. 0 ) THEN
C                        -I TO +J BOUNDARY!!!
                         IBOUN = -IVAN
                     ELSE
C                        +I TO -J BOUNDARY!!!
                         IBOUN = -INAAR
                     ENDIF
                     IFRAC = INBTYP(IBOUN)
                 ELSE
C                    INTERNAL
                     BOUNDA = .FALSE.
                     IFRAC = 1
                     IF ( .NOT. LUMPTR ) THEN
                        IF ( IQ .LT. 0 ) THEN
                           IF ( IVAN .GT. 0 ) IFRAC = SEGDMP(IVAN) + 1
                        ELSE
                           IF ( INAAR .GT. 0 ) IFRAC = SEGDMP(INAAR) + 1
                        ENDIF
                     ENDIF
                 ENDIF

C                Find fluxes
                 IF ( IQ .GT. 0 ) THEN
                    IPQ  = IQDMP(IQ)
                    DO ISYS = 1 , NOSYS
                       FLTRAN(1,ISYS) = DMPQ(ISYS,IPQ,2)
                       FLTRAN(2,ISYS) = -DMPQ(ISYS,IPQ,1)
                    ENDDO
                 ELSE
                    IPQ  = IQDMP(-IQ)
                    DO ISYS = 1 , NOSYS
                       FLTRAN(1,ISYS) = DMPQ(ISYS,IPQ,1)
                       FLTRAN(2,ISYS) = -DMPQ(ISYS,IPQ,2)
                    ENDDO
                 ENDIF

C                Update balances
                 IF ( BOUNDA ) THEN
                     IF ( LUMPEM ) THEN
                         CALL UPDBAL ( IDUMP_OUT, NOSYS , IMASSA, IEMISS,
     J                                 0        , BALANS, NOSUM , FLTRAN,
     J                                 1.0      , 2     , SFACTO, NOOUT ,
     J                                 NOTOT    )
                     ELSE
                         CALL UPDBAL ( IDUMP_OUT  , NOSYS , IMASSA, IEMISS,
     J                                 (IFRAC-1)*2, BALANS, NOSUM , FLTRAN,
     J                                 1.0        , 2     , SFACTO, NOOUT ,
     J                                 NOTOT      )
                     ENDIF
                 ELSE
                     CALL UPDBAL ( IDUMP_OUT  , NOSYS , IMASSA, ITRANS,
     J                             (IFRAC-1)*2, BALANS, NOSUM , FLTRAN,
     J                             1.0        , 2     , SFACTO, NOOUT ,
     J                             NOTOT      )
                 ENDIF
             ENDDO

c            Loads

             IF ( LUMPEM ) THEN
                 CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA           , IEMISS, 0     ,
     J                         BALANS   , NOSUM , ASMASS(1,IDUMP,3), 1.0   , 1     ,
     J                         SFACTO   , NOOUT , NOTOT )
                 CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA           , IEMISS, 1     ,
     J                         BALANS   , NOSUM , ASMASS(1,IDUMP,4),-1.0   , 1     ,
     J                         SFACTO   , NOOUT , NOTOT )
             ELSE
                 DO IW = 1 , NOWST
                    IF ( IWDMP(IW) .EQ. IDUMP_OUT ) THEN
                       IFRAC = INWTYP(IW)
                       IBAL_OFF = (NOBTYP+IFRAC-1)*2
                       CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA        , IEMISS, IBAL_OFF,
     J                               BALANS   , NOSUM , WSTDMP(1,IW,1), 1.0   , 1       ,
     J                               SFACTO   , NOOUT , NOTOT         )
                       IBAL_OFF = (NOBTYP+IFRAC-1)*2+1
                       CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA        , IEMISS, IBAL_OFF,
     J                               BALANS   , NOSUM , WSTDMP(1,IW,2),-1.0   , 1       ,
     J                               SFACTO   , NOOUT , NOTOT         )
                    ENDIF
                 ENDDO
             ENDIF

c            Process sources and sinks

             IF ( LUMPPR ) THEN
C                Copy term from ASMASS array
                 CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA, IPROCS, 0     ,
     J                         BALANS   , NOSUM , ASMASS(1,IDUMP,2)     ,
     J                         1.0      , 1     , SFACTO, NOOUT , NOTOT )
             ELSE

C                Loop over substances, including sum parameters
                 DO ISYS = 1,NOTOT+NOSUM
                     IF ( IMASSA(ISYS) .GT. 0 ) THEN
c                        Substance (sum parameter) is active
c                        Loop over relevant processes
                         DO ITEL = 1,NPROCS(ISYS)
c                            Pointers to balance and fluxes array
                             IOUT = IPROCS(ISYS)+ITEL-1
                             IFLUX = FL2BAL(ISYS,ITEL)
c                            Find stoichiometry constant
                             IF ( ISYS .LE. NOTOT ) THEN
                                 ST = STOCHI(ISYS,IFLUX)
                             ELSE
                                 ST = STOCHL(ISYS-NOTOT,IFLUX)
                             ENDIF
c                            Update balance
                             BALANS(IOUT,IDUMP_OUT) = FLXINT(IFLUX,IDUMP) * ST
                         ENDDO
                     ENDIF
                 ENDDO
             ENDIF

          ELSE

             ! dump area excluded from mass balance

             NQC = IPDMP(IDUMP)
             ITEL1 = ITEL1 + NQC

          ENDIF

C         End of actions only if we are not at the first level!
          ENDIF
      ENDDO

c     Fill balance matrix for sum of areas
c     zero accumulation term of sum segment first
      DO ISYS = 1,NOTOT+NOSUM
          IOUT = IMASSA(ISYS)
          IF (IOUT.GT.0)
     J    BALANS(IOUT,NDMPAR_OUT+1) = 0.0
      ENDDO
      DO IDUMP_OUT = 1,NDMPAR_OUT
      DO IOUT  = 1,NOOUT
          BALANS(IOUT,NDMPAR_OUT+1) = BALANS(IOUT,NDMPAR_OUT+1)
     J                          + BALANS(IOUT,IDUMP_OUT)
      ENDDO
      ENDDO

c     Update integrated balance matrix FOR ALL TERMS EXCEPT ACCUMULATION
      DO IDUMP_OUT = 1,NDMPAR_OUT+1
          DO ISYS = 1,NOTOT+NOSUM
              IF ( IMASSA(ISYS) .GT. 0 ) THEN
                  ITEL1 = IMASSA(ISYS)+1
                  ITEL2 = IPROCS(ISYS)+NPROCS(ISYS)-1
                  DO IOUT = ITEL1,ITEL2
                      BALTOT(IOUT,IDUMP_OUT) = BALTOT(IOUT,IDUMP_OUT) +
     J                BALANS(IOUT,IDUMP_OUT)
                  ENDDO
              ENDIF
          ENDDO
      ENDDO

c     Optionally scale the balance per volume or area

      IF ( B_AREA ) THEN

          ALLOCATE(DMP_SURF(NDMPAR), STAT = IERR )
          IF ( IERR .GT. 0 ) GOTO 9000
          CALL DMPVAL(NDMPAR,IPDMP(NDMPAR+NTDMPQ+1),SURF,DMP_SURF)
          IDUMP_OUT = 0
          DO IDUMP = 1 , NDMPAR
             IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                IDUMP_OUT = IDUMP_OUT + 1
                DO IOUT  = 1,NOOUT
                  BALANS(IOUT,IDUMP_OUT) = BALANS(IOUT,IDUMP_OUT)/DMP_SURF(IDUMP)
                ENDDO
                TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
             ENDIF
          ENDDO
          DO IOUT  = 1,NOOUT
             BALANS(IOUT,NDMPAR_OUT+1) = BALANS(IOUT,NDMPAR_OUT+1)/TOT_SURF
          ENDDO
          DEALLOCATE(DMP_SURF)

      ELSEIF ( B_VOLU ) THEN

          ALLOCATE(DMP_VOLU(NDMPAR), STAT = IERR )
          IF ( IERR .GT. 0 ) GOTO 9000
          CALL DMPVAL(NDMPAR,IPDMP(NDMPAR+NTDMPQ+1),VOLUME,DMP_VOLU)
          IDUMP_OUT = 0
          DO IDUMP = 1 , NDMPAR
             IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                IDUMP_OUT = IDUMP_OUT + 1
                DO IOUT  = 1,NOOUT
                  BALANS(IOUT,IDUMP_OUT) = BALANS(IOUT,IDUMP_OUT)/DMP_VOLU(IDUMP)
                ENDDO
                TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
             ENDIF
          ENDDO
          DO IOUT  = 1,NOOUT
             BALANS(IOUT,NDMPAR_OUT+1) = BALANS(IOUT,NDMPAR_OUT+1)/TOT_VOLU
          ENDDO
          DEALLOCATE(DMP_VOLU)

      ENDIF


C     Write time dependent output
      IF ( IFIRST ) THEN
          IINIT = 1
      ELSE
          IINIT = 0
      ENDIF
      IF ( .NOT. SUPPFT ) THEN
          IF ( ONLYSM ) THEN
              CALL OUTHIS ( LUNOUT             , CDUM                , ITIME , MONAME, 1     ,
     +                      JDUMP(NDMPAR_OUT+1), DANAMP(NDMPAR_OUT+1),
     J                      NOOUT              , OUNAME              , BALANS,
     +                      0                  , CDUM                , RDUM  , IINIT )
          ELSE
              CALL OUTHIS ( LUNOUT, CDUM  , ITIME , MONAME, NDMPAR_OUT+1,
     +                      JDUMP , DANAMP, NOOUT , OUNAME, BALANS      ,
     +                      0     , CDUM  , RDUM  , IINIT )
          ENDIF
      ENDIF

c     Zero output matrix
      DO IOUT  = 1,NOOUT
      DO IDUMP = 1,NDMPAR_OUT+1
          BALANS(IOUT,IDUMP) = 0.0
      ENDDO
      ENDDO

c     Store current mass in Mass term as a starting point for next step
      IDUMP_OUT = 0
      DO IDUMP = 1 , NDMPAR
         IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
            IDUMP_OUT = IDUMP_OUT + 1
            CALL UPDBAL ( IDUMP_OUT, NOTOT , IMASSA, IMASSA, 0     ,
     J                    BALANS   , NOSUM , ASMASS(1,IDUMP,1)     ,
     J                    1.0      , 1     , SFACTO, NOOUT , NOTOT )
c           Sum segment (OBSOLETE??)
            DO ISYS = 1,NOTOT+NOSUM
                IOUT = IMASSA(ISYS)
                IF ( IOUT .GT. 0 ) THEN
                    BALANS(IOUT,NDMPAR_OUT+1) = BALANS(IOUT,NDMPAR_OUT+1)
     J                                    + BALANS(IOUT,IDUMP_OUT)
                ENDIF
            ENDDO
         ENDIF
      ENDDO

c     Update integrated balance matrix
c     Mass/accumulation term only FIRST time
      IF ( IFIRST ) THEN
          DO ISYS = 1,NOTOT+NOSUM
              IOUT = IMASSA(ISYS)
              IF ( IOUT .GT. 0 ) THEN
                  DO IDUMP = 1,NDMPAR_OUT+1
                      BALTOT(IOUT,IDUMP) = BALANS(IOUT,IDUMP)
                  ENDDO
              ENDIF
          ENDDO
      ENDIF

c     This is an incorrect statement in case ITIME never reaches
c     one of the two time levels
      IF ( ITIME .GE. ITSTOP .OR. ITIME .GE. IMSTOP ) THEN

          IF ( .NOT. SUPPFT ) CLOSE ( LUNOUT )
          DO ISYS = 1,NOTOT+NOSUM
              IOUT = IMASSA(ISYS)
              IF ( IOUT .GT. 0 ) THEN
                  DO IDUMP = 1,NDMPAR_OUT+1
                      BALTOT(IOUT,IDUMP) = BALTOT(IOUT,IDUMP)
     J                                   - BALANS(IOUT,IDUMP)
                  ENDDO
              ENDIF
          ENDDO

          FILNAM=LCHOUT
          INDX = INDEX(FILNAM,'-bal.his')
          IF ( INDX .GT. 0 ) THEN
             FILNAM(INDX:)='-bal.prn'
          ELSE
             FILNAM = 'sobwqbal.prn'
          ENDIF
          OPEN ( IOBALI , FILE = FILNAM )

c         In mass
          CALL OUTBAI (IOBALI, MONAME      , IMSTRT, IMSTOP, NOOUT ,
     J                 NOTOT , NDMPAR_OUT+1, DANAMP, OUNAME, SYNAMP,
     J                 IMASSA, IEMISS      , NEMISS, ITRANS, NTRANS,
     J                 IPROCS, NPROCS      , BALTOT, ONLYSM, NOSUM ,
     J                 SFACTO, 0           , 1     )

c         In mass/m2
          ALLOCATE(DMP_SURF(NDMPAR), STAT = IERR )
          IF ( IERR .GT. 0 ) GOTO 9000
          CALL DMPVAL(NDMPAR,IPDMP(NDMPAR+NTDMPQ+1),SURF,DMP_SURF)
          IDUMP_OUT = 0
          DO IDUMP = 1 , NDMPAR
             IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                IDUMP_OUT = IDUMP_OUT + 1
                DO IOUT  = 1,NOOUT
                  BALTOT(IOUT,IDUMP_OUT) = BALTOT(IOUT,IDUMP_OUT)/DMP_SURF(IDUMP)
                ENDDO
                TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
             ENDIF
          ENDDO
          DO IOUT  = 1,NOOUT
             BALTOT(IOUT,NDMPAR_OUT+1) = BALTOT(IOUT,NDMPAR_OUT+1)/TOT_SURF
          ENDDO
          CALL OUTBAI (IOBALI, MONAME      , IMSTRT, IMSTOP, NOOUT ,
     J                 NOTOT , NDMPAR_OUT+1, DANAMP, OUNAME, SYNAMP,
     J                 IMASSA, IEMISS      , NEMISS, ITRANS, NTRANS,
     J                 IPROCS, NPROCS      , BALTOT, ONLYSM, NOSUM ,
     J                 SFACTO, 1           , 0     )

c         In mass/m3
          ALLOCATE(DMP_VOLU(NDMPAR), STAT = IERR )
          IF ( IERR .GT. 0 ) GOTO 9000
          CALL DMPVAL(NDMPAR,IPDMP(NDMPAR+NTDMPQ+1),VOLUME,DMP_VOLU)
          IDUMP_OUT = 0
          DO IDUMP = 1 , NDMPAR
             IF ( DMPBAL(IDUMP) .EQ. 1 ) THEN
                IDUMP_OUT = IDUMP_OUT + 1
                DO IOUT  = 1,NOOUT
                  BALTOT(IOUT,IDUMP_OUT) = BALTOT(IOUT,IDUMP_OUT)*DMP_SURF(IDUMP)/DMP_VOLU(IDUMP)
                ENDDO
                TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
             ENDIF
          ENDDO
          DO IOUT  = 1,NOOUT
             BALTOT(IOUT,NDMPAR_OUT+1) = BALTOT(IOUT,NDMPAR_OUT+1)*TOT_SURF/TOT_VOLU
          ENDDO
          CALL OUTBAI (IOBALI, MONAME      , IMSTRT, IMSTOP, NOOUT ,
     J                 NOTOT , NDMPAR_OUT+1, DANAMP, OUNAME, SYNAMP,
     J                 IMASSA, IEMISS      , NEMISS, ITRANS, NTRANS,
     J                 IPROCS, NPROCS      , BALTOT, ONLYSM, NOSUM ,
     J                 SFACTO, 2           , 0     )
          DEALLOCATE(DMP_VOLU)
          DEALLOCATE(DMP_SURF)

          CLOSE ( IOBALI )
      ENDIF

      INIOUT = 0

      if ( timon ) call timstop ( ithandl )
      RETURN
 9000 STOP 'Error allocating memory'
 9010 STOP 'Error deallocating memory'
      END
      SUBROUTINE OUTBAI (IOBALI, MONAME, IMSTRT, IMSTOP, NOOUT ,
     J                   NOTOT , NDMPAR, DANAMP, OUNAME, SYNAME,
     J                   IMASSA, IEMISS, NEMISS, ITRANS, NTRANS,
     J                   IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM ,
     J                   SFACTO, IUNIT , INIT  )
      use timers

      INTEGER      IOBALI, IMSTRT, IMSTOP, NOOUT , NOTOT , NDMPAR,
     J             IMASSA(*), IEMISS(*), NEMISS, ITRANS(*), NTRANS,
     J             IPROCS(*), NPROCS(*), NOSUM    , IUNIT , INIT
      CHARACTER*40 MONAME(4)
      CHARACTER*20 DANAMP(NDMPAR),OUNAME(*),SYNAME(*)
      REAL         BALTOT(NOOUT,NDMPAR),SFACTO(NOSUM,*)
      LOGICAL      ONLYSM

      REAL         VALUE , VALUE1, VALUE2, SUMPOS, SUMNEG
      INTEGER      IDUMP , ISYS  , ITEL  , I     , ITEL2 , ISUM
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outbai", ithandl )


      IF ( INIT .EQ. 1 ) THEN
C         Write header
          WRITE (IOBALI,1000) MONAME(1), MONAME(2), MONAME(3), MONAME(4)

c         Write timers
          WRITE (IOBALI,1010) REAL(IMSTRT)/86400., REAL(IMSTOP)/86400.

c         Write sum parameters
          DO ISUM = 1,NOSUM
              IF ( IMASSA(NOTOT+ISUM) .GT. 0 ) THEN
                  WRITE (IOBALI,1020) SYNAME(NOTOT+ISUM)
                  DO ISYS = 1,NOTOT
                      IF ( SFACTO(ISUM,ISYS) .GE. 0.0001 ) THEN
                          WRITE (IOBALI,1030) SYNAME(ISYS),SFACTO(ISUM,ISYS)
                      ENDIF
                  ENDDO
              ENDIF
          ENDDO
      ENDIF

c     Write the unit of the balance
      IF ( IUNIT .EQ. 0 ) THEN
          WRITE (IOBALI,1040)
      ELSEIF ( IUNIT .EQ. 1 ) THEN
          WRITE (IOBALI,1050)
      ELSEIF ( IUNIT .EQ. 2 ) THEN
          WRITE (IOBALI,1060)
      ENDIF


c     The balance per area
      DO IDUMP = 1,NDMPAR
          IF ( .NOT. ONLYSM .OR. IDUMP .EQ. NDMPAR ) THEN
              WRITE (IOBALI,1100) DANAMP(IDUMP)
              DO ISYS = 1,NOTOT+NOSUM
                  ITEL = IMASSA(ISYS)
                  IF ( ITEL .GT. 0 ) THEN
                      WRITE (IOBALI,1110) SYNAME(ISYS)
                      SUMPOS = 0.0
                      SUMNEG = 0.0

C                     Mass term
                      VALUE = BALTOT(ITEL,IDUMP)
                      IF ( VALUE .GT. 0.0 ) THEN
                          VALUE1 = VALUE
                          VALUE2 = 0.0
                      ELSE
                          VALUE1 = 0.0
                          VALUE2 = VALUE
                      ENDIF
                      WRITE (IOBALI,1120) SYNAME(ISYS)(1:6),
     J                                    VALUE1, VALUE2
                      SUMPOS = SUMPOS + VALUE1
                      SUMNEG = SUMNEG + VALUE2

C                     Inputs from boundaries
                      DO I = 1,NEMISS/2
                          ITEL2 = (I-1)*2 + IEMISS(ISYS)
                          VALUE1 = BALTOT(ITEL2  ,IDUMP)
                          VALUE2 = BALTOT(ITEL2+1,IDUMP)
                          WRITE (IOBALI,1130) OUNAME(ITEL2)(1:16),
     J                                        VALUE1, VALUE2
                          SUMPOS = SUMPOS + VALUE1
                          SUMNEG = SUMNEG + VALUE2
                      ENDDO

C                     Internal transport
                      IF ( NTRANS .EQ. 2 ) THEN
                          VALUE1 = BALTOT(ITRANS(ISYS)  ,IDUMP)
                          VALUE2 = BALTOT(ITRANS(ISYS)+1,IDUMP)
                          WRITE (IOBALI,1140) SYNAME(ISYS)(1:6),
     J                                        VALUE1, VALUE2
                          SUMPOS = SUMPOS + VALUE1
                          SUMNEG = SUMNEG + VALUE2
                      ELSE
                          DO I = 1,NTRANS/2
                              ITEL2 = (I-1)*2 + ITRANS(ISYS)
                              VALUE1 = BALTOT(ITEL2  ,IDUMP)
                              VALUE2 = BALTOT(ITEL2+1,IDUMP)
                              WRITE (IOBALI,1130) OUNAME(ITEL2)(1:16),
     J                                            VALUE1, VALUE2
                              SUMPOS = SUMPOS + VALUE1
                              SUMNEG = SUMNEG + VALUE2
                          ENDDO
                      ENDIF

C                     Processes
                      DO I = 1,NPROCS(ISYS)
                          ITEL2 = IPROCS(ISYS)-1+I
                          VALUE = BALTOT(ITEL2,IDUMP)
                          IF ( VALUE .GE. 0.0 ) THEN
                              VALUE1 = VALUE
                              VALUE2 = 0.0
                          ELSE
                              VALUE1 = 0.0
                              VALUE2 = VALUE
                          ENDIF
                          WRITE (IOBALI,1150) OUNAME(ITEL2),
     J                                        VALUE1, VALUE2
                          SUMPOS = SUMPOS + VALUE1
                          SUMNEG = SUMNEG + VALUE2
                      ENDDO
                      WRITE (IOBALI,1160) SUMPOS,SUMNEG
                  ENDIF
              ENDDO
          ENDIF
      ENDDO

 1000 FORMAT ('Mass balances output file'//
     j        a40/a40/a40//
     j        'All terms in basic mass units,'
     j        'for Processes Library always (g)'//
     j        'Simulation starts: ',a40)
 1010 FORMAT ('Mass balances output period:'/
     j        'start: ',f9.3,' days'/
     j        'stop : ',f9.3,' days')
 1020 FORMAT (/'Balance for sum parameter ',a,' consists of:'/
     j         'substance           scale factor')
 1030 FORMAT (A20,F10.4)
 1040 FORMAT (//
     j        'MASS BALANCE PER DUMPAREA'/
     j        'All terms in basic mass units,'
     j        'for Processes Library always (g)')
 1050 FORMAT (//
     j        'MASS BALANCE PER SURFACE'/
     j        'All terms in basic mass units/m2,'
     j        'for Processes Library always (g)')
 1060 FORMAT (//
     j        'MASS BALANCE PER VOLUME'/
     j        'All terms in basic mass units/m3,'
     j        'for Processes Library always (g)')
 1100 FORMAT (//
     j'============================================================'/
     j'Mass balances for ',a/
     j'============================================================')
 1110 FORMAT (/'Substance ',a20,' Sources/Inflows Sinks/Outflows'/
     j'------------------------------------------------------------')
 1120 FORMAT (a6,'_Storage ',15x,2e15.5)
 1130 FORMAT (a16,14x,2e15.5)
 1140 FORMAT (a6,'_Internal transport',5x,2e15.5)
 1150 FORMAT (a20,10x,2e15.5)
 1160 FORMAT ('SUM OF ALL TERMS              ',2e15.5)

      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      SUBROUTINE COMSUM (NOSUM , TFACTO, NOTOT , SYNAME, SFACTO,
     J                   NOCONS, CONAME, CONS  )
      use timers
      INTEGER            NOSUM , NOTOT , NOCONS
      CHARACTER*20       SYNAME(NOTOT),  CONAME(NOCONS)
      REAL               TFACTO(NOSUM), SFACTO(NOSUM,NOTOT),
     J                   CONS(NOCONS)

      INCLUDE 'cblbal.inc'

      INTEGER              ISUM  , ISYS  , ICONS , IRES  , NRES1 ,
     J                     NRES2 , ITYP
      REAL                 FACTOR
      PARAMETER           (NRES1 = 23, NRES2 = 2)
      CHARACTER*20         RESNA1(NRES1),RESNA2(NRES2)
      character*10         RATNA2(2,NRES2)
      REAL                 FACRES(2,NRES1),RATDEF(2,NRES2)

      DATA RESNA1   / 'DetP                ',
     J                'OOP                 ',
     J                'AlgP                ',
     J                'AAP                 ',
     J                'PO4                 ',
     J                'PAP                 ',
     J                'POP1                ',
     J                'POP2                ',
     J                'POP3                ',
     J                'POP4                ',
     J                'DOP                 ',
     J                'APATP               ',
     J                'VIVP                ',
     J                'DetN                ',
     J                'OON                 ',
     J                'AlgN                ',
     J                'NO3                 ',
     J                'NH4                 ',
     J                'PON1                ',
     J                'PON2                ',
     J                'PON3                ',
     J                'PON4                ',
     J                'DON                 '/
      DATA FACRES   / 0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                0.0,1.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0,
     J                1.0,0.0/
      DATA RESNA2   / 'Diat                ',
     J                'Green               '/
      DATA RATNA2   / 'NCRatDiat ','PCRatDiat ',
     J                'NCRatGreen','PCRatGreen'/
      DATA RATDEF   / 0.16,0.02,
     J                0.16,0.02/
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "consum", ithandl )

C     Compose sum parameters
C     LOCAL FUNCTIONALITY NOSUM = 2, CHECK!!!!!!!!!!!!!

      IF ( NOSUM .NE. 2 ) STOP 'BUG IN COMSUM!'

c     Initialise substance shares in sum parameters as well as totals
c     (totals are used to find out if sum parameter is active)

      DO ISUM = 1,NOSUM
          TFACTO(ISUM) = 0.0
          DO ISYS = 1,NOTOT
              SFACTO(ISUM,ISYS) = 0.0
          ENDDO
      ENDDO

      DO ISYS = 1,NOTOT

C         Reserved substance names, FIXED scale factor
          CALL ZOEK   (SYNAME(ISYS),NRES1,RESNA1,20,IRES)
          IF ( IRES .GT. 0 ) THEN
              DO ISUM = 1,NOSUM
                  TFACTO(ISUM) = TFACTO(ISUM) + FACRES(ISUM,IRES)
                  SFACTO(ISUM,ISYS) = FACRES(ISUM,IRES)
              ENDDO
          ENDIF

C         Reserved substance names, scale factors from CONS with default
          CALL ZOEK   (SYNAME(ISYS),NRES2,RESNA2,20,IRES)
          IF ( IRES .GT. 0 ) THEN
              DO ISUM = 1,NOSUM
                  CALL ZOEK   (RATNA2(ISUM,IRES),NOCONS,CONAME,10,ICONS)
                  IF ( ICONS .GT. 0 ) THEN
                      FACTOR = CONS(ICONS)
                  ELSE
                      FACTOR = RATDEF(ISUM,IRES)
                  ENDIF
                  TFACTO(ISUM) = TFACTO(ISUM) + FACTOR
                  SFACTO(ISUM,ISYS) = FACTOR
              ENDDO
          ENDIF
      ENDDO

C     BLOOM algae

      IF ( NTYPA2 .GT. 0 ) THEN

C         BLOOM active!

          DO ITYP = 1,NTYPA2
              ISYS = IBLSUB(ITYP)
              FACTOR = NCRALG(ITYP)
              TFACTO(1) = TFACTO(1) + FACTOR
              SFACTO(1,ISYS) = FACTOR
              FACTOR = PCRALG(ITYP)
              TFACTO(2) = TFACTO(2) + FACTOR
              SFACTO(2,ISYS) = FACTOR
          ENDDO
      ENDIF

      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      SUBROUTINE UPDBAL ( IDUMP , NOTOT , IMASSA, ITERMS, IOFFSE,
     J                    BALANS, NOSUM , DMASSA, FACTOR, NTEL  ,
     J                    SFACTO, NOOUT , NOLAST)

c     IDUMP               index of current monitoring area
c     NOTOT               nr of substances to be processed
c     IMASSA              position of accumulation term per substance
c                         (used as indicator if substance has balance)
c     ITERMS              position of (first) balance term to be updated
c     IOFFSE              offset to be added to ITERMS
c     BALANS              mass balances array to be updated
c     NOSUM               nr. of sum parameters
c     DMASSA              fluxes to be added to mass balances
c     FACTOR              scale factor to be applied
c     NTEL                nr of terms to be updated
c     SFACTO              relations between sum parameters and state variables
c     NOOUT               total nr of mass balance terms
c     NOLAST              last state variable
c

      use timers
      INTEGER             IDUMP , NOTOT , NOSUM , NOOUT , IOFFSE, NTEL ,
     J                    NOLAST
      INTEGER             IMASSA(*), ITERMS(*)
      REAL                BALANS(NOOUT,*), DMASSA(NTEL,*),
     J                    FACTOR, SFACTO(NOSUM,*)

      INTEGER             ISYS  , IOUT  , ISYSS , IOUT2 , ISUM  ,
     J                    ITEST , ITEL  , ITEL2
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "updbal", ithandl )

      DO ISYS = 1 , NOTOT
          IOUT = ITERMS(ISYS) + IOFFSE
          DO ITEL = 1,NTEL
              ITEL2 = IOUT + (ITEL-1)
              BALANS(ITEL2,IDUMP) =
     J        BALANS(ITEL2,IDUMP) + DMASSA(ITEL,ISYS)*FACTOR
          ENDDO
          DO ISUM = 1,NOSUM
C              Bug fix, 5-1-2002
C              ISYSS = NOTOT+ISUM
              ISYSS = NOLAST+ISUM
              ITEST = IMASSA(ISYSS)
              IF ( ITEST .GT. 0 ) THEN
c                 Sum parameter is active
                  IF ( SFACTO(ISUM,ISYS) .GE. 0.0001 ) THEN
c                     Current substance contributes
                      IOUT2 = ITERMS(ISYSS) + IOFFSE
                      DO ITEL = 1,NTEL
                          ITEL2 = IOUT2 + (ITEL-1)
                          BALANS(ITEL2,IDUMP) = BALANS(ITEL2,IDUMP)
     J                    + DMASSA(ITEL,ISYS)*SFACTO(ISUM,ISYS)
     J                    * FACTOR
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO
      ENDDO

      if ( timon ) call timstop ( ithandl )
      RETURN
      END


