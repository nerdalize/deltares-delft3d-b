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

      subroutine vtrans ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Vertical distribution after a longer time span to correct 3D-BLOOM

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : 3d-licht
C     Author  : Jan van Beek
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     200717  Jan van Beek    initial version
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C NOLAY   I*4 1 I     number of layers
C
C***********************************************************************

      USE      DATA_VTRANS

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declarations
C
      INTEGER  IERR_ALLOC      , LUNREP
      INTEGER  IP1   , IP2   , IP3   , IP4   , IP5   ,
     +         IP6   , IP7   , IP8   , IP9   , IP10  ,
     +         IP11  , IP12
      INTEGER  IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8   , IN9   , IN10  ,
     +         IN11  , IN12
      INTEGER  IDT   , NOLAY , NOSEGL, NOQ   , NOQ12 ,
     +         ILAY  , ISEGL , ISEG  , IFROM , ITO   ,
     +         NOSUB , ISUB  , IQ    , IQTEST, IKMRK1,
     +         NOSEGW, IKMRK2, ITIME
      REAL     DISP  , AREA  , LENFR , LENTO , AL    ,
     +         E     , DIAG  , CODIAG, RHS   , VOLUME,
     +         DELT  , PERIOD
      LOGICAL     LFOUND
      CHARACTER   CDUMMY
      REAL        RDUMMY
      logical                  :: l_initial
      logical, save            :: l_restart
      character(len=256)       :: file_initial
      character(len=256), save :: file_restart
      integer                  :: ilun
      integer                  :: nosegi
      integer                  :: nolayi
      integer                  :: idummy
      integer                  :: ierr2
      logical                  :: dhltim
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP5  = IPOINT( 5)
      IP11 = IPOINT(11)
C
      IDT    = NINT(PMSA(IP1))
      DELT   =      PMSA(IP2)
      PERIOD =      PMSA(IP3)/24.
      ITIME  = NINT(PMSA(IP5))

      CALL DHNOSEG(NOSEGW)
      CALL DHNOLAY(NOLAY)
C
C     initialise and allocate memory in module data_vtrans
C
      IF ( .NOT. INIT_VTRANS ) THEN
         INIT_VTRANS = .TRUE.
         CALL GETMLU(LUNREP)
         IF ( NOQ3 .GT. 0 ) THEN
            NOSEGL = NOSEGW/NOLAY
            ACTIVE_VTRANS=.TRUE.
            PMSA(IP11) = 1.0
            IF ( NOSEGL*NOLAY .NE. NOSEGW ) THEN
               WRITE(LUNREP,*) ' WARNING unstructured 3D application'
               WRITE(LUNREP,*) ' Vertical distribution routine VTRANS not possible'
               NOLAY = 1
               ACTIVE_VTRANS=.FALSE.
               PMSA(IP11) = 0.0
            ENDIF
         ELSE
            WRITE(LUNREP,*) ' WARNING 2D application'
            WRITE(LUNREP,*) ' Vertical distribution routine VTRANS not possible'
            NOLAY = 1
            ACTIVE_VTRANS=.FALSE.
            PMSA(IP11) = 0.0
         ENDIF
         IF ( ACTIVE_VTRANS ) THEN
            NOLAYLOCAL = NOLAY
            NOSEGLOCAL = NOSEG
            ALLOCATE(CONCV(NOLAY,NOSEG),
     +               TIMEV(NOLAY,NOSEG),
     +               FRACV(NOLAY,NOSEG),
     +               DERVV(NOLAY,NOSEG),
     +               STAT=IERR_ALLOC)
            IF ( IERR_ALLOC .NE. 0 ) THEN
               WRITE ( LUNREP , 1000 ) IERR_ALLOC
               WRITE ( LUNREP , 1001 ) NOSEG
               WRITE ( LUNREP , 1002 ) NOLAY
               CALL SRSTOP(1)
            ENDIF

            ! read initial file?

            call getcom('-vtrans_initial', 3 , l_initial, idummy, rdummy, file_initial, ierr2)
            call getcom('-vtrans_restart', 3 , l_restart, idummy, rdummy, file_restart, ierr2)
            if ( l_initial ) then
               write(lunrep,*) 'vtrans using initial condition from file:',trim(file_initial)
               call dhnlun(200,ilun)
               open(ilun,file=file_initial,form='binary')
               read(ilun) nosegi, nolayi
               read(ilun) timtot
               read(ilun) concv
               read(ilun) timev
               read(ilun) fracv
               close(ilun)
            else

               ! initialise concentration level 1.0 in the specific layer, 0.0 rest layers, init timev 0.0

               CONCV=0.0
               TIMEV=0.0
               FRACV=0.0
               TIMTOT = 0.0
               DO ILAY = 1 , NOLAY
                  ISEGL = (ILAY-1)*NOSEGL+1
                  CONCV(ILAY,ISEGL:ISEGL+NOSEGL-1)=1.0
               ENDDO
            endif
            if ( l_restart ) then
               write(lunrep,*) 'vtrans will write restart condition to file:',trim(file_restart)
            endif
         ENDIF
      else
         l_initial = .false.
      ENDIF
C
      IF ( .NOT. ACTIVE_VTRANS ) RETURN
C
      NOLAY  = NOLAYLOCAL
      NOSEGL = NOSEGW/NOLAY
      NOSUB  = NOLAY
      NOQ    = NOQ1 + NOQ2 + NOQ3
      NOQ12  = NOQ1 + NOQ2

      ! not the first time if initialised to prevent double step

      if ( .not. l_initial ) then
C
C        make masses , volumes on diagonal + test for active exchange for z model
C
         IN4  = INCREM(4)
         IP4  = IPOINT(4)
         DO ISEG = 1 , NOSEGW
               VOLUME = PMSA(IP4)
               DO ILAY = 1 , NOLAY
                  CONCV(ILAY,ISEG) = CONCV(ILAY,ISEG) * VOLUME
                  DERVV(ILAY,ISEG) = VOLUME
               ENDDO
            IP4 = IP4 + IN4
         ENDDO
C
C        do a transport step in the vertical, dispersion only, double sweep see also DLWQD1
C
         IN6  = INCREM(6)
         IN7  = INCREM(7)
         IN8  = INCREM(8)
         IN9  = INCREM(9)
         IN10 = INCREM(10)
         IP6  = IPOINT(6) + NOQ12*IN6
         IP7  = IPOINT(7) + NOQ12*IN7
         IP8  = IPOINT(8) + NOQ12*IN8
         IP9  = IPOINT(9) + NOQ12*IN9
         IP10 = IPOINT(10)+ NOQ12*IN10
         DO IQ = NOQ12+1,NOQ
            IFROM = IEXPNT(1,IQ)
            ITO   = IEXPNT(2,IQ)
            IF ( IFROM .GT. 0 .AND. ITO .GT. 0 ) THEN
               CALL DHKMRK(1,IKNMRK(ITO),IKMRK1)
               IF (IKMRK1.EQ.1) THEN
                  DISP  = PMSA(IP6) + PMSA(IP10)
               ELSE
                  DISP  = 0.0
               ENDIF
               AREA  = PMSA(IP7)
               LENFR = PMSA(IP8)
               LENTO = PMSA(IP9)
C
               AL = LENFR + LENTO
               E  = IDT*DISP*AREA/AL
               DO ISUB=1,NOSUB
C
C                 row of the 'from' segment
C
                  DIAG              = DERVV(ISUB,IFROM) + E
                  CODIAG            = -E / DIAG
                  RHS               = CONCV(ISUB,IFROM) / DIAG
                  DERVV(ISUB,IFROM) = CODIAG
                  CONCV(ISUB,IFROM) = RHS
C                 row of the 'to  ' segment
                  DERVV(ISUB,ITO)   = DERVV(ISUB,ITO) + E + E*CODIAG
                  CONCV(ISUB,ITO)   = CONCV(ISUB,ITO) + E*RHS
               ENDDO
            ENDIF
            IP6  = IP6  + IN6
            IP7  = IP7  + IN7
            IP8  = IP8  + IN8
            IP9  = IP9  + IN9
            IP10 = IP10 + IN10
         ENDDO
C
C            Loop over exchanges, single sweep backward
C
         DO IQ = NOQ , NOQ12+1 , -1
            IFROM = IEXPNT(1,IQ)
            ITO   = IEXPNT(2,IQ)
            IF ( IFROM .GT. 0 .AND. ITO .GT. 0 ) THEN
               DO ISUB=1,NOSUB
                  CODIAG            = DERVV(ISUB,IFROM)
                  DIAG              = DERVV(ISUB,ITO)
                  RHS               = CONCV(ISUB,ITO)/DIAG
                  CONCV(ISUB,IFROM) = CONCV(ISUB,IFROM) - CODIAG*RHS
                  CONCV(ISUB,ITO)   = RHS
                  DERVV(ISUB,IFROM) = 1.0
                  DERVV(ISUB,ITO)   = 1.0
               ENDDO
            ENDIF
         ENDDO

         do iseg = 1, nosegw      !  for if some diagonal entries are not 1.0
            do ilay = 1, nolay
               concv(ilay,iseg) = concv(ilay,iseg) / dervv(ilay,iseg)
               dervv(ilay,iseg) = 1.0
            enddo
         enddo
C
C        cummulate time
C
         TIMEV = TIMEV + CONCV*DELT
         TIMTOT = TIMTOT + DELT
C
C        if accumulated time equal or greater then accumulation period then calculate fraction of time
C        and reset the distribution
C
         IF ( TIMTOT .GE. (PERIOD-DELT*0.5) ) THEN
            FRACV = TIMEV/TIMTOT
            CONCV=0.0
            TIMEV=0.0
            TIMTOT = 0.0
            DO ILAY = 1 , NOLAY
               ISEGL = (ILAY-1)*NOSEGL+1
               CONCV(ILAY,ISEGL:ISEGL+NOSEGL-1)=1.0
            ENDDO
         ENDIF

      endif
C
C     output IP11 is switch for the PLCT/BLOOM
C     furthermore there is a max of 100 output, the fraction of time
C
      DO ISEG = 1 , NOSEG
         DO ILAY = 1 , MIN(100,NOLAY)
            IP12 = IPOINT(11+ILAY)+(ISEG-1)*INCREM(11+ILAY)
            PMSA(IP12) = FRACV(ILAY,ISEG)
         ENDDO
      ENDDO

      ! write restart file? at last time

      if ( l_restart ) then
         if ( dhltim(itime,idt) ) then
            call dhnlun(200,ilun)
            open(ilun,file=file_restart,form='binary')
            write(ilun) noseg, nolay
            write(ilun) timtot
            write(ilun) concv
            write(ilun) timev
            write(ilun) fracv
            close(ilun)
         endif
      endif
C
      RETURN
 1000 FORMAT(' ERROR: allocating memory in VTRANS :',I10)
 1001 FORMAT(' NOSEG = ',I10)
 1002 FORMAT(' NOLAY = ',I10)
      END
