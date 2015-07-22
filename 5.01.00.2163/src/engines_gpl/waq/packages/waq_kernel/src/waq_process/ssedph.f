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

      subroutine ssedph ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sum of sedimentation flux of algae Dynamo - Bloom - GEM

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Jos van Gils
C     Date    : 940725             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     940725  Jos van Gils    Created
C     980428  Jos van Gils    Switch from d-fluxes to f-fluxes
C     980604  Nolte/vGils     Change for processing GEM as well
C     980717  Jos van Gils    Computation of VxSed added
c     121015  Jos van Gils    Differences for BLOOM and DYNAMO removed
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IFLUX , ISEG  , IKMRK1, IKMRK2, IN    , IP    , IP2   ,
     J         IALG  , IQ    , IVAN  , INAAR , IKMRKN, IKMRKV
      REAL     DEPTH , SEDCAR, SEDDM , SEDNIT, SEDPHO, SEDSIL, SEDSPE,
     J         CTODRY, NCRAT , PCRAT , SCRAT , TOTFLX, TOTCON, CONSPE,
     J         VELSPE
C
C     Local
C
      INTEGER  NALG
C
      NALG  = NINT(PMSA(IPOINT(1)))
      IFLUX = 0
      IP2   = IPOINT(  2 )

      DO 9000 ISEG = 1 , NOSEG
      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
          DEPTH   = PMSA(IP2)
          SEDCAR  = 0.0
          SEDDM   = 0.0
          SEDNIT  = 0.0
          SEDPHO  = 0.0
          SEDSIL  = 0.0
          DO 100 IALG = 1,NALG

              IN = 2 + 0*NALG + IALG
              SEDSPE = PMSA( IPOINT(IN) + (ISEG-1)*INCREM(IN) )
              IN = 2 + 1*NALG + IALG
              CTODRY = PMSA( IPOINT(IN) + (ISEG-1)*INCREM(IN) )

              SEDCAR = SEDCAR + SEDSPE
              SEDDM  = SEDDM  + SEDSPE*CTODRY

c              IF ( NALG .GT. 6 ) THEN

                 IN = 2 + 2*NALG + IALG
                 NCRAT  = PMSA( IPOINT(IN) + (ISEG-1)*INCREM(IN) )
                 IN = 2 + 3*NALG + IALG
                 PCRAT  = PMSA( IPOINT(IN) + (ISEG-1)*INCREM(IN) )
                 IN = 2 + 4*NALG + IALG
                 SCRAT  = PMSA( IPOINT(IN) + (ISEG-1)*INCREM(IN) )

                 SEDNIT = SEDNIT + SEDSPE*NCRAT
                 SEDPHO = SEDPHO + SEDSPE*PCRAT
                 SEDSIL = SEDSIL + SEDSPE*SCRAT

c              ENDIF

  100     CONTINUE

          IP =  IPOINT(2+7*NALG+1) + (ISEG-1)*INCREM(2+7*NALG+1)
          PMSA (IP) = SEDCAR
          IP =  IPOINT(2+7*NALG+2) + (ISEG-1)*INCREM(2+7*NALG+2)
          PMSA (IP) = SEDDM

C         NO LONGER Define fluxes only for Bloom (NALG .GT. 6)

c          IF (NALG.GT.6) THEN
             IF (DEPTH .GT. 0.0) THEN
                FL(IFLUX+1) = SEDCAR/DEPTH
                FL(IFLUX+2) = SEDNIT/DEPTH
                FL(IFLUX+3) = SEDPHO/DEPTH
                FL(IFLUX+4) = SEDSIL/DEPTH
             ELSE
                FL(IFLUX+1) = 0.0
                FL(IFLUX+2) = 0.0
                FL(IFLUX+3) = 0.0
                FL(IFLUX+4) = 0.0
             ENDIF
c          ENDIF

      ENDIF
      ENDIF
      IFLUX = IFLUX + NOFLUX
      IP2   = IP2   + INCREM(  2 )
c
 9000 CONTINUE
c
c.....Exchangeloop over de horizontale richting ter initialisatie
      DO 8000 IQ=1,NOQ1+NOQ2+NOQ3

          IP =  IPOINT(2+7*NALG+3) + (IQ-1)*INCREM(2+7*NALG+3)
          PMSA (IP) = 0.0

 8000 CONTINUE

c.....Exchangeloop over de verticale richting
      DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3+NOQ4

         IVAN  = IEXPNT(1,IQ)
         INAAR = IEXPNT(2,IQ)

C        Zoek eerste kenmerk van- en naar-segmenten

         IF ( IVAN.GT.0 .AND. INAAR.GT.0 ) THEN
         CALL DHKMRK(1,IKNMRK(IVAN ),IKMRKV)
         CALL DHKMRK(1,IKNMRK(INAAR),IKMRKN)
         IF (IKMRKV.EQ.1.AND.IKMRKN.EQ.1 .OR.
     +       IKMRKV.EQ.1.AND.IKMRKN.EQ.3) THEN

C            Water-water uitwisseling

           TOTFLX = 0.0
           TOTCON = 0.0
           DO 7100 IALG = 1,NALG
             IP = IPOINT(2+5*NALG+IALG) + (IVAN-1)*INCREM(2+5*NALG+IALG)
             CONSPE = PMSA( IP )
             IP = IPOINT(2+6*NALG+IALG) + (IQ-1)*INCREM(2+6*NALG+IALG)
             VELSPE = PMSA( IP )
             TOTFLX = TOTFLX + CONSPE*VELSPE
             TOTCON = TOTCON + CONSPE
 7100      CONTINUE
           IP = IPOINT(2+7*NALG+3) + (IQ-1)*INCREM(2+7*NALG+3)
           IF ( TOTCON .GT. 0.0 ) THEN
             PMSA(IP) = TOTFLX/TOTCON
           ELSE
             PMSA(IP) = 0.0
           ENDIF
         ENDIF
         ENDIF

 7000 CONTINUE
      RETURN
C
      END
