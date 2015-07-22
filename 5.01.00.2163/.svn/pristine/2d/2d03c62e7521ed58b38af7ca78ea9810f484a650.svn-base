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

      subroutine somsed ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Total of all sedimenting substances

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Pascal Boderie
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     930406  Pascal Boderie  Create first version
C     940722  Jos van Gils    Removed computation of phytoplankton flux
C     980605  Arno Nolte      Removed computation of POC flux (--> SSedPOC)
C             Jos van Gils    and removed AAP flux from fSedDM
C     980717  Jos van Gils    Computation of VxSedTIM added
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C DMCFy   R*4 1 I  conversion factor for gX->dry matter subst y   [gDM/gX]
C FLXy    R*4 1 I  sedimentation flux substance y                [gX/m3/d]
C TDMSED  R*4 1 O  total dry matter sedimentation flux          [gDM/m2/d]
C TIMSED  R*4 1 O  total inorganic mattter sedimentation flux   [gDM/m2/d]

C     Logical Units : -
C     Modules called : -
C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT none

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( 37 ) , INCREM(37) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IP(37)
      REAL     FLX1  , FLX2  , FLX3  , FLPOC , FLPOM , FLALGC,
     J         FLALGM, DMCF1 , DMCF2 , DMCF3 , TIMSED, TDMSED, POCSED,
     J         C1    , C2    , C3    , V1    , V2    , V3    , CTOT,
     J         FLPOC1, FLPOC2, FLPOC3, FLPOC4, DMPOC1, DMPOC2, DMPOC3,
     J         DMPOC4, POC1  , POC2  , POC3  , POC4  , CPTOT ,
     J         CP1   , VP1   ,
     J         CP2   , VP2   ,
     J         CP3   , VP3   ,
     J         CP4   , VP4   
      INTEGER  IFLUX , ISEG  , IKMRK1, IKMRK2, IQ    , IVAN  , INAAR
      INTEGER  IKMRKN, IKMRKV

      IP = IPOINT
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C

      FLX1    = PMSA(IP(1 ))
      FLX2    = PMSA(IP(2 ))
      FLX3    = PMSA(IP(3 ))
      FLPOC1  = PMSA(IP(4 ))
      FLPOC2  = PMSA(IP(5 ))
      FLPOC3  = PMSA(IP(6 ))
      FLPOC4  = PMSA(IP(7 ))
      FLALGC  = PMSA(IP(8 ))
      FLALGM  = PMSA(IP(9 ))
      DMCF1   = PMSA(IP(10))
      DMCF2   = PMSA(IP(11))
      DMCF3   = PMSA(IP(12))
      DMPOC1  = PMSA(IP(13))
      DMPOC2  = PMSA(IP(14))
      DMPOC3  = PMSA(IP(15))
      DMPOC4  = PMSA(IP(16))

C*******************************************************************************
C**** Calculations connected to the sedimentation
C***********************************************************************

C    Calculate som sedimentation of dry matter
      TIMSED = FLX1 * DMCF1 +
     &         FLX2 * DMCF2 +
     &         FLX3 * DMCF3
      FLPOC  = FLPOC1 + FLPOC2 + FLPOC3 + FLPOC4
      FLPOM  = FLPOC1*DMPOC1 + FLPOC2*DMPOC2 + FLPOC3*DMPOC3 
     J                                       + FLPOC4*DMPOC4

      TDMSED = TIMSED + FLPOM + FLALGM

      POCSED = FLPOC + FLALGC

      PMSA (IP(31)) = TDMSED
      PMSA (IP(32)) = TIMSED
      PMSA (IP(33)) = POCSED
      PMSA (IP(34)) = FLPOC
      PMSA (IP(35)) = FLPOM

      ENDIF
      ENDIF
      IFLUX = IFLUX + NOFLUX
      IP    = IP    + INCREM 
c
 9000 CONTINUE
c

c.....Exchangeloop over de horizontale richting
      IP = IPOINT
      DO 8000 IQ=1,NOQ1+NOQ2
         PMSA(IP(36)) = 0.0
         PMSA(IP(37)) = 0.0
         IP = IP + INCREM
 8000 CONTINUE

c.....Exchangeloop over de verticale richting
      DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3

         PMSA(IP(36)) = 0.0
         PMSA(IP(37)) = 0.0
         IVAN  = IEXPNT(1,IQ)
         INAAR = IEXPNT(2,IQ)

C        Zoek eerste kenmerk van- en naar-segmenten

         IF ( IVAN .GT. 0 .AND. INAAR .GT. 0 ) THEN
         CALL DHKMRK(1,IKNMRK(IVAN ),IKMRKV)
         CALL DHKMRK(1,IKNMRK(INAAR),IKMRKN)
         IF (IKMRKV.EQ.1.AND.IKMRKN.EQ.1) THEN

C            Water-water uitwisseling

             C1 = PMSA(IPOINT(17)+(IVAN-1)*INCREM(17))
             C2 = PMSA(IPOINT(18)+(IVAN-1)*INCREM(18))
             C3 = PMSA(IPOINT(19)+(IVAN-1)*INCREM(19))
             CP1 = PMSA(IPOINT(20)+(IVAN-1)*INCREM(20))
             CP2 = PMSA(IPOINT(21)+(IVAN-1)*INCREM(21))
             CP3 = PMSA(IPOINT(22)+(IVAN-1)*INCREM(22))
             CP4 = PMSA(IPOINT(23)+(IVAN-1)*INCREM(23))
             V1 = PMSA(IP(24))
             V2 = PMSA(IP(25))
             V3 = PMSA(IP(26))
             VP1 = PMSA(IP(27))
             VP2 = PMSA(IP(28))
             VP3 = PMSA(IP(29))
             VP4 = PMSA(IP(30))
             CTOT = C1 + C2 + C3
             CPTOT = CP1 + CP2 + CP3 + CP4
             IF ( CTOT .GT. 0.0 ) 
     J       PMSA(IP(36)) = ( C1*V1+C2*V2+C3*V3 ) / CTOT
             IF ( CPTOT .GT. 0.0 ) 
     J       PMSA(IP(37)) = ( CP1*VP1+CP2*VP2+CP3*VP3+CP4*VP4 ) / CPTOT
         ENDIF
         ENDIF

         IP = IP + INCREM

 7000 CONTINUE


      RETURN
C
      END
