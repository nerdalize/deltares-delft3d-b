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

C    Version 0.3 16 August 2010
C    Version 0.2 22 July 1994
C    Version 0.1 7 Januari 1994
C    Program:    BLMORT.FOR
C    Programmer: Jos van Gils
C
C    Compute fluxes associated with mortality
C
C    Called by: BLOOMC
C    Calls    : NATMOR

      SUBROUTINE BLMORT (BIOMAS, TEMP  , FAUT  , FDET  , FLAUTN, FLDETN,
     J                   FLOOXN, FLMORA, DEAT4 , TSTEPI)

C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     BIOMAS  R*4   NUSPEC   I    Biomass (gC/m3)
C     TEMP    R*4   1        I    Temperature (deg.C)
C     FAUT    R*4   NUSPEC   I    Fraction autolysis (-)
C     FDET    R*4   NUSPEC   I    Fraction detritus (-)
C     FLAUTN  R*4   4        O    Nutrient autolysis fluxes (g/m3/d)
C     FLDETN  R*4   4        O    Detritus production fluxes (g/m3/d)
C     FLOOXN  R*4   4        O    OOX production fluxes (g/m3/d)
C     FLMORA  R*4   NUSPEC   O    Algae mortality fluxes (gC/m3/d)
C     DEAT4   R*4   1        O    ??$Check necessity to transfer$
C     TSTEPI  R*4   1        I    Time step (d)

      REAL            BIOMAS(*), TEMP, FAUT(*), FDET(*), FLAUTN(*),
     J                FLDETN(*), FLOOXN(*), FLMORA(*), DEAT4, TSTEPI

C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     NUSPEC  I     1        I    phyt2     Number of types
C     RMORT   R*8   MT       O    size      Mortality rate (1/day)
C     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
C     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     TEMP8   R*8   1             Temperature (deg.C)
C     DEAT    R*8   1             ??
C     ZOODD   R*8   1             Dummy??$Check$
C     CPHYT   R*4   1             Biomass (gC/m3)
C     CMORT   R*4   1             Mortality flux (gC/m3/d)
C     CMORTA  R*4   1             Autolysis flux (gC/m3/d)
C     CMORTD  R*4   1             Detritus prod. (gC/m3/d)
C     CMORTO  R*4   1             OOx production (gC/m3/d)
C     J       I     1

      REAL            CMORT , CMORTA, CMORTD, CMORTO, CPHYT
      REAL*8          TEMP8 , ZOODD , DEAT
      INTEGER         J
C
C  Zero fluxes
C
      DO 1 J = 1,4
         FLAUTN(J) = 0.0
         FLDETN(J) = 0.0
         FLOOXN(J) = 0.0
    1 CONTINUE
C
C  Call subroutine NATMOR: calculate natural mortality rate constants.
C
      DEAT  = 0D0
      ZOODD = 0D0
      TEMP8 = DBLE(TEMP)
      CALL NATMOR ( DEAT  , ZOODD , TEMP8 , 1)
      DEAT4 = SNGL(DEAT)
C
C  Mortality module.
C
C  Objective: obtain nutrient fluxes to detritus, OOx and dissolved
C  nutrient pools due to mortality.
C
C  Again note that nutrient fluxes are computed from BLOOM's
C  stochiometry matrix and hence follow from biomasses in units dry
C  weight. The biomass mortality flux for DLWQWQ, however, is in units
C  of carbon.
C
C  Loop over algae species

      DO 220 J=1,NUSPEC
C$ For the sake of testing we allow mortality with small negative
C$ biomasses. This should be corrected eventually
         CPHYT = MAX ( BIOMAS(J) , 0.0 )
c        CPHYT =       BIOMAS(J)

C  Compute total mortality for this species and store the flux
c  JvG 16-8-2010 avoid undershoots leading to negative biomass

         CMORT = MIN ( CPHYT * SNGL(RMORT(J)) , CPHYT/TSTEPI )
         FLMORA(J) = CMORT
C
C Partition the mortality flux over detritus(D)/OOx(O)/autolysis(A)
C
         FOOX   = (1. - FAUT(J) - FDET(J))
         CMORTA = CMORT * FAUT(J)
         CMORTD = CMORT * FDET(J)
         CMORTO = CMORT * FOOX
C
C Detritus production for C, N, P, Si (for C including part autolysis)
C
         FLDETN(1) = FLDETN(1) + CMORTD + CMORTA *FDET(J)/(FDET(J)+FOOX)
         FLDETN(2) = FLDETN(2) + CMORTD * SNGL(CTODRY(J)*AA(1,J))
         FLDETN(3) = FLDETN(3) + CMORTD * SNGL(CTODRY(J)*AA(2,J))
         FLDETN(4) = FLDETN(4) + CMORTD * SNGL(CTODRY(J)*AA(3,J))
C        DETN,DETP or FIXN
         IF (NUNUCO.GE.4) FLDETN(2) = FLDETN(2) +
     1                                CMORTD * SNGL(CTODRY(J)*AA(4,J))
         IF (NUNUCO.GE.5) FLDETN(3) = FLDETN(3) +
     1                                CMORTD * SNGL(CTODRY(J)*AA(5,J))
         IF (NUNUCO.EQ.6) FLDETN(2) = FLDETN(2) +
     1                                CMORTD * SNGL(CTODRY(J)*AA(6,J))
C
C Autolysis for C, N, P, Si (0 for carbon)
C
         FLAUTN(1) = FLAUTN(1) + 0.0
         FLAUTN(2) = FLAUTN(2) + CMORTA * SNGL(CTODRY(J)*AA(1,J))
         FLAUTN(3) = FLAUTN(3) + CMORTA * SNGL(CTODRY(J)*AA(2,J))
         FLAUTN(4) = FLAUTN(4) + CMORTA * SNGL(CTODRY(J)*AA(3,J))
C        DETN,DETP or FIXN
         IF (NUNUCO.GE.4) FLAUTN(2) = FLAUTN(2) + CMORTA *
     1                                SNGL(CTODRY(J)*AA(4,J))
         IF (NUNUCO.GE.5) FLAUTN(3) = FLAUTN(3) + CMORTA *
     1                                SNGL(CTODRY(J)*AA(5,J))
         IF (NUNUCO.EQ.6) FLAUTN(2) = FLAUTN(2) + CMORTA *
     1                                SNGL(CTODRY(J)*AA(6,J))
C
C OOx production for C, N, P, Si (for C including part autolysis)
C
         FLOOXN(1) = FLOOXN(1) + CMORTO + CMORTA * FOOX / (FDET(J)+FOOX)
         FLOOXN(2) = FLOOXN(2) + CMORTO * SNGL(CTODRY(J)*AA(1,J))
         FLOOXN(3) = FLOOXN(3) + CMORTO * SNGL(CTODRY(J)*AA(2,J))
         FLOOXN(4) = FLOOXN(4) + CMORTO * SNGL(CTODRY(J)*AA(3,J))
C        DETN,DETP or FIXN
         IF (NUNUCO.GE.4) FLOOXN(2) = FLOOXN(2) + CMORTO *
     1                                SNGL(CTODRY(J)*AA(4,J))
         IF (NUNUCO.GE.5) FLOOXN(3) = FLOOXN(3) + CMORTO *
     1                                SNGL(CTODRY(J)*AA(5,J))
         IF (NUNUCO.EQ.6) FLOOXN(2) = FLOOXN(2) + CMORTO *
     1                                SNGL(CTODRY(J)*AA(6,J))

  220 CONTINUE

      RETURN
      END

