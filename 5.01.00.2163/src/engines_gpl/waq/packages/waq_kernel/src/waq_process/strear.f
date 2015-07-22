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

      subroutine strear ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Aeration at weirs (Gameson and Nakasone) (input is array of structures)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |          Inland water systems          |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Q2943.00 Osiris
C     Author  : Annette Kuin
C     Date    : 030320             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     030320  Annette Kuin    Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C NOSTR   R*4 1 I number of structures                                 [-]
C TEMP    R*4 1 I ambient temperature                                 [°C]
C OXYDN   R*4 1 I oxygen concentration downstream of structure     [gO/m3]
C OXYUP   R*4 1 I oxygen concentration upstream of structure       [gO/m3]
C BOD5    R*4 1 I calculated carbonaceous BOD at 5 days            [gO/m3]
C DELT    R*4 1 I DELWAQ timestep                                    [scu]
C CSAT    R*4 1 I saturation concentration of oxygen               [gO/m3]
C b       R*4 1 I dam reaeration coefficient                           [-]
C DISCH   R*4 1 I discharge over structure                          [m3/s]
C WIDTH   R*4 1 I width of structure                                   [m]
C DEPTH   R*4 1 I depth in delwaq segment                              [m]
C SWAER   R*4 1 I switch for gameson (0) or hybride (1)                [-]
C WLL     R*4 1 I water level in delwaq segment left of structure      [m]
C WLR     R*4 1 I water level in delwaq segment right of structure     [m]
C UPWL    R*4 1 L water level in delwaq segment upstream of structure  [m]
C DNWL    R*4 1 L water level in delwaq segment downstream of structure[m]
C SEGL    R*4 1 I segment number left of structure                     [-]
C SEGR    R*4 1 I segment number right of structure                    [-]
C UPSEG   R*4 1 L segment number upstream of structure                 [-]
C DNSEG   R*4 1 L segment number downstream of structure               [-]
C WLDIF   R*4 1 I difference in water level up- and downstream of struc[m]
C a       R*4 1 L water quality factor                                 [-]
C OXYDR   R*4 1 L oxygen deficit ratio                                 [-]
C LOGNAK  R*4 1 L logarithm of deficit ratio by Nakasone               [-]
C DRNAK   R*4 1 L oxygen deficit ratio by Nakasone                     [-]
C OXYCAL  R*4 1 L oxygen concentration after dam aeration            [g/d]
C OXYPL   R*4 1 O oxygen production flux at weirs               [gO2/m3/d]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
      REAL     PMSA  (*) , FL  (*)
      INTEGER  NOSEG , NOFLUX, NOQ1, NOQ2, NOQ3, NOQ4
      INTEGER  IPOINT(*)       , INCREM(*),
     +         IEXPNT(4,*)     , IKNMRK(*)
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,
     +         IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8
      INTEGER  ITEL, ISTRUC, NOSTR, SEGL, SEGR, UPSEG, DNSEG
      INTEGER  LUNREP
      REAL     WLL, WLR
      REAL     OXYDN, OXYUP, DELT, CSAT, BOD5, B, UPWL, DNWL,
     +         TEMP, DISCH, WIDTH, DEPTH, WLDIF, A, SWAER,
     +         OXYDR, LOGNAK, DRNAK, OXYCAL, OXYPL
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
C
      NOSTR = NINT(PMSA(IP1))
C
      IF (NOSTR .GT. 100.0) THEN
         CALL GETMLU(lunrep)
         write(lunrep,*) 'Error: Number of structures',
     +                   ' greater than 100'
         CALL SRSTOP(1)
      ENDIF
C
C     segment loop over structures-------------------------------
C
      DO 9000 ISTRUC = 1 , NOSTR
C
C
         ITEL = 8 + (ISTRUC - 1) * 7
C
C        read input structure---------------------------------------
C
         DISCH = PMSA(IPOINT(ITEL + 1))
         WLL   = PMSA(IPOINT(ITEL + 2))
         WLR   = PMSA(IPOINT(ITEL + 3))
         SEGL  = NINT(PMSA(IPOINT(ITEL + 4)))
         SEGR  = NINT(PMSA(IPOINT(ITEL + 5)))
         b     = PMSA(IPOINT(ITEL + 6))
         WIDTH = PMSA(IPOINT(ITEL + 7))
C
C        Oxygen production if discharge over crest is larger than
C        zero. Aeration takes place in downstream segment-----------
C
         IF (DISCH .GT. 0.0) THEN
             UPSEG = SEGL
             DNSEG = SEGR
             UPWL  = WLL
             DNWL  = WLR
         ELSEIF (DISCH .LT. 0.0) THEN
             UPSEG = SEGR
             DNSEG = SEGL
             UPWL  = WLR
             DNWL  = WLL
         ELSE
             GOTO 9000
         ENDIF
C
         WLDIF = UPWL - DNWL
C
C        reading input of segment downstream of structure-----------
         OXYDN  = MAX(0.0, PMSA(IP2 + (DNSEG - 1) * IN2))
         DELT   = PMSA(IP3 + (DNSEG -1) * IN3)
         CSAT   = PMSA(IP4 + (DNSEG -1) * IN4)
         BOD5   = MAX(0.0, PMSA(IP5 + (DNSEG -1) * IN5))
         TEMP   = PMSA(IP6 + (DNSEG -1) * IN6)
         DEPTH  = PMSA(IP7 + (DNSEG -1) * IN7)
         SWAER  = PMSA(IP8 + (DNSEG -1) * IN8)
C

C        reading input of segment upstream of structure-----------
C
         OXYUP  = MAX(0.0, PMSA(IP2 + (UPSEG -1) * IN2))
C
C        calculation a, a factor for the contamination of the water-
C
         a = MIN(1.90 / (BOD5 ** 0.44) , 1.80)
C
C        Switch for gameson (SWAER = 0) or hybride of gameson and
C        nakasone (SWAER = 1)-----------------------------------
C
         IF (SWAER .LT. 0.5) THEN
C
C            SWAER = 0, gameson---------------------------------
C
             OXYDR = 1.0 + 0.38 * a * b * WLDIF *
     j       (1 - 0.11 * WLDIF) * (1 + 0.046 * TEMP)
C
         ELSE
C
C            SWAER = 1, hybride of gameson and nakasone---------
C
             LOGNAK = 0.0675 * (WLDIF ** 1.28) *
     j       ((DISCH / WIDTH * 3600) ** 0.62) *
     j       (DEPTH ** 0.439)

C
             DRNAK = EXP(LOGNAK)

C
             OXYDR = ((DRNAK - 1) * a * b * (1 + 0.02 *
     j       (TEMP - 20))) + 1
C
         ENDIF

C
C        Oxygen concentration downstream of structure-----------
C
         OXYCAL = (CSAT * (OXYDR - 1) + OXYUP) / OXYDR
C
C        Calculate flux-----------------------------------------
C
         OXYPL = (OXYCAL - OXYDN) / DELT
         FL(1 + (DNSEG - 1) * NOFLUX) = OXYPL
C
 9000 CONTINUE
C
      RETURN
C
      END
