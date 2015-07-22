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

      subroutine heatfl ( pmsa   , fl     , ipoint , increm, noseg ,
     &                    noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                    noq3   , noq4   )
!>\file
!>       Total heat flux for surface water absolute temperature model

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Temperatuursmodellering Maas 2003 Q3527
C     Author  : Annette Kuin
C     Date    : 030707             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     030707  Annette Kuin    total heat flux for surface water
C     161006  PBO3            Bowen ratio
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C beta    R*4 1 L Bowens constant                                [mbar/°C]
C cloud   R*4 1 L Cloud coverage (fraction)                            [-]
C cpa     R*4 1 I Specific heat capacity of air                   [J/kg,K]
C cp      R*4 1 I Specific heat capacity of water                [J/kg,°C]
C CwindA  R*4 1 I Coefficient in wind function a                       [-]
C CwindB  R*4 1 I Coefficient in wind function b                       [-]
C CwindC  R*4 1 I Coefficient in wind function c                       [-]
C DeltaT  R*4 1 I Correction factor for TempWa near discharge          [-]
C Depth   R*4 1 I Water depth of segment                               [m]
C dTemp   R*4 1 O Flux on water temperature                         [°C/d]
C Emiss   R*4 1 L Emissivity (colour factor) for the atmosphere        [-]
C EWater  R*4 1 I Emissivity (colour factor) of surface water          [-]
C Fa      R*4 1 I Reflection factor for Qa                             [-]
C Fsw     R*4 1 I Reflection factor for Qsw                            [-]
C Fwind   R*4 1 L Wind function for evaporation                 [m/s,mbar]
C Fwind2  R*4 1 L Wind function for evaporation, incl corr.    [W/m2,mbar]
C Ha      R*4 1 I Height of wind speed needed for wind function        [m]
C Hm      R*4 1 I Height at which wind speed is measured               [m]
C HTVap   R*4 1 L Heat of evaporation                               [J/kg]
C HTVap   R*4 1 L Heat of evaporation at reference temperature      [J/kg]
C K       R*4 1 I Roughness coef for calculation vwind                 [-]
C Patm    R*4 1 I Air pressure                                 [mb of HPa]
C Pvap    R*4 1 L Actual vapour pressure in air                     [mbar]
C Psvap   R*4 1 L Saturated vapour pressure in air                  [mbar]
C PvapWa  R*4 1 L Saturated vapour pressure at water-air interface  [mbar]
C Qa      R*4 1 L Long wave atmospheric radiation reaching water    [W/m2]
C Qan     R*4 1 L Non refl. (net) atm. long wave rad. reaching water[W/m2]
C Qbr     R*4 1 L Long wave back radiation from water               [W/m2]
C Ql      R*4 1 L Latent heat flux by evaporation or condensation   [W/m2]
C Qrb     R*4 1 I Heat exchange between water and riverbed          [W/m2]
C Qsg     R*4 1 L Sensible heat of conduction                       [W/m2]
C Qsn     R*4 1 L Non refl. (net) short wave rad. reaching water    [W/m2]
C Qsw     R*4 1 L Short wave radiation reaching water               [W/m2]
C Qt      R*4 1 L Total heat flux                                   [W/m2]
C RatBow  R*4 1 L Bowens ratio                                         [-]
C RelHum  R*4 1 I Relative air humidity                                [%]
C Rho0    R*4 1 I Density of surface water at 4°C                  [kg/m3]
C RhoRef  R*4 1 L Density of surface water at reference temperature[kg/m3]
C SBC     R*4 1 I StefanBolzman Constant                        [W/m2/K-4]
C SunFac  R*4 1 I Percentage sunshine                                  [%]
C SWEmis  R*4 1 I Switch for calculation of emissivity                 [-]
C TempAt  R*4 1 I Air temperature                                     [°C]
C TempWa  R*4 1 I Surface water temperature                           [°C]
C Tref    R*4 1 I Reference water temperature for wind function       [°C]
C Vevap   R*4 1 L Evaporation rate                                   [m/s]
C VWinda  R*4 1 L Calculated wind speed                              [m/s]
C VWindm  R*4 1 I Measured wind speed                                [m/s]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE
      REAL     PMSA  ( * ) , FL  (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4,
     +         ISEG, IFLUX, IKMRK1, IKMRK2
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19,
     +         IP20, IP21, IP22, IP23, IP24, IP25, IP26, IP27, IP28,
     +         IP29, IP30, IP31, IP32, IP33, IP34, IP35, IP36, IP37,
     +         IP38, IP39, IP40, IP41
      INTEGER  ISWTEMP, ISWEMIS
      REAL     Qsw, Fsw, SunFac, Pvap, TempAt, SBC, Fa, EWater,
     j         TempWa, RhoWat, CWindA, CWindB, CWindC, VWindm, K,
     j         PvapWa, Cpa, Patm, Qsn, Cloud, Emiss, Qa, Qan, Qbr, C,
     j         HTVap, Fwind, Ql, Beta, RatBow, Qsg, Qt, Qrb, Tref,
     j         dTemp, cp, DEPTH, RelHum, Psvap, Hm, Ha, Vevap, Fwind2,
     j         VWinda, DeltaT, HtVRef, RhoRef, Rho0, MODTEMP, mindeptht

      IP1   = IPOINT( 1)
      IP2   = IPOINT( 2)
      IP3   = IPOINT( 3)
      IP4   = IPOINT( 4)
      IP5   = IPOINT( 5)
      IP6   = IPOINT( 6)
      IP7   = IPOINT( 7)
      IP8   = IPOINT( 8)
      IP9   = IPOINT( 9)
      IP10  = IPOINT(10)
      IP11  = IPOINT(11)
      IP12  = IPOINT(12)
      IP13  = IPOINT(13)
      IP14  = IPOINT(14)
      IP15  = IPOINT(15)
      IP16  = IPOINT(16)
      IP17  = IPOINT(17)
      IP18  = IPOINT(18)
      IP19  = IPOINT(19)
      IP20  = IPOINT(20)
      IP21  = IPOINT(21)
      IP22  = IPOINT(22)
      IP23  = IPOINT(23)
      IP24  = IPOINT(24)
      IP25  = IPOINT(25)
      IP26  = IPOINT(26)
      IP27  = IPOINT(27)
      IP28  = IPOINT(28)
      IP29  = IPOINT(29)
      IP30  = IPOINT(30)
      IP31  = IPOINT(31)
      IP32  = IPOINT(32)
      IP33  = IPOINT(33)
      IP34  = IPOINT(34)
      IP35  = IPOINT(35)
      IP36  = IPOINT(36)
      IP37  = IPOINT(37)
      IP38  = IPOINT(38)
      IP39  = IPOINT(39)
      IP40  = IPOINT(40)
      IP41  = IPOINT(41)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG

C     Heat exchange only for active water segments
C
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C     Heat exchange only for top layer segments
C
         CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
         IF (IKMRK2.EQ.0 .OR. IKMRK2.EQ.1) THEN
C
            Qsw     = PMSA(IP1 )
            Fsw     = PMSA(IP2 )
            SunFac  = PMSA(IP3 )
            ISWEmis = NINT(PMSA(IP4 ))
            TempAt  = PMSA(IP5 )
            SBC     = PMSA(IP6 )
            Fa      = PMSA(IP7 )
            Ewater  = PMSA(IP8 )
            TempWa  = PMSA(IP9 )
            Rho0    = PMSA(IP10)
            CWinda  = PMSA(IP11)
            CWindb  = PMSA(IP12)
            CWindc  = PMSA(IP13)
            VWindm  = PMSA(IP14)
            hm      = PMSA(IP15)
            ha      = PMSA(IP16)
            k       = PMSA(IP17)
            cpa     = PMSA(IP18)
            Patm    = PMSA(IP19)
            Qrb     = PMSA(IP20)
            Cp      = PMSA(IP21)
            DEPTH   = PMSA(IP22)
            RelHum  = PMSA(IP23) / 100.0
            DeltaT  = PMSA(IP24)
            Tref    = PMSA(IP25)
            ISWTEMP = NINT(PMSA(IP26))
            mindeptht = PMSA(IP27)

C
C     ------Short wave solar radiation-----------
C
            Qsn = Qsw * (1- Fsw)
C
C     ------Long wave atmospheric radiation------
C
            cloud = (100.0 - SunFac) / 100.0
C
            Psvap = 6.131 + 0.467 * TempAt + 0.0089 *
     j               TempAt ** 2.0 + 0.000527 * TempAt ** 3.0
C
            Pvap = RelHum * Psvap
C
C           ----1.  Option 1 = Brunt, 1932
C
            IF (ISWEmis.EQ.1) THEN
C
               Emiss = (0.51 + 0.066 * SQRT(Pvap)) *
     j                 (1.0 + 0.17 * cloud ** 2.0)
C
               Qa = Emiss * SBC * (TempAt + 273.15) ** 4.0
C
C           ----2.  Option 2 = Edinger, 1965 and Koberg, 1962
C
            ELSEIF (ISWEmis.EQ.2) THEN
C
C           Pascal heeft formulering voor C (Brunt coefficient)
C           aangevraagd, wordt later ingevuld...
C
               C = 1
C
               Emiss = 1.1 * C + 0.030 * SQRT(Pvap)
C
               Qa = Emiss * SBC * (TempAt + 273.15) ** 4.0
C
C           ----3.  Option 3 = Edinger, 1965
C
            ELSEIF (ISWEmis.EQ.3) THEN
C
               Emiss = 0.74 * (1 + 0.17 * cloud) +
     j                 0.0045 * (1 - 0.4 * cloud) *
     j                 Pvap
C
               Qa = Emiss * SBC * (TempAt + 273.15) ** 4.0
C
C           ----4.  Option 4 = Ludikhuize, 1994 as in WAQUA
C
            ELSEIF (ISWEmis.EQ.4) THEN
C
               Qa = (218.0 + 6.3 * TempAt) *
     j                 (1.0 + 0.17 * cloud ** 2.0)
C
            ELSE
               WRITE (*,*) ' Illegal option for emissivity formula'
               CALL SRSTOP(1)
            ENDIF
C
            Qan = Qa * (1 - Fa)
C
C     ------Long wave back radiation from water------
C
            Qbr = Ewater * SBC * ((TempWa + DeltaT + 273.15) ** 4.0)
C
C     ------Latent heat (evaporation or condensation)-----
C
            RhoWat = Rho0 * (1.0 - 7.17e-6 *
     j               (TempWa + DeltaT - 4.0) ** 2.0)
C
            RhoRef = Rho0 * (1.0 - 7.17e-6 *
     j               (Tref - 4.0) ** 2.0)
C
            HtVap = 2.5e+06 - 2300.0 * (TempWa + DeltaT)
C
            HtVRef = 2.5e+06 - 2300.0 * Tref
C
            PvapWa = 6.131 + 0.467 * (TempWa + DeltaT) + 0.0089 *
     j               (TempWa + DeltaT) ** 2.0 + 0.000527 *
     j               (TempWa + DeltaT) ** 3.0
C
            IF (ABS(Hm - Ha) .LT. 0.0001) THEN
               VWinda = VWindm
            ELSE
               IF  ((Hm .GT. (10 * k)) .AND. (Ha .GT. (10 * k))) THEN
                   VWinda = VWindm * LOG(Hm / k) / LOG(Ha / k)
               ELSE
                   VWinda = VWindm
               ENDIF
            ENDIF
C
            FWind = CwindA + CwindB * VWinda + CwindC * VWinda ** 2.0
C
            FWind2 = Fwind / (RhoRef * HtVRef)
C
            Vevap = FWind2 * (PvapWa - Pvap)
C
            Ql = RhoWat * HtVap * Vevap
C
C     ------Convective heat-----------------------------
C
            beta = (cpa * Patm) / (0.62 * HtVap)
C
            Qsg = beta * (TempWa + DeltaT - TempAt) * RhoWat * HtVap * Fwind2
C
C     ------Convective heat to river bed----------------
C           formulations can be included later...
C
C     ------Total heat flux-----------------------------
C
            Qt = Qsn + Qan - Qbr - Ql - Qsg + Qrb
C
C     ------Change of water temperature-----------------
C
            dTemp = (Qt * 86400.0) / (RhoWat * Cp * Depth)
C
C     ------For output to exces temperature moddeling
C
            IF ( ISWTEMP .EQ. 0 ) THEN
               MODTEMP = TEMPWA
            ELSE
               MODTEMP = 0.0
            ENDIF

            PMSA(IP28 ) = Qsn
            PMSA(IP29 ) = cloud
            PMSA(IP30 ) = Psvap
            PMSA(IP31 ) = Emiss
            PMSA(IP32 ) = Qan
            PMSA(IP33 ) = Qbr
            PMSA(IP34 ) = Ql
            PMSA(IP35 ) = Qsg
            PMSA(IP36 ) = Qt
            PMSA(IP37 ) = PvapWa
            PMSA(IP38 ) = VWinda
            PMSA(IP39 ) = Vevap
            PMSA(IP40 ) = RhoWat
            PMSA(IP41 ) = MODTEMP

            IF ( depth .lt. mindeptht ) THEN

               ! depth less then mindeptht no fluxes

               dTemp = 0.0

            ENDIF
C
         ELSE
C
C           For non top layer segments no heat flux is calculated
C
            dTemp = 0.0
C
         ENDIF
C
      ENDIF
C
      FL( 1+IFLUX) = dTemp

      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
      IP16  = IP16  + INCREM ( 16 )
      IP17  = IP17  + INCREM ( 17 )
      IP18  = IP18  + INCREM ( 18 )
      IP19  = IP19  + INCREM ( 19 )
      IP20  = IP20  + INCREM ( 20 )
      IP21  = IP21  + INCREM ( 21 )
      IP22  = IP22  + INCREM ( 22 )
      IP23  = IP23  + INCREM ( 23 )
      IP24  = IP24  + INCREM ( 24 )
      IP25  = IP25  + INCREM ( 25 )
      IP26  = IP26  + INCREM ( 26 )
      IP27  = IP27  + INCREM ( 27 )
      IP28  = IP28  + INCREM ( 28 )
      IP29  = IP29  + INCREM ( 29 )
      IP30  = IP30  + INCREM ( 30 )
      IP31  = IP31  + INCREM ( 31 )
      IP32  = IP32  + INCREM ( 32 )
      IP33  = IP33  + INCREM ( 33 )
      IP34  = IP34  + INCREM ( 34 )
      IP35  = IP35  + INCREM ( 35 )
      IP36  = IP36  + INCREM ( 36 )
      IP37  = IP37  + INCREM ( 37 )
      IP38  = IP38  + INCREM ( 38 )
      IP39  = IP39  + INCREM ( 39 )
      IP40  = IP40  + INCREM ( 40 )
      IP41  = IP41  + INCREM ( 41 )
C
 9000 CONTINUE
C

      RETURN
      END
