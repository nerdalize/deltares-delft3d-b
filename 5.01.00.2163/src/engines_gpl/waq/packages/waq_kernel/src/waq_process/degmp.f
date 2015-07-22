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

      subroutine degmp  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Degradation of organic micropolutants (new, generic!)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     921210  Pascal Boderie  Create first version, based on T721.13
C     020215  Johannes Smits  New generic version, with options
C                             regarding 1)oxidising and reducing cond.
C                             and 2)fraction that is degraded (IVERSN=1)
C                             The old version is maintained (IVERSN=0)
C                             for water and sediment
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Overall degradation of organic micropollutants in the
C        water and sediment.
C
C         ----- old version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CRTEMP  R*4 1 I critical temperature for decay                       [xC]
C DEPTH   R*4 1 I actual depth of a segment                            [m]
C FL (1)  R*4 1 O decay flux mixing layer (x=C,N,P,Si)           [gX/m3/d]
C MINRC   R*4 1 I first order decay rate                             [1/d]
C FDIS    R*4 1 I fraction free dissolved mive                         [-]
C MINTCR  R*4 1 I temperature coefficient two bottom layers          [1/d]
C ORG     R*4 1 I amount decaying organic material in mixing layer    [gX]
C TEMP    R*4 1 I ambient temperature                                 [xC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
C TEMPC   R*4 1 L temperature coefficient                              [-]
C VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
C ZERMIN  R*4 1 I zeroth order decay rate mixing layer           [gX/m2/d]
C
C         ----- new version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C IVERSN  I   1 I option for version of process                        [-]
C ISWOXY  I   1 I option for oxidising or reducing condition           [-]
C ISWDEG  I   1 I option for fraction micropoll. that is degraded      [-]
C CRTEMP  R*4 1 I critical temperature for decay                      [oC]
C DEPTH   R*4 1 I actual depth of a water segment                      [m]
C FL (1)  R*4 1 O decay flux (x = C, N, P, Si)                   [gX/m3/d]
C FTOTR   R*4 1 - fraction of micropollutant that is gedraded          [-]
C FDFREE  R*4 1 I fraction free dissolved micropollutant               [-]
C FDDOC   R*4 1 I fraction DOC-bound micropollutant                    [-]
C KDEG    R*4 1 - first order degradation rate                       [1/d]
C KDEGO   R*4 1 I first order degradation rate at oxidising cond.    [1/d]
C KDEGR   R*4 1 I first order degradation rate at reducing cond.     [1/d]
C KTDEG   R*4 1 I temperature coefficient for decay                    [-]
C ORGMP   R*4 1 I concentration organic micropollutant             [gX/m3]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L ambient temperature - reference temp.(20)           [oC]
C TEMPC   R*4 1 L temperature coefficient                              [-]
C VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
C ZDEGMP  R*4 1 I zero order degradation rate                    [gX/m3/d]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14, IP15, IP16
      INTEGER  IFLUX, ISEG, IKMRK1, IKMRK2
      REAL     ZERMIN, ORG, FDIS, MINRC, MINTC
      INTEGER  IVERSN, ISWOXY, ISWDEG
      REAL     ZDEGMP, ORGMP, FDFREE, FDDOC, KDEGO, KDEGR, KTDEG,
     +         KDEG, FTOTR
      REAL     TEMP, CRTEMP, TEMPC, TEMP20, VOLUME, DEPTH
      LOGICAL  SEDIME
C
      IP1  = IPOINT( 1 )
      IP2  = IPOINT( 2 )
      IP3  = IPOINT( 3 )
      IP4  = IPOINT( 4 )
      IP5  = IPOINT( 5 )
      IP6  = IPOINT( 6 )
      IP7  = IPOINT( 7 )
      IP8  = IPOINT( 8 )
      IP9  = IPOINT( 9 )
      IP10 = IPOINT( 10)
      IP11 = IPOINT( 11)
      IP12 = IPOINT( 12)
      IP13 = IPOINT( 13)
      IP14 = IPOINT( 14)
      IP15 = IPOINT( 15)
      IP16 = IPOINT( 16)
C
C     Check sediment switch for first segment
C
      SEDIME = .FALSE.
      IF ( PMSA(IP16) .GT. 0.5 ) SEDIME = .TRUE.
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0.OR.IKMRK2.EQ.3).OR..NOT.SEDIME) THEN
      IVERSN = NINT( PMSA( IP10 ))
C
C     Use old version when IVERSN=0
C
      IF (IVERSN .EQ. 0) THEN
C
         ZERMIN  = PMSA( IP1 )
         ORG     = MAX (0.0, PMSA( IP2 ) )
         FDIS    = PMSA( IP3 )
         MINRC   = PMSA( IP4 )
         MINTC   = PMSA( IP5 )
         TEMP    = PMSA( IP6 )
         CRTEMP  = PMSA( IP7 )
         VOLUME  = PMSA( IP8 )
         DEPTH   = PMSA( IP9 )
C
C        Calculate the degradation flux
C
         IF (TEMP .LE. CRTEMP) THEN
C
C           Only the zero order term
C
            IF ( SEDIME ) THEN
               FL( 1+IFLUX) = ZERMIN  / DEPTH
            ELSE
               FL( 1+IFLUX) = ZERMIN
            ENDIF
C
         ELSE
C
C           Sum of zero and first order terms
C
            TEMP20 = TEMP - 20.0
            TEMPC  = MINTC ** TEMP20
C
            IF ( SEDIME ) THEN
               FL( 1+IFLUX) = ( ZERMIN  / DEPTH  +
     &                 FDIS * MINRC * TEMPC * ORG / DEPTH  )
            ELSE
               FL( 1+IFLUX) = ZERMIN + FDIS * MINRC * TEMPC *ORG
            ENDIF

         ENDIF
C
C     Use new version when IVERSN=1
C
      ELSE
C
         ZDEGMP = PMSA( IP1 )
         ORGMP  = MAX ( 0.0, PMSA( IP2 ) )
         FDFREE = PMSA( IP3 )
         KTDEG  = PMSA( IP5 )
         TEMP   = PMSA( IP6 )
         CRTEMP = PMSA( IP7 )
         VOLUME = PMSA( IP8 )
         DEPTH  = PMSA( IP9 )
         ISWOXY = NINT( PMSA( IP11))
         ISWDEG = NINT( PMSA( IP12))
         FDDOC  = PMSA( IP13)
         KDEGO  = PMSA( IP14)
         KDEGR  = PMSA( IP15)
C
C        Calculate the degradation flux
C
         IF (TEMP .LE. CRTEMP) THEN
C
C           Only the zero order term
C
            IF ( SEDIME ) THEN
               FL( 1+IFLUX) = ZDEGMP / DEPTH
            ELSE
               FL( 1+IFLUX) = ZDEGMP
            ENDIF
C
         ELSE
C
C           Sum of zero and first order terms
C
            TEMP20 = TEMP - 20.0
            TEMPC  = KTDEG ** TEMP20
C
C           Select rate for oxidising conditions (ISWOXY = 1)
C                        or reducing conditions (ISWOXY = 0)
C
            KDEG = KDEGO
            IF (ISWOXY .EQ. 0) KDEG = KDEGR
C
C           Select the fractions that are degraded,
C           total (ISWDEG = 0), free dissolved (ISWDEG = 1),
C           free dissolved plus DOC-bound (ISWDEG = 2)
C
            IF (ISWDEG .EQ. 1) THEN
               FTOTR = FDFREE
               ELSE IF (ISWDEG .EQ. 2) THEN
               FTOTR = FDFREE + FDDOC
               ELSE
               FTOTR = 1.0
            ENDIF
C
            IF ( SEDIME ) THEN
               FL( 1+IFLUX) = ZDEGMP / DEPTH +
     +                     KDEG * TEMPC * FTOTR * ORGMP / DEPTH
            ELSE
               FL( 1+IFLUX) = ZDEGMP + KDEG * TEMPC * FTOTR * ORGMP
            ENDIF
C
         ENDIF
C
      ENDIF
C
      ENDIF
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM ( 1 )
      IP2   = IP2   + INCREM ( 2 )
      IP3   = IP3   + INCREM ( 3 )
      IP4   = IP4   + INCREM ( 4 )
      IP5   = IP5   + INCREM ( 5 )
      IP6   = IP6   + INCREM ( 6 )
      IP7   = IP7   + INCREM ( 7 )
      IP8   = IP8   + INCREM ( 8 )
      IP9   = IP9   + INCREM ( 9 )
      IP10  = IP10  + INCREM ( 10)
      IP11  = IP11  + INCREM ( 11)
      IP12  = IP12  + INCREM ( 12)
      IP13  = IP13  + INCREM ( 13)
      IP14  = IP14  + INCREM ( 14)
      IP15  = IP15  + INCREM ( 15)
C     IP16 wordt niet opgehoogd, want deze staat buiten segmentloop!!!
C
 9000 CONTINUE
C
      RETURN
C
      END
