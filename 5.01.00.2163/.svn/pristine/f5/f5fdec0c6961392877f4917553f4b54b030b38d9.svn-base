      subroutine salchl ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Converts salinity into chloride or vice versa (Aquatic Chemistry 2nd ed 1981 p567)

C***********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Hangzou                T1000.14
C     Author  : Pascal Boderie
C     Date    : 940721             Version : 0.04
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     940721  Pascal Boderie  Create first version
C     950112  Pascal Boderie  Add temperature effect on density
C     950407  van Gils/Vos    CHLOR error corrected
C     980317  van Gils        CHLOR error corrected (in fact created!)
C     980520  van Gils        CHLOR error uncreated
C     120827  Johannes Smits  Processes SALIN and CHLOR merged 
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                  Units
C ----    --- -  -    -------------------                           ----
C CL      R*4 1 I/O  chloride concentration                         [g/m3]
C SAL     R*4 1 I/O  salinity                                       [g/kg]
C SAL0    R*4 1 I    salinity at zero chloride                      [g/kg]
C GTCL    R*4 1 I    ratio of salinity and chloride                 [g/g]
C TEMP    R*4 1 I    ambient temperature                            [oC]
C DENS    R*4 1 -    densioty of water with dissolved salt          [kg/m3]
C SWSALCL R*4 1 I    option: 0 SAL simulated, 1 CL simulated
C
C   Logical Units : -
C   Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      REAL     CL , SAL , SAL0 , GTCL , TEMP , DENS , SWSALCL 
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      SAL     = PMSA( IP1 )
      CL      = PMSA( IP2 )
      GTCL    = PMSA( IP3 )
      TEMP    = PMSA( IP4 )
      SAL0    = PMSA( IP5 )
      SWSALCL = PMSA( IP6 )
C
C***********************************************************************
C**** Processes connected to the normalization RIZA method
C***********************************************************************
C
C     factor 0.7 in density correction was derived empirically from RIZA Standard methods
C     table 210 on p 109 is repoduced within 0.15% 
C     basic relation sal-chlorinity: sal = 0.03 +1.805*chlor/density
C     density = f(temp and salt concentration)
C
      IF (NINT(SWSALCL) .EQ. 1) THEN
      DENS =   1000. + 0.7 * CL/1000 * GTCL
     +       - 0.0061 * (TEMP-4.0) * (TEMP-4.0)
      SAL = CL * GTCL / DENS + SAL0
C
      ELSE
      DENS = 1000. + 0.7 * SAL / (1-SAL/1000.)
     +       - 0.0061 * (TEMP-4.0) * (TEMP-4.0)
C
      IF (SAL .LE. SAL0) THEN
          SAL = 0.0
      ELSE
          SAL = SAL - SAL0
      ENDIF
C
C     g/m3 = (g/kg)*(kg/m3)/(g/g)
C
      CL  = SAL * DENS / GTCL
      ENDIF
C
      PMSA (IP7) = DENS
      PMSA (IP8) = SAL
      PMSA (IP9) = CL
C
      ENDIF
C
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
C
 9000 CONTINUE
C
      RETURN
      END
