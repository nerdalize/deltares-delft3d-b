      subroutine s12tra ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Generic module to process resuspension, burial, digging S1 & S2

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Author  : Jos van Gils  
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     110912  Jos van Gils    Create first version, merge RESCAR, SWBUR, DIGCAR
C
C***********************************************************************
C
C     Description of the module :
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT(23) , INCREM(23) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
     
     
      INTEGER  IP(23), IFLUX, ISEG, IKMRK2
      REAL     FRACS1, SCALS1, FRACS2, SCALS2, FRESS1, FRESS2,
     J         FBURS1, FBURS2, FDIGS1, FDIGS2, SWDS1 , SWDS2 , 
     J         DEPTH , SWITCH, FRACS3, SCALS3, B1, B2, D1, D2, R1, R2

      IP  = IPOINT
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C

      FRACS1 = PMSA(IP( 1))
      SCALS1 = PMSA(IP( 2))
      FRACS2 = PMSA(IP( 3))
      SCALS2 = PMSA(IP( 4))
      FRACS3 = PMSA(IP( 5))
      SCALS3 = PMSA(IP( 6))
      FRESS1 = PMSA(IP( 7))
      FRESS2 = PMSA(IP( 8))
      FBURS1 = PMSA(IP( 9))
      FBURS2 = PMSA(IP(10))
      FDIGS1 = PMSA(IP(11))
      FDIGS2 = PMSA(IP(12))
      SWDS1  = PMSA(IP(13))
      SWDS2  = PMSA(IP(14))
      DEPTH  = PMSA(IP(15))
      SWITCH = PMSA(IP(16))

C*******************************************************************************
C**** Processes connected to the BURIAL and DIGGING
C***********************************************************************

C     RESUSPENSION
      R1 = 0.0
      R2 = 0.0
      IF ( FRACS1*SCALS1 .GE. 0.0 ) R1 = FRESS1 * FRACS1*SCALS1
      IF ( FRACS2*SCALS2 .GE. 0.0 ) R2 = FRESS2 * FRACS2*SCALS2
	
C     BURIAL
      B1 = 0.0
      B2 = 0.0
      IF ( FRACS1*SCALS1 .GE. 0.0 ) B1 = FBURS1 * FRACS1*SCALS1
      IF ( FRACS2*SCALS2 .GE. 0.0 ) B2 = FBURS2 * FRACS2*SCALS2

C     DIGGING
      D1 = 0.0
      D2 = 0.0
      IF ( (SWDS1 .LT. 0.5) .AND. (FRACS1*SCALS1 .GE. 0.0) ) THEN
           D1 = FDIGS1 * FRACS1*SCALS1
      ELSEIF (FRACS2*SCALS2 .GE. 0.0) THEN
           D1 = FDIGS1 * FRACS2*SCALS2
      ENDIF      
      IF ( (SWDS2 .LT. 0.5) .AND. (FRACS2*SCALS2 .GE. 0.0) ) THEN
           D2 = FDIGS2 * FRACS2*SCALS2
      ELSEIF (FRACS3*SCALS3 .GE. 0.0) THEN
           D2 = FDIGS2 * FRACS3*SCALS3
      ENDIF      

C     Store results

      PMSA(IP(17)) = R1
      PMSA(IP(18)) = R2
      IF (ABS(SWITCH).LT.0.5) THEN
C       NO SWITCH
        PMSA(IP(19)) = B1
        PMSA(IP(20)) = 0.0
      ELSE
C       SWITCH
        PMSA(IP(19)) = 0.0
        PMSA(IP(20)) = B1
      ENDIF
      PMSA(IP(21)) = B2
      PMSA(IP(22)) = D1
      PMSA(IP(23)) = D2

      FL( 1 + IFLUX ) = R1/DEPTH 
      FL( 2 + IFLUX ) = R2/DEPTH 
      IF (ABS(SWITCH).LT.0.5) THEN
C       NO SWITCH
        FL( 3 + IFLUX ) = B1/DEPTH 
        FL( 4 + IFLUX ) = 0.0
      ELSE
C       SWITCH
        FL( 3 + IFLUX ) = 0.0
        FL( 4 + IFLUX ) = B1/DEPTH 
      ENDIF
      FL( 5 + IFLUX ) = B2/DEPTH 
      FL( 6 + IFLUX ) = D1/DEPTH 
      FL( 7 + IFLUX ) = D2/DEPTH 

      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP    = IP    + INCREM
c
 9000 CONTINUE
c
      RETURN
C
      END
