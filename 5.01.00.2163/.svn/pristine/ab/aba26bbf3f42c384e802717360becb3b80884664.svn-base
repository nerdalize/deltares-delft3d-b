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

      SUBROUTINE OUTDMP ( IOUT  , LCHOUT, ITIME , MNAME , NX    ,
     +                    NY    , LGRID , CGRID , NOTOT , NOSYS ,
     +                    SNAME , CONC  , BOUND , NOTOT2, SYNAM2,
     +                    CONC2 , IP    , ISFLAG, INIOUT)
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : may  1993  BY Jan van Beek
C                          from DLWQ11 : june 1988  BY M.E.Sileon
C
C     FUNCTION           : Writes concentrations in grid-layout and
C                          writes the result in IOUT .
C
C     LOGICAL UNITS      : IOUT = number of dump file
C
C     SUBROUTINES CALLED : none
C
C     PARAMETERS         : 19
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IOUT    INTEGER  1           INPUT   unit number output file
C     LCHOUT  CHAR*(*) 1           INPUT   name output file
C     ITIME   INTEGER  1           INPUT   present time in clock units
C     MNAME   CHAR*40  4           INPUT   model identhification
C     NX      INTEGER  1           INPUT   number of columns in grid
C     NY      INTEGER  1           INPUT   number of rows in grid
C     LGRID   INTEGER  NX*NY       INPUT   grid layout
C     CGRID   CHAR*6   20*NY       LOCAL   concentrations in grid layout
C     NOTOT   INTEGER  1           INPUT   total number of systems
C     NOSYS   INTEGER  1           INPUT   number of active systems
C     SNAME   CHAR*20  NOTOT       INPUT   names of substances
C     CONC    REAL     NOTOT*?     INPUT   concentration values
C     BOUND   REAL     NOTOT*?     INPUT   boundary      values
C     NOTOT2  INTEGER  1           INPUT   number of extra output vars
C     SYNAM2  CHAR*20  NOTOT       INPUT   names of extra vars
C     CONC2   REAL    NOTOT2,NX*NY INPUT   values for extra vars
C     IP      INTEGER  6           IN/OUT  paging structure
C     ISFLAG  INTEGER  1           INPUT   if 1 then dd-hh:mm'ss'
C     INIOUT  INTEGER  1           IN/OUT  Initialize flag
C
C     Declaration of arguments
C
      use timers

      INTEGER       IOUT  , ITIME , NX    , NY    , NOTOT ,
     +              NOSYS , ISFLAG, NOTOT2
      INTEGER       LGRID(*)      , IP(*)
      REAL          CONC(NOTOT,*) , BOUND(NOSYS,*),
     +              CONC2 (*)
      CHARACTER*6   CGRID ( 20, * )
      CHARACTER*20  SNAME ( * )        , SYNAM2(*)
      CHARACTER*40  MNAME ( * )
      CHARACTER*(*) LCHOUT
C
C     Local declaration
C
      PARAMETER   ( RMISS = -999. )
      CHARACTER*6   POINT , PADDER
      DATA          POINT / '  .   ' /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outdmp", ithandl )
C
      IF ( NX*NY .EQ. 0 ) goto 9999  !   RETURN
C
C         initialise the paging
C
      IF ( IP(3) .EQ. 0 ) THEN
           IP(3) = MAX(1,IP(1)/(7+(NY+5)*((NX+IP(2)-1)/IP(2))))
           IP(4) = 0
      ENDIF
C
C      repeat output for every substance
C
      DO 50 ITOT =  1 , NOTOT
C
C      Calculate maximum concentration of displayed segments
C
      CMAX = 0.0
      DO 10 I1 = 1, NY
      DO 10 I2 = 1, NX
      I3 = LGRID ( (I1-1)*NX+I2 )
      IF ( I3 .GT. 0 ) CMAX = AMAX1 ( CMAX, CONC (ITOT, I3) )
      IF ( I3 .LT. 0 .AND. ITOT .LE. NOSYS )
     *                 CMAX = AMAX1 ( CMAX, BOUND(ITOT,-I3) )
   10 CONTINUE
C
C      Calculate scale factor
C
      IF ( CMAX .LE. 0.0 ) THEN
           ISCALE = 0
      ELSE IF ( CMAX .GE. 0.5 ) THEN
              ISCALE =  AINT( ALOG10(CMAX) + 2.2E-5 )
           ELSE
              ISCALE = -AINT(-ALOG10(CMAX) - 2.2E-5 ) - 1
      ENDIF
C
C         start printing
C
      IF ( MOD(IP(4),IP(3)) .EQ. 0 ) THEN
           WRITE (IOUT,'(''1'')')
           WRITE (IOUT,2040 ) ( MNAME(K),K=1,4)
      ENDIF
      IP(4) = IP(4) + 1
      CALL REPTIM ( IOUT  , ITIME , ISFLAG, -999.)
      WRITE(IOUT,2000) SNAME(ITOT) ,ISCALE
C
C      Put concentration values in grid layout
C
      FACTOR = 10.0**ISCALE
      DO 40 I = 1, NX, IP(2)
         DO 30 J = 1, NY
            NEND = MIN ( NX, I+IP(2)-1 )
            DO 20 K = I, NEND
            CGRID ( K-I+1,J ) = POINT
            I3 = LGRID ( (J-1)*NX+K )
            IF ( I3 .GT. 0 ) THEN
                 WRITE ( PADDER, '(F6.3)')   CONC (ITOT, I3 )/FACTOR
                 CGRID(K-I+1,J) = PADDER
            ENDIF
            IF ( I3 .LT. 0 .AND. ITOT .LE. NOSYS ) THEN

                 WRITE ( PADDER, '(F6.3)')   BOUND(ITOT,-I3 )/FACTOR
                 CGRID(K-I+1,J) = PADDER
            ENDIF
   20       CONTINUE
            WRITE ( IOUT, 2030 ) ( CGRID ( K-I+1, J ) , K=I, NEND )
   30   CONTINUE
        WRITE ( IOUT, '('' '')' )
   40 CONTINUE
C
   50 CONTINUE
C
C      repeat output for extra substance
C
      DO 100 ITOT =  1 , NOTOT2
C
C      Calculate maximum concentration of displayed segments
C
      CMAX = 0.0
      DO 60 I1 = 1, NY
      DO 60 I2 = 1, NX
         CMAX = AMAX1 ( CMAX, CONC2(ITOT+(I2*I1-1)*NOTOT2) )
   60 CONTINUE
C
C      Calculate scale factor
C
      IF ( CMAX .LE. 0.0 ) THEN
           ISCALE = 0
      ELSE IF ( CMAX .GE. 0.5 ) THEN
              ISCALE =  AINT( ALOG10(CMAX) + 2.2E-5 )
           ELSE
              ISCALE = -AINT(-ALOG10(CMAX) - 2.2E-5 ) - 1
      ENDIF
C
C         start printing
C
      IF ( MOD(IP(4),IP(3)) .EQ. 0 ) THEN
           WRITE (IOUT,'(''1'')')
           WRITE (IOUT,2040 ) ( MNAME(K),K=1,4)
      ENDIF
      IP(4) = IP(4) + 1
      CALL REPTIM ( IOUT  , ITIME , ISFLAG, -999.)
      WRITE(IOUT,2000) SYNAM2(ITOT) ,ISCALE
C
C      Put concentration values in grid layout
C
      FACTOR = 10.0**ISCALE
      DO 90 I = 1, NX, IP(2)
         DO 80 J = 1, NY
            NEND = MIN ( NX, I+IP(2)-1 )
            DO 70 K = I, NEND
               CGRID ( K-I+1,J ) = POINT
               C = CONC2( ITOT + ( (J-1)*NX+K - 1 ) * NOTOT2 )
               IF ( C .NE. RMISS ) THEN
                  WRITE ( PADDER, '(F6.3)')   C/FACTOR
                  CGRID(K-I+1,J) = PADDER
               ENDIF
   70       CONTINUE
            WRITE ( IOUT, 2030 ) ( CGRID ( K-I+1, J ) , K=I, NEND )
   80   CONTINUE
        WRITE ( IOUT, '('' '')' )
   90 CONTINUE
C
  100 CONTINUE
C
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 2000 FORMAT (/' DUMP OF RESULTS OF ',A20,
     *          ' SCALE FACTOR = 10.0 EXP (',I3,' ).'//)
 2030 FORMAT (   6X , 20A6  )
 2040 FORMAT (  45X ,   A40 )
C
      END
