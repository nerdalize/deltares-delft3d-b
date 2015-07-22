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

      SUBROUTINE DLWQ5D ( LUNUT  , IAR    , RAR    , IIMAX  , IRMAX  ,
     *                    IPOSR  , NPOS   , ILUN   , LCH    , LSTACK ,
     *                    CCHAR  , CHULP  , NOTOT  , ITTIM  , NOBRK  ,
     *                    IOPT   , DTFLG1 , DTFLG3 , ITFACT , ITYPE  ,
     *                             IHULP  , RHULP  , IERR   , ierr3  )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : Boundary and waste data new style
C
C     SUBROUTINES CALLED : CNVTIM - converting times of breakpoints
C
C     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
C                          LUN(29) = unit formatted output file
C                          LUN( 2) = unit intermediate file (system)
C                          LUN(14) = unit intermediate file (boundaries)
C                          LUN(15) = unit intermediate file (wastes)
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNUT   INTEGER    1         INPUT   unit number for ASCII output
C     IAR     INTEGER  IIMAX       IN/OUT  integer   workspace
C     RAR     REAL     IRMAX       IN/OUT  real      workspace
C     IIMAX   INTEGER    1         INPUT   max. int. workspace dimension
C     IRMAX   INTEGER    1         INPUT   max. real workspace dimension
C     IPOSR   INTEGER    1         IN/OUT  Start position on input line
C     NPOS    INTEGER    1         INPUT   nr of significant characters
C     ILUN    INTEGER   LSTACK     INPUT   unitnumb include stack
C     LCH     CHAR*(*)  LSTACK     INPUT   file name stack, 4 deep
C     LSTACK  INTEGER    1         INPUT   include file stack size
C     CCHAR   CHAR*1     1         INPUT   comment character
C     CHULP   CHAR*(*)   1         OUTPUT  space for limiting token
C     NOTOT   INTEGER    1         INPUT   size of the matrix to be read
C     ITTIM   INTEGER    1         INPUT   0 if steady, 1 if time function
C     NOBRK   INTEGER    1         OUTPUT  number of records read
C     IOPT    INTEGER    1         INPUT   3 is harmonics, 4 is fourier
C     DTFLG1  LOGICAL    1         INPUT   True if time in 'date' format
C     DTFLG3  LOGICAL    1         INPUT   True if YYetc instead of DDetc
C     ITFACT  INTEGER    1         INPUT   factor between clocks
C     ITYPE   INTEGER    1         OUTPUT  type of info at end
C     IERR    INTEGER    1         OUTPUT  return code
C     IERR3   INTEGER    1         OUTPUT  actual error indicator
C
C
      use timers       !   performance timers

      INTEGER       IIMAX  , IRMAX
      CHARACTER*(*) LCH(LSTACK) , CHULP
      CHARACTER*1   CCHAR
      DIMENSION     IAR(*) , RAR(*) , ILUN( LSTACK )
      LOGICAL       NEWREC , DTFLG1 , DTFLG3, IGNORE
      integer       ihulp
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5d", ithndl )
C
C     Some initialisation
C
      IGNORE = .FALSE.
      NEWREC = .FALSE.
      IF ( ITTIM .EQ. 1 ) NEWREC = .TRUE.                          ! it is a time function
      NOBRK  = 0
      ITEL   = 1
      ITEL2  = 1
      ierr3  = 0
      IF ( ITYPE .NE. 0 ) GOTO 20                                  ! it was called with an argument
C
C     Read loop
C
   10 IF ( NEWREC ) THEN
         ITYPE = 0                                                 ! everything is valid
      ELSE
         ITYPE = 3                                                 ! a real value schould follow
      ENDIF
      CALL RDTOK1 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *              IPOSR  , NPOS   , CHULP  , IHULP  , RHULP  ,
     *                                         ITYPE  , IERR   )
C          A read error
      IF ( IERR  .NE. 0 ) goto 9999
C          A token has arrived
      IF ( ITYPE .EQ. 1 ) THEN                                     ! that must be an absolute timer string
         CALL DLWQ0T ( CHULP , IHULP, .FALSE., .FALSE., IERR )    !  2^31 =  2147483648
         IF ( IHULP .EQ. -999 ) THEN                              !       YYYYDDDHHMMSS so 64 bits integer
            IERR = 1
            WRITE ( LUNUT , 1020 ) TRIM(CHULP)
            goto 9999
         ENDIF
         IF ( IERR   .NE.    0 ) THEN                              ! the found entry is not a new time value
            if ( nobrk .le. 1 ) then
               write ( lunut, 1040 ) nobrk
               !ierr3 = ierr3 + 1
            endif
            IERR = 0
            goto 9999
         ENDIF
         IHULP = ITFACT * IHULP
      ELSEIF ( ITYPE .EQ. 2 ) THEN
         CALL Cnvtim ( IHULP, 1      , DTFLG1 , DTFLG3 )
      else
         ihulp = 0
      ENDIF
C          Getting the data of this block (no strings any more)
   20 IF ( ITTIM .EQ. 1 .AND. NEWREC ) THEN
C          it was a non-real and characters has been caught
         IF ( IHULP .EQ. -999 ) THEN
            IGNORE = .TRUE.
         ELSE                                                      ! a new breakpoint found
            IGNORE = .FALSE.
            NOBRK = NOBRK + 1
            IF ( NOBRK .LE. IIMAX ) THEN
               IAR(NOBRK) = IHULP
               if ( nobrk .gt. 1 ) then
                  if ( ihulp .le. iar(nobrk-1) ) then
                     write ( lunut, 1030 ) ihulp, iar(nobrk-1)
                     ierr3 = ierr3 + 1
                  endif
               endif
            ELSE
               WRITE ( LUNUT , 1000 ) IIMAX
               IERR = 100
               goto 9999
            ENDIF
         ENDIF
         NEWREC = .FALSE.
         GOTO 10
      ENDIF
C
      IF ( .NOT. IGNORE ) RAR(ITEL) = RHULP
C        are we to expect a new record ?
      IF ( MOD(ITEL2,NOTOT) .EQ. 0 ) NEWREC = .TRUE.
C        it was a constant, so we can now return.
      IF ( NEWREC .AND. ITTIM .NE. 1 ) THEN
         NOBRK = 1
         IAR(1) =  0
         goto 9999
      ENDIF
C        increase the counter for the next real and go to input
      IF ( .NOT. IGNORE ) ITEL = ITEL + 1
      ITEL2 = ITEL2 + 1
      GOTO 10
 9999 if (timon) call timstop( ithndl )
      return
C
 1000 FORMAT ( ' ERROR ! Number of breakpoints exceeds system',
     *         ' maximum of: ' , I10 )
 1010 FORMAT ( ' ERROR ! Number of data points exceeds system',
     *         ' maximum of: ' , I10 )
 1020 FORMAT ( ' ERROR ! Absolute timer does not fit in timer ',
     *         'format: ', A)
 1030 FORMAT (/' ERROR ! Time value ',I10,' not larger than previous time value ',I10 )
 1040 FORMAT (/' WARNING ! There are only ',I2,' breakpoints found for this time series' )
C
      END

