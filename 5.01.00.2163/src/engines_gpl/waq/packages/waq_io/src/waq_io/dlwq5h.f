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

      SUBROUTINE DLWQ5H ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                    NODIM  , IORDER , CNAMES , IOFFI  , IOFFC  ,
     *                             IODS   , IOFFD  , I      , ICNT   )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : October '00  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : Compacts USEFOR lists if unresolved externals
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
C                          LUN(29) = unit formatted output file
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNUT   INTEGER    1         INPUT   unit number for ASCII output
C     IAR     INTEGER  IIMAX       IN/OUT  integer   workspace
C     ITMNR   INTEGER    1         IN/OUT  nr of items for assignment
C     NOITM   INTEGER    1         IN      nr of items in computational rule
C     IDMNR   INTEGER    1         IN/OUT  nr of subst for assignment
C     NODIM   INTEGER    1         IN      nr of subst in computational rule
C     IORDER  INTEGER    1         IN      1 = items first, 2 is subst first
C     CNAMES  CHAR*(*)  NITM       INPUT   Items to check for presence
C     IOFFI   INTEGER    1         IN/OUT  Offset in input array
C     IOFFC   INTEGER    1         IN/OUT  Offset in character array
C     IOFFD   INTEGER    1         IN/OUT  Base offset in both arrays
C     IODS    INTEGER    1         INPUT   Shift counter ODS files
C     I       INTEGER    1         INPUT   loop counter
C     ICNT    INTEGER    1         IN/OUT  counter
C
C
      use timers       !   performance timers

      CHARACTER*(*) CNAMES(*)
      DIMENSION     IAR(*)
      CHARACTER*20  CHULP
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5h", ithndl )
C
C       Write message
C
      WRITE ( LUNUT ,   *  )
      WRITE ( LUNUT , 1010 ) I+ICNT, CNAMES(I+IOFFC)
      IF ( IORDER .EQ. 1 ) THEN
          NTT  = IDMNR
          NITM = NODIM
      ELSE
          NTT  = ITMNR
          NITM = NOITM
      ENDIF
C
C       Look backwards
C
      DO 10 I1 = I,1,-1
         I2 = IAR(I1+IOFFC)
         IF ( I2 .GT. -100000 ) GOTO 20
   10 CONTINUE
C
C       Additional messages for this sequence
C
   20 IF ( I2 .LE. 0 .AND. I2 .GT. -100000 ) THEN
C       Try to find the reference
         I4 = 0
         DO 25 I3 = 1 , I
            I5 = IAR(I3+IOFFC)
            IF ( I5 .GT. 0 ) I4 = IAR(I3+IOFFC)
            IF ( I5 .LE. 0 .AND. I5 .GT. -100000 ) I4 = I4 + 1
   25    CONTINUE
         CHULP = CNAMES(I4+IOFFD)
         IF ( CNAMES(I+IOFFC) .NE. CHULP ) THEN
            IF ( IORDER .EQ. 2 ) THEN
               WRITE (LUNUT,1030) I4,CHULP
            ELSE
               WRITE (LUNUT,1040) I4,CHULP
            ENDIF
         ENDIF
      ENDIF
      IF ( I2 .GT. 0 .AND. I2 .LT.  100000 ) THEN
         I4 = I2
         CHULP = CNAMES( I2+IOFFD)
         IF ( CNAMES(I+IOFFC) .NE. CHULP ) THEN
            IF ( IORDER .EQ. 2 ) THEN
               WRITE (LUNUT,1030)  I2,CHULP
            ELSE
               WRITE (LUNUT,1040)  I2,CHULP
            ENDIF
         ENDIF
      ENDIF
      I2 = I4
C
C       Determine the shift in locations
C
      ISHFT = 1
      DO 30 I4 = I1+1,NITM
         I3 = IAR(I4+IOFFC)
         IF ( I3 .GT. -1000000 ) GOTO 40
         ISHFT = ISHFT + 1
   30 CONTINUE
C
C      Shift the third array heap
C
   40 DO 50 I4 = I1, NITM
         IAR   (I4+IOFFI) = IAR(I4+IOFFI+ISHFT)
   50 CONTINUE
C
C      Shift the second array heap
C
      DO 60 I4 = I1, NITM*2+IODS
         IAR   (I4+IOFFC) = IAR   (I4+IOFFC+ISHFT)
         CNAMES(I4+IOFFC) = CNAMES(I4+IOFFC+ISHFT)
   60 CONTINUE
      NITM  = NITM  - ISHFT
      IOFFI = IOFFI - ISHFT
      IOFFC = IOFFC - 1
      IOFFI = IOFFI - 1
      ICNT  = ICNT  + ISHFT
C
C      Shift the base array heap
C
      DO 70 I5 = I2+IOFFD , NTT+IOFFD+NITM*2+IODS
         IAR   (I5) = IAR   (I5+1)
         CNAMES(I5) = CNAMES(I5+1)
   70 CONTINUE
C
C      Renumber the second array heap
C
      DO 80 I4 = I1 , NITM
         IF ( IAR(I4+IOFFC) .GT. I2 ) IAR(I4+IOFFC) = IAR(I4+IOFFC) -1
   80 CONTINUE
C
C      Update totals
C
      IF ( IORDER .EQ. 1 .OR.  IODS .GT. 0 ) THEN
         IDMNR = IDMNR-1
         NODIM = NODIM-ISHFT
      ENDIF
      IF ( IORDER .EQ. 2 .AND. IODS .EQ. 0 ) THEN
         ITMNR = ITMNR-1
         NOITM = NOITM-ISHFT
      ENDIF
C
      if (timon) call timstop( ithndl )
      RETURN
C
 1010 FORMAT ( ' WARNING: Input item : ',I3,' not resolved: ',A)
 1020 FORMAT ( ' WARNING: also not resolved: ',A)
 1030 FORMAT ( ' WARNING: Item number: ',I3,' also not resolved: ',A)
 1040 FORMAT ( ' WARNING: Substance  : ',I3,' also not resolved: ',A)
C
      END
