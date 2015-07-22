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

      SUBROUTINE DLWQ5E ( LUNUT , IAR    , NOITM , ITMNR , NODIM  ,
     *                    IDMNR , IORDER , RAR   , IOPT  , RMAT   ,
     *                    NOCOL , NOBRK  , AMISS , IARP  , RMATU  )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May   '98 by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : Assign matrix according to computational
C                          rules
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : LUNUT - report file
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IAR     INTEGER    *         INPUT   array with arithmetic rules
C     NOITM   INTEGER    1         IN      number of items for computation
C     ITMNR   INTEGER    1         IN      number of items for output
C     NODIM   INTEGER    1         IN/OUT  number of conc. for comput.
C     IDMNR   INTEGER    1         IN/OUT  number of conc. for output
C     IORDER  INTEGER    1         INPUT   =1 items first: =2 concen first
C     RAR     REAL       *         INPUT   real constants in formulae
C     IOPT    LOGICAL    *         INPUT   3 & 4 is Fourier or harmonics
C     RMAT    REAL       *         INPUT   real matrix of read values
C     NOBRK   INTEGER    1         OUTPUT  number of records read
C     AMISS   REAL       1         INPUT   this is a missing value
C     IARP    INTEGER    *         INPUT   array with item pointers in RMAT
C     RMATU   REAL       *         OUTPUT  real matrix of evaluated values
C
C
      use timers       !   performance timers

      DIMENSION     IAR(*) , IARP(*) , RAR(*)  , RMAT(*) , RMATU(*)
      LOGICAL       MINIEM , MAXIEM
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5e", ithndl )
C
C     Some initialisation
C
      MINIEM = .FALSE.
      MAXIEM = .FALSE.
C     Loop end-value in integer array to determine IP
      IOFF1  = NOITM+ITMNR+IDMNR+NODIM
      IF ( IORDER .EQ. 1 ) THEN
         IOFF0  = NOITM+ITMNR+IDMNR
         LOCBAS = NODIM
      ENDIF
      IF ( IORDER .EQ. 2 ) THEN
         IOFF0  = NODIM+IDMNR+ITMNR
         LOCBAS = NOITM
      ENDIF
      ILOC  = 0
C          counter in output matrix
      ITEL  = 0
      ACCUM = 0.0
      ITELS = 0
C          implied loop counter for matrix, outer loop
      IFRST = 0
      IF ( IORDER .EQ. 1 .AND. NOITM .EQ. 0 ) IFRST = -1
      IF ( IORDER .EQ. 2 .AND. NODIM .EQ. 0 ) IFRST = -1
      IBRK  = 1
      IOFF  = 0
C
C     Assignment loop
C
C          if harmonics then deal with the phase
   10 IF ( ILOC .EQ. 0 .AND. (IOPT.EQ.3.OR.IOPT.EQ.4)
     *                 .AND.  IFRST .EQ. 0            ) THEN
         IOFF  = IOFF  + 1
         ACCUM = RMAT(IOFF)
         ITEL  = ITEL + 1
         RMATU(ITEL) = 0
      ENDIF
      ILOC = ILOC + 1
      IP   = IAR(ILOC+IOFF0)
      IP2  = IAR(ILOC+IOFF1)
C          normal processing
      IF ( IP .GT. -900000 ) THEN
C          close pending arrithmatic in the PREVIOUS ITEL
         IF ( ITEL .NE. 0 ) THEN
            IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               RMATU(ITEL) = RMATU(ITEL) + ACCUM
            ELSE
               RMATU(ITEL) = AMISS
            ENDIF
            IF ( MAXIEM ) THEN
               IF ( RMATU(ITEL) .GT. AMAXV .AND.
     *              RMATU(ITEL) .NE. AMISS        ) THEN
                  WRITE ( LUNUT , 1000 ) IBRK, ILOCO, IFRST+1
                  WRITE ( LUNUT , 1010 ) RMATU(ITEL), AMAXV
                  RMATU(ITEL) = AMAXV
               ENDIF
            ENDIF
            IF ( MINIEM ) THEN
               IF ( RMATU(ITEL) .LT. AMINV .AND.
     *              RMATU(ITEL) .NE. AMISS        ) THEN
                  WRITE ( LUNUT , 1000 ) IBRK, ILOCO, IFRST+1
                  WRITE ( LUNUT , 1020 ) RMATU(ITEL), AMINV
                  RMATU(ITEL) = AMINV
               ENDIF
            ENDIF
            MAXIEM = .FALSE.
            MINIEM = .FALSE.
         ENDIF
         IF ( IP .GT. 0 ) THEN
            ACCUM = RMAT(IP2+IOFF)
         ELSE
            ACCUM = RAR(-IP)
         ENDIF
         ITEL  = ITEL + 1
         RMATU(ITEL) = 0
         ILOCO = IP
      ENDIF
C          ignore value
      IF ( IP .LE. -1300000000 ) THEN
         IP = 0
      ENDIF
C          a maximum value need to be applied
      IF ( IP .LE. -1190000000 ) THEN
         IP = IP +  1200000000
         IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
            RMATU(ITEL) = RMATU(ITEL) + ACCUM
         ELSE
            RMATU(ITEL) = AMISS
         ENDIF
         ACCUM = 0.0
         MAXIEM = .TRUE.
         IF ( IP .EQ. 0 ) THEN
            AMAXV = RMAT(IP2+IOFF)
         ENDIF
         IF ( IP .LT. 0 ) AMAXV = RAR(-IP)
         IF ( IP .GT. 0 ) AMAXV = RMATU(ITELS+IP)
      ENDIF
C          a minimum value need to be applied
      IF ( IP .LE. -1090000000 ) THEN
         IP = IP +  1100000000
         IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
            RMATU(ITEL) = RMATU(ITEL) + ACCUM
         ELSE
            RMATU(ITEL) = AMISS
         ENDIF
         ACCUM = 0.0
         MINIEM = .TRUE.
         IF ( IP .EQ. 0 ) THEN
            AMINV = RMAT(IP2+IOFF)
         ENDIF
         IF ( IP .LT. 0 ) AMINV = RAR(-IP)
         IF ( IP .GT. 0 ) AMINV = RMATU(ITELS+IP)
      ENDIF
C          a minus sign need to be applied
      IF ( IP .LE. -900000000 ) THEN
         IP = IP + 1000000000
         IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
            RMATU(ITEL) = RMATU(ITEL) + ACCUM
         ELSE
            RMATU(ITEL) = AMISS
         ENDIF
         IF ( IP .EQ. 0 ) THEN
            ACCUM = -RMAT(IP2+IOFF)
         ENDIF
         IF ( IP .LT. 0 ) ACCUM = -RAR(-IP)
         IF ( IP .GT. 0 ) ACCUM = -RMATU(ITELS+IP)
         IF ( ACCUM .EQ. -AMISS ) ACCUM = AMISS
      ENDIF
C          a plus sign need to be applied
      IF ( IP .LE. -90000000  ) THEN
         IP = IP + 100000000
         IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
            RMATU(ITEL) = RMATU(ITEL) + ACCUM
         ELSE
            RMATU(ITEL) = AMISS
         ENDIF
         IF ( IP .EQ. 0 ) THEN
            ACCUM =  RMAT(IP2+IOFF)
         ENDIF
         IF ( IP .LT. 0 ) ACCUM =  RAR(-IP)
         IF ( IP .GT. 0 ) ACCUM =  RMATU(ITELS+IP)
      ENDIF
C          a division need to be applied
      IF ( IP .LE. -9000000   ) THEN
         IP = IP + 10000000
         IF ( IP .EQ. 0 ) THEN
            IF ( RMAT(IP2+IOFF) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM / RMAT(IP2+IOFF)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
         IF ( IP .LT. 0 ) THEN
            IF ( RAR(-IP) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM / RAR(-IP)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
         IF ( IP .GT. 0 ) THEN
            IF ( RMAT(ITELS+IP) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM / RMATU(ITELS+IP)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
      ENDIF
C          a multiplication need to be applied
      IF ( IP .LE. -900000    ) THEN
         IP = IP + 1000000
         IF ( IP .EQ. 0 ) THEN
            IF ( RMAT(IP2+IOFF) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM * RMAT(IP2+IOFF)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
         IF ( IP .LT. 0 ) THEN
            IF ( RAR(-IP) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM * RAR(-IP)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
         IF ( IP .GT. 0 ) THEN
            IF ( RMAT(ITELS+IP) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
               ACCUM = ACCUM * RMATU(ITELS+IP)
            ELSE
               ACCUM = AMISS
            ENDIF
         ENDIF
      ENDIF
      IF ( ILOC .EQ. LOCBAS ) THEN
         IF ( RMATU(ITEL) .NE. AMISS .AND. ACCUM .NE. AMISS ) THEN
            RMATU(ITEL) = RMATU(ITEL) + ACCUM
         ELSE
            RMATU(ITEL) = AMISS
         ENDIF
         IF ( MAXIEM ) THEN
            IF ( RMATU(ITEL) .GT. AMAXV .AND.
     *           RMATU(ITEL) .NE. AMISS       ) THEN
               WRITE ( LUNUT , 1000 ) IBRK, ILOCO, IFRST+1
               WRITE ( LUNUT , 1010 ) RMATU(ITEL), AMAXV
               RMATU(ITEL) = AMAXV
            ENDIF
         ENDIF
         IF ( MINIEM ) THEN
            IF ( RMATU(ITEL) .LT. AMINV .AND.
     *           RMATU(ITEL) .NE. AMISS       ) THEN
               WRITE ( LUNUT , 1000 ) IBRK, ILOCO, IFRST+1
               WRITE ( LUNUT , 1020 ) RMATU(ITEL), AMINV
               RMATU(ITEL) = AMINV
            ENDIF
         ENDIF
         MAXIEM = .FALSE.
         MINIEM = .FALSE.
         ACCUM = 0.0
         IFRST = IFRST + 1
         ITELS = ITEL
         ILOC  = 0
         IOFF  = IOFF + NOCOL
      ENDIF
C        are we to expect a new record ?
      IF ( ( IORDER .EQ. 1 .AND. IFRST.EQ.NOITM ) .OR.
     *     ( IORDER .EQ. 2 .AND. IFRST.EQ.NODIM ) )THEN
         IFRST  = 0
         IF ( IORDER .EQ. 1 .AND. NOITM .EQ. 0 ) IFRST = -1
         IF ( IORDER .EQ. 2 .AND. NODIM .EQ. 0 ) IFRST = -1
         IBRK   = IBRK + 1
      ENDIF
      IF ( IBRK .LE. NOBRK ) GOTO 10
C        compact the pointers
      IOFF1  = IOFF1+LOCBAS
      IF ( IORDER .EQ. 1 ) THEN
         IOFF0  = ITMNR
         IOFF2  = NOITM+ITMNR
         DO 20 I = 1,IDMNR
            IAR(IOFF0+I) = IAR(IOFF2+I)
   20    CONTINUE
         DO 30 I = 1,NOBRK
            IAR(IOFF0+IDMNR+I) = IAR(IOFF1+I)
   30    CONTINUE
      ENDIF
      IF ( IORDER .EQ. 2 ) THEN
         IOFF0  = IDMNR
         IOFF2  = NODIM+IDMNR
         DO 40 I = 1,ITMNR
            IAR(IOFF0+I) = IAR(IOFF2+I)
   40    CONTINUE
         DO 50 I = 1,NOBRK
            IAR(IOFF0+ITMNR+I) = IAR(IOFF1+I)
   50    CONTINUE
      ENDIF
      if (timon) call timstop( ithndl )
      RETURN
C
 1000 FORMAT ( ' INFO: Processing breakpoint',I6,' for substance',I3,
     *         ' at station',I5)
 1010 FORMAT ( ' the value of ',E15.6,' is overwritten by the maximum ',
     *         ' of ',E15.6,' !' )
 1020 FORMAT ( ' the value of ',E15.6,' is overwritten by the minimum ',
     *         ' of ',E15.6,' !' )
C
      END
