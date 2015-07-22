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

      SUBROUTINE DLWQJ3 ( LUNWR  , LUNUT  , IWIDTH , NOBRK  , IAR    ,
     *                    RAR    , RMAT   , NOITM  , NODIM  , IORDER ,
     *                    SCALE  , ODS    , BINFIL , IOPT   , IPRO   ,
     *                    ITFACT , DTFLG1 , DTFLG3 , IFILSZ , JFILSZ ,
     *                    CAR    , STRNG1 , STRNG2 , STRNG3 , IOUTPT )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
cjvb  ook voor constanten hebben we in delwaq2 initieel werkruimte nodig als deze kleiner is als de werkruimte
c     voor de tijdsafhankelijke zaken. Maar als er bijvoorbeeld geen tijdsafhankelijke zaken zijn komen we in
c     delwaq2 in de problemen. Voorlopig tel ik al het werkgeheugen van de contstanten mee.
C                          March 2000 L. Postma simplified
C
C     FUNCTION           : Prints and writes blocks of data
C
C     SUBROUTINES CALLED : CONVER - converting times of breakpoints
C
C     LOGICAL UNITS      : LUNWR   = unit intermediate file
C                          LUNUT   = unit formatted output file
C
C     PARAMETERS    :
C
C     NAME    KIND      LENGTH    FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNWR   INTEGER     1       INPUT   Unit of binary work file
C     LUNUT   INTEGER     1       INPUT   Unit of ASCII output file
C     IWIDTH  INTEGER     1       INPUT   Width of the output file
C     NOBRK   INTEGER     1       INPUT   number of blocks to write
C     IAR     INTEGER     *       INPUT   integer   workspace
C     RAR     REAL        *       INPUT   real      workspace
C     RMAT    REAL        *       INPUT   matrix of values
C     NOITM   INTEGER     1       INPUT   number of items to write
C     NODIM   INTEGER     1       INPUT   number of subs  to write
C     IORDER  INTEGER     1       INPUT   1 = groups of subs per item
C     SCALE   LOGICAL     1       INPUT   T = NODIM scale factors present
C     ODS     LOGICAL     1       INPUT   T = Breakpoints are converted
C     BINFIL  LOGICAL     1       INPUT   T = Info from binary file
C     IOPT    INTEGER     1       INPUT   1 is block     2 is linear
C                                         3 is harmonics 4 is fourier
C     IPRO    INTEGER     1       INPUT   0 is non permanent memory
C     ITFACT  INTEGER     1       INPUT   factor between clocks
C     DTFLG1  LOGICAL     1       INPUT   'date'-format 1st time scale
C     DTFLG3  LOGICAL     1       INPUT   'date'-format (F;ddmmhhss,T;yydddhh)
C     IFILSZ  INTEGER     1       IN/OUT  cumulative integer space count
C     JFILSZ  INTEGER     1       IN/OUT  cumulative real space count
C     CAR     CHARACTER   *       INPUT   character workspace
C     STRNG1  CHAR*(*)    1       INPUT   write string 1 (items)
C     STRNG2  CHAR*(*)    1       INPUT   write string 2 (values/concs)
C     STRNG3  CHAR*(*)    1       INPUT   write string 3 (brkp/harm)
C     IOUTPT  INTEGER     1       INPUT   output file option
C
C
      use timers       !   performance timers

      DIMENSION     IAR(*) , RAR(*) , RMAT(*)
      LOGICAL       SCALE  , ODS    , BINFIL , DEFLTS
      LOGICAL       DTFLG1 , DTFLG3
      CHARACTER*(*) STRNG1 , STRNG2 , STRNG3 , CAR(*)
C
C     Local declarations
C
      CHARACTER*20  CAR_OF_DUM
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwqj3", ithndl )
C
C     Write headers
C
      DEFLTS = .FALSE.
      IF ( NODIM .LT. 0 ) DEFLTS = .TRUE.
      NODI2 = NODIM
      IF ( NODIM .LE. 0 ) NODI2 = 1
      IF ( IORDER .EQ. 1 ) THEN
         WRITE ( LUNUT , 1000 ) NOITM, NODI2, STRNG2
         WRITE ( LUNWR ) IORDER,
     *                   NOITM, ( IAR(K) , K=      1,      NOITM ) ,
     *                   NODIM, ( IAR(K) , K=NOITM+1,NOITM+NODIM ) ,
     *                   IOPT , IPRO
      ELSEIF ( IORDER .EQ. 2 ) THEN
         WRITE ( LUNUT , 1000 ) NODI2, NOITM, STRNG1
         IF ( LUNWR .GT. 0 )
     *      WRITE ( LUNWR ) IORDER,
     *                   NODIM, ( IAR(K) , K=      1,      NODIM ) ,
     *                   NOITM, ( IAR(K) , K=NODIM+1,NODIM+NOITM ) ,
     *                   IOPT , IPRO
      ENDIF
cjvb  IF ( IOPT .NE. 0 .AND. .NOT. BINFIL )
cjvb1 IF ( IOPT .NE. 0 )
cjvb1*                   IFILSZ = IFILSZ + NOITM + MAX(0,NODIM) + 5
                         IFILSZ = IFILSZ + NOITM + MAX(0,NODIM) + 5
cjvb1
C
C          just declare array space for binary files and return
C
      IF ( BINFIL ) THEN
         WRITE ( LUNUT , 1130 ) IPRO
         IFILSZ = IFILSZ + 3
         JFILSZ = JFILSZ + MAX(1,NODIM)*MAX(1,NOITM)*3
         GOTO 70
      ENDIF
C
C       Initialisation
C
      IF ( NOBRK .EQ. 0 ) THEN
         SCALE = .FALSE.
         GOTO 70
      ENDIF
      IOFFB = NOITM + NODI2+ 1
      IOFFI = 0
      IOFFS = NOITM
      ISKIP = 1
      ISKP2 = NODI2
      NOTOT = NOITM*NODI2
      IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) NOTOT = NOTOT + 1
      IF ( IORDER .EQ. 2 ) THEN
         IOFFI = MAX(NODIM,0)
         IOFFS = 0
         ISKIP = NOITM
         ISKP2 = 1
      ENDIF
C
C       Scale factors
C
      ISS = 1
      IF ( SCALE ) THEN
         SCALE = .FALSE.
         ISS = 1
         IF ( IOUTPT .GE. 4 ) THEN
            WRITE ( LUNUT , 1010 )
            DO 10 I2 = 1,NODIM,IWIDTH
               IE = MIN(I2+IWIDTH-1,NODIM)
               WRITE (LUNUT,1020) (    IAR(IOFFS+K) ,K=I2,IE)
               WRITE (LUNUT,1025) (CAR_OF_DUM(CAR,IAR(IOFFS+K)),K=I2,IE)
               WRITE (LUNUT,1030) (RAR(K),K=I2,IE)
   10       CONTINUE
         ENDIF
         DO 30 I1 = 1,NOBRK
            DO 20 I2 = 0,NOTOT-1
               IF ( IORDER .EQ. 1 ) ITEL2 = MOD(I2,NODIM) + 1
               IF ( IORDER .EQ. 2 ) ITEL2 = I2/NODIM + 1
               RMAT(ISS+I2) = RMAT(ISS+I2)*RAR(ITEL2)
   20       CONTINUE
            ISS = ISS + NOTOT
   30    CONTINUE
      ENDIF
C
C       Convert breakpoints
C
      IF ( NOBRK .GT. 1 ) THEN
         IF ( IOUTPT .GE. 4 ) WRITE ( LUNUT , 1040 ) STRNG3, NOBRK
         IF ( .NOT. ODS )
     *          CALL CONVER ( IAR(IOFFB), NOBRK, ITFACT, DTFLG1, DTFLG3)
         IF ( DEFLTS .AND. IOUTPT .GE. 4 ) WRITE ( LUNUT , 1050 )
      ELSE
         IF ( DEFLTS ) THEN
            IF ( IOUTPT .GE. 4 ) WRITE ( LUNUT , 1050 )
         ELSE
            IF ( IOUTPT .GE. 4 ) WRITE ( LUNUT , 1060 )
         ENDIF
      ENDIF
C
C       Write binary file
C
      IF ( LUNWR .GT. 0 ) THEN
         I1DUM = 0
         I2DUM = 0
         CALL DLWQJ2 ( LUNWR , NOBRK , NOTOT  , 1      , IAR(IOFFB) ,
     *                                 RMAT   , I1DUM  , I2DUM      )
cjvb1    IF ( IOPT .NE. 0 ) THEN
            IFILSZ = IFILSZ + I1DUM
            JFILSZ = JFILSZ + I2DUM
cjvb1    ENDIF
      ENDIF
C
C       Write formatted output
C
      IF ( IOUTPT .GE. 4 ) THEN
         ITELS = 0
         DO 60 I1 = 1,NOBRK
            IF ( NOBRK .GT. 1 ) THEN
               IF ( IOPT .EQ. 1 )
     *                 WRITE ( LUNUT, 1070 ) STRNG3, I1, IAR(IOFFB+I1-1)
               IF ( IOPT .EQ. 2 )
     *                 WRITE ( LUNUT, 1070 ) STRNG3, I1, IAR(IOFFB+I1-1)
               IF ( IOPT .EQ. 3 ) THEN
                  ITELS = ITELS + 1
                  WRITE ( LUNUT, 1080 ) I1, IAR(IOFFB+I1-1), RMAT(ITELS)
               ENDIF
               IF ( IOPT .EQ. 4 ) THEN
                  ITELS = ITELS + 1
                  WRITE ( LUNUT, 1090 ) I1, IAR(IOFFB+I1-1), RMAT(ITELS)
               ENDIF
            ENDIF
            DO 50 I2 = 1,NODI2,IWIDTH
               IE2 = MIN(I2+IWIDTH-1,NODI2)
               IF ( NODIM .GT. 0 ) THEN
                  WRITE ( LUNUT, 1100 ) STRNG2, (IAR(IOFFS+K) ,K=I2,IE2)
                  WRITE ( LUNUT, 1150 ) STRNG1,
     *                           (CAR_OF_DUM(CAR,IAR(IOFFS+K)),K=I2,IE2)
               ENDIF
               ITEL = ITELS
               DO 40 I3 = 1,NOITM
                  WRITE ( LUNUT, 1120 )  ABS(IAR(IOFFI+I3)),
     *            ( RMAT(ITEL+K),K=(I2-1)*ISKIP+1,(IE2-1)*ISKIP+1,ISKIP)
                  ITEL = ITEL + ISKP2
   40          CONTINUE
   50       CONTINUE
            ITELS = ITELS + NODI2*NOITM
   60    CONTINUE
      ENDIF
C
   70 WRITE ( LUNUT , 1140 )
      if (timon) call timstop( ithndl )
      RETURN
C
 1000 FORMAT (/' DATA grouped in',I10,' blocks of',I10,' ',A )
 1010 FORMAT ( ' Scale factors for this block of data: ' )
 1020 FORMAT ( ' Scale    :' ,I6,9I12 )
 1025 FORMAT ( ' Substance:' , 10('  ',A10) )
 1030 FORMAT ( ' values   :' , 10E12.4 )
 1040 FORMAT (/' Number of ',A,'s with full data:',I5 )
 1050 FORMAT ( ' Default values in this block.' )
 1060 FORMAT ( ' Constant values in this block.' )
 1070 FORMAT ( ' ',A,' ',I7,' :',I10 )
 1080 FORMAT ( ' Harmonic: ',I3,' :',I10,' Phase: ',10E12.4 )
 1090 FORMAT ( ' Fourier : ',I3,' :',I10,' Phase: ',10E12.4 )
 1100 FORMAT ( ' ',A,I6,9I12)
 1150 FORMAT ( ' ',A,' ' , 10('  ',A10) )
 1120 FORMAT (   I10,2X,1P,10E12.4 )
 1130 FORMAT ( ' Info comes at runtime from binary file at unit: ',I3 )
 1140 FORMAT(/' ====> input item completed <==== '//   )
 1160 FORMAT ( ' All data from this block ignored' )
C
      END
C
      CHARACTER*20 FUNCTION CAR_OF_DUM(CAR,I)
      INTEGER       I
      CHARACTER*(*) CAR(*)
      IF ( I .GT. 0 ) THEN
         CAR_OF_DUM = CAR(I)
      ELSEIF ( I .EQ. 0 ) THEN
         CAR_OF_DUM = 'FLOW'
      ELSE
         CAR_OF_DUM = 'ignored'
      ENDIF
      RETURN
      END
