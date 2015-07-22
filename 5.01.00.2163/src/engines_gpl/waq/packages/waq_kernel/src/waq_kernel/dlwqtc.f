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

      SUBROUTINE DLWQTC ( LUNUT  , IOFF   , A      , J      , IIPNT  ,
     *                    IRPNT  , IIMAX  , ITIME  , KTYPE  , FUN    ,
     +                    SFUN   , NOSEG  , IVAL   , IERR   , ISFLAG ,
     +                    IFFLAG , LTXT   , ftype  , dlwqd  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : april 1996 by L. Postma
C
C     UPDATED             : July    2002 by Leo Postma
C                           Call to DLWQT4 changed and file arrays in T4.
C
C     FUNCTION            : Updates the functions and segment functions
C
C     LOGICAL UNITNUMBERS : LUNUT - monitoring file
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNUT   INTEGER    1        INPUT   unit number monitoring file
C     IOFF    INTEGER    1        INPUT   index of first concentration
C     A       REAL       ?        INPUT   Real    boundary workspace
C     J       INTEGER    ?        INPUT   Integer boundary workspace
C     IIPNT   INTEGER    1        IN/OUT  Offset in integer array space
C     IRPNT   INTEGER    1        IN/OUT  Offset in real array space
C     IIMAX   INTEGER    1        INPUT   Maximum integer array size
C     ITIME   INTEGER    1        INPUT   Time in units of the system clock
C     KTYPE   INTEGER   NOITM     INPUT   Type of items
C     FUN     REAL       *        OUTPUT  Values of the functions
C     SFUN    REAL   NOSFUN,NOSEG OUTPUT  Values of the segment functions
C     IVAL    INTEGER NOTOT,NOITM LOCAL   Count array for averages
C     IERR    INTEGER    1        IN/OUT  error count
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C     LTXT  CHAR*(*)   *        INPUT   array with filenames binary fils
C
C     Declaration of arguments
C
      use timers
      use delwaq2_data

      PARAMETER     ( TWOPI = 6.28319 )

      integer, intent(in   )           :: ftype(*)  !<  file types
      type(delwaq_data), intent(inout) :: dlwqd

      DIMENSION       A(*)    , J(*) , KTYPE(*) , FUN(*) , SFUN(*) ,
     *                IVAL(*)
      CHARACTER*(*)   LTXT(*)
C
C     Local declarations
C
      LOGICAL         LDUMMY, LDUMM2
      REAL            RDUMMY(1)
      INTEGER         LUN(1)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqtc", ithandl )
C
C         initialise the system
C
      AMISS = -999.
C       Number of items
      IA = 0
      IJ = 0
C
C       Determine switch for this block 1 = items,subst
C                                       2 = subst,items
C       Start of the loop over blocks
C
C       Determine parameters NPNT = nr of items
C                            NPST = start of item nr's in the J-array
C                            NDIM = nr of substances
C                            NDST = start of subs nr's in the J-array
C                            IOPT = option 1 and 2 at breakpoints etc.
C                            IPRO = procedure (overrule or not)
C                                   800 < IPRO < 900 = binary file
C
   10 IJ   = IJ + 1
      IORD = J(IJ)
      IJ   = IJ + 1
      IJS  = IJ
      IF ( IORD .EQ. 1 ) THEN
         NPST = IJ
         NPNT = J(NPST)
         NDST = NPST+MAX(NPNT,0)+1
         NDIM = J(NDST)
      ENDIF
      IF ( IORD .EQ. 2 ) THEN
         NDST = IJ
         NDIM = J(NDST)
         NPST = NDST+MAX(NDIM,0)+1
         NPNT = J(NPST)
      ENDIF
      IJ   = IJ+MAX(NDIM,0)+MAX(NPNT,0)+4
      IOPT = J(IJ-2)
      IPRO = J(IJ-1)
      NTT  = NDIM*NPNT
      IF ( NDIM .LE. 0 ) NTT = NPNT
      IF ( NPNT .LE. 0 ) NTT = NDIM
      IF ( IPRO .GT. 800 .AND. IPRO .LT. 900 ) THEN
         IA = IA + 1
         IF ( IOPT .GT. 0 ) THEN
            IF ( NDIM .EQ. 0 ) THEN
C           functions
C         5 arguments of integer and real array space removed
C         LUN array instead of explicit luns in arguments
              CALL DLWQT4 ( LUN     , LTXT    , ftype   , LUNUT  , IPRO    ,
     *                      ITIME   , FUN     , J(IJS+1),   1    , NTT     ,
     *                      ISFLAG  , IFFLAG  , LDUMMY  ,   0    , .FALSE. ,
     *                      LDUMM2  , RDUMMY  , dlwqd   )
            ELSE
C           segment functions
C         5 arguments of integer and real array space removed
C         LUN array instead of explicit luns in arguments
              CALL DLWQT4 ( LUN     , LTXT    , ftype   , LUNUT  , IPRO    ,
     *                      ITIME   , SFUN    , J(IJS+1), NOSEG  , NTT     ,
     *                      ISFLAG  , IFFLAG  , LDUMMY  ,   0    , .FALSE. ,
     *                      LDUMM2  , RDUMMY  , dlwqd   )
            ENDIF
         ENDIF
         IJ = IJ + 3 - 1
         IA = IA + MAX(1,NDIM)*MAX(1,NPNT)*3 - 1
         GOTO 150
      ENDIF
C
C       Nr of breakpoints or harmonics
C
      NOBRK = J(IJ)
C
C       IOPT = 1 : Block function , IOPT = 2 : Linearly interpolated
C
      IF ( IOPT .EQ. 1 .OR. IOPT .EQ. 2 ) THEN
C
C           Get the right time in the block
C
         IF ( NOBRK .GT. 1 ) THEN
            ITIM1 = J(IJ+1)
            ITIM2 = J(IJ+NOBRK)
            IDT   = ITIM2 - ITIM1
            IF ( ITIME .LT. ITIM1 ) THEN
               IREC = 1
               ITIM1 = 0
               ITIM2 = 1
               IDT   = ITIM1+ITIM2
               GOTO 50
            ENDIF
            ITIMF = ITIME
            IF ( ITIME .GE. ITIM2 )
     *                ITIMF = ITIME - ( (ITIME-ITIM2)/IDT + 1 ) * IDT
C
C           Make interpolation constants if IOPT = 2
C
            DO 40 I = 2 , NOBRK
               IF ( J(IJ+I) .GT. ITIMF ) THEN
                  IF ( IOPT .EQ. 2 ) THEN
                     ITIM1 = ITIMF   - J(IJ+I-1)
                     ITIM2 = J(IJ+I) - ITIMF
                  ELSE
                     ITIM1 = 0
                     ITIM2 = 1
                  ENDIF
                  IDT   = ITIM1+ITIM2
                  IREC  = I-1
                  GOTO 50
               ENDIF
   40       CONTINUE
         ELSE
            IREC  = 1
            ITIM2 = 1
            ITIM1 = 0
            IDT   = 1
         ENDIF
C
C           Set or interpolate the correct values
C
   50    I = IA + (IREC-1)*NTT
C           Inner loop in A over the substances
         IF ( IORD .EQ. 1 ) THEN
            DO 70 I1 = 1 , NPNT
               IB = J(NPST+I1)
               IF ( NDIM .NE. 0 ) IB = (IB-1)*NOSEG
               DO 60 I2 = 1 , MAX(NDIM,1)
                  I  = I + 1
                  AA = A(I)
                  AB = A(I+NTT)
                  IT1C = ITIM1
                  IT2C = ITIM2
                  IDTC   = IDT
C     Dealing with missing values
                  IF ( AA .EQ. AMISS .OR. AB .EQ. AMISS )
     *                  CALL DLWMIS ( A   , I   , AMISS, NTT  , IREC,
     *                                J   , IJ  , NOBRK, ITIMF, IOPT,
     *                                IT1C, IT2C, IDTC , AA   , AB  )
C           Make the wanted value
                  AA = ( IT2C*AA + IT1C*AB ) / IDTC
                  IF ( NDIM .EQ. 0 )  FUN(IB           )  = AA
                  IF ( NDIM .GT. 0 ) SFUN(IB+J(NDST+I2))  = AA
                  IF ( NDIM .LT. 0 ) THEN
                     DO 55 ISF = 1 , NOSEG
                        SFUN(IB+ISF) = AA
   55                CONTINUE
                  ENDIF
   60          CONTINUE
C
   70       CONTINUE
C           Inner loop in A over the items
         ELSE
            DO 90 I1 = 1 , NDIM
               IC = J(NDST+I1)
               DO 80 I2 = 1 , NPNT
                  I  = I + 1
                  AA = A(I)
                  AB = A(I+NTT)
                  IT1C = ITIM1
                  IT2C = ITIM2
                  IDTC   = IDT
C     Dealing with missing values
                  IF ( AA .EQ. AMISS .OR. AB .EQ. AMISS )
     *               CALL DLWMIS ( A    , I    , AMISS , NTT   , IREC ,
     *                             J    , IJ   , NOBRK , ITIMF , IOPT ,
     *                             IT1C , IT2C , IDTC  , AA    , AB   )
C           Make the wanted value
                  AA = ( IT2C*AA + IT1C*AB ) / IDTC
                  IB = (J(NPST+I2)-1)*NOSEG
                  SFUN(IB+IC)  = AA
   80          CONTINUE
   90       CONTINUE
         ENDIF
         IJ = IJ + NOBRK
         IA = IA + NOBRK*NTT
      ENDIF
C
C       IOPT = 3 and 4 : Harmonics and fouriers, treated equally
C
      IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) THEN
C
         DO 140 I = 1 , NOBRK
C
C            harmonic function
C
            IJ = IJ + 1
            IPERIO = J(IJ)
            IA = IA + 1
            APHASE = A(IA)
c           WRITE ( LUNUT , * ) ' I, IA, APHASE: ',I, IA,APHASE
            IF ( I .EQ. 1 ) THEN
               FUNC = 1.0
            ELSE
               FUNC = SIN( (FLOAT(ITIME)/IPERIO-APHASE)*TWOPI )
            ENDIF
C
C            multiply with amplitudes and set values
C
C              Inner loop in A over the substances
            IF ( IORD .EQ. 1 ) THEN
               DO 110 I1 = 1 , NPNT
                  IB = J(NPST+I1)
                  IF ( NDIM .NE. 0 ) IB = (IB-1)*NOSEG
                  DO 100 I2 = 1 , MAX(NDIM,1)
                     IC = J(NDST+I2)
                     IA = IA + 1
                     IF ( I .EQ. 1 ) THEN
                        IF ( NDIM .EQ. 0 ) FUN(IB) = FUNC*A(IA)
                        IF ( NDIM .GT. 0 )
     *                         SFUN(IB+J(NDST+I2)) = FUNC*A(IA)
                        IF ( NDIM .LT. 0 ) THEN
                           AA = FUNC*A(IA)
                           DO 95 ISF = 1 , NOSEG
                               SFUN(IB+ISF) = AA
   95                      CONTINUE
                        ENDIF
                     ELSE
                        IF ( NDIM .EQ. 0 ) FUN(IB) = FUNC*A(IA)+FUN(IB)
                        IF ( NDIM .GT. 0 )
     *                         SFUN(IB+J(NDST+I2)) =
     *                         SFUN(IB+J(NDST+I2)) + FUNC*A(IA)
                        IF ( NDIM .LT. 0 ) THEN
                           AA = FUNC*A(IA)
                           DO 98 ISF = 1 , NOSEG
                               SFUN(IB+ISF) = SFUN(IB+ISF) + AA
   98                      CONTINUE
                        ENDIF
                     ENDIF
  100             CONTINUE
  110          CONTINUE
C              Inner loop in A over the items
            ELSE
               DO 130 I1 = 1 , NDIM
                  IC = IOFF+J(NDST+I1)
                  DO 120 I2 = 1 , NPNT
                     IB = (J(NPST+I2)-1)*NOSEG
                     IA = IA + 1
                     IF ( I .EQ. 1 ) THEN
                        SFUN(IB+IC) = FUNC*A(IA)
                     ELSE
                        SFUN(IB+IC) = FUNC*A(IA) + SFUN(IB+IC)
                     ENDIF
  120             CONTINUE
  130          CONTINUE
            ENDIF
  140    CONTINUE
      ENDIF
C
C       Return until finished
C
  150 IF ( IJ .LT. IIMAX ) GOTO 10
      IF ( IJ .EQ. IIMAX ) THEN
         IIPNT = IIPNT + IJ
         IRPNT = IRPNT + IA
         goto 9999
      ENDIF
      WRITE ( LUNUT , 2010 ) IJ, IIMAX
      IERR = IERR + 1
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 2000 FORMAT ( ' WARNING: System time: ',I10,' earlier than function',
     *         ' time: ',I10,' Function skipped !' )
 2010 FORMAT ( ' ERROR, updating time functions new style !',2I9 )
C
      END
C
