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

      SUBROUTINE DLWQIA ( LUN    , LUNUT  , A      , J      , MODE   ,
     *                    CONST  , PARAM  , NOSEG  , NOPA   , IISP   ,
     *                    IRSP   , ISFLAG , IFFLAG , ITIME  , LTXT   ,
     *                                                        IERR   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1996 by L. Postma
C
C     FUNCTION            : Initialises the complete boundary subsystem
C
C     LOGICAL UNITNUMBERS : LUN   - binary boundary system file
C                           LUNUT - monitoring file
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER    1        INPUT   unit number input file
C     A       REAL       ?        IN/OUT  Real      boundary workspace
C     J       INTEGER    ?        IN/OUT  Integer   boundary workspace
C     MODE    INTEGER    1        INPUT   File number involved
C     CONST   REAL       *        OUTPUT  Constants read
C     PARAM   REAL       *        OUTPUT  Parameters read
C     NOSEG   INTEGER    1        INPUT   Nr of segments
C     IISP    INTEGER    1        IN/OUT  Integer array space pointer
C     IRSP    INTEGER    1        IN/OUT  Real array space pointer
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C     ITIME   INTEGER    1        INPUT   system timer
C     LTXT  CHAR*(*)   *        INPUT   array with filenames binary fils
C     IERR    INTEGER    1        IN/OUT  error count
C
C     Declaration of arguments
C
      use timers

      DIMENSION       A(*)   , J(*) , CONST(*) , PARAM(*)
      CHARACTER*(*)   LTXT(*)
      CHARACTER*80    C80
      LOGICAL         LOPEN
C
C     Local declarations
C
      LOGICAL         LDUMMY, LDUMM2
      REAL            RDUMMY(1)
      INTEGER         IDUMMY(1)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqia", ithandl )
C
C         initialise the system
C
      IA = 1
      IJ = 1
      ITEL = 0
      IDUMMY(1) = 1
C
C       Read the system dimensions
C
c     WRITE ( LUNUT , * ) MODE
C
C       Read the pointers for this block of data
C
C     IOPT =  0 means that input is not time dependent
C     NDIM =  0 means a block of constants comes
C     NDIM = -1 means that defaults for parameters come
C     NDIM >  0 means that parameter+segment nr comes from the block
C     IOPT = 1,2,3 or 4 means that input is time dependent
C     IORDER = 1 means that NPNT is the number of items and
C                           NDIM is the number of infor per item
C     IORDER = 2 means the reverse
C     IPRO between 800 and 899 is the unit nr of a binary file
C
   10 READ ( LUN , END=40 , ERR=110 )  J(IJ),
     *                NPNT, ( J(IJ+K              ) , K = 2,NPNT+1 ) ,
     *                NDIM, ( J(IJ+K+MAX(NPNT,0)+2) , K = 1,NDIM   ) ,
     *                IOPT, IPRO
      IORDER = J(IJ)
      J( IJ               + 1 ) = NPNT
      J( IJ + MAX(NPNT,0) + 2 ) = NDIM
      IJS= IJ + 1
      IKS= IJ + MAX(NPNT,0) + 2
      IAS= IA - 1
      IJ = IJ + MAX(NPNT,0) + MAX(NDIM,0) + 5
      J(IJ-2) = IOPT
      J(IJ-1) = IPRO
C        default for all items
      NTOT = NPNT*NDIM
      IF ( NPNT .LE. 0 ) NTOT = NDIM
      IF ( NDIM .LE. 0 ) NTOT = NPNT
      IF ( IPRO .GT. 800 .AND. IPRO .LT. 900 ) THEN
         ILT = IPRO - 800
         IF ( IOPT .EQ. 0 ) THEN
            IF ( NDIM .EQ. 0 ) THEN
C              constants
               CALL DLWQT5( IPRO  , LUNUT    ,ITIME   , A(IA), CONST,
     *                      IDUMMY, 1        ,J(IJS+1), NPNT , 1    ,
     *                      NTOT  , LTXT(ILT),ISFLAG )
            ELSE
C              parameters
               CALL DLWQT5( IPRO    , LUNUT    , ITIME   , A(IA), PARAM,
     *                      J(IJS+1), NPNT     , J(IKS+1), NDIM , NOPA ,
     *                      NTOT    , LTXT(ILT), ISFLAG  )
            ENDIF
         ELSEIF ( IOPT .EQ. 1 ) THEN
            DO I = 1 , NPNT
               J(IJS+I) = -J(IJS+I)
            ENDDO
         ENDIF
C  NOSUB  is first complete dimension of the result
C  IPOINT points into the second dimension of the result
         IJ = IJ + 3
         IA = IA + MAX(1,NDIM)*MAX(1,NPNT)*3
         GOTO 10
      ENDIF
C
C       Nr of breakpoints or harmonics
C
      READ ( LUN , END=100 , ERR=110 ) NOBRK
      J(IJ) = NOBRK
      IJ = IJ + 1
C
C       3 and 4 are harmonics, then an additional real comes in
C
      NTAL = 0
      IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) NTAL  = 1
C
      DO 20 I=1,NOBRK
      IF ( IOPT .EQ. 0 ) THEN
C                 read the non - time functions
         IF ( NDIM .EQ. 0 ) THEN
C                 read the constants
            READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                                   ( CONST(J(IJS+K)) , K=1,NPNT )
         ENDIF
C                 read the parameters
         IF ( IORDER .EQ. 1 ) THEN
            IF ( NDIM .GT.  0 )
     *         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (( PARAM( (J(IKS+K1)-1)*NOPA + J(IJS+K2) ),
     *                          K1=1,NDIM ) , K2 = 1,NPNT )
            IF ( NDIM .EQ. -1 )
     *         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (  PARAM(                      J(IJS+K2) ),
     *                                        K2 = 1,NPNT )
         ELSE
               READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (( PARAM( (J(IJS+K2)-1)*NOPA + J(IKS+K1) ),
     *                          K1=1,NDIM ) , K2 = 1,NPNT )
         ENDIF
      ELSE
C                 read the time functions
         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                                ( A(IA+K) , K=0,NTOT-1+NTAL )
         IJ = IJ + 1
         IA = IA + NTOT + NTAL
      ENDIF
   20 CONTINUE
      IF ( IOPT .EQ. 0 ) THEN
         IF ( NDIM .EQ. -1 ) THEN
            DO 33 I = 1,NPNT
               ISP = J(IJS+I)
               AAA = PARAM(ISP)
               DO 32 K = 1 , NOSEG
                  PARAM(ISP) = AAA
                  ISP = ISP + NOPA
   32          CONTINUE
   33       CONTINUE
         ENDIF
         IJ = IJS - 1
         IA = IAS + 1
      ENDIF
      ITEL = ITEL + 1
C
C       Return until finished
C
      GOTO 10
C
C       Update linear pointers in array
C
   40 IISP = IISP + IJ-1
      IRSP = IRSP + IA-1
      goto 9999    !  RETURN
C
  100 WRITE ( LUNUT , '(A,I3)' ) ' END-OF-FILE mode:',MODE
      IERR = IERR + 1
      goto 9999    !  RETURN
  110 WRITE ( LUNUT , '(A,I3)' ) ' ERROR-ON-FILE mode:',MODE
      IERR = IERR + 1
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
