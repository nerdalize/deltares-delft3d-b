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

      SUBROUTINE GETMAT ( FNAME  , ITYPE  , IPRCOD , LOC    , TIM    ,
     *                    AMISS  , I3GL   , MAXDIM , DATA   , IERROR ,
     *                                                        OPTION )
C
C
C     Deltares        MARINE & COASTAL MANAGEMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : ODS GETMAT routine for DELWAQ HIS-files
C
C     SUBROUTINES CALLED :
C
C     LOGICAL UNITS      :
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     FNAME   CHAR*256   3        IN/LOC  Complete file name
C     ITYPE   INTEGER    1        INPUT   File type
C     IPRCOD  INTEGER  IERROR     INPUT   List of wanted parameters
C     LOC     INTEGER   3*3       INPUT   List of indices of locations
C     TIM     REAL*8     3        INPUT   Interval and step for data
C     AMISS   REAL*4     2        INPUT   Missing value in output/input
C     I3GL    INTEGER    1        INPUT   Nonsens
C     MAXDIM  INTEGER    1        INPUT   Maximum dimension of output arr
C     DATA    REAL*4   MAXDIM     OUTPUT  The produced information
C     IERROR  INTEGER    1        IN/OUT  Error code
C     OPTION  CHAR*256   1        IN/OUT  For future use
C
C
      CHARACTER*256 FNAME (3) , OPTION
      DIMENSION     LOC(*)    , DATA(*)
      REAL*8        TIM(3)    , OTIME  , ATIME    , SECOND  , JULIAN
      EXTERNAL      JULIAN
      real  amiss
C
C         Open the DELWAQ .HIS file if needed
C
      CALL DHOPNF ( 10 , FNAME(1) , 24 , 2 , IERROR )
      IF ( IERROR .NE. 0 ) RETURN
C
C         Read primary system characteristics
C
      READ ( 10 , ERR=100 )   FNAME(3)(1:160)
      IF ( FNAME(3)(121:123) .NE. 'T0: ' .AND.
     *     FNAME(3)(121:123) .NE. 't0: ' .AND.
     *     FNAME(3)(121:123) .NE. 'T0= ' .AND.
     *     FNAME(3)(121:123) .NE. 't0= '       ) THEN
         GOTO 140
      ENDIF
      READ ( FNAME(3)(125:128) , '(I4)' ) IYEAR
      READ ( FNAME(3)(130:131) , '(I2)' ) IMONTH
      READ ( FNAME(3)(133:134) , '(I2)' ) IDAY
      READ ( FNAME(3)(136:137) , '(I2)' ) IHOUR
      READ ( FNAME(3)(139:140) , '(I2)' ) IMINUT
      READ ( FNAME(3)(142:143) , '(I2)' ) ISECND
      READ ( FNAME(3)(151:158) , '(I8)' ) ISFACT
      READ ( 10 , ERR=110 )   NOTOT, NODUMP
      READ ( 10 , ERR=120 ) ( FNAME(3)(181:200) , K = 1,NOTOT )
      READ ( 10 , ERR=130 ) ( IDUMMY, FNAME(3)(221:240) , K = 1,NODUMP )
      IDATE  = IYEAR*10000+IMONTH*100+IDAY
      ITIME  = IHOUR*10000+IMINUT*100+ISECND
      OTIME  = JULIAN ( IDATE , ITIME )
      SECOND = ISFACT/864.00D+02
C
C         Standard ODS processing
C
      NTT   = NODUMP*NOTOT
      ISET  = 0
   10 I1 = (LOC(1)-1)*NOTOT + IPRCOD - 1
      I2 = (LOC(2)-LOC(1))/LOC(3)
      I3 =  LOC(3)*NOTOT - 1
      I4 =  NTT - I1 - ( 1 + I3 ) * I2 - 1
C     READ ( 10 , ERR=150 , END=200 ) IDUMMY , ( DATA(K) , K=1,NTT)
C     WRITE ( 20 , * ) I1, I2, I3, I4
C     WRITE ( 20 , * ) IDUMMY
C     WRITE ( 20 , '(25E12.6)' ) ( DATA(K),K=1,NTT )
      IF ( ISET+I2+1 .GT. MAXDIM ) GOTO 150
      READ ( 10 , ERR=150 , END=200 ) IDUMMY , ( ADUMMY , K=1,I1 ) ,
     *          ( DATA(ISET+K)    , ( ADUMMY , L=1,I3 ) , K=1,I2 ) ,
     *            DATA(ISET+I2+1) , ( ADUMMY , L=1,I4 )
      ATIME = OTIME + IDUMMY*SECOND
      IF ( ATIME .GT. TIM(1) .AND. ATIME .LT. TIM(2) ) THEN
         ISET = ISET + I2 + 1
      ENDIF
      GOTO 10
C
C         Error messages
C
  100 IERROR = 10
      GOTO 200
  110 IERROR = 11
      GOTO 200
  120 IERROR = 12
      GOTO 200
  130 IERROR = 13
      GOTO 200
  140 IERROR = 14
      GOTO 200
  150 IERROR = 15
C
  200 CLOSE ( 10 )
      RETURN
C
      END
