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

      SUBROUTINE GETTME ( FNAME  , ITYPE  , TIMDEF , MAXDEF , IPRDEP ,
     *                    LOCDEP , MAXLST , TIMLST , ITMTYP , NRLST  ,
     *                                               IERROR , OPTION )
C
C
C     Deltares        MARINE & COASTAL MANAGEMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : ODS GETTME routine for DELWAQ HIS-files
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
C     TIMDEF  REAL*8   2,MAXDEF   INPUT   Wanted start and stop time
C     MAXDEF  INTEGER    1        INPUT   Wanted time dimension
C     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
C     LOCDEP  INTEGER    1        INPUT   Par code for dimensions
C     MAXLST  INTEGER    1        INPUT   Dimension output arrays
C     TIMLST  REAL*8   MAXLST     OUTPUT  List with times found
C     ITMTYP  INTEGER  MAXLST     OUTPUT  List with time types
C     NRLST   INTEGER    1        OUTPUT  Nr of times found
C     IERROR  INTEGER    1        OUTPUT  Error code
C     OPTION  CHAR*256   1        IN/OUT  For future use
C
C
      CHARACTER*256    FNAME (3) , OPTION
      INTEGER          ITMTYP(*)
      DOUBLE PRECISION TIMLST(*) , TIMDEF(2,*) , ATIME , OTIME, SECOND,
     *                 JULIAN
      LOGICAL          SETALL
      EXTERNAL         JULIAN
C
      REAL, ALLOCATABLE :: RDATA(:)
C
C         Open the DELWAQ .HIS file
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
         GOTO 150
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
      SECOND = 1/864.00D+02
C
C         Read the values at all times
C
      NTT   = NODUMP*NOTOT
      ALLOCATE(RDATA(NTT))
      NRLST = 0
      SETALL = .FALSE.
      IF ( TIMDEF(1,1) .LT. 0.5 ) SETALL = .TRUE.
C  10 READ ( 10 , ERR=140 , END=200 ) IDUMMY, ( ADUMMY , K=1,NTT )
   10 READ ( 10 , ERR=140 , END=200 ) IDUMMY, ( RDATA(K), K=1,NTT )
      DO 20 I = 1 , MAXDEF
         ATIME = OTIME + IDUMMY*ISFACT*SECOND
         IF ( (ATIME.GT.TIMDEF(1,I) .AND. ATIME.LT.TIMDEF(2,I)) .OR.
     *                                             SETALL    ) THEN
            NRLST = NRLST + 1
            IF ( NRLST .GT. MAXLST ) GOTO 160
            TIMLST(NRLST) = ATIME
            ITMTYP(NRLST) = 2
            GOTO 10
         ENDIF
   20 CONTINUE
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
      GOTO 200
  160 IERROR = 16
C
  200 CLOSE ( 10 )
      IF (ALLOCATED(RDATA)) DEALLOCATE(RDATA)
      RETURN
C
      END
