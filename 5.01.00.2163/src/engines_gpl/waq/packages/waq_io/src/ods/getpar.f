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

      SUBROUTINE GETPAR ( FNAME  , ITYPE  , PARDEF , MAXDEF , ITMDEP ,
     *                    LOCDEP , MAXLST , LANG   , PARLST , PARUNI ,
     *                    IPRTYP , IPRCOD , NRLST  , IERROR , OPTION )
C
C
C     Deltares        MARINE & COASTAL MANAGEMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : ODS GETPAR routine for DELWAQ HIS-files
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
C     PARDEF  CHAR*20  MAXDEF     INPUT   List with wanted par's
C     MAXDEF  INTEGER    1        INPUT   Length of PARDEF
C     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
C     LOCDEP  INTEGER    1        INPUT   Loc code for dimensions
C     MAXLST  INTEGER    1        INPUT   Dimension of the output arrays
C     LANG    INTEGER    1        INPUT   Language code
C     PARLST  CHAR*20  MAXLST     OUTPUT  List of parameters found
C     PARUNI  CHAR*20  MAXLST     OUTPUT  List of parameter units found
C     IPRTYP  INTEGER  MAXLST     OUTPUT  List of parameter types
C     IPRCOD  INTEGER  MAXLST     OUTPUT  List of parameter codes
C     NRLST   INTEGER    1        OUTPUT  Nr of parameters found
C     IERROR  INTEGER    1        OUTPUT  Error code
C     OPTION  CHAR*256   1        IN/OUT  For future use
C
C
      CHARACTER*256 FNAME(3) , OPTION
      CHARACTER*20  PARDEF(MAXDEF) , PARLST(MAXLST) , PARUNI(MAXLST)
      DIMENSION     IPRTYP(MAXLST) , IPRCOD(MAXLST)
      LOGICAL       SETALL
C
C         Open the DELWAQ .HIS file
C
      CALL DHOPNF ( 10 , FNAME(1) , 24 , 2 , IERROR )
      IF ( IERROR .NE. 0 ) RETURN
C
C         Read primary system characteristics
C
      READ ( 10 , ERR=100 )   FNAME(3)(1:160)
      READ ( 10 , ERR=110 )   NOTOT, NODUMP
C
C         Read parameter names and try to find the wanted subset
C
      NRLST  = 0
      SETALL = .FALSE.
      IF ( PARDEF(1) .EQ. '*' ) SETALL = .TRUE.
      DO 40 I1 = 1 , NOTOT , MAXLST
         MAXK = MIN(NOTOT,I1+MAXLST-1) - I1 + 1
         READ ( 10 , ERR=120 ) ( PARUNI(K) , K = 1,MAXK )
         DO 30 I2 = 1 , MAXK
            DO 20 I3 = 1 , MAXDEF
               IF ( PARUNI(I2) .EQ. PARDEF(I3) .OR. SETALL ) THEN
                  NRLST = NRLST + 1
                  IF ( NRLST .EQ. MAXLST ) THEN
                     IERROR = -NOTOT
                     GOTO 50
                  ENDIF
                  PARLST(NRLST) = PARUNI(I2)
                  IPRCOD(NRLST) = I1 + I2 - 1
                  GOTO 30
               ENDIF
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
C
C         Supply the desired statistics
C
   50 DO 60 I1 = 1 , NRLST
         PARUNI(I1) = PARLST(I1)(10:20)
         IPRTYP(I1) = 2
   60 CONTINUE
      GOTO 200
C
C         Supply the desired statistics
C
  100 IERROR = 10
      GOTO 200
  110 IERROR = 11
      GOTO 200
  120 IERROR = 12
C
C         Close the unit
C
  200 CLOSE ( 10 )
      RETURN
      END
