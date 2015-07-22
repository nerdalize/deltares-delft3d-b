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

      SUBROUTINE GETDIM ( FNAME  , ITYPE  , DIM    , IPRDEP , ITMDEP ,
     *                             LOCDEP , NDIM   , IERROR , OPTION )
C
C
C     Deltares        MARINE & COASTAL MANAGEMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : ODS GETDIM routine for DELWAQ HIS-files
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
C     DIM     CHAR*3     1        INPUT   Wanted dimension
C     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
C     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
C     LOCDEP  INTEGER    1        INPUT   Loc code for dimensions
C     NDIM    INTEGER    5        OUTPUT  Wanted dimensions
C     IERROR  INTEGER    1        OUTPUT  Error code
C     OPTION  CHAR*256   1        IN/OUT  For future use
C
C     NOTE1: FNAME(3) is used as local character space
C     NOTE2: NDIM is NOT according to ODS specs, it returns:
C            NDIM(1) = nr of substances in the file
C            NDIM(2) = nr of locations  in the file
C            NDIM(3) = nr of time steps in the file
C
C
      CHARACTER*256 FNAME(3) , OPTION
      CHARACTER*3   DIM
      DIMENSION     NDIM(5)
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
      READ ( 10 , ERR=120 ) ( FNAME(3)(181:200) , K = 1,NOTOT )
      READ ( 10 , ERR=130 ) ( IDUMMY, FNAME(3)(221:240) , K = 1,NODUMP )
C
C         Read the values at all times
C
      NTT   = NODUMP*NOTOT
      NOTIM = 0
cjvb  NTT = 0
   10 READ ( 10 , ERR=140 , END=20 )   IDUMMY, ( ADUMMY , K=1,NTT )
      NOTIM = NOTIM + 1
      GOTO 10
C
C         Supply the desired statistics
C
   20 NDIM(1) = NOTOT
      NDIM(2) = NODUMP
      NDIM(3) = NOTIM
      GOTO 200
C
C         Supply the desired statistics
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
C
  200 CLOSE ( 10 )
      RETURN
C
      END
