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

      SUBROUTINE DLWQ66 ( AMASS  , VOLUME , CONC   , NOTOT  , NOSEG  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : june 1988 by L.Postma
C
C     FUNCTION            : makes masses from conc and volumes
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     AMASS   REAL   NOTOT*NOSEG  OUTPUT  closure error correction
C     VOLUME  REAL      NOSEG     INPUT   volume
C     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
C     NOTOT   INTEGER     1       INPUT   number of systems
C     NOSEG   INTEGER     1       INPUT   number of segments
C
      use timers

      DIMENSION  AMASS(NOTOT,*) , VOLUME(*) , CONC(NOTOT,*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq66", ithandl )
C
C         loop over the number of segments and systems
C
      DO 10 ISEG = 1 , NOSEG
      V1 = VOLUME(ISEG)
      DO 10 ISYS = 1 , NOTOT
      AMASS(ISYS,ISEG) = CONC(ISYS,ISEG)*V1
   10 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
