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

      SUBROUTINE DLWQCE ( AMASS  , VOLUMN , VOLUML , NOSYS  , NOTOT  ,
     +                    NOSEG  , LUN    )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:
C
C     FUNCTION            : Make closure error correction
C
C     LOGICAL UNITNUMBERS : LUN     = number of monitoring file
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     AMASS   REAL   NOTOT*NOSEG  IN/OUT  mass array to be updated
C     VOLUMN  REAL      NOSEG     INPUT   New volume
C     VOLUML  REAL      NOSEG     INPUT   Last volume
C     NOSYS   INTEGER     1       INPUT   number of active substances
C     NOTOT   INTEGER     1       INPUT   number of total substances
C     NOSEG   INTEGER     1       INPUT   number of computational elts.
C     LUN     INTEGER     1       INPUT   unitnumber of monitoring file
C
C     Declaration of arguments
C
      use timers

      INTEGER       NOSYS , NOTOT , NOSEG , LUN
      REAL          AMASS(*) , VOLUMN(*), VOLUML(*)
C
C     Local declaration
C
      INTEGER       NOBOT , ITEL  , ISEG
      REAL          CLOFAC
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqce", ithandl )
C
C     Say what you are doing
C
      WRITE (LUN,1000)
C
C     loop accross the number of computational elements
C
      TOVOLL = 0.0
      TOVOLN = 0.0
      NOBOT  = NOTOT - NOSYS
      ITEL   = 1
      DO 20 ISEG = 1, NOSEG
C
C        Calculate error
C
         if ( voluml(iseg) .gt. 1.0e-28 ) then
            CLOFAC = VOLUMN(ISEG)/VOLUML(ISEG)
         else
            clofac = 1.0
         endif
         TOVOLL = TOVOLL + VOLUML(ISEG)
         TOVOLN = TOVOLN + VOLUMN(ISEG)
C
C        Correct active substances mass
C
         DO 10 I=1,NOSYS
            AMASS(ITEL) = AMASS(ITEL) * CLOFAC
            ITEL = ITEL + 1
   10    CONTINUE
         ITEL = ITEL + NOBOT
C
   20 CONTINUE
C
C
C
      CLOERR = TOVOLN/TOVOLL
      WRITE (LUN,1010) TOVOLL
      WRITE (LUN,1020) TOVOLN
      WRITE (LUN,1030) CLOERR
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
C     Output formats
C
 1000 FORMAT ( 'Performing closure error correction')
 1010 FORMAT ( 'Total volume before rewind:',E12.4)
 1020 FORMAT ( 'Total volume after rewind :',E12.4)
 1030 FORMAT ( 'Total correction factor   :',E12.4)
C
      END
