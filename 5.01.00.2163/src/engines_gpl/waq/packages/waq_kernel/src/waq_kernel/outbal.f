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

      SUBROUTINE OUTBAL (LUBAL , FILBAL, ITIME , MONAME, NOTOT ,
     +                   NOFLUX, SYNAME, NDMPAR, DANAME, ASMASS,
     +                   FLXINT, NOTOT2, CONC2 , INIT  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1993 by Jan van Beek
C                         ( Modified version of WRIBAL by Jos van Gils )
C
C     FUNCTION            : Writes balance output
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : LUBAL , Balance output file
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 16
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUBAL   INTEGER       1     INPUT   Logical unit balance file
C     FILBAL  CHAR*(*)      1     INPUT   Name balance file
C     ITIME   INTEGER       1     INPUT   Simulation time ( scu )
C     MONAME  CHAR*40       4     INPUT   model identification
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     SYNAME  CHAR*20   NOTOT     INPUT   names of substances
C     NDMPAR  INTEGER       1     INPUT   Number of dump segments
C     DANAME  CHAR*20   NDMPAR    INPUT   names of monitoring stations
C     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
C     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Integrated fluxes
C     NOTOT2  REAL          1     INPUT   Number of extra variables
C     CONC2   REAL NOTOT2*NDMPAR  INPUT   Extra variables
C     INIT    INTEGER       1     IN/OUT  Init flag (1=yes,!1=no)
C
C     Declaration of arguments
C
      use timers

      INTEGER       LUBAL , ITIME , INIT  , NOTOT , NOFLUX,
     +              NDMPAR, NOTOT2
      REAL          ASMASS(NOTOT,NDMPAR,6), FLXINT(NOFLUX,NDMPAR),
     +              CONC2(NOTOT2,NDMPAR)
      CHARACTER*20  SYNAME(*)             , DANAME(*)
      CHARACTER*40  MONAME(4)
      CHARACTER*(*) FILBAL
C
C     Local declarations
C
      INTEGER      J     , I     , K     , ISYS  , IFLX  ,
     +             IHLP
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outbal", ithandl )
C
C     Initialize file
C
      IF ( INIT .EQ. 1 ) THEN
         INIT = 0
C
C        Write header
C
         WRITE (LUBAL) (MONAME(I),I=1,4)
         NOPOUT = 6*NOTOT+NOFLUX+2
         WRITE (LUBAL) NOPOUT,NDMPAR,NOTOT
         WRITE (LUBAL) (SYNAME(I),I=1,NOTOT)
         WRITE (LUBAL) (DANAME(I),I=1,NDMPAR)
      ENDIF
C
C     Perform output
C
      WRITE (LUBAL) ITIME,(
     +          ((ASMASS(ISYS,J,K),K=1,6),ISYS=1,NOTOT) ,
     +          (FLXINT(IFLX,J)          ,IFLX=1,NOFLUX),
     +          (CONC2(IHLP,J)           ,IHLP=1,2)     ,
     +          J=1,NDMPAR  )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
