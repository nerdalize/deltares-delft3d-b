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

      SUBROUTINE PROINT ( NOFLUX, NDMPAR, IDT   , ITURAT, FLXDMP,
     +                    FLXINT, ISDMP , IPDMP , NTDMPQ)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1993 by Jan van Beek
C
C     FUNCTION            : Integrates the fluxes for dump area's
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          :  8
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     NDMPAR  INTEGER       1     INPUT   Number of dump areas
C     IDT     INTEGER       1     INPUT   Time step system clock units
C     ITURAT  INTEGER       1     INPUT   System clock/proces clock ratio
C     FLXDMP  REAL  NOFLUX*?      INPUT   fluxes at dump segments
C     FLXINT  REAL  NOFLUX*NDMPAR IN/OUT  Integrated fluxes at dump segments
C     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
C
C     Declaration of arguments
C
      use timers

      INTEGER       NOFLUX, NDMPAR, IDT   , ITURAT, NTDMPQ
      INTEGER       ISDMP(*)        , IPDMP(*)
      REAL          FLXDMP(NOFLUX,*), FLXINT(NOFLUX,*)
C
C     Local declaration
C
      INTEGER       ITEL2 , IDUMP , NSC   , ISC   , ISEG  ,
     +              IPS   , IFLX
      REAL          FSCALE
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "proint", ithandl )
C
C     Loop over the dump area's
C
      IP1    = NDMPAR + NTDMPQ
      ITEL2  = NDMPAR + NTDMPQ + NDMPAR
      FSCALE = REAL(IDT)/REAL(ITURAT)
      DO 30 IDUMP = 1 , NDMPAR
C
C        the segment contributes
C
         NSC  = IPDMP(IP1+IDUMP)
         DO 20 ISC = 1 , NSC
            ITEL2 = ITEL2 + 1
            ISEG  = IPDMP(ITEL2)
            IF ( ISEG .GT. 0 ) THEN
               IPS   = ISDMP(ISEG)
C
C              Integrate the fluxes
C
               DO 10 IFLX = 1 , NOFLUX
                  FLXINT(IFLX,IDUMP) = FLXINT(IFLX,IDUMP) +
     +                                 FLXDMP(IFLX,IPS)*FSCALE
   10          CONTINUE
            ENDIF
C
   20    CONTINUE
C
   30 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
