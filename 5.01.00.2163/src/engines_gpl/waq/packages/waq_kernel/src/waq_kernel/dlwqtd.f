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

      SUBROUTINE DLWQTD ( LUN    , NOSEG  , NSEG2  , NOLAY  , NOGRID ,
     *                    NOQ    , NOQ4   , IGREF  , IGSEG  , NOCONS ,
     *                    NOPA   , NOFUN  , NOSFUN , CONST  , CONAME ,
     *                    PARAM  , PANAME , FUNCS  , FUNAME , SFUNCS ,
     *                    SFNAME , IPOINT , VOLUME , AREA   , FLOW   ,
     *                    ALENG  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:              January-2003 by L.Postma
C
C     LAST UPDATE:          ........
C
C     FUNCTION            : Expands volume, area etc. for bottom cells
C
C     LOGICAL UNITS       : LUN(19), error messages
C
C     SUBROUTINES CALLED  : GETVAL, to retrieve a value from the data
C                           SRSTOP, to stop with error
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSEG   INTEGER    1        INPUT   Number of water segments
C     NSEG2   INTEGER    1        INPUT   Number of bottom segments
C     NOLAY   INTEGER    1        INPUT   Number of water layers
C     NOGRID  INTEGER    1        INPUT   Nunber of grids
C     NOQ     INTEGER    1        INPUT   Nunber of water exchanges
C     NOQ4    INTEGER    1        INPUT   Nunber of bottom exchanges
C     IGREF   INTEGER  NOGRID     INPUT   Ref, neg = nr of bottom layers
C     IGSEG   INTEGER NOSEG,NOGRID INPUT  pointer from water to bottom
C     NOCONS  INTEGER    1        INPUT   Number of constants used
C     NOPA    INTEGER    1        INPUT   Number of parameters
C     NOFUN   INTEGER    1        INPUT   Number of functions ( user )
C     NOSFUN  INTEGER    1        INPUT   Number of segment functions
C     CONST   REAL     NOCONS     INPUT   value of constants
C     CONAME  CHAR*20  NOCONS     INPUT   Constant names
C     PARAM   REAL    NOPA,NOSEG  INPUT   value of parameters
C     PANAME  CHAR*20  NOPA       INPUT   Parameter names
C     FUNCS   REAL     NOFUN      INPUT   Function values
C     FUNAME  CHAR*20  NOFUN      INPUT   Function names
C     SFUNCS  REAL   NOSEG,NOSFUN INPUT   Segment function values
C     SFNAME  CHAR*20  NOSFUN     INPUT   Segment function names
C     IPOINT  INTEGER   4,NOQT    INPUT   All exchange pointers
C     VOLUME  REAL   NOSEG+NSEG2  IN/OUT  Segment volumes
C     AREA    REAL    NOQ+NOQ4    IN/OUT  Exchange surfaces
C     FLOW    REAL    NOQ+NOQ4    IN/OUT  Exchange flows
C     ALENG   REAL   2,NOQ+NOQ4   IN/OUT  Diffusion lengthes
C
C
      use timers

      INTEGER              LUN(*), IGREF(NOGRID), IGSEG(NOSEG,NOGRID),
     *                     IPOINT(  4   ,NOQ+NOQ4)
      REAL                 CONST (NOCONS), PARAM (NOPA ,NOSEG ),
     *                     FUNCS (NOFUN ), SFUNCS(NOSEG,NOSFUN),
     *                     VOLUME(NOSEG+NSEG2), AREA(NOQ+NOQ4) ,
     *                     ALENG (2,NOQ+NOQ4 ), FLOW(NOQ+NOQ4)
      CHARACTER*20         CONAME(NOCONS), PANAME(NOPA  ),
     *                     FUNAME(NOFUN ), SFNAME(NOSFUN)
C
      LOGICAL              LGET
      REAL, Allocatable :: Horsurf(:), Thickn(:)
      CHARACTER*20         CTAG
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqtd", ithandl )
C
      NOSSS = NOSEG + NSEG2
C
C     Find the bottomgrid
C
cgrd  do igrd = 1, nogrid
c        nblay = -igref(igrd)
c        ibgrd =  igrd
c        if ( nblay .gt. 0 ) goto 10
c     enddo
c     write ( lun(19) , * ) ' ERROR: Bottomgrid not found !'
cgrd  call srstop(1)
c
c     Set up the horizontal surfaces
c
   10 CTAG = 'SURF'
      LGET = .true.
      Allocate ( Horsurf(NOSSS) )
      CALL VALUES ( CTAG   , NOSSS  , Horsurf, NOCONS , NOPA   ,
     *              NOFUN  , NOSFUN , CONST  , CONAME , PARAM  ,
     *              PANAME , FUNCS  , FUNAME , SFUNCS , SFNAME ,
     *              LGET   , IERR   )
      IF ( IERR .NE. 0 ) THEN
         write ( lun(19) , * ) ' ERROR: Variabele SURF not found !'
         call srstop(1)
      endif
cgrd  nbseg = nseg2/nblay
c     if ( nbseg*nblay .ne. nseg2 ) then
c        write ( lun(19) , * ) ' ERROR: Inconsistent bottom !'
c        call srstop(1)
c     endif
c     do iseg = 1, noseg/nolay
c        hs = Horsurf(iseg)
c        ip = igseg(iseg,ibgrd)
c        do ilay = 1, nblay
c           Horsurf(noseg+ip) = Horsurf(noseg+ip) + hs
c           ip = ip + nbseg
c        enddo
c     enddo
c     LGET = .false.
c     CALL VALUES ( CTAG   , NOSSS  , Horsurf, NOCONS , NOPA   ,
c    *              NOFUN  , NOSFUN , CONST  , CONAME , PARAM  ,
c    *              PANAME , FUNCS  , FUNAME , SFUNCS , SFNAME ,
cgrd *              LGET   , IERR   )
C
C        Expand the volumes
C
      CTAG = 'FIXTH'
      LGET = .true.
      Allocate ( Thickn(NOSSS) )
      CALL VALUES ( CTAG   , NOSSS  , Thickn , NOCONS , NOPA   ,
     *              NOFUN  , NOSFUN , CONST  , CONAME , PARAM  ,
     *              PANAME , FUNCS  , FUNAME , SFUNCS , SFNAME ,
     *              LGET   , IERR   )
      IF ( IERR .NE. 0 ) THEN
         write ( lun(19) , * ) ' ERROR: Variabele FIXTH not found !'
         call srstop(1)
      endif
      do iseg = noseg+1, noseg+nseg2
         volume(iseg) = Horsurf(iseg)*Thickn(iseg)
      enddo
C
C        Expand the areas, lengthes and flows
C
      do iq = 1 , NOQ4
         area (  NOQ+iq) = Horsurf(IPOINT(1,NOQ+iq))
         aleng(1,NOQ+iq) = 1.0
         aleng(2,NOQ+iq) = 1.0
         flow (  NOQ+iq) = 0.0
      enddo
C
      deallocate ( Horsurf, Thickn )
C
      if ( timon ) call timstop ( ithandl )
      return
      end
