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

      SUBROUTINE OUTBO2 ( NOUTP , IOUTPS, NOSEG , NODUMP, NX    ,
     +                    NY    , NRVART, NBUFMX, NDMPAR, NOTOT ,
     +                    NCBUFM, NORAAI)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:           by Jan van Beek
C
C     FUNCTION            : Sets the boot variables for OUTPUT system
C
C     LOGICAL UNITNUMBERS : -
C
C     SUBROUTINES CALLED  : -
C
C     PARAMETERS          : 10
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOUTP   INTEGER       1     INPUT   Number of processes in def file
C     IOUTPS  INTEGER   7,NOUTP   INPUT   output structure
C     NOSEG   INTEGER       1     INPUT   Number of segments
C     NODUMP  INTEGER       1     INPUT   Number of monitoring points
C     NX      INTEGER       1     INPUT   Length of dump grid
C     NY      INTEGER       1     INPUT   Width of dump grid
C     NRVART  INTEGER       1     OUTPUT  Total number of output variables
C     NBUFMX  INTEGER       1     OUTPUT  Length of output buffer needed
C     NDMPAR  INTEGER       1     INPUT   number of dump areas
C     NOTOT   INTEGER       1     INPUT   Number of substances
C     NCBUFM  INTEGER       1     IN/OUT  Length of character buffer
C     NORAAI  INTEGER       1     INPUT   number of raaien
C
C     Declaration of arguments
C
      use timers       !   performance timers

      INTEGER     NOUTP , NOSEG , NODUMP, NX    , NY    ,
     +            NRVART, NBUFMX, NDMPAR, NOTOT , NCBUFM,
     +            NORAAI
      INTEGER     IOUTPS(7,NOUTP)
C
C     Local
C
      PARAMETER   ( IMON = 1 , IMO2 = 2 , IDMP = 3 , IDM2 = 4 ,
     +              IHIS = 5 , IHI2 = 6 , IMAP = 7 , IMA2 = 8 ,
     +              IBAL = 9 , IHNF =10 , IHN2 =11 , IMNF =12 ,
     +              IMN2 =13 , IMO3 =14 , IMO4 =15 , IHI3 =16 ,
     +              IHI4 =17 , IHN3 =18 , IHN4 =19 , IBA2 =20 ,
     +              IBA3 =21 )
      PARAMETER ( IGSEG = 1 , IGMON = 2 , IGGRD = 3 , IGSUB = 4 )
      INTEGER     IGRID , NOCEL , NBUFOU, ISRTO
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "outbo2", ithndl )
C
C     Loop over the output files
C
      NRVART = 0
      NBUFMX = 0
      DO 100 IOUT = 1 , NOUTP
         NRVAR  = IOUTPS(4,IOUT)
         NRVART = NRVART + NRVAR
C
C        Grid
C
         IGRID = IOUTPS(6,IOUT)
         IF ( IGRID .EQ. IGSEG ) THEN
            NOCEL = NOSEG
         ELSEIF ( IGRID .EQ. IGMON ) THEN
            NOCEL = NODUMP
         ELSEIF ( IGRID .EQ. IGGRD ) THEN
            NOCEL = NX*NY
         ELSEIF ( IGRID .EQ. IGSUB ) THEN
            NOCEL = NDMPAR
         ENDIF
C
C        Calculate outputbuffer size for this file, for some (NEFIS,SUB)
C        also a character buffer size
C
         NCBUFO = 0
         ISRTO  = IOUTPS(5,IOUT)
         IF ( ISRTO .EQ. IHNF .OR. ISRTO .EQ. IMNF     ) THEN
C
C           NEFIS file, extra array with length NOCEL needed
C           substance names and output names in char buffer.
C
            NBUFOU = NOCEL * ( NRVAR + 1 )
            NCBUFO = NOTOT + NRVAR
         ELSEIF ( ISRTO .EQ. IHN2 .OR. ISRTO .EQ. IMN2 ) THEN
C
C           NEFIS file, extra array with length NOCEL needed
C
            NBUFOU = NOCEL * ( NRVAR + 1 )
         ELSEIF ( ISRTO .EQ. IMO3 ) THEN
C
C           On subarea's substances also in buffer, only the
C           first half of the nrvar are real output vars.
C           substance names and output names in char buffer.
C
            NBUFOU = NOCEL * ( NOTOT + NRVAR/2 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IHI3 ) THEN
C
C           On subarea's substances also in buffer, only the
C           first half of the nrvar are real output vars.
C           substance names and output names in char buffer.
C           also output for raaien
C
            NBUFOU = (NOCEL+NORAAI) * ( NOTOT + NRVAR/2 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IHN3 ) THEN
C
C           NEFIS file, extra array with length NOCEL needed
C           On subarea's substances also in buffer, only the
C           first half of the nrvar are real output vars.
C           substance names and output names in char buffer.
C           also output for raaien
C
            NBUFOU = (NOCEL+NORAAI) * ( NOTOT + NRVAR/2 + 1 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IMO4 .OR. ISRTO .EQ. IHI4 ) THEN
C
C           On subarea's only the first half of the nrvar are
C           real output vars.
C
            NBUFOU = NOCEL * ( NRVAR/2 )
         ELSEIF ( ISRTO .EQ. IHN4 ) THEN
C
C           NEFIS file, extra array with length NOCEL needed
C           On subarea's only the first half of the nrvar are
C           real output vars.
C
            NBUFOU = NOCEL * ( NRVAR/2 + 1 )
         ELSE
C
C           Rest, normal
C
            NBUFOU = NOCEL * NRVAR
         ENDIF
C
C        Buffer is as big as the largest needed
C
         NBUFMX = MAX ( NBUFMX, NBUFOU )
         NCBUFM = MAX ( NCBUFM, NCBUFO )
C
  100 CONTINUE
C
      if (timon) call timstop( ithndl )
      RETURN
      END
