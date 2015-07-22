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

      SUBROUTINE PROPOI ( IPMSA  , IPSSA  , NSVAR  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : januari 1994 by Leo Postma
C
C     FUNCTION            : Fills direct pointer and increments in DELWAQ
C                           arrays
C
C     SUBROUTINES CALLED  : none
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 3
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     IPMSA   INTEGER       *     IN/OUT  IN input pointers OUT ABS pointers
C     IPMSA   INTEGER       *     IN/OUT  IN output pointers , OUT increments
C     NSVAR   INTEGER       *     INPUT   Nr. of state variables per proces
C
C     Declaration of arguments
C
      use timers
      INTEGER   IPMSA(*)      , IPSSA(*)      , NSVAR(*)
C
      INCLUDE 'sysa.inc'
C
      INCLUDE 'sysn.inc'
C
C     Local declarations
C
      PARAMETER ( NOPRED = 6 )
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "propoi", ithandl )
C
      K3    = 0
      DO 70 IPROC = 1 , NPROC
         DO 60 I = 1 , NSVAR(IPROC)
            K3 = K3 + 1
            IP = IPMSA(K3)
            IF ( IP .EQ. 0 ) IP = IPSSA(K3)
            IF ( IP .EQ. 1 ) THEN
               IPMSA(K3) = IP      + IVOL -ICONC
               IPSSA(K3) = 1
               GOTO 60
            ENDIF
            IF ( IP .LE. NOPRED      ) THEN
               IF ( IP .EQ. 0 ) IP = 1
               IPMSA(K3) = IP + IDEFA-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF =        NOPRED
            IF ( IP .LE. NOCONS+IOFF ) THEN
               IPMSA(K3) = IP-IOFF + ICONS-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF = IOFF + NOCONS
            IF ( IP .LE. NOPA  +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IPARM-ICONC
               IPSSA(K3) = NOPA
               GOTO 60
            ENDIF
            IOFF = IOFF + NOPA
            IF ( IP .LE. NOFUN +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IFUNC-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF = IOFF + NOFUN
            IF ( IP .LE. NOSFUN+IOFF ) THEN
               IPMSA(K3) = (IP-IOFF-1)*NOSEG + 1 + ISFUN-ICONC
               IPSSA(K3) = 1
               GOTO 60
            ENDIF
            IOFF = IOFF + NOSFUN
            IF ( IP .LE. NOTOT+IOFF ) THEN
               IPMSA(K3) = IP-IOFF
               IPSSA(K3) = NOTOT
               GOTO 60
            ENDIF
            IOFF = IOFF + NOTOT
            IF ( IP .LE. NOLOC+IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IPLOC-ICONC
               IPSSA(K3) = NOLOC
               GOTO 60
            ENDIF
            IOFF = IOFF + NOLOC
            IF ( IP .LE. NODEF+IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IDEFA-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF = IOFF + NODEF
            IF ( IP .LE. NFLUX+IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IFLUX-ICONC
               IPSSA(K3) = NFLUX
               GOTO 60
            ENDIF
            IOFF = IOFF + NFLUX
            IF ( IP .LE. 1    +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IFLOW-ICONC
               IPSSA(K3) = 1
               GOTO 60
            ENDIF
            IOFF = IOFF + 1
            IF ( IP .LE. 1    +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IAREA-ICONC
               IPSSA(K3) = 1
               GOTO 60
            ENDIF
            IOFF = IOFF + 1
            IF ( IP .LE. 2    +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + ILENG-ICONC
               IPSSA(K3) = 2
               GOTO 60
            ENDIF
            IOFF = IOFF + 2
            IF ( IP .LE. NODISP +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IDISP-ICONC
               IPSSA(K3) = NODISP
               GOTO 60
            ENDIF
            IOFF = IOFF + NODISP
            IF ( IP .LE. NOVELO +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IVELO-ICONC
               IPSSA(K3) = NOVELO
               GOTO 60
            ENDIF
            IOFF = IOFF + NOVELO
            IF ( IP .LE. NOFUN +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IFUNC-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF = IOFF + NOFUN
            IF ( IP .LE. NOCONS +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + ICONS-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
            IOFF = IOFF + NOCONS
            IF ( IP .LE. NDSPX +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IDSPX-ICONC
               IPSSA(K3) = NDSPX
               GOTO 60
            ENDIF
            IOFF = IOFF + NDSPX
            IF ( IP .LE. NVELX +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IVELX-ICONC
               IPSSA(K3) = NVELX
               GOTO 60
            ENDIF
            IOFF = IOFF + NVELX
            IF ( IP .LE. NLOCX +IOFF ) THEN
               IPMSA(K3) = IP-IOFF + ILOCX-ICONC
               IPSSA(K3) = NLOCX
               GOTO 60
            ENDIF
            IOFF = IOFF + NLOCX
            IF ( IP .GT. IOFF ) THEN
               IPMSA(K3) = IP-IOFF + IDEFA-ICONC
               IPSSA(K3) = 0
               GOTO 60
            ENDIF
   60    CONTINUE
   70 CONTINUE

C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
