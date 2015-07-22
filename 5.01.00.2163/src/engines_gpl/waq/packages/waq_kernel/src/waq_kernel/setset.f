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

      SUBROUTINE SETSET ( LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR , NOGRID, VGRSET)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : Jan van Beek
C
C     FUNCTION            : Initialisation of Variables structure
C
C     SUBROUTINES CALLED  :
C
C     FILES               :
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C
C     Declaration of arguments
C
      use timers

      INTEGER             LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR
      INTEGER             VGRSET(NOVAR,NOGRID)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "setset", ithandl )
C
C     Volume
C
      IVAR = 1
      VGRSET(IVAR,1) = 1
C
C     Area
C
      IVAR = IVAR + 1
      VGRSET(IVAR,1) = 1
C
C     Flow
C
      IVAR = IVAR + 1
      VGRSET(IVAR,1) = 1
C
C     Length , two length
C
      IVAR = IVAR + 1
      VGRSET(IVAR,1) = 1
      IVAR = IVAR + 1
      VGRSET(IVAR,1) = 1
C
C     Cons
C
      DO ICONS = 1 , NOCONS
         IVAR = IVAR + 1
      ENDDO
C
C     Param
C
      DO IPA = 1 , NOPA
         IVAR = IVAR + 1
      ENDDO
C
C     Func
C
      DO IFUN = 1 , NOFUN
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Seg Func
C
      DO ISFUN = 1 , NOSFUN
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Conc
C
      DO ISYS = 1 , NOSYS
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
      DO ISYS = NOSYS + 1 , NOTOT
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Mass
C
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Deriv
C
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
      ENDDO
C
C     Disp
C
      DO IDSP = 1 , NODISP
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Velo
C
      DO IVEL = 1 , NOVELO
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Default
C
      DO IDEF = 1 , NODEF
         IVAR = IVAR + 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Local
C
      DO ILOC = 1 , NOLOC
         IVAR = IVAR + 1
      ENDDO
C
C     DSPX
C
      DO IDSX = 1 , NDSPX
         IVAR = IVAR + 1
      ENDDO
C
C     VELX
C
      DO IVLX = 1 , NVELX
         IVAR = IVAR + 1
      ENDDO
C
C     LOCX
C
      DO ILCX = 1 , NLOCX
         IVAR = IVAR + 1
      ENDDO
C
C     FLUX
C
      DO IFLX = 1 , NFLUX
         IVAR = IVAR + 1
      ENDDO
C
C
      DO IGRID = 2 , NOGRID
C
C        Volume
C
         IVAR = 1
         VGRSET(IVAR,IGRID) = 0
C
C        Area
C
         IVAR = IVAR + 1
         VGRSET(IVAR,IGRID) = 0
C
C        Flow
C
         IVAR = IVAR + 1
         VGRSET(IVAR,IGRID) = 0
C
C        Length , two length
C
         IVAR = IVAR + 1
         VGRSET(IVAR,IGRID) = 0
         IVAR = IVAR + 1
         VGRSET(IVAR,IGRID) = 0
C
C        Cons
C
         DO ICONS = 1 , NOCONS
            IVAR = IVAR + 1
         ENDDO
C
C        Param
C
         DO IPA = 1 , NOPA
            IVAR = IVAR + 1
         ENDDO
C
C        Func
C
         DO IFUN = 1 , NOFUN
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Seg Func
C
         DO ISFUN = 1 , NOSFUN
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Conc
C
         DO ISYS = 1 , NOSYS
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
         DO ISYS = NOSYS + 1 , NOTOT
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Mass
C
         DO ISYS = 1 , NOTOT
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Deriv
C
         DO ISYS = 1 , NOTOT
            IVAR = IVAR + 1
         ENDDO
C
C        Disp
C
         DO IDSP = 1 , NODISP
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Velo
C
         DO IVEL = 1 , NOVELO
            IVAR = IVAR + 1
            VGRSET(IVAR,IGRID) = 0
         ENDDO
C
C        Default
C
         DO IDEF = 1 , NODEF
            IVAR = IVAR + 1
         ENDDO
C
C        Local
C
         DO ILOC = 1 , NOLOC
            IVAR = IVAR + 1
         ENDDO
C
C        DSPX
C
         DO IDSX = 1 , NDSPX
            IVAR = IVAR + 1
         ENDDO
C
C        VELX
C
         DO IVLX = 1 , NVELX
            IVAR = IVAR + 1
         ENDDO
C
C        LOCX
C
         DO ILCX = 1 , NLOCX
            IVAR = IVAR + 1
         ENDDO
C
C        FLUX
C
         DO IFLX = 1 , NFLUX
            IVAR = IVAR + 1
         ENDDO
C
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
