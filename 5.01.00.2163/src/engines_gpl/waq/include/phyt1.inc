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

      COMMON/PHYT1/AA(MN,MT),EKX(MT),DMIX(MT),CHLR(MT),
     1             RNUT(2,MN),REMINU(MN),REMEXP(MN),DETRIT(MN),
     2             CONCEN(MN),REMILI(2),AVAILN,SEDRAT,REMIOR,QMREM
      REAL*8       AA,EKX,DMIX,CHLR,
     1             RNUT,REMINU,REMEXP,DETRIT,
     2             CONCEN,REMILI,AVAILN,SEDRAT,REMIOR,QMREM
      COMMON/PHYT1A/GRNAME(MS),SPNAME(MT),CSTRA(MN+2)
      CHARACTER*8 GRNAME,SPNAME,CSTRA
C Section to be included in the following programs:
C BLOOM
C CHANGE
C CHHELP
C CSPGRO
C CSPSTO
C CZOOPL
C DOMINA
C DYNADE
C FIXINF
C HEADIN
C INPUT2
C MAXGRA
C MAXGRO
C MAXMOR
C NUTFEA
C OPTION
C POOLS
C PRINMA
C PRINSU
C PRODUC
C RUN
C SETABC
C SPINDI
