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

      COMMON/IOBLCK/POSIT,INUNI,OUUNI,IOFLAG,IPL1,IPL2,IPL3,OPL,LPLOT,
     1              LSCR,LENSTR,IOU
      COMMON/IOCHAR/LINE,STRING
      CHARACTER*1 STRING(48)
      CHARACTER*8 LINE(10)
      INTEGER IOU(99),POSIT,INUNI,OUUNI,OPL,IOFLAG,IPL1,IPL2,IPL3,
     1        LPLOT,LSCR,LENSTR
C Section to be include in the following programs:
C BLOOM
C BLOOMPC
C CHANGE
C CHHELP
C CSPGRO
C CSPSTO
C CZOOPL
C DAYEU
C DIEL
C DOMINA
C EDIT
C FIXINF
C GRAPH
C GRAZIN
C HEADIN
C INPTDT
C INPTNM
C INPUT2
C MAXGRA
C MAXGRO
C MAXPRD
C NATMOR
C NEWS
C NUTFEA
C OPHELP
C OPTION
C POOLS
C PRINMA
C PRINT6
C PRODUC
C PROMES
C PROMPT
C RUN
C SELECT
C SETABC
C SETUNI
C SPINDI
