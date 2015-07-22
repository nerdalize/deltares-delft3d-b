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

      COMMON/SIZE/PMAX1(MT),PMAX2(MT),PMAX(MT),RMORT1(MT),RMORT2(MT),
     1            RMORT(MT),RES1(MT),RES2(MT),RESP(MT),CHLTOC(MT),
     2            CTODRY(MT),SDMIX(MT),ZOOPR(MT,0:MG),SURF(MT),TOPLEV,
     3            BIOBAS,TEMLIM,BASMOR,LPMAX(MT),NPRODU,LPRODU,LDIEL,
     4            LPOOLS,LOXOUT,LDYDEA,LDYEXT,LGROCH,LMORCH,LPMORT,
     5            LTLIM,LOBFUN,RMORT3(MT)
      REAL*8      PMAX1,PMAX2,PMAX,RMORT1,RMORT2,
     1            RMORT,RES1,RES2,RESP,CHLTOC,
     2            CTODRY,SDMIX,ZOOPR,SURF,TOPLEV,
     3            BIOBAS,TEMLIM,BASMOR,RMORT3
      INTEGER     LPMAX,NPRODU,LPRODU,LDIEL,LPOOLS,LOXOUT,LDYDEA,LDYEXT,
     1            LGROCH,LMORCH,LPMORT,LTLIM,LOBFUN
C Section to be included in the following programs:
C BLOOM
C CONSBL
C CSPGRO
C CSPSTO
C DIEL
C EXCLUD
C FIXINF
C GRAZIN
C HEADIN
C INPUT2
C INPUT4
C MAXGRA
C MAXGRO
C MAXMOR
C MAXPRD
C NATMOR
C OPTION
C POOLS
C PRINSU
C PRINT6
C PRODUC
C RECORD
C RUN
C SETABC
C SOLVLP
C SPCSD
