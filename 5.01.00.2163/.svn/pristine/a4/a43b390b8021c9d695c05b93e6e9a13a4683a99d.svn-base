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

C
C  *********************************************************************
C  *    INCLUDE FILE CONTAINING THE MAIN DIMENSIONS OF BLOOM II        *
C  *********************************************************************
C
C  Note: this file should be included in nearly all subroutines of the
C  program as the dimensions of nearly all arrays depend on these
C  maximum values.
C
C
C
C  Symbol explanation:
C   MT = maximum number of phytoplankton types
C   MS = maximum number of phytoplankton species (groups)
C   MN = maximum number of nutrients
C   IA = maxmimum number of rows in A-matrix. This is equal to the sum of
C        MN + 2 (energy constraints) + 1 (exclusion row) + 2 * MS
C        mortality and growth constraints of species)
C   MX = maximum dimension of X vector. This is equal to maximum number
C        of constraints IA + MT + 1 (optimum)
C   MG = maximum number of zooplankton types (0895 MvdV)
C
      INTEGER MT,MS,MN,IA,MX,MG

      PARAMETER (MT = 30)
      PARAMETER (MS = 15)
      PARAMETER (MN =  6)
      PARAMETER (MG =  5)
      PARAMETER (IA =  MN + 2 + 1 + 2 * MS)
      PARAMETER (MX =  IA + MT + 1)
C BLOOM
C BLOOMPC
C CHANGE
C CHHELP
C CONSTR
C CONSBL
C CSPGRO
C CSPSTO
C CZOOPL
C DIEL
C DOMINA
C DYNADE
C EBCALC
C EXCLUD
C FIXINF
C GRAZIN
C HEADIN
C INPUT1
C INPUT2
C INPUT3
C INPUT4
C MAXGRA
C MAXGRO
C MAXMOR
C MAXPRD
C NATMOR
C NUTFEA
C OPTION
C POOLS
C PRINMA
C PRINSU
C PRINT6
C PRODUC
C PRPLOT
C RECORD
C RUN
C SETABC
C SETUNI
C SOLVLP
C SPCSD
C SPINDI
