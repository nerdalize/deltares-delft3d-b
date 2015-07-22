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

!
!     COMMON  /  SYSC   /   Pointers in character array workspace
!
!     IANAM   INTEGER  1    pointer to ARRNAM, array names
!     IMNAM   INTEGER  1    pointer to MNAME , model and run names
!     ISNAM   INTEGER  1    pointer to SNAME , substance names
!     IDNAM   INTEGER  1    pointer to DNAME , monitor station names
!     IBNID   INTEGER  1    pointer to BNDID , boundary ID's
!     IBNAM   INTEGER  1    pointer to BNAME , boundary names
!     IBTYP   INTEGER  1    pointer to BNTYP , boundary types
!     IWSID   INTEGER  1    pointer to WASTID, waste location ID's
!     IWNAM   INTEGER  1    pointer to WNAME , waste location names
!     IWTYP   INTEGER  1    pointer to WTYPE , waste location types
!     ICNAM   INTEGER  1    pointer to CONAM , constant names
!     IPNAM   INTEGER  1    pointer to PANAM , parameter names
!     IFNAM   INTEGER  1    pointer to FUNAM , functions names
!     ISFNA   INTEGER  1    pointer to SFNAM , segment functions names
!     IEDIT   INTEGER  1    pointer to CGRID , workspace for grid output
!     IPRNA   INTEGER  1    pointer to PRNAM , proces module name
!     IONAM   INTEGER  1    pointer to OUNAM , output variables names
!     IDINA   INTEGER  1    pointer to DINAM , dispersion array names
!     IVNAM   INTEGER  1    pointer to VENAM , velocity array names
!     IDANA   INTEGER  1    pointer to DANAM , dump area names
!     IRNAM   INTEGER  1    pointer to RANAM , raaien names
!     ICBUF   INTEGER  1    pointer to CBUFF , character array buffer
!     ILUNT   INTEGER  1    pointer to LUNTX2, names of binary files
!
      INTEGER         ICSIZE
      INTEGER         IANAM  , IMNAM  , ISNAM  , IDNAM  , IBNID  ,
     &                IBNAM  , IBTYP  , IWSID  , IWNAM  , IWTYP  ,
     &                ICNAM  , IPNAM  , IFNAM  , ISFNA  , IEDIT  ,
     &                IPRNA  , IONAM  , IDINA  , IVNAM  , IDANA  ,
     &                IRNAM  , ICBUF  , ILUNT
      COMMON / SYSC / IANAM  , IMNAM  , ISNAM  , IDNAM  , IBNID  ,
     &                IBNAM  , IBTYP  , IWSID  , IWNAM  , IWTYP  ,
     &                ICNAM  , IPNAM  , IFNAM  , ISFNA  , IEDIT  ,
     &                IPRNA  , IONAM  , IDINA  , IVNAM  , IDANA  ,
     &                IRNAM  , ICBUF  , ILUNT
!
      PARAMETER ( ICSIZE  = 23 )
