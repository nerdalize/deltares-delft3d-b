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

      subroutine filldm(elmdms    ,ielem     ,dm1       ,dm2       ,
     *                  dm3       ,dm4       ,dm5       ,dm6       )
      implicit none
c-----------------------------------------------------------------------
c         D e l f t      H y d r a u l i c s   -   Section    E & Z
c
c             Module: SUBROUTINE FILLDM
c           Function: Write element dimensions in array elmdms
c        Method used:
c               Date: 08-06-1995
c         Programmer: A. Hoekstra, H.H. Leepel
c         CVS header
c            $Author: Beek_j $
c              $Date: 17-03-03 15:38 $
c            $Source: /u/trisula/cvsroot/trisula/output/filldm.f,v $
c          $Revision: 1 $
c-----------------------------------------------------------------------
c   Calling routines:            numerous
c-----------------------------------------------------------------------
c   Called  routines:              NONE
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c DM1     I   I*4                  Number of dimensions
c DM2     I   I*4                  Size of first dimension
c DM3     I   I*4                  Size of second dimension
c DM4     I   I*4                  Size of third dimension
c DM5     I   I*4                  Size of fourth dimension
c DM6     I   I*4                  Size of fifth dimension
c ELMDMS   O  I*4   6,*            Array containing info about the
c                                  element dimensions ELMDMS(1,*) is
c                                  the number of dimensions
c                                  ELMDMS(2-ELMDMS(1,*),*) is the size
c                                  of each dimension. The size of the
c                                  array is (6,NELEMS).
c IELEM   I   I*4                  Index number of element in group
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c-----------------------------------------------------------------------
c
c  declaration and specification
c
      integer elmdms( 6, *), ielem,
     *        dm1   ,dm2   ,dm3   ,dm4   ,dm5   ,dm6
c-----------------------------------------------------------------------
c-----define element dimensions
c-----------------------------------------------------------------------
      elmdms(1,ielem) = dm1
      elmdms(2,ielem) = dm2
      elmdms(3,ielem) = dm3
      elmdms(4,ielem) = dm4
      elmdms(5,ielem) = dm5
      elmdms(6,ielem) = dm6
c
      end
