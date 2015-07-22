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

      function usedcp()
c-----------------------------------------------------------------------
c         D e l f t      H y d r a u l i c s   -   Section    E & Z
c
c             Module:   FUNCTION USEDCP
c           Function: Determines the consumed cpu system time or the
c                     elapsed time in seconds from 0.0 hour (HARDWARE
c                     dependent routine)
c        Method used:
c               Date: 08-06-1995
c         Programmer: P. Koole, Heleen Leepel
c         CVS header
c            $Author: Beek_j $
c              $Date: 7-09-98 10:30 $
c            $Source: /u/trisula/cvsroot/trisula/alg/usedcp.f,v $
c          $Revision: 3 $
c-----------------------------------------------------------------------
c   Calling routines:              SYSINI
c                                  TRICOM
c                                  TRIEND
c                                  TRISOL
c-----------------------------------------------------------------------
c   Called  routines:              CGETCP (c-routine)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c USEDCP   O  R*4                  Function name for the HARDWARE DEPEN-
c                                  DENT routine. It gets the system time
c                                  in order to derive the CPU'S
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c CPU         R*8                  Cur. time to be used for the compu-
c                                  tation of CPU's
c-----------------------------------------------------------------------
c
c declarations and specifications
c
      usedcp = 0.0
c
      end
