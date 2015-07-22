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

      SUBROUTINE BEGTIM ()
C
      COMMON / CDHTIM / IH1 , IM1 , IS1 , I1
      SAVE   / CDHTIM /
C
      CALL DHTIME ( IH1 , IM1 , IS1 , I1 )
C
      RETURN
      END
      SUBROUTINE ENDTIM ( TIME )
C
      REAL     TIME
C
      COMMON / CDHTIM / IH1 , IM1 , IS1 , I1
      SAVE   / CDHTIM /
C
      CALL DHTIME ( IH2 , IM2 , IS2 , I2 )
C
      IF ( IH2 .NE. IH1 ) THEN
         TIME = 3600.+ 60.*(IM2-IM1) + IS2-IS1 + (I2-I1)/100.
      ELSE
         TIME = 60.*(IM2-IM1) + IS2-IS1 + (I2-I1)/100.
      ENDIF
C
      RETURN
      END
