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

C    Date:       29 Apr 1993
C    Time:       09:35
C    Program:    GREGOR.FOR
C    Version:    1.01
C    Programmer: Andre Hendriks
C    Previous version(s):
C    1.00 -- 29 Apr 1993 -- 09:23 -- Operating System: DOS
C    0.0 -- 29 Apr 1993 --  9:22 -- Operating System: DOS
C    Project:    T1234.56
C    Module:     GREGOR
C    Function:
C    Comment:
C    Reference:
C    Review:
      SUBROUTINE GREGOR ( JULIAN, IYEAR , IMONTH, IDAY  , IHOUR ,
     1                    IMIN  , ISEC  )
C
C     +----------------------------------------------------------------+
C     |    W A T E R L O O P K U N D I G   L A B O R A T O R I U M     |
C     |               Sector Waterbeheer & Milieu                      |
C     +----------------------------------------------------------------+
C
C***********************************************************************
C
C     Project : T0467
C     Author  : Andre Hendriks
C     Date    : 891215             Version : 1.00
C
C     Changes in this module :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     891215  Andre Hendriks  Version 1.00
C
C***********************************************************************
C
C     Description of module :
C
C        This functions returns the Gregorian date and the time of a so
C        called Julian day, or iyear -9999 if an error occurred.
C
C        The Julian day of a date is the number of days that has passed
C        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
C        to compute differces between dates. ( See DOUBLE PRECISION
C        FUNCTION JULIAN for the reverse proces ).
C
C***********************************************************************
C
C     Arguments :
C
C     Name   Type     In/Out Size            Description
C     ------ -----    ------ -------         ---------------------------
C     JULIAN real*8   in     -               Julian day
C     IYEAR  integer  out    -               Year   ( -4713-.. )
C     IMONTH integer  out    -               Month  ( 1-12 )
C     IDAY   integer  out    -               Day    ( 1-28,29,30 or 31 )
C     IHOUR  integer  out    -               Hour   ( 0-23 )
C     IMIN   integer  out    -               Minute ( 0-59 )
C     ISEC   integer  out    -               Second ( 0-59 )
C
C     Local variables :
C
C     Name   Type     Size   Description
C     ------ -----    ------ ------------------------
C     TEMP1  real*8   -      Temporary variable
C     TEMP2  real*8   -      Temporary variable
C     TEMP3  real*8   -      Temporary variable
C     TEMP4  real*8   -      Temporary variable, JULIAN
C     TEMP5  real*8   -      Temporary variable, fractional part JULIAN
C
C     Calls to : none
C
C***********************************************************************
C
C     Variables :
C
      INTEGER           IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
      DOUBLE PRECISION  JULIAN, TEMP1 , TEMP2 , TEMP3 , TEMP4 , TEMP5
C
C***********************************************************************
C
C
C
      IF ( JULIAN .LT. 0.0 ) THEN
         IYEAR = -9999
         GOTO 999
      ELSE
         TEMP4 = JULIAN
         TEMP5 = MOD ( JULIAN, 1.0D0 )
         IF ( TEMP5 .LT. 0.5 ) THEN
            TEMP3  = 0.5 + TEMP5
            TEMP4  = DINT ( TEMP4 )
         ELSE
            TEMP3  = TEMP5 - 0.5
            TEMP4  = DINT ( TEMP4 ) + 1.0
         ENDIF
         TEMP1  = TEMP4 + 68569.0
C TEMP4 used here to avoid compiler error in POWERSTATION
         TEMP4  =   4.0 * TEMP1 / 146097.0
         TEMP2  = DINT  ( TEMP4 )
         TEMP1  = TEMP1 - DINT ( ( 146097.0 * TEMP2 + 3.0 ) / 4.0 )
         IYEAR  = INT   ( 4000.0 * ( TEMP1 + 1.0 ) / 1461001.0 )
C TEMP4 used here to avoid compiler error in POWERSTATION
         TEMP4  = 1461.0 * IYEAR / 4.0
         TEMP1  = TEMP1 - DINT ( TEMP4  ) + 31.0
         IMONTH = INT   ( 80.0 * TEMP1 / 2447.0 )
         TEMP4  = 2447.0 * IMONTH / 80.0
         IDAY   = INT   ( TEMP1 - AINT (TEMP4) )
         TEMP4  = IMONTH / 11.0
         TEMP1  = DINT  ( TEMP4 )
         IMONTH = INT   ( IMONTH + 2.0 - 12.0 * TEMP1 )
         IYEAR  = INT   ( 100.0 * ( TEMP2 - 49.0 ) + IYEAR + TEMP1 )
         IHOUR  = INT   ( TEMP3 * 24.0 )
         IMIN   = INT   ( TEMP3 * 1440.0 - 60.0 * IHOUR )
         ISEC   = NINT  ( TEMP3 * 86400.0 - 3600.0 * IHOUR - 60.0*IMIN )
      ENDIF
  999 RETURN
      END
