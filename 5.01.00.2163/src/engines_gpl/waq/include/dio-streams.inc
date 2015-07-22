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
C     dio-streams.inc
C
C     F77 Interface to
C     DelftIO Streams
C
C     Stef.Hummel@deltares.nl
C
C     (c) Deltares, Feb 01
C---------------------------------------------
C
      include 'dio-time.inc'
C
C     Sizes for character strings
C
      integer DioMaxStreamLen
      parameter ( DioMaxStreamLen = 256 )
C
C     Stream Types
C
      integer dio_ASCII_stream
      integer dio_Binary_stream
      integer dio_His_stream
      integer dio_Nefis_stream

      parameter ( dio_ASCII_stream  = 1 ,
     &            dio_Binary_stream = 2 ,
     &            dio_His_stream    = 3 ,
     &            dio_Nefis_stream  = 4 )

C
C     ORIGINAL interface (used by SRW)
C
      integer DioCreateStream, DioCreateStreamSynched
      external DioCreateStream, DioCreateStreamSynched

C
C     ODS Server interface
C
      integer DiofMaxErrMsgLen
      parameter ( DiofMaxErrMsgLen = 256 )

      integer DiofGetLastError
      external DiofGetLastError

