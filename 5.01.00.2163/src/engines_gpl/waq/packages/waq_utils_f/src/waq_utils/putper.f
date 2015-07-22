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

      SUBROUTINE PUTPEV (StreamName,DataSetName,Intval)
C
C     Deltares
C
C     CREATED             : jan  2001 by J.v.Gils
C
C     FUNCTION            : Gives permission to DIO to proceed
C                           one step (synchronised mode)
C
C
c     use dio_streams
c     use dio_plt_rw
      include 'dio-plt.inc'

      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times, Intval

      character*(dioMaxParLen) vars
      character*(dioMaxLocLen) locs
      character*(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioOutStream
      integer dioOutSet

      logical first
      save first, dioOutSet
      data first /.true./

      if ( first ) then

      first = .false.

C     Open data stream
      dioOutStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'w')

C     Create data set
      Nr_Locations = 1
      Nr_Variables = 1
      locs(1) = 'dioLocNam'
      vars(1) = 'dioVarNam'
      tims(1) = 'dioDate'
      dioOutSet = DioDefinePltDataSet (
     j              dioOutStream,
     j              DataSetName,
     +              Dio_Plt_Real,
     +              Nr_Variables,vars,
     +              Nr_Locations,locs)
      values(1,1) = 0.0

      endif

C     Put dataset values

      values(1,1) = real(Intval)
      call DioPutPltDataSetReals (dioOutSet,tims(1),1,1,values)

      RETURN
      END
      SUBROUTINE PUTPER (StreamName,DataSetName)
C
C     Deltares
C
C     CREATED             : jan  2001 by J.v.Gils
C
C     FUNCTION            : Gives permission to DIO to proceed
C                           one step (synchronised mode)
C
C
c     use dio_streams
c     use dio_plt_rw
      include 'dio-plt.inc'

      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times

      character*(dioMaxParLen) vars
      character*(dioMaxLocLen) locs
      character*(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioOutStream
      integer dioOutSet

      logical first
      save first, dioOutSet
      data first /.true./

      if ( first ) then

      first = .false.

C     Open data stream
      dioOutStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'w')

C     Create data set
      Nr_Locations = 1
      Nr_Variables = 1
      locs(1) = 'dioLocNam'
      vars(1) = 'dioVarNam'
      tims(1) = 'dioDate'
      dioOutSet = DioDefinePltDataSet (
     j              dioOutStream,
     j              DataSetName,
     +              Dio_Plt_Real,
     +              Nr_Variables,vars,
     +              Nr_Locations,locs)
      values(1,1) = 0.0

      endif

C     Put dataset values

      call DioPutPltDataSetReals (dioOutSet,tims(1),1,1,values)

      RETURN
      END
      SUBROUTINE PUTPCF (StreamName,DataSetName)
C
C     Deltares
C
C     CREATED             : jan  2001 by J.v.Gils
C
C     FUNCTION            : Gives permission to DIO to proceed
C                           one step (synchronised mode)
C
C
c     use dio_streams
c     use dio_plt_rw
      include 'dio-plt.inc'

      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times

      character*(dioMaxParLen) vars
      character*(dioMaxLocLen) locs
      character*(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioOutStream
      integer dioOutSet

      logical first
      save first, dioOutSet
      data first /.true./

      if ( first ) then

      first = .false.

C     Open data stream
      dioOutStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'w')

C     Create data set
      Nr_Locations = 1
      Nr_Variables = 1
      locs(1) = 'dioLocNam'
      vars(1) = 'dioVarNam'
      tims(1) = 'dioDate'
      dioOutSet = DioDefinePltDataSet (
     j              dioOutStream,
     j              DataSetName,
     +              Dio_Plt_Real,
     +              Nr_Variables,vars,
     +              Nr_Locations,locs)
      values(1,1) = 0.0

      endif

C     Put dataset values

      call DioPutPltDataSetReals (dioOutSet,tims(1),1,1,values)

      RETURN
      END
