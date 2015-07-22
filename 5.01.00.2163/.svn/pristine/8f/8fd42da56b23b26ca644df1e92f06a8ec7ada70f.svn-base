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

      subroutine pdfch2 ( lurep , procesdef, procha)

      ! Fill in the names of the charon coupling, modify stochiometric factors

      use processet
      implicit none

      ! declaration of arguments

      integer               :: lurep               ! unit number report file
      type(procespropcoll)  :: procesdef           ! all processes
      type(ProcesProp)      :: procha              ! charon process definition

C
C     Local declarations
C
      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      integer                   :: i_input         ! index input item
      integer                   :: indx            ! index in io list
      integer                   :: iret            ! return code
      integer                   :: i_output        ! index output item
      integer                   :: iflux           ! index flux
      integer                   :: istochi         ! index stochi
      integer                   :: naij2           ! naij2
      type(ioitempropcoll)      :: input_item      ! one collection of items
      type(ioitempropcoll)      :: output_item     ! one collection of items
      type(ioitempropcoll)      :: fluxoutput      ! one collection of items
      type(stochipropcoll)      :: fluxstochi      ! one collection of stochis

      ! Loop over the processes

      nproc = procesdef%cursize
      do iproc = 1, nproc
         proc => procesdef%procesprops(iproc)

         if ( proc%routine .eq. 'D40CHA' ) then

            ! add extra input

            input_item%cursize=proc%no_input
            input_item%maxsize=proc%no_input
            input_item%ioitemprops => proc%input_item
            do i_input = 1 , procha%no_input
               indx = proc%no_input + i_input
               procha%input_item(i_input)%indx = indx
               iret = ioitempropcolladdindx( input_item, procha%input_item(i_input), indx )
            enddo
            proc%no_input     = input_item%cursize
            proc%input_item   =>input_item%ioitemprops

            ! add extra output

            output_item%cursize=proc%no_output
            output_item%maxsize=proc%no_output
            output_item%ioitemprops => proc%output_item
            do i_output = 1 , procha%no_output
               indx = proc%no_output + i_output
               procha%output_item(i_output)%indx = indx
               iret = ioitempropcolladdindx( output_item, procha%output_item(i_output), indx )
            enddo
            proc%no_output     = output_item%cursize
            proc%output_item   =>output_item%ioitemprops

            ! add fluxes

            fluxoutput%cursize=proc%no_fluxoutput
            fluxoutput%maxsize=proc%no_fluxoutput
            fluxoutput%ioitemprops => proc%fluxoutput
            do iflux = 1 , procha%no_fluxoutput
               indx = proc%no_fluxoutput + iflux
               procha%fluxoutput(iflux)%indx = indx
               iret = ioitempropcolladdindx( fluxoutput, procha%fluxoutput(iflux), indx )
            enddo
            proc%no_fluxoutput     = fluxoutput%cursize
            proc%fluxoutput        =>fluxoutput%ioitemprops

            ! add stochi

            fluxstochi%cursize=proc%no_fluxstochi
            fluxstochi%maxsize=proc%no_fluxstochi
            fluxstochi%stochiprops => proc%fluxstochi
            do istochi = 1 , procha%no_fluxstochi
               iret = StochiPropCollAdd( fluxstochi, procha%fluxstochi(istochi) )
            enddo
            proc%no_fluxstochi     = fluxstochi%cursize
            proc%fluxstochi        =>fluxstochi%stochiprops

         endif

      enddo

      ! modify fluxes

      call chphas ( naij2 )
      call chsto2 ( lurep , naij2 , procesdef)

      return
 2000 format('ERROR: allocating workarray in PDFCH2',i8)
      end
