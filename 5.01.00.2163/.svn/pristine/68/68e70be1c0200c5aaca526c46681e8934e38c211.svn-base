subroutine wave_init (mdw_file)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
! Reads the name of the mdw file from the command line
! determines whether or not a stand alone computation is required
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    !
    implicit none
!
! Global variables
!
      character(256)       :: mdw_file
      type(wave_data_type) :: wavedata
!
! Local variables
!
   logical            :: ex
!
!! executable statements -------------------------------------------------------
!
      ! write runid obtained from flow to the file runid(s)
      !
      if (mdw_file == ' ') then
         write (*,'(a)') '*** ERROR: No mdw-file specified on command-line.'
         call usage()
         stop
      endif
      !
      ! check if file exists
      !
      inquire (file=mdw_file, exist=ex)
      if (.not. ex) then
         write (*,'(a,a,a)') '*** ERROR: mdw-file ''', trim(mdw_file),''' does not exist.'
         stop
      endif
end subroutine wave_init
