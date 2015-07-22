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
!  $Id: trapar.inc 886 2011-10-10 12:19:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/data/include/trapar.inc $
!-------------------------------------------------------------------------------
    integer, parameter :: RP_TIME  =  1
    integer, parameter :: RP_UMEAN =  2
    integer, parameter :: RP_VMEAN =  3
    integer, parameter :: RP_VELMN =  4
    integer, parameter :: RP_UCHAR =  5
    integer, parameter :: RP_VCHAR =  6
    integer, parameter :: RP_VELCH =  7
    integer, parameter :: RP_ZVLCH =  8
    integer, parameter :: RP_DEPTH =  9
    integer, parameter :: RP_CHEZY = 10
    integer, parameter :: RP_HRMS  = 11
    integer, parameter :: RP_TPEAK = 12
    integer, parameter :: RP_TETA  = 13
    integer, parameter :: RP_RLAMB = 14
    integer, parameter :: RP_UORB  = 15
    integer, parameter :: RP_D50   = 16
    integer, parameter :: RP_DSS   = 17
    integer, parameter :: RP_DSTAR = 18
    integer, parameter :: RP_D10MX = 19
    integer, parameter :: RP_D90MX = 20
    integer, parameter :: RP_MUDFR = 21
    integer, parameter :: RP_HIDEX = 22
    integer, parameter :: RP_SETVL = 23
    integer, parameter :: RP_RHOSL = 24
    integer, parameter :: RP_RHOWT = 25
    integer, parameter :: RP_SALIN = 26
    integer, parameter :: RP_TEMP  = 27
    integer, parameter :: RP_GRAV  = 28
    integer, parameter :: RP_VICML = 29
    integer, parameter :: RP_TAUB  = 30
    integer, parameter :: RP_UBED  = 31
    integer, parameter :: RP_VBED  = 32
    integer, parameter :: RP_VELBD = 33
    integer, parameter :: RP_ZVLBD = 34
    integer, parameter :: RP_VNKAR = 35
    integer, parameter :: RP_Z0CUR = 36
    integer, parameter :: RP_Z0ROU = 37
    integer, parameter :: RP_KTUR  = 38
    integer, parameter :: RP_DG    = 39
    integer, parameter :: RP_SNDFR = 40
    integer, parameter :: RP_DGSD  = 41
    integer, parameter :: MAX_RP   = 41
!

    integer, parameter :: IP_NM    =  1
    integer, parameter :: IP_N     =  2
    integer, parameter :: IP_M     =  3
    integer, parameter :: IP_ISED  =  4
    integer, parameter :: MAX_IP   =  4
!
    integer, parameter :: SP_RUNID =  1
    integer, parameter :: SP_USRFL =  2
    integer, parameter :: MAX_SP   =  2
