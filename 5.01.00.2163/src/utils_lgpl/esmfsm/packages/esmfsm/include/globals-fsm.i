!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: globals-fsm.i 2144 2013-01-25 16:35:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/esmfsm/packages/esmfsm/include/globals-fsm.i $
!-------------------------------------------------------------------------------
!   Delft-FSM (FORTRAN Shared Memory)
!   Fortran definitions for "fortapi.f90"
!
!   Irv.Elshoff@deltares.nl
!   12 nov 04
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!   C Implementation routines (in "fsm.cpp")

    external FSM_Init
    integer FSM_Init

    external FSM_MakePointer
    integer(kind=pntrsize) FSM_MakePointer

    external FSM_GetPointer
    integer(kind=pntrsize) FSM_GetPointer

    external FSM_ReleasePointer
    integer FSM_ReleasePointer

    external FSM_PrintKeys
    integer FSM_PrintKeys

    external FSM_Err
    integer FSM_Err

    external FSM_TraceFile
    integer FSM_TraceFile


!-------------------------------------------------------------------------------
!   Fortran data in "fsmf.f"

    integer nbytes (7)
    integer(pntrsize) alignment (7)
    common /fsmglo/ nbytes, alignment


!-------------------------------------------------------------------------------
!   Limits (coupled with definitions in "esm.h")

    integer &
        FSM_MAX_NAME
    
    parameter ( &
        FSM_MAX_NAME = (20+1) &
        )
