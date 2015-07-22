//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2012.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dredgemerge.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/dredgemerge.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Dredge Merge - DEFINITIONS
//
//  Adri.Mourits@Deltares.NL
//  Irv.Elshoff@Deltares.NL
//  31 may 11
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"

#include "precision.h"


#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DREDGESTARTCOMMUNICATE  FC_FUNC(dredgestartcommunicate,DREDGESTARTCOMMUNICATE)
#   define DREDGECOMMUNICATE       FC_FUNC(dredgecommunicate,DREDGECOMMUNICATE)
#   define DREDGENOCOMMUNICATION   FC_FUNC(dredgenocommunication,DREDGENOCOMMUNICATION)

#else
// WIN32
#   define STDCALL  /* nothing */
#   define DREDGESTARTCOMMUNICATE  DREDGESTARTCOMMUNICATE
#   define DREDGECOMMUNICATE       DREDGECOMMUNICATE
#   define DREDGENOCOMMUNICATION   DREDGENOCOMMUNICATION
#endif


//-------------------------------------------------------------------------------


namespace DredgeMerge {
    enum {
        F2DM_init                   = 107901,
        F2DM_voldred                = 107902,
        DM2F_init                   = 701901,
        DM2F_mergedvoldred          = 701902,
        };
    };


//-------------------------------------------------------------------------------


void
DredgeMerge_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    );


extern "C" {
    void STDCALL
    DREDGESTARTCOMMUNICATE (
        int     * domainnumber,
        int     * numdomains
        );

    void STDCALL
    DREDGECOMMUNICATE (
        REAL_FP * voldred,
        int     * numelements
        );

    void STDCALL
    DREDGENOCOMMUNICATION (
        void
        );
    }
