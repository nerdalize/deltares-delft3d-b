//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2012.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id: c2c.h 1180 2012-01-13 17:05:48Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/nefis/packages/nefis/include/c2c.h $
#ifndef __C2C__
#define __C2C__

#include "nefis.h"
#include "nef-tag.h"

extern BInt4 nefis_errcnt;
extern BInt4 nefis_errno;
extern BChar error_text[LENGTH_ERROR_MESSAGE+1];
extern BInt4 nefis_flush;

extern BInt4 close_nefis_files   ( BInt4 *);
extern BInt4 create_nefis_files  ( BInt4 *, BText  , BText  , BChar  , BChar  );
extern BInt4 DBG_nefis_tree      ( BInt4  , BText  );
extern BInt4 Define_cel          ( BInt4  , BText  , BInt4  , BText  );
extern BInt4 Define_data         ( BInt4  , BText  , BText  );
extern BInt4 Define_element      ( BInt4  , BText  , BText  , BText  , BText  ,
                                   BText  , BInt4  , BInt4  , BInt4 *);
extern BInt4 Define_group        ( BInt4  , BText  , BText  , BInt4  , BInt4 *,
                                   BInt4 *);
extern BInt4 Get_element         ( BInt4  , BText  , BText  , BInt4 *, BInt4 *,
                                   BUInt4 , BData  );
extern BInt4 GP_flush_hash       ( BInt4  , BInt4  );
extern BInt4 GP_get_next_cell    ( BInt4   , BInt4  , BText  , BText  , BUInt4 *,
                                   BUInt8 *);
extern BInt4 GP_get_next_elm     ( BInt4  , BInt4  , BText  , BText  , BText  ,
                                   BText  , BText  , BUInt4 *, BUInt4 *, BUInt4 *,
                                   BUInt8 *);
extern BInt4 GP_get_next_def_grp ( BInt4  , BInt4  , BText  , BText  , BInt4 *,
                                   BInt4 *, BInt4 *);
extern BInt4 GP_get_next_grp     ( BInt4  , BInt4  , BText  , BText  );
extern BInt4 GP_get_attribute    ( BInt4  , BInt4  , BText  , BText  , BData  ,
                                   BText  );
extern BInt4 GP_inquire_cel      ( BInt4  , BText   , BUInt4 *, BText   , BUInt8 *);
extern BInt4 GP_inquire_dat      ( BInt4  , BUInt8 *, BText, BText);
extern BInt4 GP_inquire_elm      ( BInt4  , BText   , BText   , BText   , BText,
                                   BText  , BUInt4 *, BUInt4 *, BUInt4 *, BUInt8 *);
extern BInt4 GP_inquire_grp_def  ( BInt4   , BText   , BText   ,
                                   BUInt4 *, BUInt4 *, BUInt4 *);
extern BInt4 GP_inquire_max      ( BInt4  , BText  , BUInt4 *);
extern BUInt8 GP_read_file        ( BInt4  , BText  , BUInt8  , BUInt8  );
extern BInt4 GP_put_attribute    ( BInt4 *, BText  , BText  , BData  , BText  );
extern BInt4 nefis_error         ( BInt4  , BText  );
extern BInt4 Put_element         ( BInt4  , BText  , BText  , BInt4 *, BInt4 *,
                                   BData  );
extern BInt4 OC_get_version      ( BText  );
#endif
