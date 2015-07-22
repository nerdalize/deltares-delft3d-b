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
// $Id: d_hydro.h 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/utils_lgpl/d_hydro_lib/include/d_hydro.h $
//------------------------------------------------------------------------------
//  D_Hydro Main Program
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------


#pragma once


#if HAVE_CONFIG_H
#   include "config.h"
#endif


#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#if HAVE_CONFIG_H
#   include <sys/wait.h>
#   include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <string>


class DeltaresHydro;
class Clock;
class Exception;
class Log;


#include "clock.h"
#include "component.h"
#include "exception.h"
#include "log.h"
#include "stringutils.h"
#include "xmltree.h"


//------------------------------------------------------------------------------


class DeltaresHydro {
    public:
        typedef bool (*StartComponentEntry) (DeltaresHydro *);

    public:
        DeltaresHydro (
            int     argc,
            char *  argv [],
            char *  envp []
            );
    public:
        ~DeltaresHydro (
            void
            );

        void
        Run (
            void
            );

    public:
        bool        ready;      // true means constructor succeeded and DH ready to run
        char *      exePath;    // name of running d_hydro executable (argv[0])
        char *      exeName;    // short name of executable
        Clock *     clock;      // timing facility
        Log *       log;        // logging facility
        XmlTree *   config;     // top of entire XML configuration tree
        XmlTree *   start;      // root of subtree for the start module
        char *      mainArgs;   // reassembled command-line arguments (argv[1...])
        char *      slaveArg;   // command-line argument for slave mode

        enum {
            MAXSTRING = 1000    // max string length in bytes
            };

        // String constants; initialized below, outside class definition

        static const char startEntry [];    // name of function in start component to invoke after loading
        Component * startComponent;

    private:
        char *      configfile;             // name of configuration file
        bool        done;                   // set to true when it's time to stop
        XmlTree *   curnode;                // current XML tree node during parsing of input file

    };


//------------------------------------------------------------------------------


#ifdef D_HYDRO_MAIN
    DeltaresHydro * DH;     // global pointer to single object instance

    // String constant initialization
    const char DeltaresHydro::startEntry [] = "DeltaresHydroEntry";

#else
    extern DeltaresHydro * DH;
#endif
