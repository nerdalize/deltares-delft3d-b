#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libkernel_general.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general

#-------------------------------------------------------------------------------
#Sources
libkernel_general_la_SOURCES = \
         angle.f90 \
         area4.f90 \
         chckit.f90 \
         checkmeteoresult.f90 \
         copykcuv.f90 \
         d3stop.f90 \
         dattim.f90 \
         defsub.f90 \
         delfil.f90 \
         delnef.f90 \
         distance_gdp.f90 \
         dittar.f90 \
         dtn.f90 \
         endscr.f90 \
         errmsg.f90 \
         evenquotes.f90 \
         exifil.f90 \
         filfmt.f90 \
         flhnew.f90 \
         flwlic.f90 \
         fouini.f90 \
         getcel.f90 \
         iniid.f90 \
         ipon.f90 \
         keyinp.f90 \
         modlen.f90 \
         n_and_m_to_nm.f90 \
         newlun.f90 \
         nm_to_diag.f90 \
         nm_to_n_and_m.f90 \
         noendspaces.f90 \
         noextspaces.f90 \
         nointextspaces.f90 \
         nuliar.f90 \
         open_datdef.f90 \
         pldep.F90 \
         prterr.f90 \
         prtnefiserr.f90 \
         read1c.f90 \
         read1i.f90 \
         read1r.f90 \
         read2c.f90 \
         read2i.f90 \
         read2r.f90 \
         readnc.f90 \
         readni.f90 \
         readnr.f90 \
         regel.f90 \
         rmdel.f90 \
         setcurrentdatetime.f90 \
         setlun.f90 \
         sortindices.f90 \
         srckey.f90 \
         step_to_screen.f90 \
         timdat.f90 \
         tmpcheck.f90 \
         txtmrk.f90 \
         txtstr.f90 \
         vermdf.f90 \
         wrline.f90 \
         z_ainpro.f90 \
         z_taubotmodifylayers.f90 \
         search.f90

#-------------------------------------------------------------------------------
#Libraries

