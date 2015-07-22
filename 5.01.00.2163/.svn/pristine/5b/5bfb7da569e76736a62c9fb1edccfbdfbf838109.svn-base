% Delft3D-MATLAB interface toolbox.
% Version <VERSION> (<CREATIONDATE>)
%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2012 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%
% Graphical user interfaces
%   d3d_qp              - QuickPlot user interface: plotting interface for Delft3D output data.
%   ecoplot             - EcoPlot: Case Analysis Tool for Delft3D-WAQ/ECO/SED data.
%   qpsa                - Get handle to the current QuickPlot axis.
%   qpsf                - Get handle to the current QuickPlot figure.
%
% Delft3D QuickPlot functions
%   qpfopen             - General routine for open various types of data files.
%   qpread              - Read data from various types of data files.
%   qpfile              - Get information about the active file in QuickPlot.
%   hslice              - Horizontal data slice of 3D data set.
%   vslice              - Vertical slice/section of 2D/3D data set.
%   vmean               - Compute average of data in vertical direction.
%   vrange              - Selection of data based on a vertical coordinate range.
%   qpcross             - Cross-section of data: one point in time.
%   qpcrosst            - Cross-section of data: time-dependent track.
%
% NEFIS Viewer Selector functions
%   vs_use              - Initiates the use of a NEFIS file.
%   vs_disp             - Displays the filestructure of a NEFIS file.
%   vs_let              - Read one or more elements from a NEFIS file.
%   vs_get              - Read one or more elements from a NEFIS file.
%   vs_find             - Locates an element in the filestructure of a NEFIS file.
%   vs_type             - Determines the type of the NEFIS file.
%   vs_diff             - Locates the differences between two NEFIS files.
%
% Other data access functions
%   bct_io              - Read/write boundary condition tables.
%   d3d_attrib          - Read/write a various Delft3D attribute files.
%   ecomsed             - Read an ECOMSED data file.
%   enclosure           - Read/write enclosure files and convert enclosures.
%   golder              - Read/write a Golder data file.
%   landboundary        - Read/write land boundary files.
%   mdf                 - Manipulate Delft3D-FLOW mdf files.
%   samples             - Read/write sample data from file.
%   shape               - Read ESRI shape files.
%   tekal               - Read/write for Tekal files.
%   tekal2tba           - Parses comments of a TEKAL to determine tidal analysis data.
%   trirst              - Read/write Delft3D-FLOW restart file.
%   weir                - Read/write a weir file.
%   wldep               - Read/write Delft3D field files (e.g. depth files).
%   wlfdep              - Read/write Delft3D-MOR field files.
%   wlgrid              - Read/write a Delft3D grid file.
%   xyveloc             - Reads X,Y,U,V from a trim- or com-file.
%
% Special plot routines
%   classbar            - Converts a color bar into a classbar.
%   colquiver           - Color quiver plot.
%   drawgrid            - Plots the grid.
%   md_clock            - Create a clock or calendar.
%   md_paper            - Add border to plot.
%   plotlimitingfactors - Create a limiting factors plot.
%   plot_tidalellipses  - Plot tidal ellipses on a map.
%   qp_drawsymbol       - Draw a north arrow or incident wave arrow.
%   tba_compare         - Plot computed versus observed tidal analysis data.
%   tba_plotellipses    - Plot tidal ellipses from Delft3D-TRIANA TBA file.
%   thindam             - Plot dams, weirs and vanes.
%   tick                - Create ticks and ticklabels.
%
% Additional tools
%   average_trim        - Average fields of a TRIM file.
%   clipgrid            - Clip a grid away from the inside/outside of a polygon.
%   degstr              - Convert degree values to string representation.
%   floodmask           - Find connected points in an array.
%   qp_unitconversion   - Convert unit strings.
%   ustrcmpi            - Find a unique string.
%   wildstrmatch        - Find matching strings using wildcards.

%   @(#)Deltares, Delft3D-MATLAB interface, Version <VERSION>, <CREATIONDATE>
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

% Helper routines
%   ap2ep               - Convert tidal amplitude and phase lag (ap-) parameters into tidal ellipse
%   calldll             - Calls a DLL.
%   cellarea            - Compute surface area of grid cells.
%   clrmap              - Creates a colormap based on a few colors.
%   columnpatch         - Create stacked patches plot.
%   convertnval         - Convert NVal between string and number.
%   dimprint            - Convert dimension structure into string for printing.
%   face2surf           - Construct surface data from values at patch face centers.
%   grib                - Read GRIB files. (BETA VERSION)
%   isenvironment       - Checks the code evaluation environment.
%   matlabversionnumber - Obtain the MATLAB version number.
%   md_colormap         - Colour map editor.
%   none                - True if all elements of a vector are zero.
%   pathdistance        - Computes the distance along a path.
%   printdims           - Display dimension information.
%   qck_anim            - Helper function for QuickPlot Animations.
%   qp_colorbar         - Display color bar (color scale).
%   qp_getdata          - General interface for various data files
%   qp_gridview         - Helper routine for Grid View interface.
%   qp_validate         - Helper function to validate Delft3D-QUICKPLOT.
%   splitcellstr        - Split cell string at delimiters.
%   stagprint           - Convert stagger name into string for printing.
%   tdelft3d            - Conversion procedure for Delft3D date & time.
%   topodescription     - Returns a topology description string for a location.
%   ui_inspectstruct    - Inspect a structure.
%   ui_message          - Graphical display for errors/warnings.
%   vs_copy             - Copy data from one NEFIS file to another.
%   vs_def              - Changes the groups, cells and element definitions.
%   vs_ini              - Creates a NEFIS file.
%   vs_pack             - Remove inaccessible/unused space from a NEFIS file.
%   vs_put              - Write data to a NEFIS file.
%   arbcross            - Arbitrary cross-section through grid.
%   delwaq              - Read/write Delwaq files.
%   qpsi                - Get tag object of the current QuickPlot item.
%   tricontour          - Contour plot for triangulated data.
%   tricontourf         - Filled contour plot for triangulated data.
%   waquaio             - Read SIMONA SDS file.
%   waqfil              - Read various Delwaq binary files.
