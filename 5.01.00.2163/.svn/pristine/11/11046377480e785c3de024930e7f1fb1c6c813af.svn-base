function [G,GridFileName]=get_matching_grid(MapSeg,pn,filterspec)
%GET_MATCHING_GRID Get grid file that matches size of current dataset.
%   [GRIDINFO,GRIDFILENAME] = GET_MATCHING_GRID(MAPSEG) opens a dialog to
%   select a file with MAPSEG elements in the (aggregated) grid.
%
%   [GRIDINFO,GRIDFILENAME] = GET_MATCHING_GRID(GRIDSIZE) opens a dialog to
%   select a grid specified GRIDSIZE.

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
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

GridSeg=-1;
PerLayer=0;
CouldReadGridData = 0;
filters = {'*.cco;*.lga' 'Delft3D Grid (Aggregation) Files'
   '*.m2b' 'SOBEK Grid Aggregation Files'
   '*.geo;geo*' 'Telemac Grid Files'
   '*.nc' 'UGRID netCDF Files (D-Flow FM, Untrim)'};
if nargin<3
   filterspec = '';
end
first = 1;
while 1
   if exist(filterspec)==2
      if first
         GridFileName=filterspec;
         first = 0;
      else
         error('Specified grid file can''t be read or doesn''t match data file.\n%s',filterspec)
      end
   else
      cp=pwd;
      cd(pn);
      [gfn,gpn]=uigetfile(filters,'Select matching grid file ...');
      cd(cp);
      if ~ischar(gfn)
         G=[];
         GridFileName='';
         break
      end
      GridFileName=[gpn gfn];
   end
   [pn,fn,ex]=fileparts(GridFileName);
   trytp = lower(ex);
   switch trytp
      case {'.lga','.cco'} % multiple extensions supported
         trytp = '.lga';
      case {'.m2b','.arc'} % multiple extensions supported
         trytp = '.m2b';
      case {'.grd','.geo','.nc'} % single extension cases
         %trytp = trytp;
      otherwise
         trytp = '.lga';
   end
   trytp0 = trytp;
   CouldReadGridData = 0;
   GetError = 0;
   while 1
      GridSize=[NaN NaN];
      GridSeg=NaN;
      switch trytp
         case '.lga'
            try
               G=delwaq('open',GridFileName);
               GridSeg=G.NoSeg;
               PerLayer=G.NoSegPerLayer;
               CouldReadGridData = 1;
            catch
               trytp = '.grd';
            end
         case '.grd'
            try
               G=wlgrid('open',GridFileName);
               GridSize=size(G.X);
               CouldReadGridData = 1;
            catch
               trytp = '.m2b';
            end
         case '.m2b'
            try
               G=arcgrid('read',GridFileName);
               GridSeg=max(G.Data(:));
               PerLayer=GridSeg;
               G.MNK=[size(G.Data) 1];
               G.Index=G.Data;
               G.Index(isnan(G.Data))=0;
               CouldReadGridData = 1;
            catch
               trytp = '.nc';
            end
         case '.nc'
            try
               G = nc_info(GridFileName);
               G = nc_interpret(G);
               %
               AggrTable=false(length(G.Dataset),1);
               for i=1:length(G.Dataset)
                   for j=1:length(G.Dataset(i).Attribute)
                       if strcmp(G.Dataset(i).Attribute(j).Name,'delwaq_role') && ...
                                strcmp(G.Dataset(i).Attribute(j).Value,'segment_aggregation_table')
                            table = nc_varget(G.Filename,G.Dataset(i).Name);
                            if max(table)==MapSeg
                               AggrTable(i) = true;
                            end
                            break
                       end
                   end
               end
               %
               if any(AggrTable)
                   iAggr = find(AggrTable);
                   iAggr = iAggr(1);
                   GridSeg = MapSeg;
                   CouldReadGridData = 1;
                   xbounds = G.Dataset(iAggr).XBounds;
                   ybounds = G.Dataset(iAggr).YBounds;
                   x = G.Dataset(iAggr).X;
                   y = G.Dataset(iAggr).Y;
                   G.Aggregation = G.Dataset(iAggr).Name;
                   G.AggregationDims = [G.Dataset(x).Dimension setdiff(G.Dataset(iAggr).Dimension,G.Dataset(x).Dimension)];
%                   [G.Index, errmsg] = qp_netcdf_get(G,G.Aggregation,G.AggregationDims);
%                   G.Index(isnan(G.Index))=0;
                   G.MNK=[GridSeg 1 1];
                   G.Index=(1:GridSeg)';
               else
                   C = cell(0,4);
                   for i=1:length(G.Dataset)
                       for j=1:length(G.Dataset(i).Attribute)
                           if strcmp(G.Dataset(i).Attribute(j).Name,'bounds')
                               C{end+1,1} = G.Dataset(i).Attribute(j).Value;
                               C{end,2} = G.Dataset(i).Name;
                               C{end,3} = G.Dataset(i).Dimension;
                               C{end,4} = G.Dataset(i).Size;
                           end
                       end
                   end
                   %
                   if ~isempty(C)
                       matching = 0;
                       for i=1:size(C,1)
                           if isequal(C{i,4},MapSeg)
                               matching = i;
                               break
                           end
                       end
                       %
                       if ~matching
                           error('Unable to identify coordinate bounds variables for %i segments.',segdim)
                       else
                           segdim = C{matching,3};
                           GridSeg = C{matching,4};
                           %
                           matching = false(size(C,1),1);
                           for i=1:size(C,1)
                               matching(i) = isequal(C{i,3},segdim);
                           end
                           C = C(matching,:);
                           %
                           % locate x/longitude coordinate
                           %
                           ytype='';
                           for i=1:size(C,1)
                               j=strmatch(C{i,2},{G.Dataset.Name},'exact');
                               switch G.Dataset(j).Type
                                   case 'longitude'
                                       ytype = 'latitude';
                                       break
                                   case 'x-coordinate'
                                       ytype = 'y-coordinate';
                                       break
                               end
                           end
                           if isempty(ytype)
                               error('Unable to identify x-coordinate/longitude for segments')
                           else
                               x = j;
                               xbounds = strmatch(C{i,1},{G.Dataset.Name},'exact');
                           end
                           %
                           % locate y/latitude coordinate
                           %
                           y=[];
                           for i=1:size(C,1)
                               j=strmatch(C{i,2},{G.Dataset.Name},'exact');
                               switch G.Dataset(j).Type
                                   case ytype
                                       y = j;
                                       ybounds = strmatch(C{i,1},{G.Dataset.Name},'exact');
                               end
                           end
                           if isempty(y)
                               error('Unable to identify %s for segments',ytype)
                           end
                           %
                           CouldReadGridData = 1;
                       end
                   end
                   G.MNK=[GridSeg 1 1];
                   G.Index=(1:GridSeg)';
               end
               G.BCoordinates = {G.Dataset(xbounds).Name G.Dataset(ybounds).Name};
               G.CCoordinates = {G.Dataset(x).Name G.Dataset(y).Name};
               XBoundsDimensions = G.Dataset(xbounds).Dimension;
               XDimensions = G.Dataset(x).Dimension;
               G.CoordDims = [intersect(XDimensions,XBoundsDimensions) setdiff(XBoundsDimensions,XDimensions)];
               G.FileType = 'netCDF';
               %
               for j = 1:length(G.Dataset(x).Attribute)
                   if isequal(G.Dataset(x).Attribute(j).Name,'units')
                       switch G.Dataset(x).Attribute(j).Value
                           case {'degrees_east','degree_east','degreesE','degreeE', ...
                                   'degrees_north','degree_north','degreesN','degreeN'}
                               G.Unit = 'deg';
                           otherwise
                               G.Unit = G.Dataset(x).Attribute(j).Value;
                       end
                   end
               end
            catch
               trytp = '.geo';
            end
         case '.geo'
            try
               G=telemac('open',GridFileName);
               GridSeg=G.Discr.NPnts;
               PerLayer=GridSeg;
               G.MNK=[GridSeg 1];
               G.Index=(1:GridSeg)';
               CouldReadGridData = 1;
            catch
               trytp = '.lga';
            end
      end
      if CouldReadGridData || GetError
         break
      end
      if isequal(trytp,trytp0) % gone full circle
         GetError = 1;
      end
   end
   if CouldReadGridData
      if length(MapSeg)==1
         %
         % Number of segments given.
         %
         if MapSeg==GridSeg || ... % exact match for ordinary map files
               MapSeg==GridSeg+PerLayer % one shift for PART map files with sediment layer
            break
         elseif round(MapSeg/GridSeg)==MapSeg/GridSeg && GridSeg>0 % data amount matches multiple layers
            G.MNK(3) = MapSeg/GridSeg;
            G.Index = repmat(G.Index,[1 1 G.MNK(3)]);
            for k=2:G.MNK(3)
               G.Index(:,:,k)=G.Index(:,:,k)+(k-1)*GridSeg;
            end
            break
         else
            ui_message('error','Number of segments in map file (%i) does not\nmatch the number of segments in the\ngrid file (%i)',MapSeg,GridSeg);
         end
      else
         %
         % Grid dimensions given.
         %
         if isequal(GridSize,MapSeg)
            break
         else
            ui_message('error','Grid size (%ix%i) does not match data size (%ix%i)',GridSize,MapSeg);
         end
      end
   else
      ui_message('error',lasterr);
   end
end
