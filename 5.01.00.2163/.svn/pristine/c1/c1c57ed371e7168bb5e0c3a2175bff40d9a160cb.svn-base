function [data,Slice] = vslice(data,v_slice,pnts)
%VSLICE Vertical slice/section of 2D/3D data set.
%   DATA2DV = VSLICE(DATA3D,'XY',XY) extracts a vertical data slice out of
%   a 3D data set obtained from QPREAD along the specified (X,Y) line.
%   XY(:,1) should contain the X coordinates, and XY(:,2) the Y
%   coordinates. This function works for structured and unstructured meshes.
%
%   DATA2DV = VSLICE(DATA3D,'MN',MN) extracts a vertical data slice out of
%   a 3D data set obtained from QPREAD along a line the points indicates by
%   MN. For a structured grid, MN(:,1) should contain the indices in the
%   first index direction, and MN(:,2) should contain the indices in the
%   second index direction. For unstructured grid, MN should contain the
%   linear index of all points.
%
%   DATA1D = VSLICE(DATA2D,'XY',XY) extracts 1D line data out of a 2D data
%   set obtained from QPREAD along the specified (X,Y) line. XY(:,1) should
%   contain the X coordinates, and XY(:,2) the Y coordinates. The term
%   VSLICE is degenerate in this case.
%
%   DATA1D = VSLICE(DATA2D,'MN',MN) extracts 1D line data out of a 2D data
%   set obtained from QPREAD along a line the points indicates by MN. For a
%   structured grid, MN(:,1) should contain the indices in the first index
%   direction, and MN(:,2) should contain the indices in the second index
%   direction. For unstructured grid, MN should contain the linear index of
%   all points.
%
%   [DATA_OUT,XY_STRUCT] = VSLICE(DATA_IN,'XY',XY) returns a structure with
%   all information on the cross-sections of the XY line with the grid
%   associated with DATA_IN. If the (horizontal) grid doesn't change, the
%   XY_STRUCT can be reused for speed using the syntax:
%   DATA_OUT = VSLICE(DATA_IN,'XY',XY_STRUCT)
%
%   Example
%      quantities = qpread(FI);
%      ivel = strmatch('velocity',{quantities.Name},'exact');
%      Qvel = quantities(ivel);
%      sz = qpread(FI,Qvel,'size');
%      X = [0 35000]';
%      Y = [15000 15000]';
%      for t = 1:sz(1); %select time step where 1<=t<=sz(1)
%         data3d = qpread(FI,Qvel,'griddata',t);
%         data2dv = vslice(data3d,'XY',[X Y]);
%         surf(squeeze(data2dv.X),squeeze(data2dv.Z), ...
%                squeeze(data2dv.XComp.^2+data2dv.YComp.^2+data2dv.ZComp.^2))
%         view(0,90)
%         shading interp
%         drawnow
%      end
%
%   See also QPFOPEN, QPREAD, HSLICE, VRANGE.

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

switch v_slice
   case 'MN'
      if isfield(data,'TRI') | isfield(data,'SEG')
         if isfield(data,'TRI')
            data.X = data.XYZ(:,pnts,:,1);
            data.Y = data.XYZ(:,pnts,:,2);
            if size(data.XYZ,4)>2
               data.Z = data.XYZ(:,pnts,:,3);
            end
            data = rmfield(data,'TRI');
            data = rmfield(data,'XYZ');
         else
            %data.XY = data.XY(pnts,:);
            data.SEG = data.SEG(all(ismember(data.SEG,pnts),2),:);
         end
         Flds = {'Val','XComp','YComp','ZComp'};
         for i=1:length(Flds)
            fld = Flds{i};
            if isfield(data,fld)
               Tmp = getfield(data,fld);
               data = setfield(data,fld,Tmp(:,pnts,:));
            end
         end
      else
         szX = size(data.X);
         szX = szX(1:2); nX=szX(1)*szX(2);
         szX1 = szX-1; nX1=szX1(1)*szX1(2);
         sliceMN = piecewise(pnts,szX);
         ind = sub2ind(szX,sliceMN(:,1),sliceMN(:,2));
         ind1 = [];
         Flds = {'X','Y','Z','Val','XComp','YComp','ZComp'};
         for i=1:length(Flds)
            fld = Flds{i};
            if isfield(data,fld)
               Tmp = getfield(data,fld);
               szTmp = size(Tmp);
               if isequal(szTmp(1:2),szX)
                  Tmp = reshape(Tmp,[nX 1 szTmp(3:end)]);
                  data = setfield(data,fld,Tmp(ind,:,:));
               elseif isequal(szTmp(1:2),szX1)
                  if isempty(ind1)
                     ind1 = sub2ind(szX1,sliceMN(:,1),sliceMN(:,2));
                  end
                  Tmp = reshape(Tmp,[nX1 1 szTmp(3:end)]);
                  data = setfield(data,fld,Tmp(ind1,:,:));
               end
            end
         end
      end
   case 'XY'
      istri = 0;
      if isfield(data,'TRI')
         istri = 1;
      end
      if istri
         geomin = {data.TRI,data.XYZ(:,:,:,1),data.XYZ(:,:,:,2)};
      else
         geomin = {data.X,data.Y};
      end
      if isstruct(pnts)
         Slice = pnts;
      else
         Slice = arbcross(geomin{:},pnts(:,1),pnts(:,2));
      end
      Flds = {'X','Y','Z','Val','XComp','YComp','ZComp'};
      for i=1:length(Flds)
         fld = Flds{i};
         if isfield(data,fld)
            if isequal(fld,'X')
               data = setfield(data,fld,Slice.x);
            elseif isequal(fld,'Y')
               data = setfield(data,fld,Slice.y);
            else
               Tmp = getfield(data,fld);
               data = setfield(data,fld,arbcross(Slice,Tmp));
            end
         end
      end
      if istri
         data.X = arbcross(Slice,data.XYZ(:,:,:,1));
         data.Y = arbcross(Slice,data.XYZ(:,:,:,2));
         if size(data.XYZ,4)>2
            data.Z = arbcross(Slice,data.XYZ(:,:,:,3));
         end
         data=rmfield(data,'TRI');
         data=rmfield(data,'XYZ');
      end
   otherwise
      error('Expected ''XY'' or ''MN'' slice TYPE.')
end
