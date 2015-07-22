function [varargout]=arbcross(varargin)
%ARBCROSS Arbitrary cross-section through grid.
%   [X,Y,V1,V2, ...]=ARBCROSS(TRI,XTRI,YTRI,VTRI1,VTRI2, ... ,XB,YB)
%   Intersects a triangular mesh defined by TRI, XTRI and YTRI with an
%   arbitrary line defined by base points XB, YB. The output vectors X
%   and Y contain the co-ordinates at which the line crosses the grid
%   lines of the mesh. The vector Vi contains interpolated values at
%   these locations given the values VTRIi at the mesh points.
%
%   [X,Y,V1,V2, ...]=ARBCROSS(XGRID,YGRID,VGRID1,VGRID2, ... ,XB,YB)
%   Intersects a curvilinear mesh defined by XGRID and YGRID with an
%   arbitrary line.
%
%   Computing the locations of the intersections of the mesh and the line
%   can take a significant amount of time. It can be more efficient to
%   compute these intersections and the associated coefficients for the
%   interpolation only once. The necessary intermediate information can
%   be stored in a structure by using the following syntax:
%   STRUCT=ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   [X,Y,STRUCT]=ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   STRUCT=ARBCROSS(XGRID,YGRID,XB,YB)
%   [X,Y,STRUCT]=ARBCROSS(XGRID,YGRID,XB,YB)
%
%   Subsequently, the interpolation of data to that line can be carried
%   out efficiently by providing the structure as a first argument
%   instead of the original coordinates:
%   [V1,V2, ...] = ARBCROSS(STRUCT,VGRID1,VGRID2, ...)
%   [X,Y,V1,V2, ...] = ARBCROSS(STRUCT,VGRID1,VGRID2, ...)

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

varargout=cell(1,nargout);
if (nargout+1==nargin | nargout-1==nargin) & nargin>1 & isstruct(varargin{1})
    %
    % One input argument more than there are output arguments: besides the
    % fields that match between input and output (VGRIDi - Vi), the list of
    % input arguments contains also a structure. The structure contains all
    % information necessary for creating the cross-section; this structure
    % should be created using a previous ARBCROSS call.
    %
    input_offset = 1;
    if nargout+1==nargin
        output_offset = 0;
    else
        output_offset = 2;
    end
    input_skip_end = 0;
    %
    Keep = varargin{1};
    x = Keep.x;
    y = Keep.y;
    wght = Keep.wght;
    ind = Keep.ind;
    indtri = Keep.indtri;
    outside = Keep.outside;
    VGRIDStr = Keep.VGRIDStr;
    szXGRID = Keep.szXGRID;
    szTRI = Keep.szTRI;
    QUADTRI = Keep.QUADTRI;
else
    %
    % No structure containing all necessary information.
    %
    structure_out = 0;
    input_skip_end = 2;
    layeredxy = 0;
    if nargout+2==nargin | ((nargout==1 | nargout==3) & nargin==4)
        %
        % CURVILINEAR GRID:
        % (1) Two input arguments more than there are output arguments:
        % besides the fields that match between input and output (VGRIDi -
        % Vi), the list of output arguments also contains X and Y (the
        % coordinates of the intersections of the grid and the line) whereas
        % the list of input arguments also contains XGRID, YGRID (first
        % two arguments) and XB, YB (last two arguments). The first two
        % arguments define the curvilinear grid on which the data are
        % defined, and the last two arguments define the line of the
        % cross-section.
        % (2) When you create a structure, there are two options:
        % -a- one output argument (the structure) and four input arguments,
        % namely XGRID, YGRID, XB and YB. See description above.
        % -b- three output arguments: the structure, X and Y. Same input
        % arguments as in case -a-.
        %
        structure_out = nargout+2~=nargin;
        %
        XGRID = varargin{1}(:,:,1);
        YGRID = varargin{2}(:,:,1);
        [TRI,QUADTRI] = grid2tri(XGRID,YGRID);
        input_offset = 2;
        layeredxy = size(varargin{1},3)>1;
        output_offset = input_offset;
        if nargout==1
            output_offset = 0;
        end
        VGRIDStr = 'VGRID';
        szTRI = [];
    elseif nargout+3==nargin | ((nargout==1 | nargout==3) & nargin==5)
        %
        % TRIANGULAR MESH
        % (1) Three input arguments more than there are output arguments:
        % besides the fields that match between input and output (VGRIDi -
        % Vi), the list of output arguments also contains X and Y (the
        % coordinates of the intersections of the grid and the line) whereas
        % the list of input arguments also contains TRI, XTRI, YTRI (first
        % two arguments) and XB, YB (last two arguments). The first three
        % arguments define the triangular mesh on which the data are defined,
        % and the last two arguments define the line of the cross-section.
        % (2) When you create a structure, there are two options:
        % -a- one output argument (the structure) and five input arguments,
        % namely TRI, XTRI, YTRI, XB and YB. See description above.
        % -b- three output arguments: the structure, X and Y. Same input
        % arguments as in case -a-.
        %
        structure_out = nargout+3~=nargin;
        %
        TRI = varargin{1};
        QUADTRI = [];
        XGRID = varargin{2}(:,:,1);
        YGRID = varargin{3}(:,:,1);
        input_offset = 3;
        output_offset = 2;
        if nargout==1
            output_offset = 0;
        end
        VGRIDStr = 'VTRI';
        szTRI = size(TRI,1);
    else
        error('Number of input arguments does not match number of output arguments.');
    end

    %
    % Determine intersection points of cross-section line and curvilinear
    % grid or triangular mesh.
    %
    XB=varargin{end-1};
    YB=varargin{end};
    [x,y,ind,wght,indtri,fracudist]=int_lntri(XB,YB,TRI,XGRID,YGRID);
    outside = isnan(indtri);
    indtri(outside)=1;

    %
    % Add dummy points where the slice goes out of the computational domain
    % such that there will appear a break in the plots.
    %
    for i=length(outside):-1:1
        if outside(i)
            ii = [1:i i i+1:length(x)];
            x=x(ii); x(i+1)=NaN;
            y=y(ii); y(i+1)=NaN;
            ind=ind(ii,:);
            wght=wght(ii,:); wght(i+1,:)=NaN;
            fracudist=fracudist(ii);
            ii = [1:i i i+1:length(indtri)];
            indtri=indtri(ii);
            outside=outside(ii);
        end
    end

    if layeredxy
        x = repmat(x,[1 1 size(varargin{1},3)]);
        y = repmat(y,[1 1 size(varargin{2},3)]);
    end
    %
    % Define structure for future use
    %
    szXGRID = size(XGRID);
    if structure_out
        Keep.x = x;
        Keep.y = y;
        Keep.wght = wght;
        Keep.ind = ind;
        Keep.fracudist = fracudist;
        Keep.indtri = indtri;
        Keep.outside = outside;
        Keep.VGRIDStr = VGRIDStr;
        Keep.szXGRID = szXGRID;
        Keep.szTRI = szTRI;
        Keep.QUADTRI = QUADTRI;
        varargout{nargout} = Keep;
    end
end

%
% Define output
%
if output_offset>0
    varargout{1}=x;
    varargout{2}=y;
end
%
% For each data field VGRIDi in the list of input arguments, i.e. those
% starting after the grid information (unless it is a 3D grid) and stopping
% before the last two arguments.
%
for i=1:nargin-input_offset-input_skip_end
    VGRID = varargin{input_offset+i};
    szVGRID = size(VGRID);
    if isequal(szVGRID(1:2),szXGRID)
        %
        % Values defined at mesh points (triangular or curvilinear)
        %
        v=[];
        for k = size(VGRID,3):-1:1
            vgrid = VGRID(:,:,k);
            v(:,1,k) = sum(wght.*vgrid(ind),2);
        end
    elseif isequal(szVGRID([2 1]),szXGRID) & szXGRID(2)==1
        v = sum(wght.*VGRID(ind),2);
    elseif isequal(szVGRID(1:2),szXGRID-1)
        %
        % Values defined on patches of curvilinear grid
        %
        v=[];
        indquad = QUADTRI(indtri);
        for k = size(VGRID,3):-1:1
            vgrid = VGRID(:,:,k);
            v(:,1,k) = vgrid(indquad);
            v(outside,1,k) = NaN;
        end
    elseif numel(VGRID)==szTRI
        %
        % Values defined on patches of triangular mesh
        %
        v = VGRID(indtri);
        v(outside) = NaN;
    else
        errmsg = sprintf('Invalid size of %s%i',VGRIDStr,i);
        error(errmsg)
    end
    varargout{output_offset+i} = v;
end


function [tri,quadtri]=grid2tri(X,Y)
% GRID2TRI converts a curvilinear grid into a triangular grid
%       [TRI,QUADTRI]=GRID2TRI(XGRID,YGRID)
%       Splits the quadrangles of the curvilinear grid along the main
%       diagonal and returns the triangle definition table TRI (indicating
%       the corner points of the triangles as indices into XGRID, YGRID)
%       and an array QUADTRI that contains for every triangle the index of
%       the quadrangle to which the triangle belongs (index into an array
%       of size SIZE(XGRID)-1).

szX=size(X);
% [m,n]=ndgrid(1:szX(1),1:szX(2));
I=reshape(1:prod(szX),szX);
I=I(1:end-1,1:end-1);
I=I(:);
quad=(1:prod(szX-1))';

tri= [I I+1 I+szX(1)+1; I I+szX(1) I+szX(1)+1];
quadtri= [quad;quad];
% mtri= [m(I);m(I)];
% ntri= [n(I);n(I)];

k=any(isnan(X(tri)) | isnan(Y(tri)),2);
tri(k,:)=[];
quadtri(k)=[];
