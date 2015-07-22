function varargout=shape(cmd,varargin)
%SHAPE Read ESRI shape files.
%   FI = SHAPE('open',FILENAME) opens the ESRI shape file FILENAME and
%   returns a file information data structure to be used in the SHAPE read
%   command described below.
%
%   data = SHAPE('read',FI,OBJECTNR,DATATYPE) reads data from the ESRI
%   shape file. The input arguments are to be specified as follows:
%      FI            - file information data structure as obtained from
%                      SHAPE open file command (explained above).
%      OBJECTNR      - list of object numbers in shape file to be
%                      retrieved; use 0 to load all objects
%      DATATYPE      - currently supported: 'point' or 'polyline'
%
%   SHAPE('write',FILENAME,DATATYPE,XY,PATCHES) writes patches to a shape
%   file triplet (FILENAME.shp,FILENAME.shx,FILENAME.dbf) based on XY an
%   N x 3 matrix of X and Y co-ordinates and PATCHES an Ni x NP matrix of
%   point indices: each of the Ni rows of the matrix represents one
%   polygon/polyline with NP points. The string DATATYPE should read
%   'polygon' (default value if skipped) or 'polyline' and thus determines
%   the shape file type. All polygons/polylines contain the same number of
%   points. Use the following alternative if the number of points varies.
%
%   SHAPE('write',FILENAME,DATATYPE,XYCell) also writes patches to a shape
%   file triplet. Here, XYCell should be a cell array of which each element
%   is a Ni x 2 array defining the polygon consisting of Ni points (X,Y)
%   co-ordinates. The string DATATYPE should again read 'polygon' (default
%   value if skipped) or 'polyline'.
%
%   SHAPE('write',FILENAME,'point',XY) write points to a shape file
%   triplet. XY should be an Ni x 2 matrix.
%
%   SHAPE('write',...,LABELS,VALUES) writes data associated with the
%   polygons to the dBase file: FILENAME.dbf. VALUES should be an Ni x M
%   matrix where Ni equals the number of polygons/polylines/points and M is
%   the number of values per polygon. LABELS should be a cell array of
%   length M; if LABELS is skipped then the default labels are 'Val_1',
%   'Val_2', etc. are used. Note that the label length is restricted to a
%   maximum of 10 characters.

% To be added shortly:
%
%   [XYVERTICES,INDEXFACES,VALUES] = MATRIX2PATCH(X,Y,V)
%   where X and Y are M x N dimensional arrays, V is a (M-1) x (N-1) x NVAL
%   array. The returned arrays can be used in the SHAPE call.
%
%   XYCELL = MATRIX2PATCH(X,Y,V,LEVELS)
%   where X, Y and V are M x N dimensional arrays. The returned arrays can be
%   used in the SHAPE call.

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

if nargin==0
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end

switch lower(cmd)
    case {'open'}
        Info=Local_open_shape(varargin{:});
        varargout={Info};
    case {'read'}
        [Data,Obj]=Local_read_shape(varargin{:});
        varargout={Data,Obj};
    case {'write'}
        shapewrite(varargin{:})
    otherwise
        error('Unknown command: %s',cmd)
end


function S=Local_open_shape(filename)
S.Check='NotOK';
S.FileType='ESRI-Shape';

if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.shp','Select shape file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

[p,n,e]=fileparts(filename);
if strcmpi(e,'.shx')
    % change from .shx into .shp but keep the case the same
    filename(end)=filename(end)-'X'+'P';
    e(end)=e(end)-'X'+'P';
end
if ~strcmpi(e,'.shp')
    if isempty(e) && exist([filename '.shp'])
        filename=[filename '.shp'];
        e='.shp';
    else
        error('Invalid name of shape file.')
    end
end
if isempty(p)
    p=pwd;
end
if ~isequal(p(end),filesep)
    S.FileBase=[p filesep n];
else
    S.FileBase=[p n];
end
S.ShapeExt=e;

S.HeaderByteOrder='b';
fid=fopen(filename,'r',S.HeaderByteOrder);
if ~isequal(fread(fid,[1 6],'int32'),[9994 0 0 0 0 0])
    %
    % This is a fix for TatukGIS files ... the white paper of
    % shape files allows only big endian formatting for the header.
    %
    fclose(fid);
    S.HeaderByteOrder='l';
    fid=fopen(filename,'r',S.HeaderByteOrder);
    if ~isequal(fread(fid,[1 6],'int32'),[9994 0 0 0 0 0])
        error('Invalid shape file header.')
    end
end

S.FileSize=fread(fid,1,'int32')*2; % file size stored in words (16 bit)
fseek(fid,0,1);
if S.FileSize~=ftell(fid)
    warning('Invalid file size stored in file.')
end
fclose(fid);
fid=fopen(filename,'r','l');
fseek(fid,28,-1); % skip header

S.Version=fread(fid,1,'int32');
S.ShapeTp=fread(fid,1,'int32');
ShapeTps={'null shape' 'point'  '' 'polyline'  '' 'polygon'  '' '' 'multipoint'  '' ...
    ''           'pointz' '' 'polylinez' '' 'polygonz' '' '' 'multipointz' '' ...
    ''           'pointm' '' 'polylinem' '' 'polygonm' '' '' 'multipointm' '' ...
    ''           'multipatch'};
S.ShapeTpName=ShapeTps{S.ShapeTp+1};
Tmp=fread(fid,[1 8],'float64');
S.XRange=Tmp([1 3]);
S.YRange=Tmp([2 4]);
S.ZRange=Tmp([5 6]);
S.MRange=Tmp([7 8]);
NShapes=0;
TNPrt=0;
TNPnt=0;
Index=0;
if exist([S.FileBase '.shx'])
    Index=1;
    fidx=fopen([S.FileBase '.shx'],'r',S.HeaderByteOrder);
    if ~isequal(fread(fidx,[1 6],'int32'),[9994 0 0 0 0 0])
        error('Invalid shape index header.');
    end
    fseek(fidx,100,-1);
    S.Idx=2*fread(fidx,[2 inf],'int32'); % stored in bytes!
    fclose(fidx);
    S.IndexExt='.shx';
end

while ~feof(fid)
    if Index && NShapes<size(S.Idx,2)
        fseek(fid,S.Idx(1,NShapes+1),-1);
    end
    [NrSize,k]=fread(fid,2,'int32','b');
    if k==0
        break
    end
    ShapeTp=fread(fid,1,'int32');
    NShapes=NShapes+1;
    if ~Index
        S.Idx(NShapes)=ftell(fid)-12;
    end
    switch ShapeTp
        case 0 % null shape
            % nothing to read
        case 1 % point
            % x,y
            fread(fid,2,'float64');
            TNPnt=TNPnt+1;
            TNPrt=TNPrt+1;
        case {3,5} % polyline, polygon
            % box, NPrt, NPnt, {iprt}, {x,y}
            fread(fid,4,'float64');
            NPrt=fread(fid,1,'int32');
            NPnt=fread(fid,1,'int32');
            TNPrt=TNPrt+NPrt;
            TNPnt=TNPnt+NPnt;
            fread(fid,[1 NPrt],'int32');
            fread(fid,[2 NPnt],'float64');
        case 8 % multipoint
            % box, N, {x,y}
            fread(fid,4,'float64');
            NPnt=fread(fid,1,'int32');
            TNPnt=TNPnt+NPnt;
            fread(fid,[2 NPnt],'float64');
        case 11 % pointz
            % x,y,z,m
            fread(fid,4,'float64');
        case {13,15} % polylinez, polygonz
            % box, NPrt, NPnt, {iprt}, {x,y} zrange, {z}, mrange, {m}
            fread(fid,4,'float64');
            NPrt=fread(fid,1,'int32');
            NPnt=fread(fid,1,'int32');
            TNPrt=TNPrt+NPrt;
            TNPnt=TNPnt+NPnt;
            fread(fid,[1 NPrt],'int32');
            fread(fid,[2 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
        case 18 % multipointz
            % box, N, {x,y}, zrange, {z}, mrange, {m}
            fread(fid,4,'float64');
            NPnt=fread(fid,1,'int32');
            TNPnt=TNPnt+NPnt;
            fread(fid,[2 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
        case 21 % pointm
            % x,y,m
            fread(fid,3,'float64');
        case {23,25} % polylinem, polygonm
            % box, NPrt, NPnt, {iprt}, {x,y}, mrange, {m}
            fread(fid,4,'float64');
            NPrt=fread(fid,1,'int32');
            NPnt=fread(fid,1,'int32');
            TNPrt=TNPrt+NPrt;
            TNPnt=TNPnt+NPnt;
            fread(fid,[1 NPrt],'int32');
            fread(fid,[2 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
        case 28 % multipointm
            % box, N, {x,y}
            fread(fid,4,'float64');
            NPnt=fread(fid,1,'int32');
            TNPnt=TNPnt+NPnt;
            fread(fid,[2 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
        case 31 % multipatch
            % box, NPrt, NPnt, {iprt}, {prttyp}, {x,y}, zrange, {z}, mrange, {m}
            PrtTyps={'trianglestrip','trianglefan','outerring','innerring','firstring','ring'}; % 0 -- 5
            fread(fid,4,'float64');
            NPrt=fread(fid,1,'int32');
            NPnt=fread(fid,1,'int32');
            TNPrt=TNPrt+NPrt;
            TNPnt=TNPnt+NPnt;
            fread(fid,[1 NPrt],'int32');
            fread(fid,[1 NPrt],'int32');
            fread(fid,[2 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
            fread(fid,2,'float64');
            fread(fid,[1 NPnt],'float64');
        otherwise % skip unknown
            fread(fid,NrSize(2)-2,'int16');
    end
end
fclose(fid);
S.NShapes=NShapes;
S.NPrt=TNPrt;
S.NPnt=TNPnt;
S.Check='OK';

if ~exist([S.FileBase '.dbf'])
    return
else
    S.dBase=dbase('open',[S.FileBase '.dbf']);
    if S.dBase.NRec~=S.NShapes
        error('Number of records in dBase file does not match number of shapes.')
    end
end


function [Out,Obj]=Local_read_shape(S,shapes,datatype)
if ~isfield(S,'FileType') || ~strcmp(S.FileType,'ESRI-Shape')
    error('No shape file specified.')
end
if isequal(shapes,0)
    shapes=1:S.NShapes;
elseif min(shapes(:))<1 || max(shapes(:))>S.NShapes || ~isequal(shapes,round(shapes))
    error('Invalid shape number.')
else
    shapes=shapes(:)'; % make sure shapes is a row vector, otherwise Matlab will do just one step in the loop!
end
switch datatype
    case {'point','points'}
        fid=fopen([S.FileBase S.ShapeExt],'r','l');
        fseek(fid,S.Idx(1,shapes(1)),-1);
        TNPnt=0;
        Out=zeros(S.NPnt,2);
        Obj=zeros(S.NPnt,1);
        for shp=shapes
            fseek(fid,S.Idx(1,shp),-1);
            [NrSize,k]=fread(fid,2,'int32','b');
            ShapeTp=fread(fid,1,'int32');
            switch ShapeTp
                case 0 % null shape
                    % nothing to read
                case 1 % point
                    % x,y
                    TNPnt=TNPnt+1;
                    Out(TNPnt,1:2)=fread(fid,[1 2],'float64');
                    Obj(TNPnt)=shp;
                case {3,5} % polyline, polygon
                    % box, NPrt, NPnt, {iprt}, {x,y}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    fread(fid,[1 NPrt],'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 8 % multipoint
                    % box, N, {x,y}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 11 % pointz
                    % x,y,z,m
                    TNPnt=TNPnt+1;
                    Out(TNPnt,1:4)=fread(fid,[1 4],'float64');
                    Obj(TNPnt)=shp;
                case {13,15} % polylinez, polygonz
                    % box, NPrt, NPnt, {iprt}, {x,y} zrange, {z}, mrange, {m}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    fread(fid,[1 NPrt],'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),4)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 18 % multipointz
                    % box, N, {x,y}, zrange, {z}, mrange, {m}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),4)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 21 % pointm
                    % x,y,m
                    TNPnt=TNPnt+1;
                    Out(TNPnt,1:3)=fread(fid,[1 3],'float64');
                    Obj(TNPnt)=shp;
                case {23,25} % polylinem, polygonm
                    % box, NPrt, NPnt, {iprt}, {x,y}, mrange, {m}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    fread(fid,[1 NPrt],'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 28 % multipointm
                    % box, N, {x,y}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                case 31 % multipatch
                    % box, NPrt, NPnt, {iprt}, {prttyp}, {x,y}, zrange, {z}, mrange, {m}
                    PrtTyps={'trianglestrip','trianglefan','outerring','innerring','firstring','ring'}; % 0 -- 5
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    fread(fid,[1 NPrt],'int32');
                    fread(fid,[1 NPrt],'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),4)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt;
                otherwise % skip unknown
                    fread(fid,NrSize(2)-2,'int16');
            end
        end
    case {'polyline','lines'}
        fid=fopen([S.FileBase S.ShapeExt],'r','l');
        fseek(fid,S.Idx(1,shapes(1)),-1);
        TNPnt=0;
        Out=repmat(NaN,S.NPnt+S.NPrt-1,2);
        Obj=repmat(NaN,S.NPnt+S.NPrt-1,1);
        for shp=shapes
            fseek(fid,S.Idx(1,shp),-1);
            [NrSize,k]=fread(fid,2,'int32','b');
            ShapeTp=fread(fid,1,'int32');
            switch ShapeTp
                case 0 % null shape
                    % nothing to read
                case 1 % point
                    % x,y
                    Out(TNPnt+1,1:2)=fread(fid,[1 2],'float64');
                    Obj(TNPnt+1)=shp;
                    TNPnt=TNPnt+2;
                case {3,5} % polyline, polygon
                    % box, NPrt, NPnt, {iprt}, {x,y}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    PSz=fread(fid,[1 NPrt],'int32');
                    PSz=diff([PSz NPnt]);
                    for p=PSz
                        Out(TNPnt+(1:p),1:2)=fread(fid,[2 p],'float64')';
                        Obj(TNPnt+(1:p))=shp;
                        TNPnt=TNPnt+p+1;
                    end
                case 8 % multipoint
                    % box, N, {x,y}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt+1;
                case 11 % pointz
                    % x,y,z,m
                    Out(TNPnt+1,1:4)=fread(fid,[1 4],'float64');
                    Obj(TNPnt+1)=shp;
                    TNPnt=TNPnt+2;
                case {13,15} % polylinez, polygonz
                    % box, NPrt, NPnt, {iprt}, {x,y} zrange, {z}, mrange, {m}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    PSz=fread(fid,[1 NPrt],'int32');
                    PSz=diff([PSz NPnt]);
                    TNPnt0=TNPnt;
                    for p=PSz
                        Out(TNPnt+(1:p),1:2)=fread(fid,[2 p],'float64')';
                        Obj(TNPnt+(1:p))=shp;
                        TNPnt=TNPnt+p+1;
                    end
                    fread(fid,2,'float64');
                    TNPnt=TNPnt0;
                    for p=PSz
                        Out(TNPnt+(1:p),3)=fread(fid,[p 1],'float64');
                        TNPnt=TNPnt+p+1;
                    end
                    fread(fid,2,'float64');
                    TNPnt=TNPnt0;
                    for p=PSz
                        Out(TNPnt+(1:p),3)=fread(fid,[p 1],'float64');
                        TNPnt=TNPnt+p+1;
                    end
                case 18 % multipointz
                    % box, N, {x,y}, zrange, {z}, mrange, {m}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),4)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt+1;
                case 21 % pointm
                    % x,y,m
                    Out(TNPnt+1,1:3)=fread(fid,[1 3],'float64');
                    Obj(TNPnt+1)=shp;
                    TNPnt=TNPnt+2;
                case {23,25} % polylinem, polygonm
                    % box, NPrt, NPnt, {iprt}, {x,y}, mrange, {m}
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    PSz=fread(fid,[1 NPrt],'int32');
                    PSz=diff([PSz NPnt]);
                    TNPnt0=TNPnt;
                    for p=PSz
                        Out(TNPnt+(1:p),1:2)=fread(fid,[2 p],'float64')';
                        Obj(TNPnt+(1:p))=shp;
                        TNPnt=TNPnt+p+1;
                    end
                    fread(fid,2,'float64');
                    TNPnt=TNPnt0;
                    for p=PSz
                        Out(TNPnt+(1:p),3)=fread(fid,[p 1],'float64');
                        TNPnt=TNPnt+p+1;
                    end
                case 28 % multipointm
                    % box, N, {x,y}
                    fread(fid,4,'float64');
                    NPnt=fread(fid,1,'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt+1;
                case 31 % multipatch
                    % box, NPrt, NPnt, {iprt}, {prttyp}, {x,y}, zrange, {z}, mrange, {m}
                    PrtTyps={'trianglestrip','trianglefan','outerring','innerring','firstring','ring'}; % 0 -- 5
                    fread(fid,4,'float64');
                    NPrt=fread(fid,1,'int32');
                    NPnt=fread(fid,1,'int32');
                    fread(fid,[1 NPrt],'int32');
                    fread(fid,[1 NPrt],'int32');
                    Out(TNPnt+(1:NPnt),1:2)=fread(fid,[2 NPnt],'float64')';
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),3)=fread(fid,[NPnt 1],'float64');
                    fread(fid,2,'float64');
                    Out(TNPnt+(1:NPnt),4)=fread(fid,[NPnt 1],'float64');
                    Obj(TNPnt+(1:NPnt))=shp;
                    TNPnt=TNPnt+NPnt+1;
                otherwise % skip unknown
                    fread(fid,NrSize(2)-2,'int16');
            end
        end
        if TNPnt<size(Out,1)
            Out(TNPnt:end,:)=[];
            Obj(TNPnt:end,:)=[];
        end
    case 'patches'
    otherwise
end
try
    fclose(fid);
end
