function shapewrite(filename,varargin)
%SHAPEWRITE Write ESRI shape files.
%   SHAPEWRITE(filename,XYCell)
%   Write patches to shape file (.shp,.shx,.dbf).
%   XYCell should be a cell array of which each
%   element is a Nix2 array defining the polygon
%   consisting of Ni points (X,Y) co-ordinates.
%   The polygons will be closed automatically if
%   they are open.
%
%   Alternatively use:
%   SHAPEWRITE(filename,XY,Patches)
%   with XY a Nx2 matrix of X and Y co-ordinates
%   and Patches a matrix of point indices: each
%   row of the matrix represents one polygon. All
%   polygons contain the same number of points.
%
%   SHAPEWRITE(...,Values)
%   SHAPEWRITE(...,ValLabels,Values)
%   Write data associated with the polygons to the
%   dBase file. Values should be a NPxM matrix where
%   NP equals the number of polygons and M is the
%   number of values per polygon. The default data
%   labels are 'Val_1', 'Val_2', etc. Use a cell
%   array ValLabels if you want other labels. The
%   label length is restricted to a maximum of 10
%   characters.
%
%   SHAPEWRITE(filename,'polyline', ...)
%   Write polylines instead of polygons.
%   SHAPEWRITE(filename,'polygon', ...)
%   Write polygons (i.e. default setting).
%
%   SHAPEWRITE(filename,'point',XY)
%   Write points instead of polygons. XY should be a
%   NPx2 matrix. The number of rows in the optional
%   Value array should match the number of points.
%
%   See also SHAPE, DBASE.

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

DataType=5; % polygon
ValLbl={};
IN=varargin;
if ischar(IN{1})
    switch lower(IN{1})
        case 'point'
            DataType=1;
        case {'line','polyline'}
            DataType=3;
        case 'polygon'
            DataType=5;
        otherwise
            error('Unknown shape identification: %s',IN{1})
    end
    IN=IN(2:end);
end
if iscell(IN{1})
    XY=IN{1}(:);
    NShp=length(XY);
    for i=1:NShp
        if size(XY{i},2)~=2
           error('Invalid number of columns in XY')
        end
    end
    Patch=[];
    switch length(IN)
        case 1
            Val=[];
        case 2
            Val=IN{2};
        case 3
            ValLbl=IN{2};
            if ~iscellstr(ValLbl)
                error('Expected cell string for labels.')
            end
            Val=IN{3};
        otherwise
            error('Invalid number of input arguments.')
    end
else
    XY=IN{1};
    if DataType==1
        NShp=size(XY,1);
        offset=0;
    else
        Patch=IN{2};
        NShp=size(Patch,1);
        NPnt=size(Patch,2);
        offset=1;
        if NPnt<=2, error('Invalid number of columns in Patch, should be at least 3'); end
    end
    %data3d=size(XY,2)==3;
    if size(XY,2)~=2, error('Invalid number of columns in XY'); end
    switch length(IN)
        case 1+offset
            Val=[];
        case 2+offset
            Val=IN{2+offset};
        case 3+offset
            ValLbl=IN{2+offset};
            if ~iscellstr(ValLbl)
                error('Expected cell string for labels.')
            end
            Val=IN{3+offset};
        otherwise
            error('Invalid number of input arguments.')
    end
end
if isempty(Val)
    Val=zeros(NShp,0);
elseif size(Val,1)~=NShp
    error('Invalid length of value vector.');
end
StoreVal=size(Val,2);
if isempty(ValLbl)
    ValLbl(1:StoreVal)={''};
else
    if length(ValLbl)>StoreVal
        error('More value labels than values encountered.');
    else
        if length(ValLbl)<StoreVal
            ValLbl(end+1:StoreVal)={''};
        end
        ValLbl=ValLbl(:)';
        for i=1:length(ValLbl)
            if length(ValLbl{i})>10
                warning('Label %i: ''%s'' truncated to ''%s''.',i,ValLbl{i},ValLbl{i}(1:10));
                ValLbl{i}=ValLbl{i}(1:10);
            end
        end
    end
end

switch DataType
    case 3
        %
        % remove double points
        %
        NParts = ones(length(XY),1);
        Start  = zeros(length(XY),1);
        if iscell(XY)
            for i=1:NShp
                XY{i}(all(abs(diff(XY{i}))<1e-8,2),:)=[]; % remove double values
            end
        end
    case 5
        %
        % close polygons and remove double points
        %
        NParts = ones(length(XY),1);
        Start  = zeros(length(XY),1);
        if iscell(XY)
           for i=1:NShp
              j = 1;
              while ~isequal(XY{i}(end,:),XY{i}(j,:))
                 % check if polygon consists of multiple parts
                 Matching = all(XY{i}(j+1:end,:)-repmat(XY{i}(j,:),size(XY{i},1)-j,1)==0,2);
                 if any(Matching)
                    NParts(i) = NParts(i)+1;
                    j = j+min(find(Matching))+1;
                 else
                    XY{i}(end+1,:)=XY{i}(j,:);
                 end
              end
              XY{i}(all(abs(diff(XY{i}))<1e-8,2),:)=[]; % remove double values
              NParts(i) = 1;
              j = 1;
              while ~isequal(XY{i}(end,:),XY{i}(j,:))
                 % check if polygon consists of multiple parts
                 Matching = all(XY{i}(j+1:end,:)-repmat(XY{i}(j,:),size(XY{i},1)-j,1)==0,2);
                 if any(Matching)
                    NParts(i) = NParts(i)+1;
                    j = j+min(find(Matching))+1;
                    Start(i,NParts(i)) = j-1;
                 else
                    XY{i}(end,:)=XY{i}(j,:);
                 end
              end
            end
        else
            if ~isequal(Patch(:,end),Patch(:,1))
                Patch(:,end+1)=Patch(:,1);
            end
        end
        %
        % remove polygons of two or less points
        %
        if iscell(XY)
            for i=NShp:-1:1
                if size(XY{i},1)<=2
                    XY(i)=[];
                    Val(i,:)=[]; % remove associated data
                    NParts(i,:)=[];
                end
            end
            NShp=length(XY);
        end
end
if length(filename)>3
    switch lower(filename(end-3:end))
       case {'.shp','.shx','.dbf'}
          filename=filename(1:end-4);
    end
end
shapenm=[filename,'.shp'];
shapeidxnm=[filename,'.shx'];
shapedbf=[filename,'.dbf'];
fid=fopen(shapenm,'w','b');
fidx=fopen(shapeidxnm,'w','b');
fwrite(fid,[9994 0 0 0 0 0],'int32');
fwrite(fidx,[9994 0 0 0 0 0],'int32');
fwrite(fid,[0 0 0 0],'int8');
fwrite(fidx,[0 0 0 0],'int8');
fclose(fid);
fclose(fidx);

fid=fopen(shapenm,'a','l');
fidx=fopen(shapeidxnm,'a','l');
fwrite(fid,1000,'int32'); %version
fwrite(fidx,1000,'int32'); %version
%ShapeTps={'null shape' 'point'  '' 'polyline'  '' 'polygon'  '' '' 'multipoint'  '' ...
%          ''           'pointz' '' 'polylinez' '' 'polygonz' '' '' 'multipointz' '' ...
%          ''           'pointm' '' 'polylinem' '' 'polygonm' '' '' 'multipointm' '' ...
%          ''           'multipatch'};
fwrite(fid,DataType,'int32');
fwrite(fidx,DataType,'int32');
ranges=zeros(1,8);
if iscell(XY)
    ranges(1)=inf;
    ranges(2)=inf;
    ranges(3)=-inf;
    ranges(4)=-inf;
    for i=1:NShp
        ranges(1)=min(ranges(1),min(XY{i}(:,1)));
        ranges(2)=min(ranges(2),min(XY{i}(:,2)));
        ranges(3)=max(ranges(3),max(XY{i}(:,1)));
        ranges(4)=max(ranges(4),max(XY{i}(:,2)));
    end
else
    ranges(1)=min(XY(:,1));
    ranges(2)=min(XY(:,2));
    ranges(3)=max(XY(:,1));
    ranges(4)=max(XY(:,2));
end
fwrite(fid,ranges,'float64');
fwrite(fidx,ranges,'float64');
fclose(fidx);
fidx=fopen(shapeidxnm,'r+','b'); fseek(fidx,0,1);
%
FileStorage='A'; % (A - ascii or B - binary) % Binary doesn't work with ArcView 3.0
switch FileStorage
   case 'A'
      WIDTH=19;
   case 'B'
      WIDTH=8;
end
fidb=fopen(shapedbf,'w','l');
fwrite(fidb,3,'uint8');
dv=clock;
fwrite(fidb,[dv(1)-1900 dv(2) dv(3)],'uint8');
fwrite(fidb,NShp,'uint32');
NFld=1+StoreVal;
fwrite(fidb,33+32*NFld,'uint16');
fwrite(fidb,1+11+WIDTH*StoreVal,'uint16'); % NBytesRec includes deleted flag (= first space): 1 + 11
fwrite(fidb,[0 0],'uint8'); % reserved
fwrite(fidb,0,'uint8'); % dBase IV flag
fwrite(fidb,0,'uint8');
fwrite(fidb,zeros(1,12),'uint8'); % dBase IV multi-user environment
fwrite(fidb,0,'uint8'); % Production Index Exists (Fp,dB4,dB5)
fwrite(fidb,0,'uint8'); % 1: USA, 2: MultiLing, 3: Win ANSI, 200: Win EE, 0: ignored
fwrite(fidb,[0 0],'uint8'); % reserved
for i=1:1+StoreVal
    Str=zeros(1,11);
    if i==1
        Str(1:2)='ID';
    elseif ~isempty(ValLbl{i-1})
        LStr=length(ValLbl{i-1});
        Str(1:LStr)=ValLbl{i-1};
    else
        Str(1:4)='Val_';
        ValNr=sprintf('%i',i-1);
        Str(5:4+length(ValNr))=ValNr;
    end
    fwrite(fidb,Str,'uchar');
    if i==1
       fwrite(fidb,'N','uchar');
       fwrite(fidb,[0 0 0 0],'uint8'); % memory address, record offset, ignored in latest versions
       fwrite(fidb,11,'uint8'); % Width
       fwrite(fidb,0,'uint8'); % Type='C' also Width
    else
       switch FileStorage
          case 'A'
             fwrite(fidb,'N','uchar');
             fwrite(fidb,[0 0 0 0],'uint8'); % memory address, record offset, ignored in latest versions
             fwrite(fidb,WIDTH,'uint8'); % Width
             fwrite(fidb,8,'uint8'); % Type='C' also Width
          case 'B'
             fwrite(fidb,'8','uchar');
             fwrite(fidb,[0 0 0 0],'uint8'); % memory address, record offset, ignored in latest versions
             fwrite(fidb,WIDTH,'uint8'); % Width
             fwrite(fidb,0,'uint8'); % Type='C' also Width
       end
    end
    fwrite(fidb,[0 0],'uint8'); % reserved
    fwrite(fidb,0,'uint8'); % dBase IV,V work area ID
    fwrite(fidb,[0 0],'uint8'); % multi-user dBase
    fwrite(fidb,0,'uint8'); % set fields
    fwrite(fidb,zeros(1,7),'uint8'); % reserved
    fwrite(fidb,0,'uint8'); % field is part of production index
end
fwrite(fidb,13,'uint8'); % end of header = 13
switch FileStorage
   case 'A'
      fprintf(fidb,[' %11i' repmat('%19.8f',1,StoreVal)],[1:NShp;Val']);
   case 'B'
      off = ftell(fidb);
      fprintf(fidb,[' %11i' repmat('xxxxxxxx',1,StoreVal)],1:NShp);
      fseek(fidb,0,-1); % search only works if we go back to beginning first
      fseek(fidb,off,-1);
      fwrite(fidb,Val',sprintf('%i*float64',StoreVal),12);
end
fclose(fidb);
%
strtidx=ftell(fid);
checkclockwise=0;
ranges=zeros(1,4);
if DataType==1
    Admin = zeros(2,NShp);
    Admin(1,:) = 1:NShp;
    Admin(2,:) = 10;
    nBytes = Admin(2,:)*2;
    Admin = int32_byteflip(Admin);
    Admin(3,:)=DataType;
    for i=1:NShp
        xy=XY(i,:);
        fwrite(fid,Admin(:,i),'int32');
        fwrite(fid,xy,'float64');
    end
elseif iscell(XY)
    NPntAll=cellfun('size',XY,1);
    Admin = zeros(2,NShp);
    Admin(1,:) = 1:NShp;
    Admin(2,:) = 22+2*NParts'+8*NPntAll';
    nBytes = Admin(2,:)*2;
    Admin = int32_byteflip(Admin);
    Admin(3,:)=DataType;
    for i=1:NShp
        xy=XY{i};
        if checkclockwise && (DataType==5) && (clockwise(xy(:,1),xy(:,2))<0)
            xy=flipud(xy);
        end
        NPnt=NPntAll(i);
        fwrite(fid,Admin(:,i),'int32');
        ranges(1)=min(xy(:,1));
        ranges(2)=min(xy(:,2));
        ranges(3)=max(xy(:,1));
        ranges(4)=max(xy(:,2));
        fwrite(fid,ranges,'float64');
        fwrite(fid,[NParts(i) NPnt Start(i,1:NParts(i))],'int32'); % # parts, # points total, starting offset for each part
        fwrite(fid,xy','float64');
    end
else
    NPnt=size(Patch,2);
    Admin = zeros(2,NShp);
    Admin(1,:) = 1:NShp;
    Admin(2,:) = 24+8*NPnt;
    nBytes = Admin(2,:)*2;
    Admin = int32_byteflip(Admin);
    Admin(3,:)=DataType;
    for i=1:NShp
        ind=Patch(i,:);
        xy=XY(ind,:);
        if checkclockwise && (DataType==5) && (clockwise(xy(:,1),xy(:,2))<0)
            xy=flipud(xy);
        end
        fwrite(fid,Admin(:,i),'int32');
        ranges(1)=min(xy(:,1));
        ranges(2)=min(xy(:,2));
        ranges(3)=max(xy(:,1));
        ranges(4)=max(xy(:,2));
        fwrite(fid,ranges,'float64');
        fwrite(fid,[1 NPnt 0],'int32'); % one part, # points, single part starting at point 0
        fwrite(fid,xy','float64');
    end
end
fidx_data = [strtidx+cumsum([0 nBytes(1:end-1)+8]); nBytes]/2;
fwrite(fidx,fidx_data,'int32');
flid=ftell(fid);
fclose(fid);
%
fid=fopen(shapenm,'r+','b');
fseek(fid,24,-1);
fwrite(fid,flid/2,'int32');
fclose(fid);
%
flidx=ftell(fidx);
fseek(fidx,24,-1);
fwrite(fidx,flidx/2,'int32');
fclose(fidx);
