function Out=telemac(cmd,varargin)
%TELEMAC Read Telemac selafin files.
%   F = TELEMAC('open',FileName)
%   Opens the file and returns structure containing file
%   information.
%
%   Data = TELEMAC('read',F,TimeIndex,VarNr,PntNrs)
%   Read data from file for specified time indices, variable
%   indices and point numbers.

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

switch cmd,
    case 'open',
        Out=telemac_open(varargin{:});
    case 'read',
        Out=telemac_read(varargin{:});
    otherwise,
        error('Unknown command: %s.',cmd)
end;


function Struct=telemac_open(filename)
Struct.Check='NotOK';
Struct.FileType='Serafin';

if (nargin==0) | strcmp(filename,'?'),
    [fname,fpath]=uigetfile('*.slf','Select Telemac file');
    if ~ischar(fname),
        return;
    end;
    filename=fullfile(fpath,fname);
end;

Struct.FileName=filename;
fid=fopen(Struct.FileName,'r','b');
if fid<0
    return
end

Struct.Title=char(fortranread(fid,[40 2],'uchar')');

Struct.NVar=fortranread(fid,[1 2],'int32');
NVar1=Struct.NVar(1);
NVar2=Struct.NVar(2);
NVar=NVar1+NVar2;

for i=1:NVar
    X=char(fortranread(fid,[16 2],'uchar')');
    Struct.Var(i).Name = deblank(X(1,:));
    Struct.Var(i).Unit = deblank(X(2,:));
    Struct.Var(i).Type = 1 + (i>NVar1);
end

Struct.IParam=fortranread(fid,[1 10],'int32');

if Struct.IParam(10)
    Time=fortranread(fid,[1 6],'int32');
    Struct.RefTime=datenum(Time(1),Time(2),Time(3),Time(4),Time(5),Time(6));
end

%
% Size of grid
%
X=fortranread(fid,[1 4],'int32');
Struct.Discr(1).NElem=X(1);
Struct.Discr(1).NPnts=X(2);
Struct.Discr(1).NPntsPerElem=X(3);
Struct.Discr(1).Type=X(4); % 1P1 (or 3P2)
if NVar2>0
    X=fortranread(fid,[1 4],'int32');
    Struct.Discr(2).NElem=X(1);
    Struct.Discr(2).NPnts=X(2);
    Struct.Discr(2).NPntsPerElem=X(3);
    Struct.Discr(2).Type=X(4); % 1P1 (or 3P2)
end

%
% Element definition: reference to corner points in global index array
%
Struct.Discr(1).Elem=fortranread(fid,[Struct.Discr(1).NPntsPerElem Struct.Discr(1).NElem],'int32')';
if NVar2>0
    Struct.Discr(2).Elem=fortranread(fid,[Struct.Discr(2).NPntsPerElem Struct.Discr(2).NElem],'int32')';
end

%
% Boundary definition: for each point index in array of boundary points (0
% if not the point is not a boundary point).
%
Struct.Discr(1).BoundPnt=fortranread(fid,[Struct.Discr(1).NPnts 1],'int32');
if NVar2>0
    Struct.Discr(2).BoundPnt=fortranread(fid,[Struct.Discr(2).NPnts 1],'int32');
end

%
% X co-ordinates.
%
Struct.Discr(1).X=fortranread(fid,[Struct.Discr(1).NPnts 1],'float32');
if NVar2>0
    Struct.Discr(2).X=fortranread(fid,[Struct.Discr(2).NPnts 1],'float32');
end

%
% Y co-ordinates.
%
Struct.Discr(1).Y=fortranread(fid,[Struct.Discr(1).NPnts 1],'float32');
if NVar2>0
    Struct.Discr(2).Y=fortranread(fid,[Struct.Discr(2).NPnts 1],'float32');
end

Struct.Offset=ftell(fid);

if Struct.IParam(1)==0
    % Not encountered yet (every thing in one record)
elseif Struct.IParam(1)==1
    Struct.RecordSize=12+NVar1*(Struct.Discr(1).NPnts+2)*4;
    if NVar2>0
        Struct.RecordSize=Struct.RecordSize+NVar2*(Struct.Discr(2).NPnts+2)*4;
    end
    fread(fid,[1 1],'int32');
    Struct.Times=fread(fid,[1 inf],'float32',Struct.RecordSize-4)/3600/24;
end
Struct.NTimes=length(Struct.Times);

fseek(fid,0,1);
FileSize=ftell(fid);
RNTimes=(FileSize-Struct.Offset)/Struct.RecordSize;
if Struct.NTimes~=RNTimes
    warning(sprintf('%i time steps read, %g time steps according to file size.',Struct.NTimes,RNTimes))
end

fclose(fid);
Struct.Check='OK';


function Data=telemac_read(Struct,time,var,pnts)
if any(time>Struct.NTimes)
    error('Time step number too large.')
end
fid=fopen(Struct.FileName,'r','b');
if fid<0
    error('Cannot open data file.')
end
if nargin<4
    pnts=1:Struct.Discr.NPnts;
end
Data=zeros(length(time),length(pnts));
for t=1:length(time)
    Offset = Struct.Offset + Struct.RecordSize*(time(t)-1);
    if Struct.IParam(1)==0
    elseif Struct.IParam(1)==1
        if var<=Struct.NVar(1)
            Offset = Offset+12+(var-1)*(Struct.Discr(1).NPnts+2)*4;
        elseif var<=Struct.NVar(1)+Struct.NVar(2)
            Offset = Offset+12+Struct.NVar(1)*(Struct.Discr(1).NPnts+2)*4+(var-Struct.NVar(1)-1)*(Struct.Discr(2).NPnts+2)*4;
        else
            fclose(fid);
            error('Variable number too large.')
        end
        fseek(fid,Offset,-1);
        DataTmp=fortranread(fid,[1 Struct.Discr(1).NPnts],'float32');
        Data(t,:)=DataTmp(1,pnts);
    end
end
fclose(fid);


function Data=fortranread(fid,size,type)
switch type
    case 'int32'
        NBytesPE=4;
    case 'float32'
        NBytesPE=4;
    case 'uchar'
        NBytesPE=1;
    otherwise
        error('Undefined type: %s.',type)
end
NBytesExpected=prod(size)*NBytesPE;
NBytes=fread(fid,[1 1],'int32');
if ~isequal(NBytes,NBytesExpected)
    fclose(fid);
    error('Unexpected size at beginning of record: %i (expected: %i)',NBytes,NBytesExpected)
end
Data=fread(fid,size,type);
NBytes=fread(fid,[1 1],'int32');
if ~isequal(NBytes,NBytesExpected)
    fclose(fid);
    error('Unexpected size at end of record: %i (expected: %i)',NBytes,NBytesExpected)
end
