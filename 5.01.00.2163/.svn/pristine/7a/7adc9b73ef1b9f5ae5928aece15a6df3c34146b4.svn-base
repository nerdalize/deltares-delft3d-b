function Out=pcraster(cmd,varargin);
%PCRASTER Read/write PC-Raster files.
%   FileInfo = pcraster('open',filename);
%      opens a PC raster file.

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

if nargin==0,
    if nargout>0,
        Out=[];
    end;
    return;
end;
switch cmd,
    case 'open',
        Out=Local_open_file(varargin{:});
    case 'field',
        Out=Local_open_field(varargin{:});
end;


function StructOut=Local_open_field(StructIn,i),
if isequal(i,StructIn.Current)
    StructOut=StructIn;
else
    n=num2str(StructIn.ExtensionLength);
    nm=[StructIn.Base '.' sprintf(['%',n,'.',n,'i'],StructIn.Series(i))];
    StructOut=Local_open_one_file(nm);
end


function Structure=Local_open_file(varargin),
Structure=Local_open_one_file(varargin{:});
if ~isequal(Structure.Check,'OK')
    return
end

[pn0,fn0,ex]=fileparts(Structure.FileName);
ex=ex(2:end);
if ~isempty(ex)
    exnr=str2num(ex);
    if ~isempty(exnr)
        d=dir(fullfile(pn0,[fn0,'.*']));
        if length(d)>1
            exnrs=zeros(1,length(d));
            j=0;
            for i=1:length(d)
                [pn,fn,ex]=fileparts(d(i).name);
                ex=ex(2:end);
                new_exnr=str2num(ex);
                if ~isempty(new_exnr)
                    j=j+1;
                    exnrs(j)=new_exnr;
                end
            end
            exnrs(j+1:end)=[];
            exnrs=sort(exnrs);
            Structure.Base=fullfile(pn0,fn0);
            Structure.ExtensionLength=length(ex);
            Structure.Series=exnrs;
            Structure.Current=find(exnrs==exnr);
        end
    end
end


function Structure=Local_open_one_file(filename),
Structure.Check='NotOK';
Structure.FileType='PCraster';
Structure.Title='';

if (nargin==0) | strcmp(filename,'?'),
    [fn,fp]=uigetfile('*.map');
    if ~ischar(fn),
        return;
    end;
    filename=[fp fn];
end;
fid=fopen(filename,'r','l');
Structure.FileName=filename;
if ~strcmp(char(fread(fid,[1 27],'uchar')),'RUU CROSS SYSTEM MAP FORMAT')
    fclose(fid);
    error('Invalid PC raster header')
end
fread(fid,1,'uchar'); % 0
fread(fid,1,'int32'); % 0
fread(fid,1,'int16'); % 2
Structure.FileID=fread(fid,1,'int16'); % 55
fread(fid,1,'int16'); % 0
if fread(fid,1,'int16') % 0 or 1
    Structure.YDir='from bottom to top';
else
    Structure.YDir='from top to bottom';
end
Structure.InfoTableOffset=fread(fid,1,'int32');
fread(fid,[1 2],'int16'); % 1 1
fread(fid,[1 4],'int32'); % 0 0 0 0
X=fread(fid,[1 2],'int16');
switch X(1)
    case 235 % scalar, real
        Structure.PCRType='scalar';
        Structure.DataType='float32';
        NBytes=4;
    case 242 % ordinal, integer
        Structure.PCRType='ordinal';
        if X(2)==0
            Structure.DataType='int8';
            NBytes=1;
        else
            Structure.DataType='int32';
            NBytes=4;
        end
    case 226 % nominal, integer
        Structure.PCRType='nominal';
        if X(2)==0
            Structure.DataType='int8';
            NBytes=1;
        else
            Structure.DataType='int32';
            NBytes=4;
        end
    case 240 % ldd, integer
        Structure.PCRType='ldd';
        Structure.DataType='int8';
        NBytes=1;
    case 251 % logical, integer
        Structure.PCRType='directional';
        Structure.DataType='int8';
        NBytes=1;
    case 224 % logical, integer
        Structure.PCRType='boolean';
        Structure.DataType='int8';
        NBytes=1;
    otherwise
        fclose(fid);
        error('Unknown data type in PC raster file.');
end
Structure.MinData=fread(fid,1,Structure.DataType);
fread(fid,8-NBytes,'uchar'); % -1
Structure.MaxData=fread(fid,1,Structure.DataType);
fread(fid,8-NBytes,'uchar'); % -1
Structure.Offset=fread(fid,[1 2],'float64'); % xoffset yoffset
Structure.Size=fread(fid,[1 2],'int32');
Structure.CellSize=fread(fid,[1 2],'float64'); % xsize ysize

fseek(fid,256,-1);
%Structure.DataOffset=ftell(fid);
Structure.Data=fread(fid,fliplr(Structure.Size),Structure.DataType)';

if Structure.InfoTableOffset~=0
    if Structure.InfoTableOffset~=ftell(fid)
        fclose(fid);
        error('Invalid PC raster info table offset');
    end
    Tp(10)=0;
    for t=1:10,
        Tp(t)=fread(fid,1,'int16'); % 6
        X=fread(fid,1,'int32'); % table
        if Tp(t)~=-1
            Structure.TableOffset(t)=X;
        end
        fread(fid,1,'int32'); % table size ...
    end
    for t=1:length(Structure.TableOffset),
        fseek(fid,Structure.TableOffset(t),-1);
        j=fread(fid,1,'int32');
        Str=char(fread(fid,[1 60],'uchar'));
        Structure.Title=deblank(Str);
        k=0;
        for i=Structure.MinData:Structure.MaxData,
            k=k+1;
            j=fread(fid,1,'int32');
            if i~=j
                fclose(fid);
                error('Invalid PC raster info table');
            end
            Str=char(fread(fid,[1 60],'uchar'));
            if t==1
                Structure.Table{k,1}=i;
            end
            Structure.Table{k,t+1}=deblank(Str);
        end
    end
end
fclose(fid);
Structure.Check='OK';
