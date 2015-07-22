function Out=bil(cmd,varargin)
%BIL Read bil/hdr files.
%   FileData = bil('open',filename);
%      Opens the file and interprets and verifies the header
%      information.
%
%   Data = bil('read',FileData,Idx,Precision);
%      Reads data field Idx from the selected file.
%      Returns the data as a variable of the specified precision.
%      By default, the function returns the data in the same precision
%      as stored in the file.

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
        Out=[];
    end
    return
end
switch cmd
    case 'open'
        Out=Local_open_file(varargin{:});
    case 'read'
        Out=Local_read_file(varargin{:});
    otherwise
        error('Unknown command: "%s"',cmd)
end

function Structure=Local_open_file(filename)
Structure.Check = 'NotOK';
Structure.FileType = 'HDR Raster File - BIP/BIL/BSQ';

if (nargin==0) | strcmp(filename,'?')
    [fn,fp] = uigetfile('*.hdr');
    if ~ischar(fn)
        return
    end
    filename = [fp fn];
end
[p,n,e] = fileparts(filename);
switch lower(e)
    case '.hdr'
        Structure.HdrExtension=e;
    case {'.bil','.bip','.bsq'}
        Structure.HdrExtension=char(e-[' ' upper(e(2:end))]+' HDR');
    otherwise
        error('Invalid file extension for raster file: ''%s''.',e)
end
Structure.FileBase = fullfile(p,n);
Structure.FileName = [Structure.FileBase Structure.HdrExtension];
Structure.nBands = 1;
Structure.nBlocks = 1;
Structure.nBits = 8;
[c,maxsize,Structure.ByteOrder] = computer;
Structure.SkipBytes = 0;
Structure.Layout = 'BIL';
%
filename = Structure.FileName;
fid=fopen(filename,'r');
if fid<0
    error('Cannot open header file ''%s''.',filename)
end
KnownKeywords = {'ByteOrder','Layout','nRows','nCols','nBands', ...
    'nBlocks','nBits','BandRowBytes','TotalRowBytes','BandGapBytes', ...
    'NoData','ULXmap','ULYmap','Xdim','Ydim','SkipBytes'};
LowerKeywords = lower(KnownKeywords);
iLine=0;
while ~feof(fid)
    Line = fgetl(fid);
    iLine = iLine+1;
    if length(Line)>100
        fclose(fid);
        error('Line %i considered to be too long.',iLine)
    end
    [T,R] = strtok(Line);
    [R,R2] = strtok(R);
    if isempty(R)
        fclose(fid);
        error('Too few tokens on line %i.',iLine)
    elseif ~isempty(deblank(R2))
        fclose(fid);
        error('Too many tokens on line %i.',iLine)
    end
    iT = strmatch(lower(T),LowerKeywords,'exact');
    if ~isempty(iT)
        if iT>2
            R = sscanf(R,'%f',1);
        end
        Structure = setfield(Structure,KnownKeywords{iT},R);
    else
        fclose(fid);
        error('Unknown keyword ''%s''.',T)
    end
end
fclose(fid);
%
RequiredKeywords = {'nRows','nCols'};
for i=1:length(RequiredKeywords)
    if ~isfield(Structure,RequiredKeywords{i})
        error('Required keyword ''%s'' not found in header file.',RequiredKeywords{i})
    end
end
%
if Structure.nBits == 1 & Structure.nBands~=1
    error('A file with nBits equal to one, should have nBands also equal to one.')
end
%
if isfield(Structure,'Xdim') & ~isfield(Structure,'Ydim')
    error('Missing Ydim keyword.')
elseif isfield(Structure,'Ydim') & ~isfield(Structure,'Xdim')
    error('Missing Xdim keyword.')
elseif ~isfield(Structure,'Xdim') & ~isfield(Structure,'Ydim')
    Structure.Xdim = 1;
    Structure.Ydim = 1;
end
%
if isfield(Structure,'ULXmap') & ~isfield(Structure,'ULYmap')
    error('Missing ULYmap keyword.')
elseif isfield(Structure,'ULYmap') & ~isfield(Structure,'ULXmap')
    error('Missing ULXmap keyword.')
elseif ~isfield(Structure,'ULXmap') & ~isfield(Structure,'ULYmap')
    Structure.ULXmap = Structure.Xdim/2;
    Structure.ULYmap = Structure.Ydim*(Structure.nRows-0.5);
end
%
Structure.Layout = upper(Structure.Layout);
if strcmp(Structure.Layout,'BIL')
    if ~isfield(Structure,'BandGapBytes')
        Structure.BandGapBytes = 0;
        if isfield(Structure,'BandRowBytes')
            Structure.BandGapBytes = Structure.BandRowBytes - Structure.nBits*Structure.nCols/8;
        end
    end
    if ~isfield(Structure,'BandRowBytes')
        Structure.BandRowBytes = Structure.nBits*Structure.nCols/8 + Structure.BandGapBytes;
    end
end
%
switch Structure.Layout
    case {'BIL','BIP','BSQ'}
        filename = [Structure.FileBase use_ext(Structure,Structure.Layout)];
        fid=fopen(filename,'r');
        if fid<0
            error('Cannot open %s file ''%s''.',Structure.Layout,filename)
        end
        fseek(fid,0,1);
        filesize = ftell(fid);
        %
        expectedfilesize = Structure.nBands*Structure.nRows*Structure.nCols*Structure.nBits/8;
        if filesize ~= expectedfilesize
            fclose(fid);
            error('Size of %s file (%u) does not match expected size (%u)',Structure.Layout,filesize,expectedfilesize)
        end
        %
        fclose(fid);
    otherwise
        error('Layout ''%s'' not yet supported.',Structure.Layout)
end
%
Structure.Check='OK';

%--------------------------------------------------------------------------
function Data=Local_read_file(Structure,i,tFormatUser)
if isequal(i,'xcc')
    Data = Structure.ULXmap+Structure.Xdim*(0:Structure.nCols-1);
    if nargin>2
        Data = repmat(Data,Structure.nRows,1);
    end
elseif isequal(i,'ycc')
    Data = Structure.ULYmap-Structure.Ydim*(0:Structure.nRows-1)';
    if nargin>2
        Data = repmat(Data,1,Structure.nCols);
    end
elseif isequal(i,'xco')
    Data = Structure.ULXmap+Structure.Xdim*((0:Structure.nCols)-0.5);
    if nargin>2
        Data = repmat(Data,Structure.nRows,1);
    end
elseif isequal(i,'yco')
    Data = Structure.ULYmap-Structure.Ydim*((0:Structure.nRows)'-0.5);
    if nargin>2
        Data = repmat(Data,1,Structure.nCols);
    end
elseif isnumeric(i)
    if i~=round(i) | i<1 | i>Structure.nBands
        error('Invalid band index %g.',i)
    else
        filename = [Structure.FileBase use_ext(Structure,'BIL')];
        switch upper(Structure.ByteOrder)
            case {'I','L'}
                bOrder = 'l';
            case {'M','B'}
                bOrder = 'b';
        end
        fid=fopen(filename,'r',bOrder);
        if fid<0
            error('Cannot open BIL file ''%s''.',filename)
        end
        fseek(fid,Structure.SkipBytes,-1);
        switch Structure.nBits
            case 1
                sFormat = 'bit1';
                tFormat = 'int8';
            case 2
                sFormat = 'bit2';
                tFormat = 'int8';
            case 4
                sFormat = 'bit4';
                tFormat = 'int8';
            case 8
                sFormat = 'int8';
                tFormat = 'int8';
            case 16
                sFormat = 'int16';
                tFormat = 'int16';
            case 32
                sFormat = 'float32';
                tFormat = 'float32';
            case 64
                sFormat = 'float64';
                tFormat = 'float64';
            otherwise
                error('Invalid number of bits (%i)',Structure.nBits)
        end
        if nargin>2
            tFormat = tFormatUser;
            cFormat = [sFormat '=>' tFormat];
        else
            cFormat = ['*' sFormat];
        end
        %
        switch tFormat
            case 'int8'
                nodata = int8(Structure.NoData);
            case 'int16'
                nodata = int16(Structure.NoData);
            case 'float32'
                nodata = single(Structure.NoData);
            case 'float64'
                nodata = Structure.NoData;
        end
        %
        switch Structure.Layout
            case 'BIL'
                Data = repmat(nodata,Structure.nRows,Structure.nCols);
                fseek(fid,(i-1)*Structure.BandRowBytes,0);
                SkipBytes = (Structure.nBands-1)*Structure.BandRowBytes;
                for r = 1:Structure.nRows
                    DataRow = fread(fid,[1 Structure.nCols],cFormat);
                    Data(r,:) = DataRow;
                    fseek(fid,SkipBytes,0);
                end
            case 'BIP'
                fseek(fid,(i-1)*Structure.nBits/8,0);
                Skip = Structure.nBits*(Structure.nBands-1);
                if ~strcmp('bit',sFormat(1:3))
                    Skip = Skip/8;
                end
                Data = fread(fid,[Structure.nCols Structure.nRows],cFormat,Skip);
                Data = Data';
            case 'BSQ'
                fseek(fid,(i-1)*Structure.nCols*Structure.nRows*Structure.nBits/8,0);
                Data = fread(fid,[Structure.nCols Structure.nRows],cFormat);
                Data = Data';
        end
        Data(Data == nodata) = NaN;
        fclose(fid);
    end
else
    error('Invalid field indicator.')
end

%--------------------------------------------------------------------------
function bil=use_ext(Structure,bilstr)
bil = char(Structure.HdrExtension-' HDR'+[' ' bilstr]);
