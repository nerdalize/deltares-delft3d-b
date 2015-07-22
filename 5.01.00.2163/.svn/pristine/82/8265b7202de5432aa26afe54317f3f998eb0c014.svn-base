function [Out,Out2]=arcgrid(cmd,varargin)
%ARCGRID Read/write operations for arcgrid files.
%   FileData = arcgrid('open',filename);
%      Opens data from an arcgrid file
%      and determines the dimensions of
%      the grid. Detects the presence of
%      multiple arcgrid-files (for FLS).
%
%   Data = arcgrid('read',filename);
%      Reads data from a not-opened
%      arcgrid file.
%
%   Data = arcgrid('read',FileData);
%      Reads data from an opened
%      arcgrid file.
%   Data = arcgrid('read',FileData,i);
%      Reads data from the i-th data
%      file in a series.
%
%   arcgrid('write',FileData,filename);
%      Writes data to an arcgrid file.
%
%   AxesHandle = arcgrid('plot',FileData);
%      Plots arcgrid data as elevations.

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
        DX=Local_read_file(varargin{:});
        if iscell(DX)
            Out=DX{1};
            if length(DX)>1
                Out2=DX{2};
            end
        else
            Out=DX;
        end
    case 'resample'
        Out=Local_resample_file(varargin{:});
    case 'write'
        if nargin==1
            error('Not enough input arguments.')
        end
        Local_write_file(varargin{:});
    case 'plot'
        if nargin==1
            H=[];
        else
            H=Local_plot_file(varargin{:});
        end
        if nargout>0
            Out=H;
        end
    otherwise
        error('Unknown command: "%s"',cmd)
end


function Structure=Local_open_file(filename)
Structure.Check='NotOK';
Structure.FileType='arcgrid';

if (nargin==0) || strcmp(filename,'?')
    [fn,fp]=uigetfile('*.arc');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'r');
Structure.FileName=filename;
if fid<0
    return
end
[p,n,e]=fileparts(filename);
if isempty(p)
    p=pwd;
end
Structure.Extension=e(2:end);
Line=fgetl(fid); % First lines might be comment lines: /* ....
time_in_file=0;
while strmatch('/*',Line)
    if ~isempty(strfind(Line,'time=')) || ~isempty(strfind(Line,'time ='))
        time_in_file=1;
    end
    Line=fgetl(fid);
end
%
Structure.XCorner=0;
Structure.YCorner=0;
Structure.NCols=0;
Structure.NRows=0;
Structure.CellSize=0;
Structure.NoData=NaN;
Structure.DataStart=0;
%
while ~feof(fid)
    [keyw,remLine]=strtok(Line);
    switch lower(keyw)
        case 'ncols'
            Structure.NCols=sscanf(remLine,'%i',1);
        case 'nrows'
            Structure.NRows=sscanf(remLine,'%i',1);
        case 'xllcorner'
            Structure.XCorner=sscanf(remLine,'%f',1);
        case 'yllcorner'
            Structure.YCorner=sscanf(remLine,'%f',1);
        case {'xllcentre','xllcenter'}
            XCentre=sscanf(remLine,'%f',1);
            Structure.XCorner='centre';
        case {'yllcentre','yllcenter'}
            YCentre=sscanf(remLine,'%f',1);
            Structure.YCorner='centre';
        case 'cellsize'
            Structure.CellSize=sscanf(remLine,'%f',[1 2]);
            if length(Structure.CellSize)==1
                Structure.CellSize=Structure.CellSize([1 1]);
            end
        case 'nodata_value' % both nodata_value and NODATA_value occur
            Structure.NoData=sscanf(remLine,'%f',1);
        otherwise
            if length(keyw)>2 && isequal(keyw(1:2),'/*')
                % skip comment
            else
                fseek(fid,Structure.DataStart,-1);
                break
            end
    end
    Structure.DataStart=ftell(fid);
    Line=fgetl(fid);
end
if isequal(Structure.XCorner,'centre')
    Structure.XCorner=XCentre-Structure.CellSize(1)/2;
end
if isequal(Structure.YCorner,'centre')
    Structure.YCorner=YCentre-Structure.CellSize(2)/2;
end
%
% End of normal reading of header. Now, let's check whether we have opened
% a wind or pressure file of Delft3D which may include multiple data fields
% separated by time stamps. This check is performed by skipping all comment
% lines and scanning for a line stating "TIME (HRS)" ...
%
Line=fgetl(fid);
Time=[];
while strmatch('/*',Line)
    Structure.DataStart=ftell(fid);
    if isempty(Time)
        Time=sscanf(lower(Line),'/* time (hrs) %f',1);
    end
    Line=fgetl(fid);
end
Times=Time;
%
% If I did find such a line, I have to scan the whole file for similar
% lines ...
%
if ~isempty(Time)
    Time=[];
    while ~feof(fid)
        LineStartsAt=ftell(fid);
        Line=fgetl(fid);
        if length(Line)>2 && strcmp(Line(1:2),'/*') && isempty(Time)
            Time=sscanf(lower(Line),'/* time (hrs) %f',1);
        elseif ~isempty(Time)
            Structure.DataStart(end+1,1)=LineStartsAt;
            Times(end+1,1)=Time;
            Time=[];
        end
    end
end
%
% File reading finished. Close file and do some final checks ...
%
fclose(fid);
if Structure.NCols<=0
    error('Number of columns not specified or invalid')
elseif Structure.NRows<=0
    error('Number of rows not specified or invalid')
elseif Structure.CellSize<=0
    error('Cell size not specified or invalid')
end
Structure.Check='OK';
if ~isempty(Times)
    Structure.Times=Times;
    %
    Structure.FileBase=[p filesep n];
    lwc=Structure.Extension-upper(Structure.Extension);
    switch lower(Structure.Extension)
        case 'amu'
            amv=char('AMV'+lwc);
            fid=fopen([Structure.FileBase '.' amv],'r');
            if fid>0
                Structure.Extension=char('AMUV'+lwc([1:3 3]));
                fclose(fid);
            end
        case 'amv'
            amu=char('AMU'+lwc);
            fid=fopen([Structure.FileBase '.' amu],'r');
            if fid>0
                Structure.Extension=char('AMUV'+lwc([1:3 3]));
                fclose(fid);
            end
    end
    %
    return
end
Structure.Times=[];
%
% For case sensitive file systems assume that all extensions have the same
% case (upper/lower characters) on a character by character basis.
%
ndigits=0;
while ndigits<=length(n) && abs(n(end-ndigits))>47 && abs(n(end-ndigits))<58
    ndigits=ndigits+1;
end
digits=n(length(n)-ndigits+1:end);
if all(abs(digits)>47 & abs(digits)<58)
    Structure.FileBase=[p filesep n(1:end-ndigits)];
    Structure.NDigits=ndigits;
    [Structure.Times,FileNr]=getfiletimes(Structure.FileBase,Structure.Extension,time_in_file);
    if ~isempty(FileNr)
        Structure.FileNr = FileNr;
    end
end
lwc=Structure.Extension-upper(Structure.Extension);
if strcmpi(Structure.Extension,'amu')
    amv=char('AMV'+lwc);
    Times=getfiletimes(Structure.FileBase,amv,time_in_file);
    if isequal(Times,Structure.Times)
        Structure.Extension=char('AMUV'+lwc([1:3 3]));
    end
elseif strcmpi(Structure.Extension,'amv')
    amu=char('AMU'+lwc);
    Times=getfiletimes(Structure.FileBase,amu,time_in_file);
    if isequal(Times,Structure.Times)
        Structure.Extension=char('AMUV'+lwc([1:3 3]));
    end
end


function [Times,FileNr]=getfiletimes(FileBase,Extension,time_in_file)
[FilePath,FileName]=fileparts(FileBase);
last_char=length(FileName);
len_ext=length(Extension)+1;
%
Files=dir([FileBase '*.' Extension]);
ntimes=length(Files);
FileNr=cell(ntimes,1);
Times=zeros(ntimes,1);
if ntimes==0
    return
elseif time_in_file
    for i=1:ntimes
        FileNr{i}=Files(i).name(last_char+1:end-len_ext);
        %
        fl=fopen(fullfile(FilePath,Files(i).name),'r');
        if fl>0
            Line=fgetl(fl);
            n = strfind(Line,'time=');
            if ~isempty(n)
                Times(i) = sscanf(Line(n+5:end),'%f');
            else
                n = strfind(Line,'time =');
                Times(i) = sscanf(Line(n+6:end),'%f');
            end
            fclose(fl);
        end
    end
else
    Times=zeros(ntimes,1);
    for i=1:ntimes
        FileNr{i}=Files(i).name(last_char+1:end-len_ext);
        Times(i)=(FileNr{i}-48)*10.^(length(FileNr{i})-1:-1:0)';
    end
end
[Times,I] = sort(Times);
FileNr = FileNr(I);


function Structure=Local_read_file(filename,nr)
if (nargin==0)
    filename='';
    Structure=Local_open_file;
elseif ischar(filename)
    Structure=Local_open_file(filename);
else
    Structure=filename;
    if isfield(Structure,'Data'),
        Structure=Structure.Data;
        return
    end
end
if strcmp(Structure.Check,'NotOK')
    if isstruct(filename)
        Structure=[];
    end
    return
end

ext=3;
if strcmpi(Structure.Extension,'amuv')
    ext=[3 4];
end
for i=ext
    fil=Structure.FileName;
    subnr=1;
    if nargin>1 && length(Structure.DataStart)==1
        if isfield(Structure,'FileNr')
            Nr=Structure.FileNr{nr};
        else
            if isfield(Structure,'NDigits')
                ndigits=Structure.NDigits;
            else
                ndigits=3;
            end
            format=sprintf('%%%i.%ii',ndigits,ndigits);
            Nr=sprintf(format,Structure.Times(nr));
        end
        fil=[Structure.FileBase Nr '.' Structure.Extension([1 2 i])];
    elseif nargin>1
        subnr=nr;
        fil=[Structure.FileBase '.' Structure.Extension([1 2 i])];
    end
    fid=fopen(fil,'r');
    fseek(fid,Structure.DataStart(subnr),-1);

    Structure.Check='NotOK';
    [Data,NumRead]=fscanf(fid,'%f',[Structure.NCols Structure.NRows]);
    if NumRead<(Structure.NCols*Structure.NRows)
        if feof(fid)
            fclose(fid);
            error('Insufficient data values found in the file: %s.',Structure.FileName)
        end
        X=char(fread(fid,1,'uchar'));
        if isequal(X,'*')
            fseek(fid,Structure.DataStart,-1);
            fprintf(1,'Trying to read file as free formatted.\n');
            Data=Local_read_freeformat(fid,[Structure.NCols Structure.NRows]);
            Structure.FreeFormat=1;
        else
            fclose(fid);
            fprintf(1,['Unexpected character ''' X ''' encountered.\n']);
            error('Not all data values could be read from %s.',Structure.FileName)
        end
    else
        [X,More]=fscanf(fid,'%f',1);
        if More
            fprintf(1,'File seems to contain more data values than indicated in header.\n');
        end
    end

    if ~isnan(Structure.NoData)
        Data(Data==Structure.NoData)=NaN;
    end
    if i==3
        Structure.Data=Data;
    else
        Structure.Data2=Data;
    end
    fclose(fid);
    Structure.Check='OK';
end
if isstruct(filename)
    if isequal(ext,3)
        Structure={Structure.Data};
    else
        Structure={Structure.Data Structure.Data2};
    end
end


function Local_write_file(Structure,filename)

if nargin==1
    [fn,fp]=uiputfile('*.arc');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'wt');
if fid<0
    error(['Could not create or open: ',filename])
end
fprintf(fid,'/* arc grid file created by Matlab\n');
fprintf(fid,'ncols         %i\n',Structure.NCols);
fprintf(fid,'nrows         %i\n',Structure.NRows);
fprintf(fid,'xllcorner     %f\n',Structure.XCorner);
fprintf(fid,'yllcorner     %f\n',Structure.YCorner);
if length(Structure.CellSize)==1
    fprintf(fid,'cellsize      %f\n',Structure.CellSize);
elseif Structure.CellSize(1)==Structure.CellSize(2)
    fprintf(fid,'cellsize      %f\n',Structure.CellSize(1));
else
    fprintf(fid,'cellsize      %f %f\n',Structure.CellSize(1:2));
end
Data=Structure.Data;

if ~isnan(Structure.NoData)
    fprintf(fid,'nodata_value  %f\n',Structure.NoData);
    Data(isnan(Data))=Structure.NoData;
end

FormatString=[repmat(' %f',[1 Structure.NCols]) '\n'];
fprintf(fid,FormatString,Data);
fclose(fid);


function Resampled=Local_resample_file(Structure,StepSize)
Resampled=[];
if nargin<2
    try
        StepSize=stdinputdlg('step size','Question',1,{num2str(1)});
    catch
        StepSize=inputdlg('step size','Question',1,{num2str(1)});
    end
    if isempty(StepSize)
        return
    end
    StepSize=str2num(StepSize{1});
end
if ~isequal(size(StepSize),[1 1]) || ~isfinite(StepSize) || ...
        StepSize~=round(StepSize) || StepSize<0 || ...
        StepSize>min(Structure.NRows,Structure.NCols)
    return
end

Resampled=Structure;

ColsSticks=1:StepSize:Structure.NCols+1;
RowsSticks=1:StepSize:Structure.NRows+1;
if ColsSticks(end)~=Structure.NCols+1
    ColsSticks = cat(2,ColsSticks,Structure.NCols+1);
end
if RowsSticks(end)~=Structure.NRows+1
    RowsSticks = cat(2,RowsSticks,Structure.NRows+1);
end

if isfield(Structure,'Data')
    Data = Structure.Data;
else
    Data = Local_read_file(Structure);
    Data = Data{1};
end
if StepSize~=1
    % take average data ...
    nRS = length(RowsSticks);
    nCS = length(ColsSticks);
    %
    plotData = repmat(NaN,nCS-1,nRS-1);
    for c=1:nCS-1
        for r=1:nRS-1
            subData = Data(ColsSticks(c):ColsSticks(c+1)-1,RowsSticks(r):RowsSticks(r+1)-1);
            Mask = ~isnan(subData);
            NPnt = sum(Mask(:));
            if NPnt>0
                plotData(c,r)=sum(subData(Mask))/NPnt;
            end
        end
    end
else
    plotData = Data;
end
Resampled.Data = plotData;
Resampled.NRows = size(plotData,2);
Resampled.NCols = size(plotData,1);
Resampled.CellSize = StepSize * Resampled.CellSize;


function H=Local_plot_file(Structure,Axes,StepSize)
H=[];
if nargin<3
    try
        StepSize=stdinputdlg('step size','Question',1,{num2str(1)});
    catch
        StepSize=inputdlg('step size','Question',1,{num2str(1)});
    end
    if isempty(StepSize)
        return
    end
    StepSize=str2num(StepSize{1});
end
if ~isequal(size(StepSize),[1 1]) || ~isfinite(StepSize) || ...
        StepSize~=round(StepSize) || StepSize<0 || ...
        StepSize>min(Structure.NRows,Structure.NCols)
    return
end

if nargin<2
    Axes=gca;
    view(0,90);
end

ColsSticks=1:StepSize:Structure.NCols+1;
RowsSticks=1:StepSize:Structure.NRows+1;
if ColsSticks(end)~=Structure.NCols+1
    ColsSticks = cat(2,ColsSticks,Structure.NCols+1);
end
if RowsSticks(end)~=Structure.NRows+1
    RowsSticks = cat(2,RowsSticks,Structure.NRows+1);
end

x=Structure.XCorner+(ColsSticks-1)*Structure.CellSize(1);
y=Structure.YCorner+(Structure.NRows+1)*Structure.CellSize(2)-RowsSticks*Structure.CellSize(2);

if isfield(Structure,'Data')
    Data = Structure.Data;
else
    Data = Local_read_file(Structure);
    Data = Data{1};
end
if StepSize~=1
    % take average data and ...
    % transpose data for plotting
    nRS = length(RowsSticks);
    nCS = length(ColsSticks);
    %
    plotData = repmat(NaN,nRS-1,nCS-1);
    for c=1:nCS-1
        for r=1:nRS-1
            subData = Data(ColsSticks(c):ColsSticks(c+1)-1,RowsSticks(r):RowsSticks(r+1)-1);
            Mask = ~isnan(subData);
            NPnt = sum(Mask(:));
            if NPnt>0
                plotData(r,c)=sum(subData(Mask))/NPnt;
            end
        end
    end
    % z data
    zData = repmat(NaN,nRS,nCS);
    for c=1:nCS
        for r=1:nRS
            %
            % Determine z level at corner points based on original data
            %
            c1 = max(ColsSticks(c)-1,1);
            c2 = min(ColsSticks(c),Structure.NCols);
            r1 = max(RowsSticks(r)-1,1);
            r2 = min(RowsSticks(r),Structure.NRows);
            subData = Data(c1:c2,r1:r2);
            Mask = ~isnan(subData);
            NPnt = sum(Mask(:));
            if NPnt>0
                zData(r,c)=sum(subData(Mask))/NPnt;
            else
                %
                % However, if the corner point lies far from the actual data,
                % the point may be missing data. In that case use the data from
                % the coarse plotData set.
                %
                c1 = max(c-1,1);
                c2 = min(c,nCS-1);
                r1 = max(r-1,1);
                r2 = min(r,nRS-1);
                subData = plotData(r1:r2,c1:c2);
                Mask = ~isnan(subData);
                NPnt = sum(Mask(:));
                if NPnt>0
                    zData(r,c)=sum(subData(Mask))/NPnt;
                end
            end
        end
    end
else
    % transpose data for plotting
    plotData = transpose(Data);
    Mask = isnan(plotData);
    masked_pD = plotData;
    masked_pD(Mask) = 0;
    %
    zData = masked_pD([1:end end],[1:end end]) + ...
        masked_pD([1 1:end],[1:end end]) + ...
        masked_pD([1:end end],[1 1:end]) + ...
        masked_pD([1 1:end],[1 1:end]);
    zMask = Mask([1:end end],[1:end end]) + ...
        Mask([1 1:end],[1:end end]) + ...
        Mask([1:end end],[1 1:end]) + ...
        Mask([1 1:end],[1 1:end]);
    zMask = 4 - zMask;
    zData = zData./max(zMask,1);
    zData(zMask==0) = NaN;
end
H=surface(x,y,zData,plotData,'parent',Axes,'edgecolor','none');
set(Axes,'dataaspectratio',[1 1 1]);


function [Data,NextValue]=Local_read_freeformat(fid,Size)
Data=repmat(NaN,Size);
NextValue=1;
while ~feof(fid)
    Line=fgetl(fid);
    Line=strrep(Line,',',' ');
    Star=strfind(Line,'*');
    NTimes=1;
    for s=1:length(Star)
        if s==1
            X=sscanf(Line(1:(Star(1)-1)),'%f');
        else
            X=sscanf(Line((Star(s-1)+1):(Star(s)-1)),'%f');
        end
        if length(X)==1
            NTimes=X(1);
        else
            Data(NextValue-1+(1:NTimes))=X(1);
            if length(X)>2
                Data(NextValue-1+NTimes+(1:(length(X)-2)))=X(2:(end-1));
            end
            NextValue=NextValue+max(0,length(X)-2)+NTimes;
            NTimes=X(end);
        end
    end
    if isempty(Star)
        X=sscanf(Line,'%f');
    else
        X=sscanf(Line((Star(end)+1):end),'%f');
    end
    Data(NextValue-1+(1:NTimes))=X(1);
    if length(X)>1
        Data(NextValue-1+NTimes+(1:(length(X)-1)))=X(2:end);
    end
    NextValue=NextValue+max(0,length(X)-1)+NTimes;
end
NextValue=NextValue-1;
