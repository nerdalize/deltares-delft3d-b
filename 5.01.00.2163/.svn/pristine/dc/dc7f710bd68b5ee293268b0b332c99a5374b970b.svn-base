function varargout = asciiwind(cmd,varargin)
%ASCIIWIND Read operations for ascii wind files.
%   FileData = ASCIIWIND('open',filename) opens the ascii wind file and
%   determines the wind time series characteristics.
%
%   See also ARCGRID.

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
switch cmd
    case 'open'
        varargout{1} = Local_open_file(varargin{:});
    case 'read'
        varargout{1} = Local_read_file(varargin{:});
    case 'merge'
        varargout{1} = merge_files(varargin{:});
    case 'grid'
        [varargout{1:3}] = Local_get_grid(varargin{:});
    otherwise
        error('Unknown command: "%s"',cmd)
end


function Data = Local_read_file(Structure,Quant,t,varargin)
if ischar(Quant)
    iQuant = ustrcmpi(Quant,Structure.Header.quantity);
    if iQuant<0
        error('Unknown quantity: %s',Quant)
    end
    Quant = iQuant;
elseif any(Quant>Structure.Header.n_quantity)
    error('Quantity index %i out of range',max(Quant))
end
%
if length(Quant)>1
    error('Requesting multiple quantities not yet supported.')
end
QuantName = [Structure.Header.quantity{Quant} '_spw_eye'];
%
if isequal(t,':')
    t = 1:length(Structure.Data);
end
%
rows = varargin{1};
cols = varargin{2};
fid = fopen(Structure.FileName,'r');
for i = 1:length(t)
    fseek(fid,Structure.Data(t(i)).offset,-1);
    fscanf(fid,'%f',[Quant-1 Structure.NVal]);
    OneTime = fscanf(fid,'%f',[Structure.NVal 1]);
    switch lower(Structure.Header.filetype)
        case 'meteo_on_equidistant_grid'
            OneTime = reshape(OneTime,[Structure.Header.n_cols Structure.Header.n_rows])';
            OneTime = flipud(OneTime);
        case 'meteo_on_curvilinear_grid'
            switch Structure.Header.data_row
                case 'grid_row' % which corresponds to column here
                    sz = size(Structure.Header.grid_file.X);
                    OneTime = reshape(OneTime,sz);
                case {'grid_col','grid_column'} % which corresponds to row here
                    sz = size(Structure.Header.grid_file.X);
                    sz = fliplr(sz);
                    OneTime = reshape(OneTime,sz)';
            end
            switch Structure.Header.first_data_value
                case 'grid_llcorner' % (1,1) coordinate
                    % default
                case 'grid_ulcorner' % (1,NMAX) coordinate
                    OneTime = fliplr(OneTime);
                case 'grid_lrcorner' % (MMAX,1) coordinate
                    OneTime = flipud(OneTime);
                case 'grid_urcorner' % (MMAX,NMAX) coordinate
                    OneTime = rot90(OneTime,2);
            end
        case 'meteo_on_spiderweb_grid'
            OneTime = reshape(OneTime,[Structure.Header.n_cols Structure.Header.n_rows])';
            OneTime = OneTime([1 1:end],[1:end 1]);
            if isfield(Structure.Data(t(i)),QuantName)
                OneTime(1,:) = Structure.Data(t(i)).(QuantName);
            else
                OneTime(1,:) = 0;
            end
        case 'meteo_on_computational_grid'
            if isfield(Structure.Header,'grid_file')
                OneTime = reshape(OneTime,size(Structure.Header.grid_file.X)+1);
            end
    end
    if ~isempty(varargin)
        OneTime = OneTime(rows,cols);
    end
    if isfield(Structure.Header,'nodata_value')
       OneTime(OneTime==Structure.Header.nodata_value)=NaN;
    end
    if i == 1
        Data = zeros([length(t) size(OneTime)]);
    end
    Data(i,:) = OneTime(:);
end
fclose(fid);


function [X,Y,grid_unit] = Local_get_grid(Structure,t,varargin)
if isequal(t,':')
    t = 1:length(Structure.Data);
elseif isempty(t)
    t = 1;
end
%
Header = Structure.Header;
rows = varargin{1};
cols = varargin{2};
grid_unit = '';
for i = 1:length(t)
    switch lower(Header.filetype)
        case 'meteo_on_equidistant_grid'
            x = Header.x_llcorner + ...
                repmat((cols-1)*Header.dx,length(rows),1);
            y = Header.y_llcorner + ...
                repmat((rows'-1)*Header.dy,1,length(cols));
            if isfield(Header,'grid_unit')
                grid_unit = Header.grid_unit;
            end
            t = 1;
        case {'meteo_on_curvilinear_grid','meteo_on_computational_grid'}
            x = Header.grid_file.X(rows,cols);
            y = Header.grid_file.Y(rows,cols);
            if isfield(Header.grid_file,'CoordinateSystem')
                switch lower(Header.grid_file.CoordinateSystem)
                    case 'spherical'
                        grid_unit = 'deg';
                    case 'cartesian'
                        grid_unit = 'm';
                end
            end
            t = 1;
        case 'meteo_on_spiderweb_grid'
            if isfield(Header,'grid_unit')
                grid_unit = Header.grid_unit;
            end
            if strcmpi(grid_unit,'degree')
                [x,y] = getSphericalSpiderweb(...
                    Structure.Data(t(i)).x_spw_eye, ...
                    Structure.Data(t(i)).y_spw_eye, ...
                    Header.spw_radius, ...
                    Header.n_rows,rows, ...
                    Header.n_cols,cols);
            else
                r = repmat(Header.spw_radius*(rows-1)'/Header.n_rows,1,length(cols));
                phi = repmat(2*pi*(cols-1)/Header.n_cols,length(rows),1);
                x = Structure.Data(t(i)).x_spw_eye + r.*cos(phi);
                y = Structure.Data(t(i)).y_spw_eye + r.*sin(phi);
            end
    end
    if i == 1
        X = zeros([length(t) size(x)]);
        Y = zeros([length(t) size(y)]);
    end
    X(i,:) = x(:);
    Y(i,:) = y(:);
end


function [lon,lat]=getSphericalSpiderweb(lon_spw_eye,lat_spw_eye,spw_radius,n_rows,rows,n_cols,cols)
lonrad = lon_spw_eye*pi/180;
latrad = lat_spw_eye*pi/180;

earth_radius = 6378137;

% create grid surrounding north pole (lon=0, lat=pi/2)
%
%x = [cos(lonrad)*cos(latrad)
%     sin(lonrad)*cos(latrad)
%     sin(latrad)]

latrad_grid = repmat(asin(cos((rows-1)'*spw_radius/n_rows/earth_radius)),1,length(cols));
lonrad_grid = repmat(pi*(1-2*(cols-1)/n_cols),length(rows),1);

x_grid_np = cos(lonrad_grid).*cos(latrad_grid);
y_grid_np = sin(lonrad_grid).*cos(latrad_grid);
z_grid_np = sin(latrad_grid);

% shift grid to lon=0, lat=0
%
%latrad = -pi/2;
%
%x = [cos(latrad) 0 -sin(latrad)
%     0           1  0
%     sin(latrad) 0  cos(latrad)]*x;

x_grid_eq =  z_grid_np;
y_grid_eq =  y_grid_np;
z_grid_eq = -x_grid_np;

% shift grid to eye location (lonrad,latrad)
%
%x = [cos(lonrad)*cos(latrad) -sin(lonrad) -sin(latrad)*cos(lonrad)
%     sin(lonrad)*cos(latrad)  cos(lonrad) -sin(latrad)*sin(lonrad)
%     sin(latrad)              0            cos(latrad)]*x;

x_grid_eye = x_grid_eq*cos(lonrad)*cos(latrad) -y_grid_eq*sin(lonrad) -z_grid_eq*sin(latrad)*cos(lonrad);
y_grid_eye = x_grid_eq*sin(lonrad)*cos(latrad) +y_grid_eq*cos(lonrad) -z_grid_eq*sin(latrad)*sin(lonrad);
z_grid_eye = x_grid_eq*sin(latrad)                                    +z_grid_eq*cos(latrad);

% convert to lon,lat
%
%x = [cos(lonrad)*cos(latrad)
%     sin(lonrad)*cos(latrad)
%     sin(latrad)]

lat = asin(z_grid_eye)*180/pi;
lon = atan2(y_grid_eye,x_grid_eye)*180/pi;


function Structure=Local_open_file(filename,vector)
if nargin<2
    vector = 0;
end
Structure.Check='NotOK';
Structure.FileType='asciiwind';

fid=fopen(filename,'r');
Structure.FileName=filename;
if fid<0
    return
end
%
itime = 1;
while ~feof(fid)
    [keyw,value] = fgetl_keyval(fid);
    %
    switch keyw
        case {'nodata_value','n_cols','n_rows','x_llcorner','y_llcorner', ...
                'x_llcenter','y_llcenter','dx','dy','n_quantity','spw_radius','fileversion'}
            value = sscanf(value,'%f');
        case 'time'
            [Structure.Data(itime).time,Structure.Data(itime).timezone] = value2time(value);
            break
        case ''
            fclose(fid);
            error('Error reading line:\n%s\nMissing comment symbol or keyword assignment.',value)
        otherwise
            %value = value;
    end
    %
    % Support version 1.0 keywords
    %
    switch keyw
        case 'curvi_grid_file'
            keyw = 'grid_file';
        case {'meteotype','filetype'}
            keyw = 'filetype';
            switch value
                case 'arcinfo'
                    value = 'meteo_on_equidistant_grid';
                case 'spiderweb'
                    value = 'meteo_on_spiderweb_grid';
                case {'svwp_on_flow_grid','meteo_on_flow_grid'}
                    value = 'meteo_on_computational_grid';
                case 'curvi'
                    value = 'meteo_on_curvilinear_grid';
            end
    end
    %
    if ~isvarname(keyw)
        fclose(fid);
        error('Invalid keyword "%s"',keyw)
    end
    Structure.Header.(keyw) = value;
end
%
% Support version 1.01 keywords
%
if Structure.Header.fileversion<1.02 && strcmp(Structure.Header.filetype,'meteo_on_spiderweb_grid')
    nr = Structure.Header.n_cols;
    Structure.Header.n_cols = Structure.Header.n_rows;
    Structure.Header.n_rows = nr;
end
%
if Structure.Header.fileversion<1.03 && strcmp(Structure.Header.filetype,'meteo_on_curvilinear_grid')
    ui_message('error',{Structure.FileName,'This curvilinear grid meteo file is interpreted incorrectly by Delft3D versions 3.28.00 until 3.28.09!'})
end
%
if strcmp(Structure.Header.filetype,'meteo_on_equidistant_grid')
    xllcenter_given = isfield(Structure.Header,'x_llcenter');
    yllcenter_given = isfield(Structure.Header,'y_llcenter');
    xllcorner_given = isfield(Structure.Header,'x_llcorner');
    yllcorner_given = isfield(Structure.Header,'y_llcorner');
    xll_given = xllcenter_given | xllcorner_given;
    yll_given = yllcenter_given | yllcorner_given;
    if ~xll_given
        fclose(fid);
        error('Missing X coordinate data in file "%s"',Structure.FileName)
    elseif ~yll_given
        fclose(fid);
        error('Missing Y coordinate data in file "%s"',Structure.FileName)
    elseif xllcorner_given && xllcenter_given
        fclose(fid);
        error('Multiple X coordinates specified in file "%s"',Structure.FileName)
    elseif yllcorner_given && yllcenter_given
        fclose(fid);
        error('Multiple Y coordinates specified in file "%s"',Structure.FileName)
    else
        % both given and unique. Now it's time to become picky ...
        if (xllcenter_given && ~yllcenter_given) || (xllcorner_given && ~yllcorner_given)
           ui_message('error',{Structure.FileName,'Mixing center and corner coordinate info not allowed for computational engines!'})
        end
        if xllcenter_given
            Structure.Header.x_llcorner = Structure.Header.x_llcenter - Structure.Header.dx/2;
        end
        if yllcenter_given
            Structure.Header.y_llcorner = Structure.Header.y_llcenter - Structure.Header.dy/2;
        end
        % now we always have x_llcorner and y_llcorner.
    end
    %
    if Structure.Header.fileversion>=1.03
        if isfield(Structure.Header,'value_pos')
            ui_message('error',{Structure.FileName,'The value_pos keyword is not allowed anymore!'})
        end
    else
        switch Structure.Header.value_pos
            case 'centre' % x_llcenter and y_llcenter given
                Structure.Header.x_llcorner = Structure.Header.x_llcorner - Structure.Header.dx/2;
                Structure.Header.y_llcorner = Structure.Header.y_llcorner - Structure.Header.dy/2;
            case 'corner' % x_llcorner and y_llcorner given
                % nothing to do
        end
    end
end
%
Q = cell(1,Structure.Header.n_quantity);
U = cell(1,Structure.Header.n_quantity);
for i = 1:Structure.Header.n_quantity
    f = sprintf('quantity%i',i);
    Q{i} = Structure.Header.(f);
    Structure.Header = rmfield(Structure.Header,f);
    %
    f = sprintf('unit%i',i);
    U{i} = Structure.Header.(f);
    Structure.Header = rmfield(Structure.Header,f);
end
Structure.Header.quantity = Q;
Structure.Header.unit = U;
%
max_ntimes = 1000;
Structure.Data(max_ntimes).time = [];
%
switch lower(Structure.Header.filetype)
    case 'meteo_on_equidistant_grid'
        nval = Structure.Header.n_quantity * ...
            Structure.Header.n_cols * ...
            Structure.Header.n_rows;
    case 'meteo_on_curvilinear_grid'
        p = fileparts(Structure.Header.grid_file);
        if isempty(p)
            p = fileparts(filename);
            Structure.Header.grid_file = fullfile(p,Structure.Header.grid_file);
        end
        Structure.Header.grid_file = wlgrid('read',Structure.Header.grid_file);
        nval = Structure.Header.n_quantity * ...
            numel(Structure.Header.grid_file.X);
        %
        % do some keyword checks
        %
    case 'first_data_value'
        if ~ismember(Structure.Header.first_data_value,{'grid_llcorner','grid_ulcorner','grid_lrcorner','grid_urcorner'})
            fclose(fid);
            error('Unknown value for "first_data_value" in file: %s',Structure.Header.first_data_value)
        end
        if ~ismember(Structure.Header.data_row,{'grid_row','grid_col','grid_column'})
            fclose(fid);
            error('Unknown value for "data_row" in file: %s',Structure.Header.data_row)
        end
    case 'meteo_on_spiderweb_grid'
        nval = Structure.Header.n_quantity * ...
            Structure.Header.n_cols * ...
            Structure.Header.n_rows;
    case 'meteo_on_computational_grid'
        nval = inf;
    otherwise
        fclose(fid);
        error('Unknown ASCII wind type: %s',Structure.Header.filetype)
end
%
while 1
    %
    % read parameters for current time
    %
    while ~isempty(keyw)
        offset = ftell(fid);
        [keyw,value] = fgetl_keyval(fid);
        switch keyw
            case {'x_spw_eye','y_spw_eye','p_drop_spw_eye'}
                Structure.Data(itime).(keyw) = sscanf(value,'%f');
            case ''
                break
            otherwise
                Structure.Data(itime).(keyw) = value;
        end
    end
    fseek(fid,offset,-1);
    %
    % skip data
    %
    Structure.Data(itime).offset = offset;
    dummy = fscanf(fid,'%f',nval);
    if ~isfinite(nval)
        nval = length(dummy);
    elseif nval > length(dummy)
        fclose(fid);
        error('Not enough data available for data block %i (%s).',itime,datestr(Structure.Data(itime).time,0))
    end
    %
    % read next time
    %
    [keyw,value] = fgetl_keyval(fid);
    if isempty(keyw)
        if ~isempty(value)
            fclose(fid);
            error('Unexpected data while reading time block %i (%s).\nExpected new time block or end-of-file, but encountered data: ''%s''.',itime,datestr(Structure.Data(itime).time,0),value)
        end
        break
    end
    %
    itime = itime+1;
    if (itime>max_ntimes)
        max_ntimes = 2*max_ntimes;
        Structure.Data(max_ntimes).time = [];
    end
    %
    [Structure.Data(itime).time,Structure.Data(itime).timezone] = value2time(value);
end
%
Structure.Data(itime+1:end) = [];
Structure.NVal = nval/Structure.Header.n_quantity;
Structure.Check = 'OK';
fclose(fid);
%
if ~vector && ...
        Structure.Header.n_quantity==1 && ...
        strcmp(Structure.Header.quantity{1}(2:end),'_wind')
    [p,f,e] = fileparts(Structure.FileName);
    switch lower(e)
        case '.amu'
            FileName2 = Structure.FileName;
            FileName2(end) = FileName2(end)+1; %u->v
        case '.amv'
            FileName2 = Structure.FileName;
            FileName2(end) = FileName2(end)-1; %v->u
    end
    vector = 1;
    try
        S2 = Local_open_file(FileName2,vector);
        Structure = merge_files(Structure,S2);
    end
end


function S = merge_files(S,S2)
if isequal(S2.Check,'OK')
    if isequal(S2.Header.quantity{1},S.Header.quantity{1})
        ui_message('','The two wind files should not both contain: %s.',S.Header.quantity{1})
    elseif ~isequal(S2.Header.filetype,S.Header.filetype)
        ui_message('','Grid types of wind files for x and y don''t match.')
    elseif strcmp(S2.Header.filetype,'meteo_on_equidistant_grid')
        if S2.Header.n_cols~=S.Header.n_cols || ...
                S2.Header.n_rows~=S.Header.n_rows
            ui_message('','Grid dimensions in wind files for x and y don''t match.')
        elseif S2.Header.x_llcorner~=S.Header.x_llcorner || ...
                S2.Header.y_llcorner~=S.Header.y_llcorner || ...
                S2.Header.dx~=S.Header.dx || ...
                S2.Header.dy~=S.Header.dy
            ui_message('','Grid locations in wind files for x and y don''t match.')
        elseif  ~isequal(S2.Header.value_pos,S.Header.value_pos)
            ui_message('','Data locations in wind files for x and y don''t match.')
        elseif  ~isequal(S2.Data,S.Data)
            ui_message('','Times in wind files for x and y don''t match.')
        else
            S.Vector = S2;
        end
    elseif strcmp(S2.Header.filetype,'meteo_on_curvilinear_grid')
        if ~isequal(S2.Header.grid_file,S.Header.grid_file)
            ui_message('','Grids of wind files for x and y don''t match.')
        else
            T1 = rmfield(S.Data,'offset');
            T2 = rmfield(S2.Data,'offset');
            if ~isequal(T2,T1)
                ui_message('','Times in wind files for x and y don''t match.')
            else
                % the two grids were proven to be equal; however, since the
                % two grids were read independently, the data is stored
                % twice in memory. MATLAB's memory management should
                % keep only one copy in memory if we do the following:
                S2.Header.grid_file = S.Header.grid_file;
                %
                S.Vector = S2;
            end
        end
    else
        S.Vector = S2;
    end
end


function Line = fgetl_noncomment(fid)
% Skip comment lines starting with #
Line = '';
while isempty(Line)
    Line = fgetl(fid);
    if ~ischar(Line) % end-of-file
        Line = '';
        break
    end
    % Remove comments
    Line = deblank(sscanf(Line,'%[^#]'));
end


function [keyw,value] = fgetl_keyval(fid)
floc = ftell(fid);
Line = fgetl_noncomment(fid);
eq = strfind(Line,'=');
if isempty(eq)
    fseek(fid,floc,-1);
    keyw = '';
    value = Line;
else
    keyw = lower(deblank(Line(1:eq-1)));
    value = deblank2(Line(eq+1:end));
end


function [Time,TimeZone] = value2time(value)
X = sscanf(value,'%f %*s since %4d-%2d-%2d %2d:%2d:%2d %c%2d:%2d');
[Time,Remainder] = strtok(value);
tunit = strtok(Remainder);
RefDate = datenum(X(2:7)');
TimeZone = (X(9)+X(10)/60)*(44-abs(X(8)));
switch lower(tunit)
    case {'minutes','min','m'}
        Time = RefDate + X(1)/24/60;
    case {'hours','hrs','h'}
        Time = RefDate + X(1)/24;
    otherwise
        warning('Unknown time unit.')
        Time = 0;
end
