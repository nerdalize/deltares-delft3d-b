function nc = nc_interpret(nc)
%NC_INTERPRET  Interpret the netCDF data based on conventions.
%    NC_OUT = NC_INTERPRET(NC_IN)
%
% where NC_IN  = nc_info(filename)
%
%    See also netcdf, MEXNC, NC_DUMP, NC_INFO

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

if isfield(nc,'Group') && ~isempty(nc.Group)
    for g = 1:length(nc.Group)
        gname = nc.Group(g).Name(2:end);
        for d = 1:length(nc.Group(g).Dimension)
            nc.Group(g).Dimension(d).Name = ...
                [gname '/' nc.Group(g).Dimension(d).Name];
        end
        for d = 1:length(nc.Group(g).Dataset)
            nc.Group(g).Dataset(d).Name = ...
                [gname '/' nc.Group(g).Dataset(d).Name];
            for i = 1:length(nc.Group(g).Dataset(d).Dimension)
                nc.Group(g).Dataset(d).Dimension{i} = ...
                    [gname '/' nc.Group(g).Dataset(d).Dimension{i}];
            end
        end
    end
    nc.Dimension = cat(1,nc.Dimension,nc.Group.Dimension);
    nc.Dataset = cat(1,nc.Dataset,nc.Group.Dataset);
end

ndims = length(nc.Dimension);
if isfield(nc,'DataSet')
    DataSet = nc.DataSet;
    nc = rmfield(nc,'DataSet');
    nc.Dataset = DataSet;
end
nvars = length(nc.Dataset);
%ngatts = length(nc.Attribute);

[nc.Dimension(1:ndims).Type] = deal('unknown');
[nc.Dataset(1:nvars).Info] = deal([]);
[nc.Dataset(1:nvars).StdName] = deal('');
[nc.Dataset(1:nvars).Type] = deal('unknown');
[nc.Dataset(1:nvars).Coordinates] = deal({});
[nc.Dataset(1:nvars).X] = deal([]);
[nc.Dataset(1:nvars).XBounds] = deal([]);
[nc.Dataset(1:nvars).Y] = deal([]);
[nc.Dataset(1:nvars).YBounds] = deal([]);
[nc.Dataset(1:nvars).Z] = deal([]);
[nc.Dataset(1:nvars).Time] = deal([]);
[nc.Dataset(1:nvars).Station] = deal([]);
[nc.Dataset(1:nvars).SubField] = deal([]);
[nc.Dataset(1:nvars).TSMNK] = deal([NaN NaN NaN NaN NaN]);
[nc.Dataset(1:nvars).SubFieldDim] = deal([]);
%[nc.Dataset(1:nvars).Vector] = deal([]);

%
% Determine auxiliary coordinates: loop over all variables and collect the
% variables listed by the coordinates attributes. At the same time, mark
% all coordinate (dimension) variables.
%
DimensionNames = {nc.Dimension.Name};
AuxCoordVars = {};
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    if isfield(Info.Attribute,'Name')
        Attribs = {Info.Attribute.Name};
    else
        Attribs = {};
    end
    %
    if ~isfield(Info,'Dimid') || isempty(Info.Dimid)
        nc.Dataset(ivar).Varid = ivar-1;
        nc.Dataset(ivar).Dimid = zeros(size(Info.Dimension));
        nc.Dataset(ivar).Rank = length(Info.Dimension);
        for idim = 1:length(Info.Dimension)
            nc.Dataset(ivar).Dimid(idim) = strmatch(Info.Dimension{idim},DimensionNames,'exact')-1;
        end
        Info = nc.Dataset(ivar);
    end
    %
    j = strmatch('standard_name',Attribs,'exact');
    if isempty(j)
        j = strmatch('NAVO_code',Attribs,'exact');
        if ~isempty(j)
            Info.Attribute = navo2stdname(Info.Attribute,Info.Attribute(j).Value);
            Attribs = {Info.Attribute.Name};
        end
        %
        j = strmatch('standard_name',Attribs,'exact');
        if ~isempty(j)
            Info.StdName = Info.Attribute(j).Value;
        end
    else
        Info.StdName = Info.Attribute(j).Value;
    end
    %
    if Info.Nctype==2
        %
        % for character variables the second dimension is the string length
        %
        if Info.Rank==2
            if strcmp(Info.Dimension{1},Info.Name);
                Info.Type = 'coordinate';
            end
        end
    elseif Info.Rank==1
        if strcmp(Info.Dimension{1},Info.Name);
            Info.Type = 'coordinate';
        end
    end
    %
    coords = strmatch('coordinates',Attribs,'exact');
    if ~isempty(coords)
        %
        % read coordinate names
        %
        coordstr = Info.Attribute(coords).Value;
        %
        i=0;
        coords={};
        while ~isempty(coordstr)
            [tok,coordstr]=strtok(coordstr);
            if ~isempty(tok)
                i=i+1;
                coords{i}=tok;
            end
            coordstr=deblank(coordstr);
        end
        Info.Coordinates = coords;
        AuxCoordVars=union(AuxCoordVars,coords);
    end
    %
    nc.Dataset(ivar) = Info;
end

iCoordDim = strmatch('coordinate',{nc.Dataset.Type});
CoordDims = {nc.Dataset(iCoordDim).Name};
CoordVarDims = {};

%
% Mark auxiliary coordinate variables.
% Collect coordinates per variable.
% Collect coordinate dimensions used by auxiliary variables.
%
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    if strcmp(Info.Type,'unknown') & ismember(Info.Name,AuxCoordVars)
        nc.Dataset(ivar).Type = 'auxiliary coordinate';
        CoordVarDims = union(CoordVarDims,Info.Dimension);
    end
    %
    nc.Dataset(ivar).Coordinates = ...
        union(Info.Coordinates,intersect(Info.Dimension,CoordDims));
end


%
% Try to detect type of (auxiliary) coordinate variables.
%
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    %if strcmp(Info.Type,'unknown')
    %    continue
    %else
    if strcmp(Info.Type,'coordinate')
        idim = Info.Dimid+1;
    else % auxiliary coordinate
        idim = [];
    end
    %
    if isfield(Info.Attribute,'Name')
        Attribs = {Info.Attribute.Name};
    else
        Attribs = {};
    end
    %
    % character variables are labels
    %
    if Info.Nctype == 2
        nc = setType(nc,ivar,idim,'label');
        continue
    end
    %
    % look for known units
    %
    j = strmatch('units',Attribs,'exact');
    if ~isempty(j)
        unit = Info.Attribute(j).Value;
        [unit1,unit2] = strtok(lower(unit));
        if ~ischar(unit1)
            unit1='';
        end
        switch unit1
            case {'degrees_north','degree_north','degrees_N','degree_N','degreesN','degreeN'}
                nc = setType(nc,ivar,idim,'latitude');
                continue
            case {'degrees_east','degree_east','degrees_E','degree_E','degreesE','degreeE'}
                nc = setType(nc,ivar,idim,'longitude');
                continue
            case {'level','layer','sigma_level'}
                %
                % deprecated z-coordinate "units"
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
            case {'millisecond','milliseconds','millisec','millisecs','ms'}
                dt = 0.001;
            case {'second','seconds','sec','secs','s'}
                dt = 1;
            case {'minute','minutes','min','mins'}
                dt = 60;
            case {'hour','hours','hr','hrs','h'}
                dt = 3600;
            case {'day','days','d'}
                dt = 86400; % 24*3600
            case {'month','months'}
                dt = 365.242198781*24*3600/12;
            case {'year','years','yr','yrs'}
                dt = 365.242198781*24*3600;
            case {'common_year','common_years'}
                dt = 365          *24*3600;
            case {'leap_year','leap_years'}
                dt = 366          *24*3600;
            case {'Julian_year','Julian_years'}
                dt = 365.25       *24*3600;
            case {'Gregorian_year','Gregorian_years'}
                dt = 365.2425     *24*3600;
            otherwise
                f = qp_unitconversion(unit1,'Pa');
                if ~ischar(f)
                    %
                    % dimension unit is compatible with pressure (Pa)
                    % according CF 1.4 this must be a z-coordinate
                    %
                    nc = setType(nc,ivar,idim,'z-coordinate');
                    continue
                end
                dt = [];
        end
        if ~isempty(dt)
            %
            % time dimension
            %
            nc = setType(nc,ivar,idim,'time');
            refdate = sscanf(unit2,' since %d-%d-%d %d:%d:%f %d:%d',[1 8]);
            if length(refdate)>=6
                refdate = datenum(refdate(1:6));
                % optional time zone shift not yet taken into account
            elseif length(refdate)==3
                refdate = datenum(refdate);
            else
                refdate = [];
            end
            nc.Dataset(ivar).Info.DT      = dt/86400;
            if ~isempty(unit2) & isempty(refdate)
                nc.Dataset(ivar).Info.RefDate = unit;
            else
                nc.Dataset(ivar).Info.RefDate = refdate;
            end
            continue
        end
    end
    %
    % unit not recognized, check standard_name
    %
    j = strmatch('standard_name',Attribs,'exact');
    if ~isempty(j)
        switch Info.Attribute(j).Value
            case {'atmosphere_sigma_coordinate', ...
                        'atmosphere_hybrid_sigma_pressure_coordinate', ...
                        'atmosphere_hybrid_height_coordinate', ...
                        'atmosphere_sleve_coordinate', ...
                        'ocean_sigma_coordinate', ...
                        'ocean_s_coordinate', ...
                        'ocean_sigma_z_coordinate', ...
                        'ocean_double_sigma_coordinate'}
                %
                % Vertical dimension
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
            case 'latitude'
                nc = setType(nc,ivar,idim,'latitude');
                continue
            case 'longitude'
                nc = setType(nc,ivar,idim,'longitude');
                continue
            case 'projection_x_coordinate'
                nc = setType(nc,ivar,idim,'x-coordinate');
                continue
            case 'projection_y_coordinate'
                nc = setType(nc,ivar,idim,'y-coordinate');
                continue
            case 'time'
                nc = setType(nc,ivar,idim,'time');
                continue
        end
    end
    %
    % neither unit nor standard_name, try another approach
    %
    j = strmatch('axis',Attribs,'exact');
    if ~isempty(j)
        switch lower(Info.Attribute(j).Value)
            case 'x'
                %
                % x coordinate (may or may not be longitude)
                %
                nc = setType(nc,ivar,idim,'x-coordinate');
                continue
            case 'y'
                %
                % y coordinate (may or may not be latitude)
                %
                nc = setType(nc,ivar,idim,'y-coordinate');
                continue
            case 'z'
                %
                % Vertical dimension
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
        end
    end
    %
    j = strmatch('positive',Attribs,'exact');
    if ~isempty(j)
        switch lower(Info.Attribute(j).Value)
            case {'up','down'}
                %
                % Vertical dimension
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
            otherwise
                % produce error?
        end
    end
    %
    j = strmatch('formula_terms',Attribs,'exact');
    if ~isempty(j)
        %
        % Vertical dimension
        %
        nc = setType(nc,ivar,idim,'z-coordinate');
        continue
    end
end

%
% Try to detect x/y/z/time coordinates.
%
varNames = {nc.Dataset.Name};
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    [coords,ia,ib]=intersect(Info.Coordinates,varNames);
    for icvar = ib
        vDims = Info.Dimension;
        cvDims = nc.Dataset(icvar).Dimension;
        if nc.Dataset(icvar).Nctype==2
            % in case of a character array, leave out the last dimension
            % (sring length)
            nmDims = setdiff(cvDims(1:end-1),vDims);
        else
            nmDims = setdiff(cvDims,vDims);
        end
        if ~isempty(nmDims)
            vDimsStr = sprintf('%s, ',vDims{:});
            cvDimsStr = sprintf('%s, ',cvDims{:});
            Msg = sprintf(['Dimensions of variable and auxiliary coordinate do not match.\n', ...
                    'Variable %s: %s\nCoordinate %s: %s\nSkipping auxiliary coordinate.'], ...
                Info.Name, vDimsStr(1:end-2), ...
                nc.Dataset(icvar).Name, cvDimsStr(1:end-2));
            ui_message('error',Msg)
            continue
        end
        switch nc.Dataset(icvar).Type
            case {'longitude','x-coordinate'}
                Info.X = [Info.X icvar];
            case {'latitude','y-coordinate'}
                Info.Y = [Info.Y icvar];
            case 'z-coordinate'
                Info.Z = [Info.Z icvar];
            case 'time'
                Info.Time = [Info.Time icvar];
            case 'label'
                Info.Station = [Info.Station icvar];
            otherwise
                Info.SubField = [Info.SubField icvar];
        end
    end
    %
    % For every dimension I may have multiple (auxiliary) coordinates. How
    % to deal with that?
    %
    if ~isempty(Info.Time)
        %
        % Assumption: time is always unique and coordinate dimension.
        %
        if length(Info.Time)>1 | length(nc.Dataset(Info.Time).Dimid)>1
            ui_message('error','Unsupported case encountered: multiple time coordinates encountered.')
            Info.Time = Info.Time(1);
        end
        Info.TSMNK(1) = nc.Dataset(Info.Time).Dimid;
    end
    if ~isempty(Info.Z)
        %
        % Assumption: vertical coordinate is always unique and coordinate dimension.
        %
        if length(Info.Z)>1
            ui_message('error','Unsupported case encountered: multiple vertical coordinates encountered.')
            Info.Z = Info.Z(1);
        end
        ZDims = nc.Dataset(Info.Z).Dimid;
        if length(ZDims)>1
            % Z coordinate is multidimensional variable; try to determine
            % z dimension by excluding any assigned dimension ...
            ZDims = setdiff(ZDims,Info.TSMNK );
            % ... and dimensions associated to X and Y coordinates
            % (assuming that those ain't have a vertical dimension).
            for ix = 1:length(Info.X)
                ZDims = setdiff(ZDims,nc.Dataset(Info.X(ix)).Dimid);
            end
            for iy = 1:length(Info.Y)
                ZDims = setdiff(ZDims,nc.Dataset(Info.Y(iy)).Dimid);
            end
        end
        if length(ZDims)==1
           Info.TSMNK(5) = ZDims;
        else
            ui_message('error','Unable to uniquely identify z coordinate.')
        end
    end
    if ~isempty(Info.Station)
        %
        % Assumption: station is always unique and coordinate dimension.
        %
        statdim = intersect(Info.Dimid,nc.Dataset(Info.Station).Dimid);
        if length(Info.Station)>1 | length(statdim)>2
            ui_message('error','Problem detecting station coordinate. Contact support.')
            Info.Station = Info.Station(1);
        end
        Info.TSMNK(2) = statdim;
    end
    if ~isempty(Info.X)
        iDim = {nc.Dataset(Info.X).Dimid};
        iDim = unique(cat(2,iDim{cellfun('length',iDim)==1}));
        if length(Info.X)>1
            xType = {nc.Dataset(Info.X).Type};
            xCoord = {nc.Dataset(Info.X).Name};
            iLon = ismember(xType,'longitude');
            iAux = ismember(xCoord,AuxCoordVars);
            if any(iAux & iLon)
                Info.X = Info.X(iAux & iLon);
            elseif any(iLon)
                Info.X = Info.X(iLon);
            elseif any(iAux)
                Info.X = Info.X(iAux);
            end
        end
        %
        % Assumption: x coordinate is now unique.
        %
        if length(Info.X)>1
            ui_message('error','Problem detecting x/longitude coordinate. Contact support.')
            Info.X = Info.X(1);
        end
        iDims = setdiff(nc.Dataset(Info.X).Dimid,Info.TSMNK);
        iDim = intersect(iDim,iDims);
        if ~isempty(iDim)
            Info.TSMNK(3) = iDim(1);
        elseif ~isempty(iDims)
            Info.TSMNK(3) = iDims(1);
        end
        %
        % If X coordinates have been defined, check whether bounds have been given.
        %
        coordAttribs = {nc.Dataset(Info.X).Attribute.Name}';
        j = strmatch('bounds',coordAttribs,'exact');
        if ~isempty(j)
            Info.XBounds = strmatch(nc.Dataset(Info.X).Attribute(j).Value,varNames);
            nc.Dataset(Info.XBounds).Type = nc.Dataset(Info.X).Type;
        end
    end
    if ~isempty(Info.Y)
        iDim = {nc.Dataset(Info.Y).Dimid};
        iDim = unique(cat(2,iDim{cellfun('length',iDim)==1}));
        if length(Info.Y)>1
            yType = {nc.Dataset(Info.Y).Type};
            yCoord = {nc.Dataset(Info.Y).Name};
            iLat = ismember(yType,'latitude');
            iAux = ismember(yCoord,AuxCoordVars);
            if any(iAux & iLat)
                Info.Y = Info.Y(iAux & iLat);
            elseif any(iLat)
                Info.Y = Info.Y(iLat);
            elseif any(iAux)
                Info.Y = Info.Y(iAux);
            end
        end
        %
        % Assumption: y coordinate is now unique.
        %
        if length(Info.Y)>1
            ui_message('error','Problem detecting y/latitude coordinate. Contact support.')
            Info.Y = Info.Y(1);
        end
        iDims = setdiff(nc.Dataset(Info.Y).Dimid,Info.TSMNK);
        iDim = intersect(iDim,iDims);
        if ~isempty(iDim)
            Info.TSMNK(4) = iDim(1);
        elseif ~isempty(iDims)
            Info.TSMNK(4) = iDims(1);
        end
        %
        % If Y coordinates have been defined, check whether bounds have been given.
        %
        coordAttribs = {nc.Dataset(Info.Y).Attribute.Name}';
        j = strmatch('bounds',coordAttribs,'exact');
        if ~isempty(j)
            Info.YBounds = strmatch(nc.Dataset(Info.Y).Attribute(j).Value,varNames);
            nc.Dataset(Info.YBounds).Type = nc.Dataset(Info.Y).Type;
        end
    end
    %
    % SubField variables must be one-dimensional.
    % Their dimension should not match any of time/coordinate dimensions.
    %
    Info.SubFieldDim = setdiff(Info.Dimid,Info.TSMNK);
    %
    % try to reassign subfield dimensions to M, N, K
    %
    if all(isnan(Info.TSMNK(2:end)))
        for i=1:min(3,length(Info.SubFieldDim))
            Info.TSMNK(2+i) = Info.SubFieldDim(i);
        end
        Info.SubFieldDim = Info.SubFieldDim(4:end);
    end
    nc.Dataset(ivar) = Info;
end

function nc = setType(nc,ivar,idim,value)
nc.Dataset(ivar).Type = value;
if ~isempty(idim)
    nc.Dimension(idim).Type = value;
end

function attrib = navo2stdname(attrib,navo_code)
persistent NAVO_codes NAVO_index
if isempty(NAVO_codes)
    NAVO_codes=navotable;
    NAVO_index=cat(1,NAVO_codes{:,1});
end
iTable = find(NAVO_index==navo_code);
if ~isempty(iTable)
    stdname = NAVO_codes{iTable,4};
    longname = NAVO_codes{iTable,3};
else
    stdname = '';
    longname = '';
end
if ~isempty(stdname)
    attrib(end+1).Name = 'standard_name';
    attrib(end).Nctype = 2;
    attrib(end).Attnum = length(attrib)-1;
    attrib(end).Value = stdname;
end
if ~isempty(longname)
    Names = {attrib.Name};
    if isempty(strmatch('long_name',Names,'exact'))
        attrib(end+1).Name = 'long_name';
        attrib(end).Nctype = 2;
        attrib(end).Attnum = length(attrib)-1;
        attrib(end).Value = longname;
    end
end

function NAVO_codes=navotable
NAVO_codes={
    1,'lat','Latitude','latitude'
    2,'lon','Longitude','longitude'
    3,'gridx','Grid X',''
    4,'gridy','Grid Y',''
    5,'depth','Depth','depth'
    6,'water_pressure','Water Pressure','sea_water_pressure'
    7,'air_pressure','Air Pressure','air_pressure'
    8,'sigma','Sigma','ocean_sigma_coordinate'
    9,'layer','Layer',''
    10,'grid_point','Grid Point',''
    11,'land_point','Land Point',''
    12,'water_point','Water Point',''
    13,'time','Valid Time','time'
    14,'water_surf_temp','Water Surface Temperature','sea_surface_temperature'
    15,'water_temp','Water Temperature','sea_water_temperature'
    16,'salinity','Salinity','sea_water_salinity'
    17,'water_u','Eastward Water Velocity','eastward_sea_water_velocity'
    18,'water_v','Northward Water Velocity','northward_sea_water_velocity'
    19,'water_w','Vertical Water Velocity','upward_sea_water_velocity'
    20,'water_grid_u','Grid U Water Velocity',''
    21,'water_grid_v','Grid V Water Velocity',''
    22,'water_grid_w','Grid W Water Velocity',''
    23,'water_pres','Pressure','sea_water_pressure'
    24,'air_u','Eastward Air Velocity','eastward_wind'
    25,'air_v','Northward Air Velocity','northward_wind'
    26,'air_w','Vertical Air Velocity','upward_air_velocity'
    27,'air_gridu','Grid U Air Velocity',''
    28,'air_gridv','Grid V Air Velocity',''
    29,'air_gridw','Grid W Air Velocity',''
    30,'layer_thickness','Layer Thickness',''
    31,'mix_lay','Mixed Layer Thickness','ocean_mixed_layer_thickness'
    32,'surf_el','Water Surface Elevation','sea_surface_height_above_geoid'
    33,'sound_speed','Sound Speed','speed_of_sound_in_sea_water'
    34,'shal_sndch_dep','Shallow Sound Channel Axis Depth',''
    35,'deep_sndch_dep','Deep Sound Channel Axis Depth',''
    36,'snd_crit_dep','Acoustic Critical Depth',''
    37,'snd_exc_dep','Acoustic Depth Excess',''
    38,'sonic_lay_dep','Sonic Layer Depth',''
    39,'mixed_lay_dep','Mixed Layer Depth',''
    40,'bt_rms_err','Bathythermograph RMS Error',''
    41,'sig_wav_ht','Significant Wave Height','sea_surface_wave_significant_height'
    42,'sig_wav_dir','Significant Wave Direction','sea_surface_wave_to_direction'
    43,'sig_wav_per','Significant Wave Period','sea_surface_wind_wave_period'
    44,'mean_wav_ht','Mean Wave Height',''
    45,'mean_wav_dir','Mean Wave Direction',''
    46,'mean_wav_per','Mean Wave Period',''
    47,'swell_ht','Swell Height','sea_surface_swell_wave_significant_height'
    48,'swell_dir','Swell Direction','sea_surface_swell_wave_to_direction'
    49,'swell_per','Swell Period','sea_surface_swell_wave_period'
    %50,'cutoff_frequency','Cutoff Frequency',''
    50,'grid_water_dep','Grid Water Depth',''
    51,'land_mask','Land Mask',''
    52,'grid_lon','Grid Longitude','longitude'
    53,'grid_lat','Grid Latitude','latitude'
    54,'grid_orient','Grid Orientation',''
    55,'point_dep','Depth At Point',''
    56,'tau','Tau',''
    57,'surf_wnd_stress','Surface Wind Stress Magnitude','magnitude_of_surface_downward_stress'
    58,'surf_wnd_stress_e','Eastward Surface Wind Stress','surface_downward_eastward_stress'
    59,'surf_wnd_stress_n','Northward Surface Wind Stress','surface_downward_northward_stress'
    60,'surf_wnd_stress_gridx','Grid X Surface Wind Stress',''
    61,'surf_wnd_stress_gridy','Grid Y Surface Wind Stress',''
    62,'surf_atm_pres','Surface Atmospheric Pressure','air_pressure_at_sea_level'
    200,'water_err','Error Water Velocity',''
    201,'adcp_echo','ADCP Returned Echo Intensity',''
    202,'adcp_pcnt_good','ADCP Average Percent Good',''
    203,'botdep','Bottom Depth','sea_floor_depth_below_geoid'
    204,'water_temp_stdev','Water Temperature St. Dev',''
    205,'salinity_stdev','Salinity St. Dev.',''
    206,'lati','Time Dependent Latitude','latitude'
    207,'loni','Time Dependent Longitude','longitude'
    208,'ship_speed','Ship Speed',''};
