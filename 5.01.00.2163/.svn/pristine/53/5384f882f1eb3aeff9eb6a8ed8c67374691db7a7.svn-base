function fileinfo = nc_info ( ncfile )
%NC_INFO  Return information about a NetCDF file.
%   fileinfo = nc_info(ncfile) returns metadata about the netCDF file
%   ncfile.  fileinfo is a structure with four fields.
%
%       Filename  - a string containing the name of the file.
%       Format    - a string describing the file format.
%       Dimension - a struct array describing the netCDF dimensions.
%       Dataset   - a struct array describing the netCDF variables.
%       Attribute - a struct array describing the global attributes.
%
%   Each Dimension element is itself a struct containing the following
%   fields.
%       
%        Name      - A string containing the name of the dimension.
%        Length    - A scalar value, the size of this dimension
%        Unlimited - Set to 1 if the dimension is the record dimension, set 
%                    to 0 otherwise.
%
%   Each Dataset element is itself a struct containing the following
%   fields.
%
%        Name      - A string containing the name of the variable.
%        Nctype    - A number specifying the NetCDF datatype of this
%                    variable.
%        Dimension - A cell array with the names of the dimensions upon 
%                    which this variable depends.
%        Unlimited - Either 1 if the variable has an unlimited dimension
%                    or 0 if not.
%        Size      - Array that describes the size of each dimension upon 
%                    which this dataset depends.
%        Attribute - A struct array describing the variable attributes.
%                        
%    Each Attribute element is itself a struct and contains the following 
%    fields.
%
%        Name      - A string containing the name of the attribute.
%        Value     - Either a string or a double precision value 
%                    corresponding to the value of the attribute.
%
%   Example:  Retrieve the metadata in the example file that ships with
%   R2008b.
%       info = nc_info('example.nc');
%
%   See also nc_dump.



[backend,fmt] = snc_read_backend(ncfile);

switch(backend)
	case 'tmw'
		fileinfo = nc_info_tmw(ncfile);
    case 'tmw_hdf4'
        fileinfo = nc_info_hdf4(ncfile);
	case 'java'
		fileinfo = nc_info_java(ncfile);
	case 'mexnc'
		fileinfo = nc_info_mexnc(ncfile);
	otherwise
		error('SNCTOOLS:nc_info:unhandledBackend', ...
		      '%s is not a recognized backend.', backend );
end

if ~isfield(fileinfo,'Format')
    % Only supply this if not already supplied.
    fileinfo.Format = fmt;
end

return













%--------------------------------------------------------------------------
function fileinfo = nc_info_tmw ( ncfile )

ncid=netcdf.open(ncfile, nc_nowrite_mode );

fileinfo = nc_group_info_tmw(ncid);

v = netcdf.inqLibVers;
if v(1) == '4'
    switch(netcdf.inqFormat(ncid))
        case 'FORMAT_CLASSIC'
            fileinfo.Format = 'NetCDF-3 Classic';
        case 'FORMAT_64BIT'
            fileinfo.Format = 'NetCDF-3 64bit';
        case 'FORMAT_NETCDF4_CLASSIC'
            fileinfo.Format = 'NetCDF-4 Classic';
        case 'FORMAT_NETCDF4'
            fileinfo.Format = 'NetCDF-4';
    end
end

netcdf.close(ncid);


fileinfo.Filename = ncfile;

%--------------------------------------------------------------------------
function info = nc_group_info_tmw(ncid)

[ndims, nvars, ngatts] = netcdf.inq(ncid);

v = netcdf.inqLibVers;
if v(1) == '3'
    dimids = 0:ndims-1;
    info.Name = '/';
else
    dimids = netcdf.inqDimIDs(ncid);
    info.Name = netcdf.inqGrpNameFull(ncid);
end

% Get the dimensions
if ndims == 0
	Dimension = struct ( [] );
else
	if ndims > 0
		Dimension(1)=nc_getdiminfo_tmw ( ncid, dimids(1) );
	end
	Dimension = repmat ( Dimension, ndims,1 );
	for j = 2:ndims
		Dimension(j)=nc_getdiminfo_tmw(ncid,dimids(j));
	end
end



% Get the global attributes.
if ngatts == 0
	info.Attribute = struct([]);
else
	if ngatts > 0
		Attribute(1) = nc_get_attribute_struct_tmw ( ncid, nc_global, 0 );
	end
	Attribute = repmat ( Attribute, ngatts, 1 );
	for attnum = 1:ngatts-1
		Attribute(attnum+1) = nc_get_attribute_struct_tmw ( ncid, nc_global, attnum );
	end
	info.Attribute = Attribute;
end


% Get the variable information.
if nvars == 0
	Dataset = struct([]);
else
	if ( nvars > 0 )
		Dataset(1) = nc_getvarinfo_tmw ( ncid, 0 );
	end
	Dataset = repmat ( Dataset, nvars, 1 );
	for varid=1:nvars-1
		Dataset(varid+1) = nc_getvarinfo_tmw ( ncid, varid );
	end
end

info.Dimension = Dimension;
info.Dataset = Dataset;

Group = [];
if v(1) == '4'
    % Any groups?
    fmt = netcdf.inqFormat(ncid);
    if strcmp(fmt,'FORMAT_NETCDF4')
        childGroups = netcdf.inqGrps(ncid);
        if numel(childGroups) > 0
            Group = nc_group_info_tmw(childGroups(1));
            Group = repmat(Group, numel(childGroups),1);
            for j = 2:numel(childGroups)
                Group(j) = nc_group_info_tmw(childGroups(j));
            end
        end
    end
end
info.Group = Group;


return








%--------------------------------------------------------------------------
function fileinfo = nc_info_mexnc ( ncfile )


fileinfo.Filename = ncfile;

[ncid, status]=mexnc('open', ncfile, nc_nowrite_mode );
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_INFO:MEXNC:OPEN', ncerr );
end



[ndims, nvars, ngatts, record_dimension, status] = mexnc('INQ', ncid); %#ok<ASGLU>
if status ~= 0
    ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'SNCTOOLS:NC_INFO:MEXNC:INQ', ncerr );
end


%
% Get the dimensions
if ndims == 0
	Dimension = struct ( [] );
else
	if ndims > 0
		Dimension(1)=nc_getdiminfo_mexnc ( ncid, 0 );
	end
	Dimension = repmat ( Dimension, ndims,1 );
	for dimid = 1:ndims-1
		Dimension(dimid+1)=nc_getdiminfo_mexnc ( ncid, dimid );
	end
end



%
% Get the global attributes.
if ngatts == 0
	fileinfo.Attribute = struct([]);
else
	if ngatts > 0
		Attribute(1) = nc_get_attribute_struct ( ncid, nc_global, 0 );
	end
	Attribute = repmat ( Attribute, ngatts, 1 );
	for attnum = 1:ngatts-1
		Attribute(attnum+1) = nc_get_attribute_struct ( ncid, nc_global, attnum );
	end
	fileinfo.Attribute = Attribute;
end





%
% Get the variable information.
if nvars == 0
	Dataset = struct([]);
else
	if ( nvars > 0 )
		Dataset(1) = nc_getvarinfo_mexnc ( ncid, 0 );
	end
	Dataset = repmat ( Dataset, nvars, 1 );
	for varid=1:nvars-1
		Dataset(varid+1) = nc_getvarinfo_mexnc ( ncid, varid );
	end
end

fileinfo.Dimension = Dimension;
fileinfo.Dataset = Dataset;


mexnc('close',ncid);


return









