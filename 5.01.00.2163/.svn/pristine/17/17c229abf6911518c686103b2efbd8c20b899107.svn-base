function nc_varput(ncfile,varname,data,varargin)
%NC_VARPUT:  Writes data into a netCDF file.
%
%   NC_VARPUT(NCFILE,VARNAME,DATA) writes the matlab variable DATA to
%   the variable VARNAME in the netCDF file NCFILE.  The main requirement
%   here is that DATA have the same dimensions as the netCDF variable.
% 
%   NC_VARPUT(NCFILE,VARNAME,DATA,START,COUNT) writes DATA contiguously, 
%   starting at the zero-based index START and with extents given by
%   COUNT.
%
%   NC_VARPUT(NCFILE,VARNAME,DATA,START,COUNT,STRIDE) writes DATA  
%   starting at the zero-based index START with extents given by
%   COUNT, but this time with strides given by STRIDE.  If STRIDE is not
%   given, then it is assumes that all data is contiguous.
%
%   Setting the preference 'PRESERVE_FVD' to true will compel MATLAB to 
%   display the dimensions in the opposite order from what the C utility 
%   ncdump displays.  Writing large data becomes much more efficient in
%   this case.
% 
%   Example:
%       nc_create_empty('myfile.nc');
%       nc_adddim('myfile.nc','longitude',360);
%       varstruct.Name = 'longitude';
%       varstruct.Nctype = 'double';
%       varstruct.Dimension = { 'longitude' };
%       nc_addvar('myfile.nc',varstruct);
%       nc_varput('myfile.nc','longitude',[-180:179]');
%       nc_dump('myfile.nc');
%
%   Example:  write only two values to 'longitude'.
%       data = [0 1]';
%       start = 180;
%       count = 2;
%       nc_varput('myfile.nc','longitude',data,start,count);
%
%   See also nc_varget, nc_create_empty, nc_adddim, nc_addvar.



[start, count, stride] = parse_and_validate_args(ncfile,varname,varargin{:});

%% for vectors make row/columnwise irrelevent
if isvector(data)
    vinfo = nc_getvarinfo(ncfile,varname);
    if numel(vinfo.Size) == 1
        data = reshape(data,[numel(data) 1]);
    end
end

%% write
backend = snc_write_backend(ncfile);
switch(backend)
	case 'tmw'
		nc_varput_tmw(ncfile,varname,data,start,count,stride);
	case 'tmw_hdf4'
		nc_varput_hdf4(ncfile,varname,data,start,count,stride);
	case 'mexnc'
		nc_varput_mexnc(ncfile,varname,data,start,count,stride);
end

return





%-------------------------------------------------------------------------
function [start,count,stride] = parse_and_validate_args(ncfile,varname,varargin)

% Set up default outputs.
start = [];
count = [];
stride = [];

switch length(varargin)
case 2
    start = varargin{1};
    count = varargin{2};

case 3
    start = varargin{1};
    count = varargin{2};
    stride = varargin{3};
end


% Error checking on the inputs.
if ~ischar(ncfile)
    error('SNCTOOLS:NC_VARPUT:badInput','The filename must be character.');
end
if ~ischar(varname)
    error('SNCTOOLS:NC_VARPUT:badInput', ...
        'The variable name must be character.');
end

if ~isnumeric ( start )
    error ('SNCTOOLS:NC_VARPUT:badInput', ...
        'The ''start'' argument must be numeric.');
end
if ~isnumeric ( count )
    error('SNCTOOLS:NC_VARPUT:badInput', ...
        'The ''count'' argument must be numeric.' );
end
if ~isnumeric ( stride )
    error('SNCTOOLS:NC_VARPUT:badInput', ...
        'The ''stride'' argument must be numeric.' );
end


return









