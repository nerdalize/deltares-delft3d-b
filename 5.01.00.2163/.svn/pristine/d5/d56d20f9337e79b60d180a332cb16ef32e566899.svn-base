function data = nc_varget(ncfile, varname, varargin )
%NC_VARGET  Retrieve data from netCDF variable or HDF4 data set.
%   DATA = NC_VARGET(NCFILE,VARNAME) retrieves all the data from the 
%   variable VARNAME in the netCDF file NCFILE.  
%
%   DATA = NC_VARGET(NCFILE,VARNAME,START,COUNT) retrieves the contiguous
%   portion of the variable specified by the index vectors START and 
%   COUNT.  Remember that SNCTOOLS indexing is zero-based, not 
%   one-based.  Specifying Inf in COUNT means to retrieve everything 
%   along that dimension from the START coordinate.
%
%   DATA = NC_VARGET(NCFILE,VARNAME,START,COUNT,STRIDE) retrieves 
%   a non-contiguous portion of the dataset.  The amount of
%   skipping along each dimension is given through the STRIDE vector.
%
%   A '_FillValue' attribute is honored by flagging those datums as NaN.
%   A 'missing_value' attribute is honored by flagging those datums as 
%   NaN.  The data is returned as double precision.
%
%   If the named NetCDF variable has valid scale_factor and add_offset 
%   attributes, then the data is scaled accordingly. The data is returned
%   as double precision.
% 
%   EXAMPLE:  This example file is shipped with R2008b.
%       data = nc_varget('example.nc','peaks',[0 0], [20 30]);
%
%   EXAMPLE: Retrieve data from a URL.  This requires the netcdf-java 
%   backend.
%       url = 'http://coast-enviro.er.usgs.gov/models/share/balop.nc';
%       data = nc_varget(url,'lat_rho');
%
%   Example:  Retrieve data from the example HDF4 file.
%       data = nc_varget('example.hdf','Example SDS');
% 
%   See also:  nc_varput, nc_attget, nc_attput, nc_dump.
%



error(nargchk(2,5,nargin,'struct'));

[start, count, stride] = parse_and_validate_args(ncfile,varname,varargin{:});

retrieval_method = snc_read_backend(ncfile);
switch(retrieval_method)
	case 'tmw'
		data = nc_varget_tmw(ncfile,varname,start,count,stride);
    case 'tmw_hdf4'
        data = nc_varget_hdf4(ncfile,varname,start,count,stride);
	case 'java'
		data = nc_varget_java(ncfile,varname,start,count,stride);
	case 'mexnc'
		data = nc_varget_mexnc(ncfile,varname,start,count,stride);
end

return




%--------------------------------------------------------------------------
function [start, count, stride] = parse_and_validate_args(ncfile,varname,varargin)

%
% Set up default outputs.
start = [];
count = [];
stride = [];


switch nargin
case 4
    start = varargin{1};
    count = varargin{2};

case 5
    start = varargin{1};
    count = varargin{2};
    stride = varargin{3};

end



%
% Error checking on the inputs.
switch(class(ncfile))
	case { 'char', 'ucar.nc2.NetcdfFile', 'ucar.nc2.dods.DODSNetcdfFile' }
		
	otherwise
    	error ( 'SNCTOOLS:NC_VARGET:badInput', ...
			'The filename should be character or an already opened netCDF file.' );
end
if ~ischar(varname)
    error ( 'SNCTOOLS:NC_VARGET:badInput', 'the variable name must be character.' );
end

if ~isnumeric ( start )
    error ( 'SNCTOOLS:NC_VARGET:badInput', 'the ''start'' argument must be numeric.' );
end
if ~isnumeric ( count )
    error ( 'SNCTOOLS:NC_VARGET:badInput', 'the ''count'' argument must be numeric.' );
end
if ~isnumeric ( stride )
    error ( 'SNCTOOLS:NC_VARGET:badInput', 'the ''stride'' argument must be numeric.' );
end


return

