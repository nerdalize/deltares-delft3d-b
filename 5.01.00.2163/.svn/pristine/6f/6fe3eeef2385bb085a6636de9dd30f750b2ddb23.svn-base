function Dataset = nc_getvarinfo ( arg1, arg2 )
%NC_GETVARINFO  Returns metadata about a specific NetCDF variable.
%
%   VINFO = NC_GETVARINFO(NCFILE,VARNAME) returns a metadata structure 
%   about the variable VARNAME in the netCDF file NCFILE.
%
%   VINFO = NC_GETVARINFO(NCID,VARID) returns a metadata structure VINFO
%   about the variable whose netCDF variable-id is VARID, and whose parent
%   file-id is NCID.  The netCDF file is assumed to be open, and in this
%   case the file will not be closed upon completion.
%
%   VINFO will have the following fields:
%
%       Name      - A string containing the name of the variable.
%       Class     - The MATLAB class corresponding to the datatype.
%       Unlimited - Either 1 if the variable has an unlimited dimension or 
%                   0 if not.
%       Dimension - a cell array with the names of the dimensions upon 
%                   which this variable depends.
%       Size      - Size of the variable.
%       Attribute - An array of structures corresponding to the attributes 
%                   defined for the specified variable.
%                         
%    Each "Attribute" element is a struct itself and contains the following 
%    fields.
%
%       Name      - A string containing the name of the attribute.
%       Class     - The MATLAB class corresponding to the datatype.
%       Value     - Value of the attribute.
%
%   See also nc_info.


backend = snc_read_backend(arg1);
switch(backend)
	case 'tmw'
		Dataset = nc_getvarinfo_tmw(arg1,arg2);
    case 'tmw_hdf4'
        Dataset = nc_getvarinfo_hdf4(arg1,arg2);
	case 'java'
		Dataset = nc_getvarinfo_java(arg1,arg2);
	case 'mexnc'
		Dataset = nc_getvarinfo_mexnc(arg1,arg2);
	otherwise
		error('SNCTOOLS:nc_info:unhandledBackend', ...
		      '%s is not a recognized backend.', backend);
end



