function varsize = nc_varsize(ncfile, varname)
%NC_VARSIZE Return size of netCDF variable.
%   This function is deprecated, use NC_GETVARINFO instead.


if ~ischar(ncfile)
	error ( 'SNCTOOLS:NC_VARSIZE:badInputType', 'The input filename must be a string.' );
end
if ~ischar(varname)
	error ( 'SNCTOOLS:NC_VARSIZE:badInputType', 'The input variable name must be a string.' );
end


v = nc_getvarinfo ( ncfile, varname );

varsize = v.Size;

return

