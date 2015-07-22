function nc_attput(ncfile,varname,attname,attval)
%NC_ATTPUT  Writes attribute to netCDF file.
%   NC_ATTPUT(NCFILE,VARNAME,ATTNAME,ATTVAL) writes the data in ATTVAL to
%   the attribute ATTNAME of the variable VARNAME of the netCDF file
%   NCFILE. VARNAME should be the name of a netCDF VARIABLE, but one can
%   also use the mnemonic nc_global to specify a global attribute. 
%
%   The attribute datatype will match that of the class of ATTVAL.  So if
%   if you want to have a 16-bit short integer attribute, make the class of
%   ATTVAL to be INT16.  Fill value attributes, however, will be cast to 
%   the correct type.
%
%   Note:  Be aware that using NC_ATTPUT with the _FillValue attribute is
%   dangerous with netCDF-4 files.  If the variable is not empty, all its
%   data will be lost.
%
%   Example:  create an empty netcdf file and then write a global
%   attribute.
%       nc_create_empty('myfile.nc');
%       attval = sprintf('created on %s', datestr(now));
%       nc_attput('myfile.nc',nc_global,'history',attval);
%       nc_dump('myfile.nc');
%
%   See also nc_attget.
%

backend = snc_write_backend(ncfile);
switch backend
    case 'mexnc'
        nc_attput_mex(ncfile,varname,attname,attval);
    case 'tmw_hdf4'
        nc_attput_hdf4(ncfile,varname,attname,attval);
    otherwise
        nc_attput_tmw(ncfile,varname,attname,attval);
end


return


