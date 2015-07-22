function nc_varrename ( ncfile, old_variable_name, new_variable_name )
%nc_varrename Rename netCDF variable.
%   nc_varrename(ncfile,oldvarname,newvarname) renames a netCDF variable
%   from OLDVARNAME to NEWVARNAME.
%
%   Example:
%      nc_create_empty('myfile.nc');
%      nc_adddim('myfile.nc','x',10);
%      v.Name = 'y';
%      v.Datatype = 'double';
%      v.Dimension = { 'x' };
%      nc_addvar('myfile.nc',v);
%      nc_dump('myfile.nc');
%      nc_varrename('myfile.nc','y','z');
%      nc_dump('myfile.nc');
%
%   See also nc_addvar.



backend = snc_write_backend(ncfile);
switch(backend)
	case 'tmw'
    	nc_varrename_tmw( ncfile, old_variable_name, new_variable_name )
	case 'mexnc'
    	nc_varrename_mexnc( ncfile, old_variable_name, new_variable_name )
end



%--------------------------------------------------------------------------
function nc_varrename_mexnc ( ncfile, old_variable_name, new_variable_name )
[ncid,status ]=mexnc('OPEN',ncfile,nc_write_mode);
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:OPEN', ncerr );
end


status = mexnc('REDEF', ncid);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:REDEF', ncerr );
end


[varid, status] = mexnc('INQ_VARID', ncid, old_variable_name);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_VARID', ncerr );
end


status = mexnc('RENAME_VAR', ncid, varid, new_variable_name);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:RENAME_VAR', ncerr );
end


status = mexnc('ENDDEF', ncid);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:ENDDEF', ncerr );
end


status = mexnc('close',ncid);
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:CLOSE', ncerr );
end


