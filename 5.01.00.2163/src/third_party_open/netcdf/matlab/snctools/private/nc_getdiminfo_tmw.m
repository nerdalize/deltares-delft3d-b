function dinfo = nc_getdiminfo_tmw ( arg1, arg2 )

% If we are here, then we must have been given something local.
if ischar(arg1) && ischar(arg2)
    dinfo = handle_char_nc_getdiminfo_tmw(arg1,arg2);
elseif isnumeric ( arg1 ) && isnumeric ( arg2 )
	dinfo = handle_numeric_nc_getdiminfo_tmw(arg1,arg2);
else
	error ( 'SNCTOOLS:NC_GETDIMINFO_TMW:badInputDatatypes', ...
	        'Must supply either two character or two numeric arguments.' );
end

return



%--------------------------------------------------------------------------
function dinfo = handle_char_nc_getdiminfo_tmw ( ncfile, dimname )

ncid=netcdf.open(ncfile,'NOWRITE');
try
    dimid = netcdf.inqDimID(ncid, dimname);
    dinfo = handle_numeric_nc_getdiminfo_tmw(ncid,dimid);
catch me
    netcdf.close(ncid);
    rethrow(me);
end
netcdf.close(ncid);






%--------------------------------------------------------------------------
function dinfo = handle_numeric_nc_getdiminfo_tmw ( ncid, dimid )

[dud,dud,dud,unlimdim] = netcdf.inq(ncid ); %#ok<ASGLU>
[dimname, dimlength] = netcdf.inqDim(ncid, dimid);
dinfo.Name = dimname;
dinfo.Length = dimlength;

if dimid == unlimdim
	dinfo.Unlimited = true;
else
	dinfo.Unlimited = false;
end

return
