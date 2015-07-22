function tf = nc_isunlimitedvar_hdf4(ncfile,varname)     
% mexnc backend for NC_ISUNLIMITEDVAR
try
    vinfo = nc_getvarinfo ( ncfile, varname );
catch 
    e = lasterror;
    switch ( e.identifier )
        case 'SNCTOOLS:nc_getvarinfo:hdf4:variableNotChar'
            error(e.identifier,e.message);
        otherwise
            tf = false;
            return
    end
end

tf = vinfo.Unlimited;

