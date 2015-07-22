function tf = nc_isunlimitedvar_java(ncfile,varname)     
% java backend for NC_ISUNLIMITEDVAR
try
    DataSet = nc_getvarinfo ( ncfile, varname );
catch 
    e = lasterror;
    switch ( e.identifier )
        case 'SNCTOOLS:NC_GETVARINFO:badVariableName'
            tf = false;
            return
        otherwise
            error('SNCTOOLS:NC_ISUNLIMITEDVAR:unhandledCondition', e.message );
    end
end

tf = DataSet.Unlimited;

