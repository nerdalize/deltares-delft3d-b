function nc_attput_tmw ( ncfile, varname, attribute_name, attval )
%NC_ATTPUT_TMW private function for writing attribute with TMW backend.

ncid  =netcdf.open(ncfile, nc_write_mode );

try
    netcdf.reDef(ncid);

    if isnumeric(varname)
        varid = varname;
    else
        varid = netcdf.inqVarID(ncid, varname );
    end
    
    % If we are dealing with the fill value, then force the type to be
    % correct.
    if strcmp(attribute_name,'_FillValue')
        [name,xtype] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
        switch(xtype)
            case nc_double
                attval = double(attval);
            case nc_float
                attval = single(attval);
            case nc_int
                attval = int32(attval);
            case nc_short
                attval = int16(attval);
            case nc_byte
                attval = int8(attval);
            case nc_char
                attval = char(attval);
        end
    end
    
    try
        netcdf.putAtt(ncid,varid,attribute_name,attval);
    catch me
        switch(me.identifier)
            case 'MATLAB:netcdf_common:emptySetArgument'
                % Bug #609383
                % Please consult the README.
                %
                % If char, change attval to ' '
                warning('SNCTOOLS:NCATTPUT:emptyAttributeBug', ...
                    'Changing attribute from empty to single space, please consult the README.');
                netcdf.putAtt(ncid,varid,attribute_name,' ');
            otherwise
                rethrow(me);
        end
                
    end
    
    netcdf.endDef(ncid);

catch myException
    netcdf.close(ncid);
    rethrow(myException);
end

netcdf.close(ncid);

return;
