function nc_attput_mex ( ncfile, varname, attribute_name, attval )
% MEXNC handler for NC_ATTPUT.

[ncid, status] =mexnc( 'open', ncfile, nc_write_mode );
if  status ~= 0 
    ncerr = mexnc ( 'strerror', status );
    error ( 'SNCTOOLS:attget:mexnc:open', ncerr );
end

try
    % Put into define mode.
    status = mexnc ( 'redef', ncid );
    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        error ( 'SNCTOOLS:attget:mexnc:redef', ncerr );
    end
    
    if isnumeric(varname)
        varid = varname;
    else
        [varid, status] = mexnc ( 'inq_varid', ncid, varname );
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            error ( 'SNCTOOLS:attget:mexnc:inq_varid', ncerr );
        end
    end
    
    % If the attribute is '_FillValue', then force the value to have the
    % correct dataype.
    if strcmp(attribute_name,'_FillValue')
        [xtype,status] = mexnc('INQ_VARTYPE',ncid,varid);
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            error ( 'SNCTOOLS:attget:mexnc:inq_vartype', ncerr );
        end
        
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
    % Figure out which mexnc operation to perform.
    switch class(attval)
    
        case 'double'
            funcstr = 'put_att_double';
            atttype = nc_double;
        case 'single'
            funcstr = 'put_att_float';
            atttype = nc_float;
        case 'int32'
            funcstr = 'put_att_int';
            atttype = nc_int;
        case 'int16'
            funcstr = 'put_att_short';
            atttype = nc_short;
        case 'int8'
            funcstr = 'put_att_schar';
            atttype = nc_byte;
        case 'uint8'
            funcstr = 'put_att_uchar';
            atttype = nc_byte;
        case 'char'
            funcstr = 'put_att_text';
            atttype = nc_char;
        otherwise
            error ( 'SNCTOOLS:attget:unhandleDatatype', ...
                'attribute class %s is not handled by %s', ...
                 class(attval), mfilename );
    end
    
    status = mexnc ( funcstr, ncid, varid, attribute_name, atttype, length(attval), attval);
    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status ); 
        error ( ['SNCTOOLS:attget:mexnc:' upper(funcstr)], ...
            'PUT_ATT operation failed:  %s', ncerr );
    end
    
    status = mexnc ( 'enddef', ncid );
    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        error ( 'SNCTOOLS:attget:mexnc:enddef', ncerr );
    end

catch %#ok<CTCH>
	mexnc('close',ncid);
	rethrow(lasterror);	
end

status = mexnc('close',ncid);
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'SNCTOOLS:attget:mexnc:close', ncerr );
end


return;



