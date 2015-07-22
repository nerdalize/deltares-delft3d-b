function values = nc_varget_mexnc(ncfile,varname,start,count,stride)
% Handler for NC_VARGET it case where the old community mex-file mexnc must
% be used. 


[ncid,status]=mexnc('open',ncfile,'NOWRITE');
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:nc_varget:mexnc:open', ncerr );
end


try
    [varid, status]=mexnc('inq_varid',ncid,varname);
    if status ~= 0
        ncerr = mexnc('strerror', status);
        error ( 'SNCTOOLS:nc_varget:mexnc:inq_varid', ncerr );
    end
    
    [dud,var_type,nvdims,dimids,dud,status]=mexnc('inq_var',ncid,varid); %#ok<ASGLU>
    if status ~= 0
        error ( 'SNCTOOLS:nc_varget:mexnc:inq_var', mexnc('strerror',status) );
    end
    
    
    % mexnc does not preserve the fastest varying dimension.  If we want this,
    % then we flip the indices.
    preserve_fvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
    if preserve_fvd
        start = fliplr(start);
        count = fliplr(count);
        stride = fliplr(stride);
    end
    
    
    % Check that the start, count, stride parameters have appropriate lengths.
    % Otherwise we get confusing error messages later on.
    validate_index_vectors(start,count,stride,nvdims);
    
    % What mexnc operation will we use?
    [funcstr_family, funcstr] = determine_funcstr(var_type,nvdims,start,count,stride);
    
    the_var_size = determine_varsize_mex(ncid,dimids,nvdims);
    
    if isempty(stride)
        stride = ones(1,numel(start));
    end
    
    % If the user had set non-positive numbers in "count", then we replace them
    % with what we need to get the rest of the variable.
    negs = find((count<0) | isinf(count));
    count(negs) = (the_var_size(negs) - start(negs)) ./ stride(negs);
    
    % At long last, retrieve the data.
    switch funcstr_family
        case 'get_var'
            [values, status] = mexnc(funcstr,ncid,varid);
    
        case 'get_var1'
            [values, status] = mexnc(funcstr,ncid,varid,0);
    
        case 'get_vara'
            [values, status] = mexnc(funcstr,ncid,varid,start,count);
    
    
        case 'get_vars'
            [values, status] = mexnc(funcstr,ncid,varid,start,count,stride);
    
        otherwise
            error ( 'SNCTOOLS:nc_varget:mexnc:unhandledType', ...
                    'Unhandled function string type ''%s''\n', funcstr_family);
    end
    
    if ( status ~= 0 )
        error('SNCTOOLS:nc_varget:mexnc:getVarFuncstrFailure', ...
            mexnc('strerror',status) );
    end
    
    
    % If it's a 1D vector, make it a column vector.  
    % Otherwise permute the data
    % to make up for the row-major-order-vs-column-major-order issue.
    if length(the_var_size) == 1
        values = values(:);
    else
        % Ok it's not a 1D vector.  If we are not preserving the fastest
        % varying dimension, we should permute the data.
        if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
            pv = fliplr ( 1:length(the_var_size) );
            values = permute(values,pv);
        end
    end                                                                                   
    
    
    values = handle_fill_value_mex(ncid,varid,var_type,values);
    values = handle_mex_missing_value(ncid,varid,var_type,values);
    values = handle_scaling_mex(ncid,varid,values);
    
    % remove any singleton dimensions.
    values = squeeze(values);

catch %#ok<CTCH>
    mexnc('close',ncid);
    rethrow(lasterror);
end

mexnc('close',ncid);


return


















%--------------------------------------------------------------------------
function [prefix,funcstr] = determine_funcstr(var_type,nvdims,start,count,stride)
% DETERMINE_FUNCSTR
%     Determines if we are to use, say, 'get_var1_text', or 'get_vars_double',
%     or whatever.

% Determine if we are retriving a single value, the whole variable, a 
% contiguous portion, or a strided portion.
if nvdims == 0

    % It is a singleton variable.
    prefix = 'get_var1';

elseif isempty(start)
    
    % retrieving the entire variable.
    prefix = 'get_var';

elseif ~isempty(start) && ~isempty(count) && isempty(stride)
    
    % retrieving a contiguous portion
    prefix = 'get_vara';

elseif ~isempty(start) && ~isempty(count) && ~isempty(stride)
    
    % retrieving a contiguous portion
    prefix = 'get_vars';

else
    error ( 'SNCTOOLS:nc_varget:mexnc:undeterminedFuncStr', ...
        'Could not determine funcstr prefix.');
end



switch ( var_type )
    case nc_char
        funcstr = [prefix '_text'];

    case { nc_double, nc_float, nc_int, nc_short, nc_byte }
        funcstr = [prefix '_double'];

    otherwise
        error ( 'SNCTOOLS:nc_varget:mexnc:badDatatype', ...
                'Unhandled datatype %d.', var_type );

end
return





%--------------------------------------------------------------------------
function values = handle_fill_value_mex ( ncid, varid, var_type, values )
% If there is a fill value, then replace such values with NaN.

[varname,status] = mexnc('inq_varname',ncid,varid);
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'SNCTOOLS:nc_varget:mexnc:inq_varname', ncerr );
end


[att_type, dud, status] = mexnc('INQ_ATT',ncid,varid,'_FillValue'); %#ok<ASGLU>
if ( status == 0 )

    if att_type ~= var_type
        warning('SNCTOOLS:nc_varget:mexnc:fillValueMismatch', ...
                'The _FillValue datatype for %s is wrong and will not be honored.', ...
                varname);
        return
    end

    switch ( var_type )
        case nc_char
            % For now, do nothing.  Does a fill value even make sense with 
            % char data?  If it does, please tell me so.
            
        case { nc_double, nc_float, nc_int, nc_short, nc_byte }
            [fill_value, status] = mexnc('get_att_double',ncid,varid,'_FillValue');
            values(values==fill_value) = NaN;
            
        otherwise
            error ( 'SNCTOOLS:nc_varget:mexnc:unhandledFillValueDatatype', ...
                'Unhandled datatype %d.', var_type );
    end

    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        error ( 'SNCTOOLS:nc_varget:mexnc:get_att', ncerr );
    end



end

return




%--------------------------------------------------------------------------
function values = handle_mex_missing_value ( ncid, varid, var_type, values )
% If there is a missing value, then replace such values with NaN.

[varname,status] = mexnc('inq_varname',ncid,varid);
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'SNCTOOLS:nc_varget:mexnc:inq_varname', ncerr );
end

% If there is a fill value attribute, then that has precedence.  
% Do nothing in this case.
[dud, dud, status] = mexnc('INQ_ATT',ncid,varid,'_FillValue'); %#ok<ASGLU>
if status == 0
    return
end

% Handle the missing value, if any.  Change those values into NaN.
[att_type, dud, status] = mexnc('INQ_ATT',ncid,varid,'missing_value'); %#ok<ASGLU>
if ( status == 0 )

    if att_type ~= var_type
        warning('SNCTOOLS:nc_varget:mexnc:missingValueMismatch', ...
                'The missing_value datatype for %s is wrong and will not be honored.', ...
                varname);
        return
    end

    switch ( var_type )
        case nc_char           
            % For now, do nothing.  Does a missing value even make sense 
            % with char data?  If it does, please tell me so.
            
        case { nc_double, nc_float, nc_int, nc_short, nc_byte }
            [fill_value, status] = mexnc('get_att_double',ncid,varid,'missing_value');
            values(values==fill_value) = NaN;
            
        otherwise
            error ( 'SNCTOOLS:nc_varget:mexnc:unhandledDatatype', ...
                'Unhandled datatype %d.', var_type );
            
    end

    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        error ( 'SNCTOOLS:nc_varget:mexnc:get_att_double', ncerr );
    end


end

return






%--------------------------------------------------------------------------
function values = handle_scaling_mex ( ncid, varid, values )
% If there is a scale factor and/or  add_offset attribute, convert the data
% to double precision and apply the scaling.

have_scale = false;
have_addoffset = false;
[dud, dud, status] = mexnc('INQ_ATT',ncid,varid,'scale_factor'); %#ok<ASGLU>
if ( status == 0 )
    have_scale = true;
end
[dud, dud, status] = mexnc('INQ_ATT',ncid,varid,'add_offset'); %#ok<ASGLU>
if ( status == 0 )
    have_addoffset = true;
end

%
% Return early if we don't have either one.
if ~(have_scale || have_addoffset)
    return;
end

scale_factor = 1.0;
add_offset = 0.0;

if have_scale
    [scale_factor, status] = mexnc('get_att_double',ncid,varid,'scale_factor');
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        error ( 'SNCTOOLS:nc_varget:mexnc:get_att_double', ncerr );
    end
end

if have_addoffset
    [add_offset, status] = mexnc('get_att_double',ncid,varid,'add_offset');
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        error ( 'SNCTOOLS:nc_varget:mexnc:get_att_double', ncerr );
    end
end

values = double(values) * scale_factor + add_offset;

return








%-----------------------------------------------------------------------
function the_var_size = determine_varsize_mex ( ncid, dimids, nvdims )
% DETERMINE_VARSIZE_MEX: Need to figure out just how big the variable is.
%
% VAR_SIZE = DETERMINE_VARSIZE_MEX(NCID,DIMIDS,NVDIMS);

%
% If not a singleton, we need to figure out how big the variable is.
if nvdims == 0
    the_var_size = 1;
else
    the_var_size = zeros(1,nvdims);
    for j=1:nvdims,
        dimid = dimids(j);
        [dim_size,status]=mexnc('inq_dimlen', ncid, dimid);
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            error ( 'SNCTOOLS:nc_varget:mexnc:inq_dimlen', ncerr );
        end
        the_var_size(j)=dim_size;
    end
end

return





