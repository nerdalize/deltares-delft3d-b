function nc_addrecs(ncfile,new_data)
%NC_ADDRECS  Add records onto the end of netcdf file.
%   nc_addrecs(NCFILE,RECS) appends records along the unlimited 
%   dimension.  RECS is a structure containing all the variables that
%   are defined along the unlimited dimension.
%
%   This function differs from NC_ADDNEWRECS in that it does not require
%   the unlimited coordinate variable to be monotonically increasing.
%
%   See also nc_addnewrecs, nc_cat.

ncinfo = nc_info(ncfile);
preserve_fvd = getpref('SNCTOOLS','PRESERVE_FVD',false);

% Check that we were given good inputs.
if ~isstruct ( new_data )
    err_id = 'SNCTOOLS:NC_ADD_RECS:badStruct';
    error ( err_id, '2nd input argument must be a structure .\n' );
end

%
% Check that each field of the structure has the same length.
varnames = fieldnames ( new_data );
num_fields = length(varnames);
if ( num_fields <= 0 )
    err_id = 'SNCTOOLS:NC_ADD_RECS:badRecord';
    error ( err_id, 'data record cannot be empty' );
end
field_length = zeros(num_fields,1);
for j = 1:num_fields

    v = nc_getvarinfo(ncfile,varnames{j});

    if ~v.Unlimited
        error('SNCTOOLS:addRecs:notUnlimited', ...
            'All variables must have an unlimited dimension.');
    end
    
    if preserve_fvd

        if numel(v.Size) == 1
            % netCDF variable is 1D
            field_length(j) = numel(new_data.(varnames{j}));
        elseif (numel(v.Size) == 2) 
            % netCDF variable is 2D
            field_length(j) = size(new_data.(varnames{j}),2);
        elseif (numel(v.Size) > 2) && (numel(v.Size) == (ndims(new_data.(varnames{j})) + 1))
            % netCDF variable is more than 2D, but we're given just one record.
            field_length(j) = 1;
        else
            % netCDF variable is n-D
            n = ndims(new_data.(varnames{j}));
            command = sprintf ( 'field_length(j) = size(new_data.%s,%d);', ...
                varnames{j}, n );
            eval(command);
        end

    else
        if numel(v.Size) == 1
            % netCDF variable is 1D
            field_length(j) = numel(new_data.(varnames{j}));
        elseif (numel(v.Size) == 2) 
            % netCDF variable is 2D
            field_length(j) = size(new_data.(varnames{j}),1);
        elseif (numel(v.Size) > 2) && (numel(v.Size) == (ndims(new_data.(varnames{j})) + 1))
            % netCDF variable is more than 2D, but we're given just one record.
            field_length(j) = 1;
        else
            % netCDF variable is n-D
            command = sprintf ( 'field_length(j) = size(new_data.%s,1);', varnames{j} );
            eval(command);
        end

    end
end
if any(diff(field_length))
    err_id = 'SNCTOOLS:NC_ADD_RECS:badFieldLengths';
    error ( err_id, 'Some of the fields do not have the same length.\n' );
end


% So we have this many records to write.
record_count = field_length(1);

% Ok, get the unlimited dimension name and current length
unlim_idx = 0;
for j = 1:numel(ncinfo.Dimension)
    if ncinfo.Dimension(j).Unlimited
        unlim_idx = j;
    end
end



% Need to retrieve the variable sizes NOW, before the first write.
all_names = {ncinfo.Dataset.Name};
data_names = fieldnames(new_data);
num_vars = numel(data_names);
for j = 1:num_vars
    tf = strcmp(data_names{j},all_names);
    idx = find(tf);
    varsize.(ncinfo.Dataset(idx).Name) = ncinfo.Dataset(idx).Size;
end


% So we start writing here.
record_corner = ncinfo.Dimension(unlim_idx).Length;



% write out each data field
for j = 1:num_vars

    current_var = data_names{j};

    current_var_data = new_data.(current_var);

    netcdf_var_size = varsize.(current_var);

    corner = zeros( 1, length(netcdf_var_size) );
    count = netcdf_var_size;

    if preserve_fvd
        % record dimension is last.
        corner(end) = record_corner;
        count(end) = record_count;
    else
        % Old school
        corner(1) = record_corner;
        count(1) = record_count;
    end

    % Ok, we are finally ready to write some data.
    nc_varput ( ncfile, current_var, current_var_data, corner, count );

end


return



















    


