function data = nc_varget_hdf4(hfile,varname,start,edge,stride)
% HDF4 backend for NC_VARGET

preserve_fvd = getpref('SNCTOOLS','PRESERVE_FVD');

fid = fopen(hfile,'r');
fullfile = fopen(fid);
fclose(fid);

v = nc_getvarinfo(fullfile,varname);

sd_id = hdfsd('start',fullfile,'read');
if sd_id < 0
    error('SNCTOOLS:varget:hdf4:startFailed', ...
        'START failed on %s.\n', hfile);
end

try
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('SNCTOOLS:varget:hdf4:nametoindexFailed', ...
            'NAMETOINDEX failed on %s, %s.', varname, hfile);
    end
    
    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        error('SNCTOOLS:varget:hdf4:selectFailed', ...
            'SELECT failed on %s, %s.', varname, hfile);
    end
    
    
    if isempty(start) && isempty(edge) && isempty(stride)
        
        % retrieve everything.
        start = zeros(1,numel(v.Size));
        edge = v.Size;
        stride = ones(1,numel(v.Size));
        
    elseif isempty(edge) && isempty(stride)
        % if only start was provided, then the count is implied to be one.
        edge = ones(1,numel(v.Size));
        stride = ones(1,numel(v.Size));
    elseif isempty(stride)
        % just a contiguous hyperslab.
        stride = ones(1,numel(v.Size));
    end
    
    negs = find((edge<0) | isinf(edge));
    if isempty(stride)
        edge(negs) =        v.Size(negs) - start(negs);
    else
        edge(negs) = floor((v.Size(negs) - start(negs))./stride(negs));
    end
    
    if preserve_fvd
        start = fliplr(start);
        edge = fliplr(edge);
        stride = fliplr(stride);
    end
    
        
    
    [data,status] = hdfsd('readdata',sds_id,start,stride,edge);
    if status < 0
        error('SNCTOOLS:varget:hdf4:readdataFailed', ...
            'READDATA failed on %s, %s.', varname, hfile);
    end
    
    
    % fill value, scale factor, add_offset, missing value, etc
    [cal,cal_err,offset,offset_err,data_type,status] = hdfsd('getcal',sds_id); %#ok<ASGLU>
    if status == 0
        if getpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);
            data = cal*(double(data) - offset);  
        else
            % Use standard CF convention scaling.
            data = cal * double(data) + offset;
        end
    end
    
    [fill, status] = hdfsd('getfillvalue',sds_id);
    if status == 0
        fv = double(fill);
        data = double(data);
        data(data==fv) = NaN;  
    end
    
    % Missing value is to be handled the same as fill value
    attr_index = hdfsd('findattr',sds_id,'missing_value');
    if ( attr_index > -1 )
        [missing_value, status] = hdfsd('readattr',sds_id,attr_index);
        if status == 0
            fv = double(missing_value);
            data = double(data);
            data(data==fv) = NaN;
        end
    end

catch
    if exist('sds_id','var')
        hdfsd('endaccess',sds_id);
    end
    hdfsd('end',sd_id);
    e = lasterror;
    error(e.identifier,e.message);
end

status = hdfsd('endaccess',sds_id);
if status < 0
    hdfsd('end',sd_id);
    error('SNCTOOLS:varget:hdf4:endaccessFailed', ...
        'ENDACCESS failed on %s, %s.', varname, hfile);
end

status = hdfsd('end',sd_id);
if status < 0
    error('SNCTOOLS:varget:hdf4:endFailed', ...
        'END failed on %s, %s.', varname, hfile);
end

if ~preserve_fvd
    data = permute(data,ndims(data):-1:1);
end

% If 1D vector, force to be a column.
if numel(start) == 1
    data = data(:);
end

return
