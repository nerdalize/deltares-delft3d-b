function Dataset = get_varinfo ( ncid, varid )


[record_dimension, status] = mexnc ( 'INQ_UNLIMDIM', ncid );
if status ~= 0
   	ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_UNLIMDIM', ncerr );
end



[varname,datatype,ndims,dims,natts,status] = mexnc('INQ_VAR',ncid,varid);
if status ~= 0 
   	ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_VAR', ncerr );
end



Dataset.Name = varname;
Dataset.Nctype = datatype;
switch(datatype)
    case nc_nat
        Dataset.Datatype = '';
    case nc_byte
        Dataset.Datatype = 'int8';
    case nc_char
        Dataset.Datatype = 'char';
    case nc_short
        Dataset.Datatype = 'int16';
    case nc_int
        Dataset.Datatype = 'int32';
    case nc_float
        Dataset.Datatype = 'single';
    case nc_double
        Dataset.Datatype = 'double';
end


% Assume the current variable does not have an unlimited dimension until
% we know that it does.
Dataset.Unlimited = false;

if ndims == 0
	Dataset.Dimension = {};
	Dataset.Size = 1;
else

	for j = 1:ndims
	
		[dimname, dimlength, status] = mexnc('INQ_DIM', ncid, dims(j));
		if ( status ~= 0 )
   			ncerr = mexnc('strerror', status);
		    mexnc('close',ncid);
		    error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_DIM', ncerr );
		end
	
		Dataset.Dimension{j} = dimname; 
		Dataset.Size(j) = dimlength;
	
		if dims(j) == record_dimension
			Dataset.Unlimited = true;
		end
	end
end

%
% get all the attributes
if natts == 0
	Dataset.Attribute = [];
else
	for attnum = 0:natts-1
		Dataset.Attribute(attnum+1) = nc_get_attribute_struct(ncid,varid,attnum);
	end
end

v = mexnc('INQ_LIBVERS');
if v(1) == '4'
	% Get the chunksize
	[storage,chunking,status] = mexnc('INQ_VAR_CHUNKING',ncid,varid); %#ok<ASGLU>
	if ( status ~= 0 )
	   	ncerr = mexnc('strerror', status);
		mexnc('close',ncid);
		error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_VAR_DEFLATE', ncerr );
	end
	Dataset.Chunking = chunking;
	
	% Get the compression parameters
	[shuffle,deflate,deflate_level,status] = mexnc('INQ_VAR_DEFLATE',ncid,varid); %#ok<ASGLU>
	if ( status ~= 0 )
	   	ncerr = mexnc('strerror', status);
		mexnc('close',ncid);
		error ( 'SNCTOOLS:NC_VARGET:MEXNC:INQ_VAR_DEFLATE', ncerr );
	end
	Dataset.Shuffle = shuffle;
	Dataset.Deflate = deflate_level;
end



if getpref('SNCTOOLS','PRESERVE_FVD',false)
	Dataset.Dimension = fliplr(Dataset.Dimension);
	Dataset.Size = fliplr(Dataset.Size);
	if isfield(Dataset,'Chunking')
		Dataset.Chunking = fliplr(Dataset.Chunking);
	end
end





return







