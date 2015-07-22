function nc_addvar_tmw(ncfile,varstruct)
% TMW backend for NC_ADDVAR.

ncid = netcdf.open(ncfile, nc_write_mode );

try
    
    % determine the dimids of the named dimensions
    num_dims = length(varstruct.Dimension);
    dimids = zeros(1,num_dims);
    for j = 1:num_dims
        dimids(1,j) = netcdf.inqDimID(ncid, varstruct.Dimension{j} );
    end
    
    % If we are old school, we need to flip the dimensions.
    preserve_fvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
    if ~preserve_fvd
        dimids = fliplr(dimids);
    end
    
    % go into define mode
    netcdf.reDef(ncid);
    
    % Prefer to use Datatype instead of Nctype.
    if isfield(varstruct,'Datatype')
        varid = netcdf.defVar(ncid, varstruct.Name, varstruct.Datatype, dimids );
    else
        % Backwards compatible mode.
        varid = netcdf.defVar(ncid, varstruct.Name, varstruct.Nctype, dimids );
    end
      
    
    if ~isempty(varstruct.Chunking)
        
        if ~preserve_fvd
            chunking = fliplr(varstruct.Chunking);
        else
            chunking = varstruct.Chunking;
        end
        if ( numel(chunking) ~= num_dims)
            error ( 'SNCTOOLS:NC_ADDVAR:tmw:defVarChunking', ...
                'Chunking size does not jive with number of dimensions.');
        end
        
        netcdf.defVarChunking(ncid,varid,'CHUNKED',chunking);
    end
    
    if (varstruct.Shuffle || varstruct.Deflate)
		if varstruct.Shuffle
			shuffle = true;
		else
			shuffle = false;
		end
		if varstruct.Deflate
			deflate = true;
		else
			deflate = false;
		end
        netcdf.defVarDeflate(ncid,varid,shuffle,deflate,varstruct.Deflate);
    end
    
    netcdf.endDef(ncid );
    
catch me
    netcdf.close(ncid);
    rethrow(me);
end
netcdf.close(ncid );







