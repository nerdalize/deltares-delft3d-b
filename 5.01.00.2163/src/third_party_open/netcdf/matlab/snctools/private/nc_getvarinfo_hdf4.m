function vinfo = nc_getvarinfo_hdf4(hfile,varname)
% HDF4 backend for nc_getvarinfo.
if ~ischar(varname)
    error('SNCTOOLS:nc_getvarinfo:hdf4:variableNotChar', ...
        'The variable name must be a character string.');
end

fid = fopen(hfile,'r');
filename = fopen(fid);
fclose(fid);
sd_id = hdfsd('start',filename,'read');
if sd_id < 0
    error('SNCTOOLS:nc_info:hdf4:startFailed', ...
        'Unable to gain access to %s.\n', filename);
end

try
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('SNCTOOLS:nc_info:hdf4:nametoindexFailed', ...
            'Unable to index %s.\n', varname);
    end
    sds_id = hdfsd('select',sd_id,idx);
    if  sds_id < 0
        error('SNCTOOLS:nc_info:hdf4:selectFailed', ...
            'Unable to select %s.\n', varname);
    end
    
    
    [name,rank,dim_sizes,data_type,nattrs,status] = hdfsd('getinfo',sds_id);
    if status < 0
        error('SNCTOOLS:nc_info:hdf4Numeric:getinfoFailed', ...
            'Unable to get information about scientific dataset.\n');
    end
    
    vinfo.Name = name;
    switch(data_type)
    	case 'float'
    		vinfo.Datatype = 'single';
        case 'char8'
            vinfo.Datatype = 'char';
    	otherwise
    		vinfo.Datatype = data_type;
    end
    
    
    if hdfsd('isrecord',sds_id)
        vinfo.Unlimited = true;
    else
        vinfo.Unlimited = false;
    end
    
    if (rank == 0)
        vinfo.Dimension = {};
        vinfo.Size = 1;
    else
        for j = 0:rank-1
            dim_id = hdfsd('getdimid',sds_id,j);
            if dim_id < 0
                error('SNCTOOLS:nc_info:hdf4Numeric:getdimidFailed', ...
                    'Unable to get a dimension scale identifier for dimension %d for %s.\n',j, vinfo.Name);
            end
            [dname,dcount,ddatatype,dnattrs,status] = hdfsd('diminfo',dim_id); %#ok<ASGLU>
            if status < 0
                error('SNCTOOLS:nc_info:hdf4Numeric:diminfoFailed', ...
                    'Unable to get information about dimension scale %d for %s.\n',j, vinfo.Name);
            end
            vinfo.Dimension{j+1} = dname;
            
            % inf means unlimited, but currently zero.
            if isinf(dcount)
                if isinf(dim_sizes(j+1))
                    vinfo.Size(j+1) = 0;
                else
                    vinfo.Size(j+1) = dim_sizes(j+1);
                end
            else
                vinfo.Size(j+1) = dcount;
            end
        end
    end
    
    if getpref('SNCTOOLS','PRESERVE_FVD',false)
        vinfo.Dimension = fliplr(vinfo.Dimension);
        vinfo.Size = fliplr(vinfo.Size);
    end
    
    
    Attribute = [];
    if nattrs > 0
    
        Attribute = repmat(struct('Name','','Datatype','','Value',0),nattrs,1);
        for j = 0:nattrs-1
            [name,atype,acount,status] = hdfsd('attrinfo',sds_id,j); %#ok<ASGLU>
            if status < 0
                error('SNCTOOLS:getvarinfo:hdf4:attrinfoFailed', ...
                    'Could not read attribute %d.\n', j );
            end
            Attribute(j+1).Name = name;
            
            [Attribute(j+1).Value, status] = hdfsd('readattr',sds_id,j);
            if status < 0
                error('SNCTOOLS:getvarinfo:hdf4:readattrFailed', ...
                    'Could not read attribute %d.\n',j );
            end
            Attribute(j+1).Datatype = class(Attribute(j+1).Value);
        end
    
    end
    vinfo.Attribute = Attribute;

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
    error('SNCTOOLS:nc_info:hdf4:endaccessFailed', ...
        'Unable to end access to %s.\n', varname);
end
status = hdfsd('end',sd_id);
if status < 0
    error('SNCTOOLS:nc_info:hdf4:endaccessFailed', ...
        'Unable to end access to %s.\n', filename);
end
    

