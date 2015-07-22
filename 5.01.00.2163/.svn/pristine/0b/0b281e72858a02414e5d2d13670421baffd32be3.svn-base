function nc_dump(file_name, varargin)
%nc_dump  Print netCDF metadata.
%   NC_DUMP(NCFILE) displays metadata about the netCDF file NCFILE.  NC_DUMP
%   is a counterpart to the 'ncdump' utility that comes with the netCDF
%   library.
%
%   NC_DUMP(NCFILE,LOCATION) displays metadata for LOCATION, which may be
%   either a variable in the root group or a netcdf-4 group.
%
%   NC_DUMP(NCFILE,<LOCATION>,fid) prints output to file opened 
%   with fid = fopen(...) instead of to screen (default fid=1: screen).
%   NC_DUMP(NCFILE,LOCATION,<'fname'>) prints output to new file 'fname'.
%
%   If the preference 'USE_JAVA' is set to true and netcdf-java is on the
%   javaclasspath, NC_DUMP can also display metadata for GRIB2 files and 
%   OPeNDAP URLS files as if they were netCDF files.
%
%   Setting the preference 'PRESERVE_FVD' to true will compel MATLAB to 
%   display the dimensions in the opposite order from what the C utility 
%   ncdump displays.  
% 
%   Example:  This example file is shipped with R2008b.
%       nc_dump('example.nc');
%  
%   Example:  Display metadata for an OPeNDAP URL.  This requires the
%   netcdf-java backend.
%       url = 'http://coast-enviro.er.usgs.gov/models/share/balop.nc';
%       nc_dump(url);
%
%   See also nc_info.

location = ''; 
fid = 1;
switch(nargin)
    case 2
        if ischar(varargin{1})
            location = varargin{1};
        else
            fid = varargin{1};
        end
        
    case 3
        location = varargin{1};
        fid = varargin{2};

end

if ischar(fid)
    close_fid = 1;    
    fid = fopen(fid,'w');
else
    close_fid = 0;    
end

info = nc_info ( file_name );

fprintf (fid, '%s %s {\n', info.Format, info.Filename );

if strcmp(info.Format,'NetCDF-4')
    dump_group(info,true,location,fid);
else
    dump_group(info,false,location,fid);
end

if close_fid
    fclose(fid);
end

return;

%--------------------------------------------------------------------------
function dump_group(group,dump_group_name,restricted_variable,fid)

if dump_group_name
    fprintf(fid,'\nGroup ''%s'' {\n', group.Name);
end

dump_dimension_metadata(group, fid );
dump_variables(group.Dataset,restricted_variable,fid);
if isempty(restricted_variable)
    if isfield(group,'Name') && ~strcmp(group.Name,'/')
        dump_group_attributes(group,fid,false);
    else
        dump_group_attributes(group,fid,true);
    end
end


if isfield(group,'Group') && numel(group.Group) > 0 
    for j = 1:numel(group.Group)
        dump_group(group.Group(j),dump_group_name,restricted_variable,fid);
    end
end

if dump_group_name
    fprintf(fid,'} End Group ''%s''\n', group.Name);
else
    fprintf(fid,'}\n');
end
fprintf('\n');

return


%--------------------------------------------------------------------------
function dump_dimension_metadata(info,fid)

if isfield(info,'Dimension' )
    num_dims = numel(info.Dimension);
else
    num_dims = 0;
end

fprintf(fid,'\ndimensions:\n');
for j = 1:num_dims
    if info.Dimension(j).Unlimited
        fprintf(fid,'\t%s = UNLIMITED ; (%i currently)\n', ...
                 deblank(info.Dimension(j).Name), info.Dimension(j).Length );
    else
        fprintf(fid, '\t%s = %i ;\n', info.Dimension(j).Name,info.Dimension(j).Length );
    end
end
fprintf(fid,'\n');

return


%--------------------------------------------------------------------------
function dump_variables(Dataset,restricted_variable,fid)

fprintf (fid,'variables:\n' );

pfvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pfvd == 0;
   fprintf (fid,'\t// Preference ''PRESERVE_FVD'':  false,\n' );
   fprintf (fid,'\t// dimensions consistent with ncBrowse, not with native MATLAB netcdf package.\n' );
else
   fprintf (fid,'\t// Preference ''PRESERVE_FVD'':  true,\n' );
   fprintf (fid,'\t// dimensions consistent with native MATLAB netcdf package, not with ncBrowse.\n' );
end


for j = 1:numel(Dataset)

    if ~isempty(restricted_variable)
        if ~strcmp(restricted_variable,Dataset(j).Name)
            continue
        end
    end

    dump_single_variable(Dataset(j),fid);

end

fprintf (fid,'\n' );


%--------------------------------------------------------------------------
function dump_single_variable ( var_metadata , fid )

fprintf(fid,'\t%s ', var_metadata.Datatype);

fprintf(fid,'%s', var_metadata.Name );

if isempty(var_metadata.Dimension) 
    fprintf (fid, '([]), ' );
else
    fprintf (fid, '(%s', var_metadata.Dimension{1} );
    for k = 2:length(var_metadata.Size)
        fprintf (fid, ',%s', var_metadata.Dimension{k} );
    end
    fprintf (fid, '), ');
end


if isempty(var_metadata.Dimension)
    fprintf (fid, 'shape = [1]' );
else
    fprintf (fid, 'shape = [%d', var_metadata.Size(1)  );
    for k = 2:length(var_metadata.Size)
        fprintf (fid, ' %d', var_metadata.Size(k)  );
    end
    fprintf (fid, ']');
end

fprintf (fid,'\n');

if isfield(var_metadata, 'Chunking')
    if ~isempty(var_metadata.Chunking)
        fprintf(fid,'\t\tChunking: [ ' );
        fprintf(fid,'%d ', var_metadata.Chunking(1));
        for j = 2:numel(var_metadata.Chunking)
            fprintf(fid,'%d ', var_metadata.Chunking(j));
        end
        fprintf(fid,']\n');
    end
end

if isfield(var_metadata,'Deflate') && ~isempty(var_metadata.Deflate) ...
        && isfield(var_metadata, 'Chunking') && ~isempty(var_metadata.Chunking)
    fprintf(fid,'\t\tDeflate Level:  %d\n', var_metadata.Deflate);
end

% Now do all attributes for each variable.
num_atts = length(var_metadata.Attribute);
for k = 1:num_atts
    dump_single_attribute(var_metadata.Attribute(k),var_metadata.Name,fid);
end

return


%--------------------------------------------------------------------------
function dump_single_attribute ( attribute, varname , fid )

if isnumeric(varname)
   fid = varname;
   clear varname
end

switch ( attribute.Datatype )
    case ''
        att_val = '';
        att_type = 'NC_NAT';
    case 'int8'
        att_val = sprintf ('%d ', fix(attribute.Value) );
        att_type = 'b';
    case 'uint8'
        att_val = sprintf ('%d ', fix(attribute.Value) );
        att_type = 'ub';        
    case 'char'
        att_val = sprintf ('"%s" ', attribute.Value );
        att_type = '';
    case 'int16'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 's';
     case 'uint16'
        att_val = sprintf ('%d ', attribute.Value );
        att_type = 'us';       
    case 'int32'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'd';
    case 'uint32'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'ud'; 
     case 'int64'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'L';
    case 'uint64'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'UL';        
    case 'single'
        att_val = sprintf ('%f ', attribute.Value );
        att_type = 'f';
    case 'double'
        att_val = sprintf ('%g ', attribute.Value );
        att_type = '';
    otherwise
        error('unhandled datatype "%s"', attribute.Datatype);
end

if ~exist('varname','var')
    fprintf(fid, '\t\t:%s = %s%s\n', ...
         attribute.Name, att_val, att_type);
else
    fprintf(fid, '\t\t%s:%s = %s%s\n', ...
         varname, attribute.Name, att_val, att_type);
end

return
    
%--------------------------------------------------------------------------
function dump_group_attributes(group,fid,is_global)


if isfield(group,'Attribute')
    num_atts = numel(group.Attribute);
else
    num_atts = 0;
end

if num_atts > 0
    if is_global
        fprintf (fid, '//global Attributes:\n' );
    else 
        fprintf(fid,'//group Attributes:\n');
    end
end

for k = 1:num_atts
   dump_single_attribute(group.Attribute(k),fid);
end


fprintf (fid, '\n' );

return
