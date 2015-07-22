function values = nc_attget_java(ncfile, varname, attribute_name )
% NC_ATTGET_JAVA:  This function retrieves an attribute using the java API

import ucar.nc2.dods.*    
import ucar.nc2.*         

close_it = true;

% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif exist(ncfile,'file')
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
	catch %#ok<CTCH>
		try
            jncid = snc_opendap_open(ncfile);
		catch %#ok<CTCH>
			error ( 'SNCTOOLS:nc_varget_java:fileOpenFailure', ...
                'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                ncfile);
		end
	end
end

jatt = get_attribute_from_variable ( jncid, varname, attribute_name );

% Retrieve the values.  Convert it to the appropriate matlab datatype.
if ( jatt.isString() ) 
    values = jatt.getStringValue();
    values = char ( values );
	if close_it
    	close(jncid);
	end
	return
end

% Ok, so it's numeric data.
% convert it to a numeric array.
j_array = jatt.getValues();
values = j_array.copyTo1DJavaArray();
values = values';

theDataTypeString = char ( jatt.getDataType.toString() ) ;
switch ( theDataTypeString )
case 'double'
    values = double(values);
case 'float'
    values = single(values);
case 'int'
    values = int32(values);
case 'short'
    values = int16(values);
case 'byte'
    values = int8(values);
otherwise
	if close_it
    	close(jncid);
	end
    error ( 'SNCTOOLS:NC_ATTGET:badDatatype', ...
		'Unhandled attribute type ''%s'' for attribute ''%s''', ...
		theDataTypeString, attribute_name );
end

if close_it
	close(jncid);
end

return





%--------------------------------------------------------------------------
function jatt = get_attribute_from_variable ( jncid, varname, attribute_name )

if ischar ( varname ) && (isempty(varname))

	% The user passed in ''.  That means NC_GLOBAL.
	warning ( 'SNCTOOLS:nc_attget:java:doNotUseGlobalString', ...
	          'Please consider using the m-file NC_GLOBAL.M instead of the empty string.' );
    jatt = jncid.findGlobalAttribute ( attribute_name );

elseif ischar ( varname ) && (strcmpi(varname,'global'))

	% The user passed in 'global'.   Is there a variable named 'global'?
    jvarid = jncid.findVariable(varname);
	if isempty(jvarid)
		% No, it's a global attribute.
		warning ( 'SNCTOOLS:nc_attget:java:doNotUseGlobalString', ...
			'Please consider using the m-file NC_GLOBAL.M instead of the empty string.' );
    	jatt = jncid.findGlobalAttribute ( attribute_name );
	else
    	jatt = jvarid.findAttribute ( attribute_name );
	end

elseif ischar ( varname )

    % Ok, it was just a regular variable.
    jvarid = jncid.findVariable(varname);
    jatt = jvarid.findAttribute ( attribute_name );

else

    % The user passed a numeric identifier for the variable.  
    % Assume that this means a global attribute.
    jatt = jncid.findGlobalAttribute ( attribute_name );
end

if isempty(jatt)
    error ( 'SNCTOOLS:attget:java:attributeNotFound', ...
		'Could not locate attribute %s', attribute_name );
end


