function Dataset = snc_java_varid_info ( jvarid )
% SNC_JAVA_VARID_INFO:  returns metadata structure for a netcdf variable
%
% This function is private to SNCTOOLS.  It is called by nc_info and
% nc_getvarinfo, and uses the java API.
%
% USAGE:   Dataset = snc_java_varid_info ( jvarid );
% 
% PARAMETERS:
% Input:
%     jvarid:  
%         of type ucar.nc2.dods.DODSVariable
% Output:
%     Dataset:
%         array of metadata structures.  The fields are
%         
%         Name
%         Nctype
%         Unlimited
%         Dimension
%         Attribute

Dataset.Name = char ( jvarid.getName() );

% Get the datatype, store as an integer
datatype = char(jvarid.getDataType().toString());
switch ( datatype )
    case 'double'
        Dataset.Nctype = nc_double;
        Dataset.Datatype = datatype;
    case 'float'
        Dataset.Nctype = nc_float;
        Dataset.Datatype = 'single';
    case 'int'
        Dataset.Nctype = nc_int;
        Dataset.Datatype = 'int32';
    case 'short'
        Dataset.Nctype = nc_short;
        Dataset.Datatype = 'int16';
        
        % So apparently, DODSNetcdfFile returns 'String', while
        % NetcdfFile returns 'char'???
    case { 'String', 'char' }
        Dataset.Nctype = nc_char;
        Dataset.Datatype = 'char';
    case 'byte'
        Dataset.Nctype = nc_byte;
        Dataset.Datatype = 'int8';
    otherwise
        error ( 'SNCTOOLS:varinfo:unhandledDatatype', ...
            '%s:  unhandled datatype ''%s''\n', datatype );
end

% determine if it is unlimited or not
Dataset.Unlimited = double ( jvarid.isUnlimited() );

% Retrieve the dimensions
dims = jvarid.getDimensions();
nvdims = dims.size();
Dimension = cell(1,nvdims);
for j = 1:nvdims
	theDim = jvarid.getDimension(j-1);
	Dimension{j} = char ( theDim.getName() );
end
Dataset.Dimension = Dimension;

% Get the size of the variable
if nvdims == 0
	Dataset.Size = 1;
else
	Size = double ( jvarid.getShape() );
	Dataset.Size = Size';
end

if getpref('SNCTOOLS','PRESERVE_FVD',false)
	Dataset.Dimension = fliplr(Dataset.Dimension);
	Dataset.Size = fliplr(Dataset.Size);
end

% Get the list of attributes.
j_att_list = jvarid.getAttributes();
Dataset.Attribute = snc_java_bundle_atts ( j_att_list );

return

