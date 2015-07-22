function Attribute = snc_java_bundle_atts ( j_att_list )
% SNC_JAVA_BUNDLE_ATTS:  returns metadata about netcdf attributes
%
% USAGE:  Attribute = snc_java_bundle_atts ( j_att_list );
%
% PARAMETERS:
% Input:
%     j_att_list:
%         Of type "java.util.ArrayList".  Each list member is of type
%         "ucar.nc2.Attribute"
% Output:
%     Attribute:
%         Structure array of attribute metadata.  The fields are 
%         
%         Name
%         Nctype
%         Value

j_att_iterator = j_att_list.listIterator();
j = 0;

Attribute = [];

while 1
    
    % This throws an exception when we've reached the end of the list.
    try
        jatt = j_att_iterator.next();
    catch %#ok<CTCH>
        break;
    end
    
    j = j + 1;
    
    Attribute(j).Name = char(jatt.getName());
    
    datatype = char(jatt.getDataType().toString());
    switch ( datatype )
        case 'double'
            Attribute(j).Nctype = 6; %#ok<*AGROW>
            Attribute(j).Datatype = 'double';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = double(values)';
            
        case 'float'
            Attribute(j).Nctype = 5;
            Attribute(j).Datatype = 'single';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = double(values)';
            
        case { 'String', 'char' }
            Attribute(j).Nctype = 2;
            Attribute(j).Datatype = 'char';
            
            Attribute(j).Value = char ( jatt.getStringValue() );
            
        case 'byte'
            Attribute(j).Nctype = 1;
            Attribute(j).Datatype = 'int8';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = double(values)';
            
        case 'short'
            Attribute(j).Nctype = 3;
            Attribute(j).Datatype = 'int16';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = double(values)';
            
        case 'int'
            Attribute(j).Nctype = 4;
            Attribute(j).Datatype = 'int32';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = double(values)';
            
        case 'long'
            Attribute(j).Nctype = 4;
            Attribute(j).Datatype = 'int64';
            
            j_array = jatt.getValues();
            values = j_array.copyTo1DJavaArray();
            Attribute(j).Value = int64(values)';
            
        otherwise
            error ( 'SNCTOOLS:snc_java_bundle_atts:unhandledDatatype', ...
                'unhandled attribute datatype ''%s''\n', datatype );
    end
    
end

return

