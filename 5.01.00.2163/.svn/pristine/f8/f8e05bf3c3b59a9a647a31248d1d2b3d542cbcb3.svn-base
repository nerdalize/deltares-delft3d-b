function test_nc_attget_neg(ncfile)

v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a'}
        fprintf('No negative tests run on %s...  ',v);
        return
    case {'2007b','2008a','2008b','2009a','2009b','2010a'}
		test_get_att_not_there_classic ( ncfile );
        return
    otherwise
		test_get_att_not_there_classic ( ncfile );
		test_get_att_not_there_enhanced;
        return
end

return;







%--------------------------------------------------------------------------
function test_get_att_not_there_enhanced()

ncfile = 'example.nc';
global ignore_eids;

try
	nc_attget(ncfile,'z_double', 'test_double_att' );
catch me
    return;            
end
error('failed');









%--------------------------------------------------------------------------
function test_get_att_not_there_classic ( ncfile )

global ignore_eids;

try
	nc_attget ( ncfile, 'z_double', 'test_double_att' );
catch me
    if ignore_eids
        return
    end
    switch(me.identifier)
        case { 'MATLAB:netcdf:inqAtt:attributeNotFound', ...
                'MATLAB:netcdf:inqAtt:enotatt:attributeNotFound', ...
                'SNCTOOLS:NC_ATTGET:MEXNC:INQ_ATTTYPE', ...
                'SNCTOOLS:attget:hdf4:findattr', ...
                'SNCTOOLS:attget:java:attributeNotFound'}
            return
        otherwise
            rethrow(me);
    end
                
end
error('failed');










