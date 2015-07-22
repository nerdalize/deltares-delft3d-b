function test_nc_attget(mode)

if nargin < 1
	% Run usual tests.
	mode = 'nc-3';
end


fprintf ('\t\tTesting NC_ATTGET...' );

switch(mode)
	case 'nc-3'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget.nc');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);

	case 'netcdf4-classic'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget-4.nc');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);

	case 'hdf'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget.hdf');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);

	case 'grib'
		testroot = fileparts(mfilename('fullpath'));
		gribfile = fullfile(testroot,'testdata',...
		    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2');
	    run_local_grib_tests(gribfile);

	case 'http'
	    run_http_tests;
end

fprintf('OK\n');




%--------------------------------------------------------------------------
function run_local_nc_tests ( ncfile )

test_retrieveDoubleAttribute ( ncfile );
test_retrieveFloatAttribute ( ncfile );
test_retrieveIntAttribute ( ncfile );
test_retrieveShortAttribute ( ncfile );
test_retrieveUint8Attribute ( ncfile );
test_retrieveInt8Attribute ( ncfile );
test_retrieveTextAttribute ( ncfile );

test_retrieveGlobalAttribute_empty ( ncfile );
test_writeRetrieveGlobalAttributeMinusOne ( ncfile );
test_writeRetrieveGlobalAttributeNcGlobal ( ncfile );
test_writeRetrieveGlobalAttributeGlobalName ( ncfile );

return;











%--------------------------------------------------------------------------
function test_retrieveIntAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_int_att' );
if ( ~strcmp(class(attvalue), 'int32' ) )
	error('class of retrieved attribute was not int32.');
end
if ( attvalue ~= int32(3) )
	error('retrieved attribute differs from what was written.');
end

return










%--------------------------------------------------------------------------
function test_retrieveShortAttribute ( ncfile )


attvalue = nc_attget ( ncfile, 'x_db', 'test_short_att' );
if ( ~strcmp(class(attvalue), 'int16' ) )
	error('class of retrieved attribute was not int16.');
end
if ( length(attvalue) ~= 2 )
	error('retrieved attribute length differs from what was written.');
end
if ( any(double(attvalue) - [5 7])  )
	error('retrieved attribute differs from what was written.');
end

return








%--------------------------------------------------------------------------
function test_retrieveUint8Attribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_uchar_att' );
if ( ~strcmp(class(attvalue), 'int8' ) )
	error('class of retrieved attribute was not int8.');
end
if ( uint8(attvalue) ~= uint8(100) )
	error('retrieved attribute differs from what was written.');
end

return




%--------------------------------------------------------------------------
function test_retrieveInt8Attribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_schar_att' );
if ( ~strcmp(class(attvalue), 'int8' ) )
	error('class of retrieved attribute was not int8.');
end
if ( attvalue ~= int8(-100) )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveTextAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_text_att' );
if ( ~ischar(attvalue ) )
	error('class of retrieved attribute was not char.');
end

if ( ~strcmp(attvalue,'abcdefghijklmnopqrstuvwxyz') )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveGlobalAttribute_empty ( ncfile )

warning ( 'off', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseEmptyVarname' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseGlobalVarname' );

attvalue = nc_attget ( ncfile, -1, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

warning ( 'on', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseEmptyVarname' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseGlobalVarname' );

return





%--------------------------------------------------------------------------
function test_writeRetrieveGlobalAttributeMinusOne ( ncfile )

attvalue = nc_attget ( ncfile, -1, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return





%--------------------------------------------------------------------------
function test_writeRetrieveGlobalAttributeNcGlobal ( ncfile )

attvalue = nc_attget ( ncfile, -1, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return 






%--------------------------------------------------------------------------
function test_writeRetrieveGlobalAttributeGlobalName ( ncfile )

warning ( 'off', 'SNCTOOLS:nc_attget:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );

attvalue = nc_attget ( ncfile, 'GLOBAL', 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

warning ( 'on', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );
warning ( 'on', 'SNCTOOLS:nc_attget:doNotUseGlobalString' );

return
















%--------------------------------------------------------------------------
function test_retrieveDoubleAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveFloatAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_float_att' );
if ( ~strcmp(class(attvalue), 'single' ) )
	error('class of retrieved attribute was not single.');
end
if ( abs(double(attvalue) - 3.14159) > 3e-6 )
	error('retrieved attribute differs from what was written.');
end

return




%--------------------------------------------------------------------------
function run_local_grib_tests(gribfile)

test_grib2_char(gribfile);

return;







%--------------------------------------------------------------------------
function test_grib2_char(gribfile)

act_data = nc_attget(gribfile,-1,'Conventions');
exp_data = 'CF-1.4';
if ~strcmp(act_data,exp_data)
    error('failed'); 
end
return




%--------------------------------------------------------------------------
function run_http_tests()

test_retrieveAttribute_HTTP;
test_retrieveAttribute_http_jncid;
return







%--------------------------------------------------------------------------
function test_retrieveAttribute_HTTP ()

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

w = nc_attget ( url, 'w', 'valid_range' );
if ~strcmp(class(w),'single')
	error ( 'Class of retrieve attribute was not single' );
end
if (abs(double(w(2)) - 0.5) > eps)
	error ( 'valid max did not match' );
end
if (abs(double(w(1)) + 0.5) > eps)
	error ( 'valid max did not match' );
end
return


%--------------------------------------------------------------------------
function test_retrieveAttribute_http_jncid ()

import ucar.nc2.dods.*     
import ucar.nc2.*          

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';
jncid = NetcdfFile.open(url);
                           

w = nc_attget (jncid, 'w', 'valid_range' );
if ~strcmp(class(w),'single')
	error ( 'Class of retrieve attribute was not single' );
end
if (abs(double(w(2)) - 0.5) > eps)
	error ( 'valid max did not match' );
end
if (abs(double(w(1)) + 0.5) > eps)
	error ( 'valid max did not match' );
end
close(jncid);
return





%--------------------------------------------------------------------------
function run_negative_tests(ncfile)
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a'}
        fprintf('No negative tests run on %s...  ',v);
    otherwise
		test_nc_attget_neg(ncfile);
end
