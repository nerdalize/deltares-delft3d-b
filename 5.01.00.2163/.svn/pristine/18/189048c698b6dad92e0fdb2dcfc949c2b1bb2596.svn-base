function test_nc_info(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_INFO ...  ' );

run_negative_tests;

switch(mode)
	case 'nc-3'
		run_nc3_tests;
	case 'netcdf4-classic'
		run_nc4_tests;
	case 'http'
		run_http_tests;
end

fprintf('OK\n');

return


%--------------------------------------------------------------------------
function run_negative_tests()

test_noInputs;
test_tooManyInputs;
test_fileNotNetcdf;




%--------------------------------------------------------------------------
function run_nc4_tests()


testroot = fileparts(mfilename('fullpath'));

ncfile = [testroot '/testdata/empty-4.nc'];
test_emptyNetcdfFile(ncfile);

ncfile = [testroot '/testdata/just_one_dimension-4.nc'];
test_dimsButNoVars(ncfile);

ncfile = [testroot '/testdata/full-4.nc'];
test_smorgasborg(ncfile);

return




%--------------------------------------------------------------------------
function run_nc3_tests()


testroot = fileparts(mfilename('fullpath'));

ncfile = [testroot '/testdata/empty.nc'];
test_emptyNetcdfFile(ncfile);

ncfile = [testroot '/testdata/just_one_dimension.nc'];
test_dimsButNoVars(ncfile);

ncfile = [testroot '/testdata/full.nc'];
test_smorgasborg(ncfile);

return




%--------------------------------------------------------------------------
function test_noInputs( )
try
	nc_info;
catch %#ok<CTCH>
    return
end
error ( 'succeeded when it should have failed.\n'  );





%--------------------------------------------------------------------------
function test_tooManyInputs()

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot, 'testdata/empty.nc');
try
	nc_info ( ncfile, 'blah' );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_fileNotNetcdf()
ncfile = mfilename;
try
	nc_info ( ncfile );
catch %#ok<CTCH>
    return
end
error ( 'succeeded when it should have failed.' );







%--------------------------------------------------------------------------
function test_emptyNetcdfFile(ncfile)

nc = nc_info ( ncfile );
if ~strcmp ( nc.Filename, ncfile )
	error( 'Filename was wrong.');
end
if ( ~isempty ( nc.Dimension ) )
	error( 'Dimension was wrong.');
end
if ( ~isempty ( nc.Dataset ) )
	error( 'Dataset was wrong.');
end
if ( ~isempty ( nc.Attribute ) )
	error('Attribute was wrong.');
end
return









%--------------------------------------------------------------------------
function test_dimsButNoVars(ncfile)

nc = nc_info ( ncfile );
if ~strcmp ( nc.Filename, ncfile )
	error( 'Filename was wrong.');
end
if ( length ( nc.Dimension ) ~= 1 )
	error( 'Dimension was wrong.');
end
if ( ~isempty ( nc.Dataset ) )
	error( 'Dataset was wrong.');
end
if ( ~isempty ( nc.Attribute ) )
	error( 'Attribute was wrong.');
end
return










%--------------------------------------------------------------------------
function test_smorgasborg(ncfile)

nc = nc_info ( ncfile );
if ~strcmp ( nc.Filename, ncfile )
	error( 'Filename was wrong.');
end
if ( length ( nc.Dimension ) ~= 5 )
	error( 'Dimension was wrong.');
end
if ( length ( nc.Dataset ) ~= 6 )
	error( 'Dataset was wrong.');
end
if ( length ( nc.Attribute ) ~= 1 )
	error( 'Attribute was wrong.');
end
return






%--------------------------------------------------------------------------
function run_http_tests()

test_javaNcid;
return


%--------------------------------------------------------------------------
function test_javaNcid ()
import ucar.nc2.dods.*     
import ucar.nc2.*          

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';
jncid = NetcdfFile.open(url);
nc_info ( jncid );
close(jncid);
return



