function test_nc_varsize(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_VARSIZE ...  ');

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'nc-3'
		ncfile = fullfile(testroot,'testdata/full.nc');
		run_local_tests(ncfile);
	case 'netcdf4-classic'
		ncfile = fullfile(testroot,'testdata/full-4.nc');
		run_local_tests(ncfile);
end

run_negative_tests;

fprintf('OK\n');
return



%--------------------------------------------------------------------------
function run_local_tests(ncfile)
test_singleton (ncfile);
test_1D (ncfile);
test_1D_unlimited_empty (ncfile);
test_2D (ncfile);



%--------------------------------------------------------------------------
function run_negative_tests()

v = version('-release');
switch(v)
	case{'14','2006a','2006b'}
	    fprintf('Some negative tests filtered out on version %s... ', v);
    otherwise
		test_nc_varsize_neg;
end


%--------------------------------------------------------------------------
function test_singleton ( ncfile )

varsize = nc_varsize ( ncfile, 's' );
if ( varsize ~= 1 )
	error ( 'varsize was not right.');
end
return









%--------------------------------------------------------------------------
function test_1D ( ncfile )

varsize = nc_varsize ( ncfile, 's' );
if ( varsize ~= 1 )
	error ( 'varsize was not right.');
end
return











%--------------------------------------------------------------------------
function test_1D_unlimited_empty ( ncfile )

varsize = nc_varsize ( ncfile, 't3' );
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    if ( varsize(1) ~= 1 ) && ( varsize(2) ~= 0 )
        error ( '%s:  varsize was not right.\n', mfilename );
    end
else
    if ( varsize(1) ~= 0 ) && ( varsize(2) ~= 1 )
        error ( '%s:  varsize was not right.\n', mfilename );
    end
end
return










%--------------------------------------------------------------------------
function test_2D ( ncfile )


varsize = nc_varsize ( ncfile, 'v' );
if ( varsize(1) ~= 1 ) && ( varsize(2) ~= 1 )
	error ( '%s:  varsize was not right.\n', mfilename );
end
return









%--------------------------------------------------------------------------
function test_nc_varsize_neg()
% negative testing for nc_varsize
% test 1:  no input arguments
% test 2:  1 input
% test 3:  too many inputs
% test 4:  inputs are not all character
% test 5:  not a netcdf file
% test 6:  empty netcdf file
% test 7:  given variable is not present
testroot = fileparts(mfilename('fullpath'));

ncfile = fullfile(testroot, 'testdata/empty.nc' );

test_no_inputs;
test_only_one_input (ncfile);
test_too_many_inputs (ncfile);
test_varname_not_char (ncfile);
test_not_netcdf;
test_empty (ncfile);

ncfile = fullfile(testroot, 'testdata/full.nc' );
test_var_not_present (ncfile);
return



%--------------------------------------------------------------------------
function test_no_inputs ()

try
	nc_varsize;
catch %#ok<CTCH>
    return
end
error('failed');













%--------------------------------------------------------------------------
function test_only_one_input( ncfile )

try
	nc_varsize ( ncfile );
catch %#ok<CTCH>
    return
end
error('failed');











%--------------------------------------------------------------------------
function test_too_many_inputs( ncfile )

try
	nc_varsize ( ncfile, 'x', 'y' );
catch %#ok<CTCH>
    return
end
error('failed');











%--------------------------------------------------------------------------
function test_varname_not_char ( ncfile )

try
	nc_varsize ( ncfile, 1 );
catch %#ok<CTCH>
    return
end
error('failed');












%--------------------------------------------------------------------------
function test_not_netcdf ( )

% test 5:  not a netcdf file
try
	nc_varsize ( mfilename, 't' );
catch %#ok<CTCH>
    return
end
error('failed');















%--------------------------------------------------------------------------
function test_empty ( ncfile )

% no such variable
try
	nc_varsize ( ncfile, 't' );
catch %#ok<CTCH>
    return
end
error('failed');












%--------------------------------------------------------------------------
function test_var_not_present ( ncfile )

try
	nc_varsize ( ncfile, 'xyz' );
catch %#ok<CTCH>
    return
end
error('failed');






