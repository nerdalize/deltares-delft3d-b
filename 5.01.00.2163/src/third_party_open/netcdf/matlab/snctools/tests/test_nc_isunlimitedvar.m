function test_nc_isunlimitedvar(mode)

if nargin < 1
	mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_ISUNLIMITEDVAR ...' );

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'netcdf-3'
		ncfile = fullfile(testroot, 'testdata/full.nc');
		run_all_tests(ncfile);
		run_negative_tests(ncfile);
	case 'hdf4'
		ncfile = fullfile(testroot, 'testdata/full.hdf');
		run_all_tests(ncfile);
		run_negative_tests(ncfile);
	case 'netcdf4-classic'
		ncfile = fullfile(testroot, 'testdata/full-4.nc');
		run_negative_tests(ncfile);
end

fprintf('OK\n');

return










%--------------------------------------------------------------------------
function run_all_tests ( ncfile )
test_not_unlimited (ncfile);
test_1D_unlimited (ncfile);
test_2D_unlimited (ncfile);







%--------------------------------------------------------------------------
function test_2D_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 't3' );
if ( ~b  )
	error('incorrect result.');
end

return






%--------------------------------------------------------------------------
function run_negative_tests(ncfile)
% 1st set of tests, routine should fail
% test 1:  no input arguments
% test 2:  1 input
% test 3:  too many inputs
% test 4:  both inputs are not character
% test 5:  not a netcdf file
% test 6:  no such var
%
% 2nd set of tests, routine should succeed
% test 9:  given variable is not an unlimited variable
% test 10:  given 1D variable is an unlimited variable
% test 11:  given 2D variable is an unlimited variable


test_no_inputs; 
test_only_one_input (ncfile); 
test_too_many_inputs (ncfile); 
test_2nd_input_not_char (ncfile); 
test_not_netcdf; 
test_no_such_var (ncfile); 










%--------------------------------------------------------------------------
function test_no_inputs (  )

try
	nc_isunlimitedvar;
catch %#ok<CTCH>
    return
end









%--------------------------------------------------------------------------
function test_only_one_input ( ncfile )

try
	nc_isunlimitedvar ( ncfile );
catch %#ok<CTCH>
    return
end
error('failed');











%--------------------------------------------------------------------------
function test_too_many_inputs ( ncfile )

try
	nc_isunlimitedvar ( ncfile, 'blah', 'blah2' );
catch %#ok<CTCH>
    return
end
error('FAILED');









%--------------------------------------------------------------------------
function test_2nd_input_not_char ( ncfile )



try
	nc_isunlimitedvar ( ncfile, 5 );
catch %#ok<CTCH>
    return
end
error('failed');








%--------------------------------------------------------------------------
function test_not_netcdf  (  )

try
	nc_isunlimitedvar ( 'test_nc_isunlimitedvar.m', 't' );
catch %#ok<CTCH>
    return
end
error('failed');











%--------------------------------------------------------------------------
function test_no_such_var ( ncfile )

b = nc_isunlimitedvar ( ncfile, 'tt' );
if b 
    error ( 'succeeded when it should have failed.\n' );
end
return
















%--------------------------------------------------------------------------
function test_not_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 's' );
if b
	error( 'incorrect result.');
end
return







%--------------------------------------------------------------------------
function test_1D_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 't2' );
if ~b
	error( 'incorrect result.');
end
return









