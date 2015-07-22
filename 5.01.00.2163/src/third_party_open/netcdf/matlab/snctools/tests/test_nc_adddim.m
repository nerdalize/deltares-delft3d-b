function test_nc_adddim(mode)

fprintf('\t\tTesting NC_ADDDIM ...  ' );

if nargin < 1
    mode = nc_clobber_mode;
end
ncfile = 'foo.nc';
test_add_regular_dimension(ncfile,mode);                 
test_add_unlimited (ncfile,mode);                         
test_dimension_already_exists(ncfile,mode);  

run_negative_tests;

fprintf('OK\n');

return







%--------------------------------------------------------------------------
function test_add_regular_dimension(ncfile,mode)
% Positive test:  add a fixed-length dimension.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 5 );

%
% Now check that the new dimension are there.
d = nc_getdiminfo ( ncfile, 't' );
if ( ~strcmp(d.Name,'t') )
	error ( 'nc_adddim failed on fixed dimension add name');
end
if ( d.Length ~= 5 )
	error ( 'nc_adddim failed on fixed dimension add length');
end
if ( d.Unlimited ~= 0  )
	error ( 'nc_adddim incorrectly classified the dimension');
end

return


















%--------------------------------------------------------------------------
function test_add_unlimited(ncfile,mode)
% Positive test:  add an unlimited dimension.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 0 );

%
% Now check that the new dimension are there.
d = nc_getdiminfo ( ncfile, 't' );
if ( ~strcmp(d.Name,'t') )
	error ( 'nc_adddim failed on fixed dimension add name');
end
if ( d.Length ~= 0 )
	error ( 'nc_adddim failed on fixed dimension add length');
end
if ( d.Unlimited ~= 1  )
	error ( 'nc_adddim incorrectly classified the dimension');
end

return


















%--------------------------------------------------------------------------
function test_dimension_already_exists(ncfile,mode)
% Negative test:  try to add a dimension that is already there.  Should 
% error out.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 0 );
try
	nc_adddim ( ncfile, 't', 0 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






%--------------------------------------------------------------------------
function run_negative_tests()
ncfile = 'foo4.nc';
test_no_inputs ();                               
test_too_many_inputs ( ncfile );                 
test_not_netcdf_file ( ncfile );                 
test_2nd_input_not_char ( ncfile );              
test_3rd_input_not_numeric ( ncfile );           
test_3rd_input_negative ( ncfile );              


%--------------------------------------------------------------------------
function test_no_inputs ()
% NC_ADDDIM needs 3 arguments. 
try
	nc_adddim;
catch %#ok<CTCH>
	return
end
error('succeeded when it should have failed');






%--------------------------------------------------------------------------
function test_too_many_inputs ( ncfile )
% NC_ADDDIM needs only 3 arguments. 


nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_adddim ( ncfile, 'x', 10, 12 );
catch %#ok<CTCH>
	return
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_not_netcdf_file ( ncfile )
% The file must be a netcdf file

fid = fopen(ncfile,'wb');
fwrite(fid,magic(5),'integer*4');
fclose(fid);

try
	nc_adddim(ncfile,'x',3);
catch %#ok<CTCH>
	return
end
error('Failed to catch non-netcdf file.');












%--------------------------------------------------------------------------
function test_2nd_input_not_char ( ncfile )
% dimension name must be char.
nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_adddim ( ncfile, 3, 3 );
catch %#ok<CTCH>
    return
end
error ('succeeded when it should have failed.');












%--------------------------------------------------------------------------
function test_3rd_input_not_numeric ( ncfile )
% The dimension length must be numeric

% test 5:  3rd input not numeric
nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_adddim ( ncfile, 't', 't' );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_3rd_input_negative ( ncfile )
% The dimension length must be non-negative.

nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_adddim ( ncfile, 't', -1 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






