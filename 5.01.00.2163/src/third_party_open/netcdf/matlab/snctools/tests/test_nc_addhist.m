function test_nc_addhist(mode)

fprintf('\t\tTesting NC_ADDHIST... ' );

ncfile = 'foo.nc';
if nargin < 1
    mode = nc_clobber_mode;
end

nc_create_empty(ncfile,mode);

test_add_global_history(ncfile,mode);          
test_add_global_history_twice(ncfile,mode); 

run_negative_tests;

fprintf('OK\n');




%--------------------------------------------------------------------------
function test_add_global_history(ncfile,mode)
% Try to add a generic string.

nc_create_empty(ncfile,mode);
histblurb = 'blah';
nc_addhist ( ncfile, histblurb );

hista = nc_attget ( ncfile, nc_global, 'history' );
s = strfind(hista, histblurb );
if isempty(s)
	error('history attribute did not contain first attribution.');
end
return




%--------------------------------------------------------------------------
function test_add_global_history_twice ( ncfile,mode )
% Try to add a generic string.  Twice.

nc_create_empty ( ncfile,mode);
histblurb = 'blah a';
nc_addhist ( ncfile, histblurb );
histblurb2 = 'blah b';
nc_addhist ( ncfile, histblurb2 );
histatt = nc_attget ( ncfile, nc_global, 'history' );
s = strfind(histatt, histblurb2 );
if isempty(s)
	error('history attribute did not contain second attribution');
end
return



%--------------------------------------------------------------------------
function run_negative_tests()

ncfile = 'foo.nc';

test_no_inputs;                              
test_not_netcdf_file ( ncfile );           
test_2nd_input_not_char ( ncfile );       
test_3rd_input_not_char ( ncfile );      

return


%--------------------------------------------------------------------------
function test_no_inputs (  )
% Negative test.  No inputs should trigger an error.
try
	nc_addhist;
catch %#ok<CTCH>
	return
end
error('succeeded when it should have failed.' );



%--------------------------------------------------------------------------
function test_not_netcdf_file ( ncfile )
% Negative test.  If it's not a netcdf file, it should trigger an error.

nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_addhist ( 'asdfjsadjfsadlkjfsa;ljf;l', 'test' );
catch %#ok<CTCH>
	return
end
error ('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_2nd_input_not_char ( ncfile )
% Negative test.  The history blurb value should be character.

nc_create_empty ( ncfile, nc_clobber_mode );
try
	nc_addhist ( ncfile, 5 );
catch %#ok<CTCH>
	return
end
error ('succeeded when it should have failed.');




%--------------------------------------------------------------------------
function test_3rd_input_not_char ( ncfile )
% Negative test.  The history blurb value should be character.

nc_create_empty ( ncfile, nc_clobber_mode );
nc_add_dimension ( ncfile, 't', 0 );
clear varstruct;
varstruct.Name = 'T';
nc_addvar ( ncfile, varstruct );
try
	nc_addhist ( ncfile, 'T', 5 );
catch %#ok<CTCH>
	return
end
error ('succeeded when it should have failed.');




