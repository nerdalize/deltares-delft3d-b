function test_nc_varrename(mode)

if nargin < 1
	mode = nc_clobber_mode;  % netcdf-3
end

fprintf ('\t\tTesting NC_VARRENAME...  ' );

switch(mode)
	case nc_clobber_mode
		ncfile = 'foo.nc';
		test_variable_is_present ( ncfile,mode );

	case 'netcdf4-classic' 
		ncfile = 'foo-4.nc';
		test_variable_is_present ( ncfile,mode );

end

% Only test on 7b or higher
v = version('-release');
switch(v)
	case {'14','2006a','2006b','2007a'}
	    %
	otherwise
		test_nc_varrename_neg(mode);
end

fprintf('OK\n');
return





















%--------------------------------------------------------------------------
function test_variable_is_present ( ncfile,mode )


nc_create_empty ( ncfile,mode );
nc_add_dimension ( ncfile, 's', 5 );
nc_add_dimension ( ncfile, 't', 0 );
clear varstruct;
varstruct.Name = 't';
varstruct.Nctype = 'double';
varstruct.Dimension = { 't' };
nc_addvar ( ncfile, varstruct );

nc_varrename ( ncfile, 't', 't2' );



v = nc_getvarinfo ( ncfile, 't2' );
if ~strcmp ( v.Name, 't2' )
	error('rename did not seem to work.');
end

return










