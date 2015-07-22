function test_snc2mat()
fprintf ('\t\tTesting SNC2MAT...  ' );
run_negative_tests;

test_generic_file;
fprintf('OK\n');
return





%--------------------------------------------------------------------------
function run_negative_tests()
v = version('-release');
switch(v)
	case{'14','2006a','2006b','2007a'}
	    fprintf('Some negative tests filtered out on version %s...  ', v);
    otherwise
		test_file_does_not_exist;
end


%--------------------------------------------------------------------------
function test_file_does_not_exist ( ncfile )

% netcdf file does not exist.
try
	snc2mat ( 'bad.nc', 'bad.mat' );
catch %#ok<NASGU>
    %  'MATLAB:netcdf:open:noSuchFile'
    return
end

%--------------------------------------------------------------------------
function test_generic_file()

use_mexnc = getpref('SNCTOOLS','USE_MEXNC',false);
v = version('-release');
switch(v)
	case { '14', '2006a', '2006b', '2007a', '2007b', '2008a'}
		if ~use_mexnc
			fprintf('\tNo testing on java read-only configuration.\n');
			return
        end
end
ncfile= 'foo.nc';
nc_create_empty(ncfile,nc_clobber_mode);
len_x = 4; len_y = 6;
nc_adddim( ncfile, 'x', len_x );
nc_add_dimension ( ncfile, 'y', len_y );

clear varstruct;
varstruct.Name = 'z_double';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'y', 'x' };
nc_addvar ( ncfile, varstruct );




input_data = 1:1:len_y*len_x;
input_data = reshape ( input_data, len_y, len_x );

nc_varput ( ncfile, 'z_double', input_data );



matfile_name = [ ncfile '.mat' ];
snc2mat ( ncfile, matfile_name );


%
% now check it
d = load ( matfile_name );
output_data = d.z_double.data;



d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'failed' );
end
return











