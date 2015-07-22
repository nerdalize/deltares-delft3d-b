function test_local_system(mode)

fprintf('\t\tTesting sytem-level ...  ' );
if nargin < 1
    mode = nc_clobber_mode;
end

switch(mode)
	case 'hdf4'
		ncfile = 'foo.hdf';
	case nc_clobber_mode
		ncfile = 'foo.nc';
	case 'netcdf4-classic'
		ncfile = 'foo4.nc';
end

run_all_tests(ncfile,mode);
fprintf('OK\n');
return


%--------------------------------------------------------------------------
function run_all_tests(ncfile,mode)

run_open_file_handle_stress_test(mode);




%--------------------------------------------------------------------------
function run_open_file_handle_stress_test(mode)
% do a series of operations about a thousand times.  That should be enough
% to guarantee that no file handles are being left open.

% Run the test without transposing
oldpref = getpref('SNCTOOLS','PRESERVE_FVD',false);
setpref('SNCTOOLS','PRESERVE_FVD',true);

numops = 1000;
ncfile = cell(numops,1);
for j = 1:numops
	if mod(j,10) == 0
		fprintf('%d...', j);
	end
	x = tempname;
	ncfile{j} = sprintf('%s%04d.nc', x, j);
	create_ncfile(ncfile{j},mode);

	% add records to each file
	for k = 0:2
		buffer.time = k;
		buffer.temperature = single(k);
		buffer.y = [k k*2 k*3 k*4]';
		nc_addnewrecs(ncfile{j},buffer);
	end
end
for j = 1:numops
	delete(ncfile{j});
end

% Restore preferences.
setpref('SNCTOOLS','PRESERVE_FVD',oldpref);

%-------------------------------------------------------------------------------
function create_ncfile ( ncfile, mode )

if exist(ncfile,'file')
    delete(ncfile);
end

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'time',0);


% Add a variable along the time dimension
varstruct.Name = 'temperature';
varstruct.Datatype = 'float';
varstruct.Dimension = { 'time' };
varstruct.Attribute(1).Name = 'long_name';
varstruct.Attribute(1).Value = 'temperature';
varstruct.Attribute(2).Name = 'dud';
varstruct.Attribute(2).Value = int16(5);

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'y';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x', 'time' };

nc_addvar ( ncfile, varstruct );


% Don't do this if HDF4.  We already have the coordinate variable there.
if ~((nargin == 2) && ischar(mode) && strcmp(mode,'hdf4'))
    clear varstruct;
    varstruct.Name = 'time';
    varstruct.Nctype = 'double';
    varstruct.Dimension = { 'time' };
    
    nc_addvar ( ncfile, varstruct );
end



return





