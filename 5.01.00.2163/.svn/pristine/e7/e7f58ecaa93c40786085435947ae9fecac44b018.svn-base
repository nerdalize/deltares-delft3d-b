function test_nc_dump(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_DUMP ...' );

% For now we will run this test preserving the fastest varying dimension.
oldpref = getpref('SNCTOOLS','PRESERVE_FVD',false);
setpref('SNCTOOLS','PRESERVE_FVD',true);

switch(mode)
	case 'hdf'
		run_hdf4_tests;

	case 'http'
		run_http_tests;

	case 'nc-3'
		run_nc3_tests;
        run_negative_tests;

	case 'nc-4'
		run_nc4_tests;

	case 'grib'
		run_grib_tests;

	case 'opendap'
		run_opendap_tests;

end

setpref('SNCTOOLS','PRESERVE_FVD',oldpref);
fprintf('OK\n');




%--------------------------------------------------------------------------
function run_opendap_tests()

test_opendap_url;

return



%--------------------------------------------------------------------------
function test_opendap_url (  )
if getpref('SNCTOOLS','TEST_REMOTE',false) && ...
        getpref ( 'SNCTOOLS', 'TEST_OPENDAP', false ) 
    
    load('testdata/nc_dump.mat');
    % use data of today as the server has a clean up policy
    today = datestr(floor(now),'yyyymmdd');
    url = ['http://motherlode.ucar.edu:8080/thredds/dodsC/satellite/CTP/SUPER-NATIONAL_1km/current/SUPER-NATIONAL_1km_CTP_',today,'_0000.gini'];
	fprintf('\t\tTesting remote DODS access %s...  ', url );
    
    cmd = sprintf('nc_dump(''%s'')',url);
    act_data = evalc(cmd);
    
    if ~strcmp(act_data,d.opendap.unidata_motherlode)
        error('failed');
    end
    fprintf('OK\n');
else
	fprintf('Not testing NC_DUMP on OPeNDAP URLs.  Read the README for details.\n');	
end
return




%--------------------------------------------------------------------------
function outdata = post_process_dump(indata)
% R2010b allows us to collect more information about the netcdf file format
% which is reflected in the 1st line of the nc_dump output.  We need to
% remove this from the output in order to properly compare on 10a and
% below.

[t,r] = strtok(indata);
if strcmp(t,'NetCDF') || strcmp(t,'NetCDF-4') || strcmp(t,'HDF4') || strcmp(t,'NetCDF-3')
    indata = r;
end
[t,r] = strtok(indata);
if strcmp(t,'Classic')
    indata = r;
end
outdata = indata;

%--------------------------------------------------------------------------
function run_nc4_tests()

test_nc4file();
test_nc4_compressed;
run_common_files('nc_netcdf4_classic');


%--------------------------------------------------------------------------
function test_nc4_compressed()
owd = pwd;
testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);

ncfile = 'deflate9.nc';
load('nc_dump.mat');
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
if ~strcmp(act_data,d.netcdf.nc4_compressed)
    cd(owd);
    error('failed');
end

cd(owd);

return



%--------------------------------------------------------------------------
function test_nc4file() 

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'tst_pres_temp_4D_netcdf4.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.nc4);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);

return



%--------------------------------------------------------------------------
function run_http_tests()

test_http_non_dods;

return



%--------------------------------------------------------------------------
function test_http_non_dods (  )
if (getpref ( 'SNCTOOLS', 'USE_JAVA', false)  && ...
        getpref ( 'SNCTOOLS', 'TEST_REMOTE', false)  )
    
    load('testdata/nc_dump.mat');
    
    url = 'http://coast-enviro.er.usgs.gov/models/share/balop.nc';
    fprintf('\t\tTesting remote URL access %s...  ', url );
    
    cmd = sprintf('nc_dump(''%s'')',url);
    act_data = evalc(cmd);
    if ~strcmp(act_data,d.opendap.http_non_dods)
        error('failed');
    end
       
    fprintf('OK\n');
end


%--------------------------------------------------------------------------
function run_hdf4_tests()
dump_hdf4_tp;
run_common_files('hdf4');





%--------------------------------------------------------------------------
function dump_hdf4_tp()
% dumps my temperature pressure file

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);

matfile = fullfile(testroot,'testdata','nc_dump.mat');
load(matfile);
hdffile = 'temppres.hdf'; %#ok<NASGU>
act_data = evalc('nc_dump(hdffile);');

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.hdf4.temppres);
if ~strcmp(act_data,exp_data)
    error('failed');
end


cd(owd);




%--------------------------------------------------------------------------
function dump_hdf4_example()
% dumps the example file that ships with matlab


testroot = fileparts(mfilename('fullpath'));
matfile = fullfile(testroot,'testdata','nc_dump.mat');
load(matfile);

act_data = evalc('nc_dump(''example.hdf'');');
i1 = strfind(act_data,'{');

v = version('-release');
switch(v)
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a', '2008b', '2009a', '2009b', '2010a' }
        
        i2 = strfind(d.hdf4.lt_r2010b.example,'{');
        if ~strcmp(d.hdf4.lt_r2010b.example(i2:end), act_data(i1:end))
            error('failed');
        end
        
    otherwise
        i2 = strfind(d.hdf4.ge_r2010b.example,'{');
        if ~strcmp(d.hdf4.ge_r2010b.example(i2:end), act_data(i1:end))
            error('failed');
        end
end





%--------------------------------------------------------------------------
function run_grib_tests ( )

test_grib2;

return



%--------------------------------------------------------------------------
function test_grib2()

% Test a GRIB2 file.  Requires java as far as I know.
testroot = fileparts(mfilename('fullpath'));
matfile = fullfile(testroot,'testdata','nc_dump.mat');
load(matfile);
gribfile = fullfile(testroot,'testdata',...
    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2'); %#ok<NASGU>
act_data = evalc('nc_dump(gribfile);'); %#ok<NASGU>

% So long as it didn't error out, I'm cool with that.

return






%--------------------------------------------------------------------------
function run_common_files(mode) 
% Just make sure that we don't error out.

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);

load('nc_dump.mat');

switch(mode)
	case 'hdf4'
		ncfile = 'empty.hdf'; 
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

	case nc_clobber_mode
		ncfile = 'empty.nc'; 
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

	case 'nc_netcdf4_classic'
		ncfile = 'empty-4.nc'; 
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

end

cd(owd);


return




%--------------------------------------------------------------------------
function run_nc3_tests() 
test_nc3_file_with_one_dimension;
test_nc3_empty;
test_nc3_singleton;
test_nc3_unlimited_variable;
test_nc3_variable_attributes;
test_nc3_one_fixed_size_variable;

run_common_files(nc_clobber_mode);



%--------------------------------------------------------------------------
function test_nc3_empty() 

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'empty.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.empty_file);
if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);

return








%--------------------------------------------------------------------------
function test_nc3_file_with_one_dimension()

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'just_one_dimension.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.one_dimension);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);
return



%--------------------------------------------------------------------------
function test_nc3_singleton()

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'full.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.singleton_variable);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);
return





%--------------------------------------------------------------------------
function test_nc3_unlimited_variable()

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'full.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.unlimited_variable);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);
return




%--------------------------------------------------------------------------
function test_nc3_variable_attributes()

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'full.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.variable_attributes);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);
return







%--------------------------------------------------------------------------
function test_nc3_one_fixed_size_variable()

owd = pwd;

testroot = fileparts(mfilename('fullpath'));
cd([testroot '/testdata']);
load('nc_dump.mat');

ncfile = 'just_one_fixed_size_variable.nc'; 
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.one_fixed_size_variable);

if ~strcmp(act_data,exp_data)
    cd(owd);
    error('failed');
end
cd(owd);
return


%--------------------------------------------------------------------------
function run_negative_tests ( )

negative_no_arguments;

return


%--------------------------------------------------------------------------
function negative_no_arguments ( )
% should fail if no input arguments are given.

try
	nc_dump;
catch %#ok<CTCH>
	return
end
error ( 'nc_dump succeeded when it should have failed.');

