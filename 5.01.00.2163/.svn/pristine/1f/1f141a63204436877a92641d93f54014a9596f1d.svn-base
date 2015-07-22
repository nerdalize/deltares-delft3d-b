function test_snctools()

pre_testing;
run_all_tests;
restore_state;


fprintf ('\nAll  possible tests for your configuration have been ');
fprintf ('run.\n\n' );

fprintf('If this is the first time you have run SNCTOOLS, then you should\n');
fprintf('know that several preferences have been set.\n');

getpref('SNCTOOLS')

fprintf('Only the ''USE_JAVA'' and ''PRESERVE_FVD'' preferences are important\n');
fprintf('for daily use of SNCTOOLS.  Check the top-level README for details.  \n');
fprintf('Bye-bye.\n');

clear mex;

return




%--------------------------------------------------------------------------
function restore_state()

% restore all the warning states.
warning('on', 'SNCTOOLS:nc_archive_buffer:deprecated' );
warning('on', 'SNCTOOLS:nc_datatype_string:deprecated' );
warning('on', 'SNCTOOLS:nc_diff:deprecated' );
warning('on', 'SNCTOOLS:nc_getall:dangerous' );
warning('on', 'SNCTOOLS:snc2mat:deprecated' );
        


%--------------------------------------------------------------------------
function run_all_tests()
% We test the mathworks backend, mexnc backend, and java backend.

test_tmw_backend;
test_mexnc_backend;
test_java_backend;



%--------------------------------------------------------------------------
function pre_testing()
% clear the error state
lasterr(''); %#ok<LERR>

% make sure we can even run the tests.
mver = version('-release');
switch mver
    case {'11', '12'}
        error ('This version of MATLAB is too old, SNCTOOLS will not run.');
    case {'13'}
        error ('R13 is not supported in this release of SNCTOOLS');
end


% Disable these warning for the duration of the tests.
warning('off', 'SNCTOOLS:nc_archive_buffer:deprecated' );
warning('off', 'SNCTOOLS:nc_datatype_string:deprecated' );
warning('off', 'SNCTOOLS:nc_diff:deprecated' );
warning('off', 'SNCTOOLS:nc_getall:dangerous' );
warning('off', 'SNCTOOLS:snc2mat:deprecated' );



%--------------------------------------------------------------------------
function test_java_backend()
fprintf('Testing java backend ...\n');

use_java = getpref('SNCTOOLS','USE_JAVA',false);
use_mexnc = getpref('SNCTOOLS','USE_MEXNC',false);

if ~use_java
    fprintf('\tjava backend testing filtered out on ');
    fprintf('configurations where SNCTOOLS ''USE_JAVA'' ');
    fprintf('prefererence is false.\n');
    return
end

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a'}
        if use_mexnc 
            fprintf('\tjava netcdf-3 testing filtered out on ');
            fprintf('configurations where SNCTOOLS ''USE_MEXNC'' ');
            fprintf('prefererence is true.\n');    
        else
            run_nc3_read_tests;
        end
                
    otherwise
        fprintf('\tnetcdf-3 java backend testing with local files filtered out on release %s\n', v);
end

switch(v)
    case {'2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b','2010a'}
        if use_mexnc && netcdf4_capable
            fprintf('\tjava netcdf-4 testing filtered out on ');
            fprintf('configurations where SNCTOOLS ''USE_MEXNC'' ');
            fprintf('prefererence is true.\n');    
        else
            run_nc4_read_tests;
            run_nc4_enhanced_read_tests;
        end
                
    otherwise
        fprintf('\tnetcdf-4 java backend testing with local files filtered out on release %s\n', v);
end


run_http_tests;
run_opendap_tests;
run_grib_tests;

%--------------------------------------------------------------------------
function test_mexnc_backend()

fprintf('Testing mexnc backend ...\n');
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
        if strcmp(computer,'PCWIN64')
            fprintf('\tmexnc testing filtered out on PCWIN64 on release %s.\n', v);
            return;
        end
		if ~getpref('SNCTOOLS','USE_MEXNC',false)
		    fprintf('\tmexnc testing filtered out where preference USE_MEXNC set to false.\n');
		        return
		end
        run_nc3_read_tests;
        run_nc3_write_tests;
        if netcdf4_capable
            run_nc4_read_tests;
        	run_nc4_write_tests;
        end
        
    case {'2008b','2009a','2009b','2010a'}
		if ~getpref('SNCTOOLS','USE_MEXNC',false)
		    fprintf('\tmexnc testing filtered out where preference USE_MEXNC set to false.\n');
		        return
		end
        run_nc3_read_tests;
        if netcdf4_capable
            run_nc4_read_tests;
        end
        
    otherwise
        fprintf('\tmexnc testing filtered out on release %s.\n', v);
        return
end


return


%--------------------------------------------------------------------------
function test_tmw_backend()

fprintf('Testing tmw backend ...\n');

if getpref('SNCTOOLS','TEST_HDF4',false)
    run_hdf4_tests;
else
    fprintf('\tHDF4 testing filtered out where TEST_HDF4 preference set to false.\n');
end

v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
        fprintf('\ttmw testing filtered out on release %s...\n', v);
        return;
        
    case { '2008b','2009a','2009b','2010a'}
        if getpref('SNCTOOLS','USE_MEXNC',false)
            fprintf('\ttmw netcdf testing filtered out where preference USE_MEXNC set to true.\n');
            return
        end
        run_nc3_read_tests;
        run_nc3_write_tests;
        
    otherwise
        run_nc3_read_tests;
        run_nc3_write_tests;
        run_nc4_read_tests;
        run_nc4_write_tests;
        run_nc4_enhanced_read_tests;
end


return
%--------------------------------------------------------------------------
function run_nc3_write_tests()

test_nc_adddim;
test_nc_addhist;
test_nc_addnewrecs;
test_nc_varput;
test_nc_addvar(nc_clobber_mode);
test_nc_attput;
test_nc_create_empty;
test_nc_varrename;
test_nc_addrecs(nc_clobber_mode);
test_nc_cat;

%--------------------------------------------------------------------------
function run_nc4_write_tests()

mode = 'netcdf4-classic';
test_nc_adddim(mode);
test_nc_addhist(mode);
test_nc_addnewrecs(mode);
test_nc_varput(mode);
test_nc_addvar(mode);
test_nc_attput(mode);
test_nc_create_empty(mode);
test_nc_varrename(mode);
test_nc_addrecs(mode);
test_nc_cat(mode);

%--------------------------------------------------------------------------
function run_nc3_read_tests()

fprintf('\tTesting netcdf-3...\n');

test_nc_attget;
test_nc_datatype_string;
test_nc_iscoordvar;
test_nc_isunlimitedvar;
test_nc_getlast;
test_nc_isvar;
test_nc_varsize;
test_nc_getvarinfo;
test_nc_getbuffer;
test_nc_info;
test_nc_varget;
test_nc_getdiminfo;
test_snc2mat;
test_nc_getall;
test_nc_dump;

%--------------------------------------------------------------------------
function run_nc4_read_tests()


mode = 'netcdf4-classic';
fprintf('\tTesting %s..\n',mode);

test_nc_attget(mode);
test_nc_datatype_string;
test_nc_iscoordvar(mode);
test_nc_isunlimitedvar(mode);

test_nc_getlast(mode);
test_nc_isvar(mode);
test_nc_varsize(mode);
test_nc_getvarinfo(mode);
test_nc_getbuffer(mode);
test_nc_info(mode);
test_nc_varget(mode);
test_nc_getdiminfo(mode);

% Only do this on 10b for the moment.
v = version('-release');
if strcmp(v,'2010b')
    test_nc_dump('nc-4');
else
    fprintf('\t\tNo netcdf-4 NC_DUMP testing on %s.\n', v);
end

%--------------------------------------------------------------------------
function run_nc4_enhanced_read_tests()

mode = 'netcdf4-enhanced';
fprintf('\tTesting %s...\n',mode);
test_nc_varget(mode);


%--------------------------------------------------------------------------
function run_hdf4_tests()


v = version('-release');
if strcmp(v,'14')
    fprintf('\thdf testing filtered out when the version is 14.');
    fprintf('There''s a known issue with no workaround yet.');
    return
end

fprintf('\tTesting hdf4...\n');

mode = 'hdf4';
test_nc_attget(mode);

test_nc_dump('hdf');
test_nc_iscoordvar('hdf');
test_nc_isunlimitedvar(mode);
test_nc_adddim(mode);
test_nc_addvar(mode);
test_nc_addrecs(mode);
test_nc_varget(mode);
test_nc_varput(mode);
test_nc_attput(mode);
test_nc_cat(mode);
return
%--------------------------------------------------------------------------
function run_http_tests()
fprintf('\tTesting java/http...\n');

if getpref('SNCTOOLS','TEST_REMOTE',false) && getpref('SNCTOOLS','TEST_HTTP',false)
    test_nc_attget('http');
    test_nc_iscoordvar('http');
    test_nc_isvar('http');
    test_nc_info('http');
    test_nc_varget('http');
    test_nc_getvarinfo('http');
    return
end


fprintf('\t\tjava http testing filtered out when either of SNCTOOLS ');
fprintf('\n\t\tpreferences''TEST_REMOTE'' or ''TEST_HTTP'' is false.\n');

return

%--------------------------------------------------------------------------
function run_grib_tests()
fprintf('\tTesting GRIB...\n');

if ~getpref('SNCTOOLS','TEST_GRIB2',false)
    fprintf('\t\tGRIB2 testing filtered out where SNCTOOLS preference ');
    fprintf('TEST_GRIB2 is set to false.\n');
    return
end


test_nc_attget('grib');
test_nc_dump('grib');
test_nc_varget('grib');

%--------------------------------------------------------------------------
function run_opendap_tests()
fprintf('\tTesting OPeNDAP...\n');

if ~getpref ( 'SNCTOOLS', 'TEST_REMOTE', false )
    fprintf('\t\tjava http testing filtered out when SNCTOOLS ');
    fprintf('''TEST_REMOTE'' preference is false.\n');
    return
end

test_nc_dump('opendap');
test_nc_varget('opendap');

% If we are using mexnc or if the release is 8b or higher, run this
% system-level test.
v = version('-release');
switch(v)
	case { '14','2006a','2006b','2007a','2007b','2008a'}
        if getpref('SNCTOOLS','USE_MEXNC',false)
        	test_opendap_local_system;
        end
	otherwise
        test_opendap_local_system;
end



return

