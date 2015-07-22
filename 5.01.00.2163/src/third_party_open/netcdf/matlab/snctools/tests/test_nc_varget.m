function test_nc_varget(mode)

if nargin < 1
	mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_VARGET ...  ' );

testroot = fileparts(mfilename('fullpath'));
switch(mode)
	case 'hdf4';
		run_hdf_tests;

	case 'grib'
		run_grib2_tests;

	case 'netcdf-3'
        ncfile = fullfile(testroot,'testdata/varget.nc');
		run_local_tests(ncfile);

	case 'netcdf4-classic'
        ncfile = fullfile(testroot,'testdata/varget4.nc');
		run_local_tests(ncfile);

    case 'netcdf4-enhanced'
        run_nc4_enhanced;
        
	case 'opendap'
		run_opendap_tests;

end

fprintf('OK\n');


%--------------------------------------------------------------------------
function run_nc4_enhanced()
testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/enhanced.nc');

test_enhanced_group_and_var_have_same_name(ncfile);

%--------------------------------------------------------------------------
function test_enhanced_group_and_var_have_same_name(ncfile)

expData = (1:10)';
actData = nc_varget(ncfile,'/grp1/grp1');
ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.' );
end

%--------------------------------------------------------------------------
function test_bad_missing_value()

warning('off','SNCTOOLS:nc_varget:tmw:missingValueMismatch');
warning('off','SNCTOOLS:nc_varget:mexnc:missingValueMismatch');
warning('off','SNCTOOLS:nc_varget:java:missingValueMismatch');
nc_varget('testdata/badfillvalue.nc','z');
warning('on','SNCTOOLS:nc_varget:tmw:missingValueMismatch');
warning('on','SNCTOOLS:nc_varget:mexnc:missingValueMismatch');
warning('on','SNCTOOLS:nc_varget:java:missingValueMismatch');

%--------------------------------------------------------------------------
function test_bad_fill_value()

warning('off','SNCTOOLS:nc_varget:tmw:fillValueMismatch');
warning('off','SNCTOOLS:nc_varget:mexnc:fillValueMismatch');
warning('off','SNCTOOLS:nc_varget:java:fillValueMismatch');
nc_varget('testdata/badfillvalue.nc','y');
warning('on','SNCTOOLS:nc_varget:tmw:fillValueMismatch');
warning('on','SNCTOOLS:nc_varget:mexnc:fillValueMismatch');
warning('on','SNCTOOLS:nc_varget:java:fillValueMismatch');













%--------------------------------------------------------------------------
function test_readSingleValueFrom1dVariable ( ncfile )

expData = 1.2;
actData = nc_varget ( ncfile, 'test_1D', 1, 1 );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.' );
end

return








%--------------------------------------------------------------------------
function test_readSingleValueFrom2dVariable ( ncfile )

expData = 1.5;
actData = nc_varget ( ncfile, 'test_2D', [2 2], [1 1] );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error('input data ~= output data.');
end

return




%--------------------------------------------------------------------------
function test_read2x2hyperslabFrom2dVariable ( ncfile )

expData = [1.5 2.1; 1.6 2.2];
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
actData = nc_varget ( ncfile, 'test_2D', [2 2], [2 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 4
    error ( 'rank of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return






%--------------------------------------------------------------------------
function test_stride_with_negative_count ( ncfile )

expData = [0.1 1.3; 0.3 1.5; 0.5 1.7];

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
actData = nc_varget(ncfile,'test_2D',[0 0],[-1 -1],[2 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 6
    error ( 'count of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return







%--------------------------------------------------------------------------
function test_inf_count ( ncfile )
% If the count has Inf anywhere, treat that as meaning to "retrieve unto
% the end of the file.

expData = [0.1 1.3; 0.3 1.5; 0.5 1.7];

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
actData = nc_varget(ncfile,'test_2D',[0 0],[Inf Inf],[2 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 6
    error ( 'count of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return







%--------------------------------------------------------------------
function test_readFullSingletonVariable ( ncfile )


expData = 3.14159;
actData = nc_varget ( ncfile, 'test_singleton' );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return



%--------------------------------------------------------------------------
function test_readFullDoublePrecisionVariable ( ncfile )


expData = 1:24;
expData = reshape(expData,6,4) / 10;

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end

actData = nc_varget ( ncfile, 'test_2D' );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return




%--------------------------------------------------------------------------
function test_readStridedVariable ( ncfile )

expData = 1:24;
expData = reshape(expData,6,4) / 10;
expData = expData(1:2:3,1:2:3);
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end

actData = nc_varget ( ncfile, 'test_2D', [0 0], [2 2], [2 2] );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return





%--------------------------------------------------------------------------
function regression_NegSize ( ncfile )
% A negative size means to retrieve to the end along the given dimension.
expData = 1:24;
expData = reshape(expData,6,4) / 10;
sz = size(expData);
sz(2) = -1;
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
    sz = fliplr(sz);
end

actData = nc_varget ( ncfile, 'test_2D', [0 0], sz );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return


%--------------------------------------------------------------------------
function test_missing_value(ncfile)
% The last value should be nan.

actData = nc_varget ( ncfile, 'sst_mv' );

if ~isa(actData,'double')
    error ( 'short data was not converted to double');
end

if ~isnan( actData(end) )
    error ( 'missing value not converted to nan.\n'  );
end

return

%--------------------------------------------------------------------------
function test_scaling ( ncfile )

expData = [32 32 32 32; 50 50 50 50; 68 68 68 68; ...
           86 86 86 86; 104 104 104 104; 122 122 122 122]';

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
    
actData = nc_varget ( ncfile, 'temp' );

if ~isa(actData,'double')
    error ( 'short data was not converted to double');
end
ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return




%--------------------------------------------------------------------------
function run_grib2_tests()


testroot = fileparts(mfilename('fullpath'));
gribfile = fullfile(testroot,'testdata',...
    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2');
test_readFullDouble(gribfile);

return

%--------------------------------------------------------------------------
function test_readFullDouble(gribfile)
actData = nc_varget(gribfile,'lon');
expData = 10*(0:35)';
if actData ~= expData
    error('failed');
end
return










%--------------------------------------------------------------------------
function run_hdf_tests()

test_hdf4_example;
test_hdf4_scaling;

%--------------------------------------------------------------------------
function test_hdf4_example()
% test the example file that ships with matlab
exp_data = hdfread('example.hdf','Example SDS');
act_data = nc_varget('example.hdf','Example SDS');

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    act_data = act_data';
end

if exp_data ~= act_data
    error('failed');
end


%--------------------------------------------------------------------------
function test_hdf4_scaling()
testroot = fileparts(mfilename('fullpath'));

oldpref = getpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);

hdffile = fullfile(testroot,'testdata','temppres.hdf');

setpref('SNCTOOLS','USE_STD_HDF4_SCALING',true);
act_data = nc_varget(hdffile,'temp',[0 0],[2 2]);
exp_data = 1.8*([32 32; 33 33] - 32);

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    act_data = act_data';
end

if exp_data ~= act_data
    error('failed');
end


setpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);
act_data = nc_varget(hdffile,'temp',[0 0],[2 2]);
exp_data = 1.8*[32 32; 33 33] + 32;

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    act_data = act_data';
end

if exp_data ~= act_data
    error('failed');
end


setpref('SNCTOOLS','USE_STD_HDF4_SCALING',oldpref);




%--------------------------------------------------------------------------
function run_local_tests(ncfile)

test_readSingleValueFrom1dVariable ( ncfile );
test_readSingleValueFrom2dVariable ( ncfile );
test_read2x2hyperslabFrom2dVariable ( ncfile );
test_stride_with_negative_count ( ncfile );
test_inf_count ( ncfile );

test_readFullSingletonVariable ( ncfile );
test_readFullDoublePrecisionVariable ( ncfile );

test_readStridedVariable ( ncfile );
regression_NegSize(ncfile);
test_scaling(ncfile);

test_missing_value(ncfile);
test_bad_fill_value;
test_bad_missing_value;
return








%--------------------------------------------------------------------------
function run_opendap_tests()

test_readOpendapVariable;

test_nc_varget_neg_opendap;


return











%--------------------------------------------------------------------------
function test_readOpendapVariable ()
    % use data of today as the server has a clean up policy
    today = datestr(floor(now),'yyyymmdd');
    url = ['http://motherlode.ucar.edu:8080/thredds/dodsC/satellite/CTP/SUPER-NATIONAL_1km/current/SUPER-NATIONAL_1km_CTP_',today,'_0000.gini'];
    
    % I have no control over what this value is, so we'll just assume it
    % is correct.
    nc_varget(url,'y',0,1);
return



