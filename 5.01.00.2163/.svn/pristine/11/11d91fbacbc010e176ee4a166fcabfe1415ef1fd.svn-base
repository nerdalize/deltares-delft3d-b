function tf = nc_isvar(ncfile,varname)
%NC_ISVAR  Determine if variable is present in file.
%
%   BOOL = NC_ISVAR(NCFILE,VARNAME) returns true if the variable VARNAME is 
%   present in the file NCFILE and false if it is not.
%
%   Example (requires R2008b):
%       bool = nc_isvar('example.nc','temperature')
%       
%   See also nc_isatt.

% Both inputs must be character
if nargin ~= 2
	error ( 'SNCTOOLS:NC_ISVAR:badInput', 'must have two inputs' );
end
if ~ ( ischar(ncfile) || isa(ncfile,'ucar.nc2.NetcdfFile') || isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile') )
	error ( 'SNCTOOLS:NC_ISVAR:badInput', 'first argument must be character or a JAVA netCDF file object.' );
end
if ~ischar(varname)
	error ( 'SNCTOOLS:NC_ISVAR:badInput', 'second argument must be character.' );
end


retrieval_method = snc_read_backend(ncfile);

switch(retrieval_method)
	case 'tmw'
		tf = nc_isvar_tmw(ncfile,varname);
    case 'tmw_hdf4'
        tf = nc_isvar_hdf4(ncfile,varname);
	case 'java'
		tf = nc_isvar_java(ncfile,varname);
	case 'mexnc'
		tf = nc_isvar_mexnc(ncfile,varname);
	otherwise
		error ( 'SNCTOOLS:NC_ISVAR:unrecognizedCase', ...
		        '%s is not recognized method for NC_ISVAR.', retrieval_method );
end






%--------------------------------------------------------------------------
function bool = nc_isvar_hdf4(hfile,varname)
bool = true;
sd_id = hdfsd('start',hfile,'read');
if sd_id < 0
    error('SNCTOOLS:attget:hdf4:start', 'START failed on %s.', hfile);
end


idx = hdfsd('nametoindex',sd_id,varname);
if idx < 0
    bool = false;
end

hdfsd('end',sd_id);


 

%-----------------------------------------------------------------------
function bool = nc_isvar_mexnc ( ncfile, varname )

[ncid,status] = mexnc('open',ncfile, nc_nowrite_mode );
if status ~= 0
	ncerr = mexnc ( 'STRERROR', status );
	error ( 'SNCTOOLS:NC_ISVAR:MEXNC:OPEN', ncerr );
end


[varid,status] = mexnc('INQ_VARID',ncid,varname);
if ( status ~= 0 )
	bool = false;
elseif varid >= 0
	bool = true;
else
	error ( 'SNCTOOLS:NC_ISVAR:unknownResult', ...
	        'Unknown result, INQ_VARID succeeded, but returned a negative varid.  That should not happen.' );
end

mexnc('close',ncid);
return








%--------------------------------------------------------------------------
function bool = nc_isvar_java ( ncfile, varname )
% assume false until we know otherwise
bool = false;

import ucar.nc2.dods.*     
import ucar.nc2.*         
                         
close_it = true;


% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif exist(ncfile,'file')
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
	catch %#ok<CTCH>
		try
			jncid = DODSNetcdfFile(ncfile);
		catch %#ok<CTCH>
			error ( 'SNCTOOLS:nc_varget_java:fileOpenFailure', ...
                 'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                 ncfile);
		end
	end
end




jvarid = jncid.findVariable(varname);

%
% Did we find anything?
if ~isempty(jvarid)
	bool = true;
end

if close_it
	close(jncid);
end

return

