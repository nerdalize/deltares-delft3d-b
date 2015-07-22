function [retrieval_method,fmt] = snc_read_backend(ncfile)
%SNC_READ_BACKEND   determine which netCDF library to use
%
% which backend do we employ?  Many, many possibilities to consider here.
%
%   [retrieval_method,fmt] = snc_read_backend(ncfile)
%
% returns selection for specified file, http or url.
%
%   [retrieval_method,fmt] = snc_read_backend()
%
% returns all available retrieval_method and fmt options
%
%See also: snctools, snc_format

retrieval_methods.java     = 'java';
retrieval_methods.tmw_hdf4 = 'tmw_hdf4';
retrieval_methods.mexnc    = 'mexnc';
retrieval_methods.tmw      = 'tmw';

fmts = snc_format();
fmt  = '';

if nargin==0
   retrieval_method = retrieval_methods;
else   
   retrieval_method = '';
end

use_java  = getpref('SNCTOOLS','USE_JAVA' ,false);
use_mexnc = getpref('SNCTOOLS','USE_MEXNC',false);

% Check for this early.
if isa(ncfile,'ucar.nc2.NetcdfFile') && use_java
    retrieval_method = retrieval_methods.java;
	fmt = fmts.netcdf_java;
	return
end


fmt = snc_format(ncfile);

% These cases have no alternatives.
if strcmp(fmt,fmts.HDF4) 
    % always use MATLAB's HDF interface for HDF-4 files.
    retrieval_method = retrieval_methods.tmw_hdf4;
	return
elseif use_java && (strcmp(fmt,fmts.GRIB) || strcmp(fmt,fmts.GRIB2) || strcmp(fmt,fmts.URL))
    % Always use netcdf-java for grib files or URLs (when java is enabled).
    retrieval_method = retrieval_methods.java;
	return
elseif strcmp(fmt,fmts.URL)
    % If java is not available, we have to assume that mexnc was compiled with
	% opendap support.
    retrieval_method = retrieval_methods.mexnc;
	return
end

mv = version('-release');
switch ( mv )
    case { '11', '12', '13' };
		error('Not supported on releases below R14.');

    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
		% No native matlab support here.  We will favor java over
		% mexnc for now.
        if (strcmp(fmt,fmts.NetCDF) || strcmp(fmt,fmts.NetCDF4))
        	if use_java 
	            retrieval_method = retrieval_methods.java;
            else
	            retrieval_method = retrieval_methods.mexnc;
		    end
        elseif use_java
            % Last chance is if it is some format that netcdf-java can handle.
            % Hope for the best.
            retrieval_method = retrieval_methods.java;
            fmt = fmts.netcdf_java;
        end
        
    case { '2008b', '2009a', '2009b', '2010a' }
        % 2008b introduced native netcdf-3 support.
		% netcdf-4 still requires either mexnc or java, and we will favor
		% java again.
        if strcmp(fmt,fmts.NetCDF) 
			% Use TMW for all local netcdf-3 files.
            retrieval_method = retrieval_methods.tmw;
        elseif strcmp(fmt,fmts.NetCDF4) 
            if use_java
            	retrieval_method = retrieval_methods.java;
			elseif use_mexnc
            	retrieval_method = retrieval_methods.mexnc;
			else
    			error('SNCTOOLS:unknown2008bNetcdf4BackendSituation', ...
			          'The file format is netCDF-4, but the java backend is not enabled.');
			end
		else
			% not netcdf-3 or netcdf-4 
			if use_java
            	% Last chance is if it is some format that netcdf-java can handle.
	            retrieval_method = retrieval_methods.java;
			end
        end

    otherwise
        % R2010b:  introduced netcdf-4 support.
        if strcmp(fmt,fmts.NetCDF) || strcmp(fmt,fmts.NetCDF4)
            retrieval_method = retrieval_methods.tmw;
        elseif use_java
            % Last chance is if it is some format that netcdf-java can handle.
            retrieval_method = retrieval_methods.java;
            fmt = fmts.netcdf_java;
        end

end

if isempty(retrieval_method)
    error('SNCTOOLS:unknownBackendSituation', ...  
	      'Could not determine which backend to use with %s.  If the file format is not netCDF, the java backend must be enabled.', ...
       ncfile );
end
return
