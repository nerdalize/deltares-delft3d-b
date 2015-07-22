function backend = snc_write_backend(ncfile)
% SNC_WRITE_BACKEND:  figure out which backend to use, either mexnc or the
% native matlab netcdf/hdf4 package

use_mexnc = getpref('SNCTOOLS','USE_MEXNC',false);

v = version('-release');
fmt = snc_format(ncfile);
switch(fmt)
    case 'NetCDF'
        switch(v)
            case {'14','2006a','2006b','2007a','2007b','2008a'}
                if use_mexnc
                    backend = 'mexnc';
                    return
                else
                    error('No write capability without mexnc enabled.');
                end

            case {'R2008b','R2009a','R2009b','R2010a'}
                if use_mexnc
                    backend = 'mexnc';
                else
                    backend = 'tmw';
                end
                return

            otherwise
                   backend = 'tmw';
        end

    case 'NetCDF-4'
        switch(v)
            case {'14','2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b','2010a'}
                if use_mexnc
                    backend = 'mexnc';
                else
                    backend = 'tmw';
                end
                return

            otherwise
                   backend = 'tmw';
        end

    case 'HDF4'
        backend = 'tmw_hdf4';
        return

end

return




