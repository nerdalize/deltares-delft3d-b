%% SNCTOOLS (<a href="http://mexcdf.sourceforge.net/">http://mexcdf.sourceforge.net/</a>).
%
% SNCTOOLS is a package of simple tools for manipulating netCDF files, or
% at least it started out that way.  The focus used to be just on reading
% and writing netCDF files, but can also handle the following formats under
% certain conditions. 
%
%   HDF4:  can read and write datasets via MATLAB's HDFSD interface.
%
%   netCDF4, GRIB2, OPeNDAP:  can read datasets via netcdf-java.  For GRIB2
%   please check the README.
%
% NC_ADDHIST        -  Either appends or constructs a history attribute.
% NC_ADDDIM         -  Adds a dimension to an existing netcdf file
% NC_ADDRECS        -  Adds records to unlimited-dimension file.
% NC_ADDNEWRECS     -  Adds monotonically increasing records to file.
% NC_ADDVAR         -  Adds a variable to a NetCDF file.
% NC_ATTGET         -  Get the values of a NetCDF attribute.
% NC_ATTPUT         -  Put an attribute into a netcdf file.
% NC_CAT            -  Concatentate files.
% NC_CREATE_EMPTY   -  Creates an empty netCDF file.
% NC_DIFF           -  Determines if two NetCDF files contain same data.
% NC_DUMP           -  Matlab counterpart to the NetCDF utility 'ncdump'.
% NC_GETALL         -  Read the entire netCDF contents into structure
% NC_GETDIMINFO     -  Returns metadata about a specific netCDF dimension.
% NC_GETLAST        -  Get last few datums from unlimited NetCDF variable.
% NC_GETVARINFO     -  Returns metadata about a specific NetCDF variable.
% NC_INFO           -  Information about a netCDF file.
% NC_ISCOORDVAR     -  Yes if the given variable is a coordinate variable.
% NC_ISUNLIMITEDVAR -  Yes if the given variable has a record dimension.
% NC_ISATT          -  Yes if the given attribute is in the file.
% NC_ISVAR          -  Yes if the given variable is in the netcdf file.
% NC_PADHEADER      -  Pads the metadata header of a netcdf file.
% NC_VARGET         -  Retrieve data from netCDF variable or HDF4 data set.
% NC_VARPUT         -  Write data into a NetCDF file.
% NC_VARRENAME      -  Renames a NetCDF variable.
% SNC2MAT           -  Saves netcdf file to *.mat format
%
% Note that by default, SNCTOOLS will transpose data ordering and 
% dimensions for the sake of backwards compatibility.  This behavior can
% be controlled with the PRESERVE_FVD preference.
%  
% Preferences
% -----------
%   PRESERVE_FVD - Setting the preference 'PRESERVE_FVD' to true will
%                  compel NC_VARGET to preserve the fastest varying
%                  dimension, i.e. NC_VARGET will not transpose the data.
%                  This flips the order of the dimension IDs from what one
%                  would see by using the ncdump C utility.  This may
%                  result in a performance boost when working with large
%                  data.
%
%   USE_STD_HDF4_SCALING
%                - The CF conventions differ from HDF4 in how scale
%                  factors are to be applied; check their respective
%                  references for details.  By default, HDF4 datasets are
%                  scaled according to CF conventions, but setting this
%                  preference to true forces NC_VARGET to use standard HDF4
%                  conventions.
