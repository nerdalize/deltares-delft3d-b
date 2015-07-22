function nc_cat(file1,file2,recsize)
%nc_cat Concatenate netCDF files along unlimited dimension.
%   nc_cat(file1,file2) concatenates files along the unlimited dimension.
%   The records of file2 are added to file1.  Variable not defined along
%   the unlimited dimension are left untouched.
%
%   nc_cat(file1,file2,N) concatenates the files N records at a time.  This 
%   may be preferable if the files are large and all the records cannot fit 
%   into memory.
%
%   Example:
%       nc_create_empty('f1.nc');
%       nc_adddim('f1.nc','time',0);
%       v.Name = 'time';
%       v.Dimension = {'time'};
%       nc_addvar('f1.nc',v);
%       v.Name = 'money';
%       v.Dimension = {'time'};
%       nc_addvar('f1.nc',v);
%       copyfile('f1.nc','f2.nc');
%
%       % Populate the first file.
%       buf.time = [0 1 2];
%       buf.money = [0 1000 2000];
%       nc_addnewrecs('f1.nc',buf);
%
%       % Now populate the 2nd file.
%       buf.time = [3 4 5 6];
%       buf.money = [3000 4000 5000 6000];
%       nc_addnewrecs('f2.nc',buf);
%
%       % Now concatenate them.
%       nc_cat('f1.nc','f2.nc');
%       data = nc_varget('f1.nc','money');
%
%
%   See also nc_addnewrecs.

record_variable = find_record_variable(file1);


% Verify that the record variable exists in file2.
if ~nc_isvar(file2,record_variable)
    error('SNCTOOLS:nc_cat:recordVariableNotThere', ...
        'The record variables must have the same name.  Could not find "%s" in %s.', ...
        record_variable, file2);
end

% Verify that all the unlimited variables in the 2nd file exist in the first file
info2 = nc_info(file2);
for j = 1:numel(info2.Dataset)
    if info2.Dataset(j).Unlimited
        if ~nc_isvar(file1,info2.Dataset(j).Name)
            error('SNCTOOLS:nc_cat:recordVariableMissing', ...
                'Could not find unlimited variable %s in %s.', ...
                info2.Dataset(j).Name,file1);       
        end
    end
end



if nargin < 3
	b = nc_getbuffer(file2);
	nc_addrecs(file1,b);
else
	vinfo = nc_getvarinfo(file2,record_variable);
	num_ops = ceil(vinfo.Size / recsize);
	for j = 1:num_ops
		start = (j-1)*recsize;
		count = min(recsize, vinfo.Size-start);
		b = nc_getbuffer(file2,start,count);
		nc_addrecs(file1,b);
	end
end
