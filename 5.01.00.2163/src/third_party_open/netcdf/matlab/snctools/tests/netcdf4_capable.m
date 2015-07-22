function tf = netcdf4_capable()
% Is the current mexnc installation capable of netcdf-4 operations?
v = version('-release');
switch(v)
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a', '2008b', ...
            '2009a', '2009b', '2010a' }
        
    otherwise
		% 2010b is definitely netcdf4-capable.
        tf = true;
        return
end


try
	v = mexnc('inq_libvers');
catch 
	tf = false;
	return
end
if v(1) == '4'
	tf = true;
else
	tf = false;
end
return

