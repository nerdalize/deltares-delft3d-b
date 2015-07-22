function version=read_identification(sourcedir,file)
%READ_INDENTIFICATION determine version number
%   Read the version identification string from the specified file

%   $Id$

%
% Find the "%VERSION = <VERSION>" line in the specified file.
%
fid = fopen([sourcedir filesep file],'r');
str = fgetl(fid);
while isempty(strmatch('%VERSION =',str))
    str = fgetl(fid);
end
fclose(fid);
%
% Obtain the version number from the string.
%
baseversion = deblank(str(11:end));
%
% Determine the latest revision.
%
[revmin,revmax,changed] = determine_revision(sourcedir);
if revmax<0
    revstring = '[unknown revision]';
else
    revstring = sprintf('%05.5i',revmax);
    if changed
        revstring = [revstring ' (changed)'];
    end
end
%
% Combine version and revision to file version string.
%
[a,b] = strtok(baseversion);
version = sprintf('%s.%s%s',a,revstring,b);
%
% Done.
%