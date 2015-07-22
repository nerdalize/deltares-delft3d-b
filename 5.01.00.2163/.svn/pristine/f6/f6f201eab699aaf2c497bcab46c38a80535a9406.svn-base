function fstrrep(file,orgtext,newtext)
%FSTRREP Replace string in file
%   FSTRREP(FileName,OrgText,NewText)
%   replace the OrgText by NewText in the specified file.
%
%   FSTRREP(FileName,'<version>','2.05.00')
%
%   See Also: STRREP

%   $Id$

%
% Read the target file and replace the orgtext by the newtext.
%
fid=fopen(file,'r');
i=1;
str={};
while ~feof(fid)
    tmp=fgetl(fid);
    str{i}=strrep(tmp,orgtext,newtext);
    i=i+1;
end
fclose(fid);
%
% Write the updated file.
%
fid=fopen(file,'w');
fprintf(fid,'%s\n',str{:});
fclose(fid);
