function countmlines
%COUNTMLINES Count lines in QUICKPLOT source code
%   Count number of lines in the m-files in the
%   directories progsrc and progsrc/private

%   $Id$

[X1,n1,nc1]=countmlines_dir('progsrc/');
[X2,n2,nc2]=countmlines_dir('progsrc/private/');
fprintf('Grand total: %i (%i)\n',n1+n2,nc1+nc2);


function [X,total_nl,total_ncl]=countmlines_dir(subdir)
%COUNTMLINES_DIR
%    Count number of lines in the m-files in the
%    specified directory

fprintf('%s:\n',subdir);
X={};
for d=dir([subdir '*.m'])'
    nl=0;
    ncl=0;
    fid=fopen([subdir d.name],'r');
    while ~feof(fid)
        line = fgetl(fid);
        nl=nl+1;
        tok = strtok(line);
        if ~isempty(tok) && tok(1)~='%'
           ncl=ncl+1;
        end
    end
    fclose(fid);
    nl=nl-1;
    X(end+1,1:3)={d.name nl ncl};
    fprintf('%-30s %i (%i)\n',X{end,:});
end
total_nl=sum([X{:,2}]);
total_ncl=sum([X{:,3}]);
fprintf('This directory: %i\n\n',total_nl);
Xt=X';
