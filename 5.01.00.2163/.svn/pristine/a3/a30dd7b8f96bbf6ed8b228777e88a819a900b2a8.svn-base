function cleanup(X)
%CLEANUP  removes files and directories
%   CLEANUP(X)
%   X is a cell array of files and/or directories
%   for instance:
%   X={'*.bak' 'subdir'}

%   $Id$

if isunix
    % linux rm command is not recursive
    d = dir;
    for id=1:length(d)
        if d(id).isdir && ~isequal(d(id).name(1),'.')
            % recursively call cleanup
            cd(d(id).name)
            cleanup(X);
            cd ..
        end
    end
    % finally process this directory
    for i=1:length(X(:))
        unix(['rm -rf ' X{i}]);
    end
else
    for i=1:length(X(:))
        X{i}=strrep(X{i},'/','\');
        if isdir(X{i})
            [s,msg]=dos(['rmdir /s/q ' X{i}]);
        else
            [s,msg]=dos(['del /s/f/q ' X{i}]);
        end
        if s~=0
            if isdir(X{i})
                [s,msg]=dos(['rmdir /s/q ' X{i}]);
            end
            if s~=0
                error(msg);
            end
        end
    end
end
