function make_d3dmatlab(basedir)
%MAKE_D3DMATLAB Pre-compile Delft3D-MATLAB toolbox
%   Pre-compile MATLAB m-code to p-code for distribution as Delft3D-MATLAB toolbox.
%
%   MAKE_D3DMATLAB(BASEDIR)
%   Use specified directory instead of current directory as base directory

%   $Id$

curd=pwd;
if nargin>0
    cd(basedir);
end
try
    err=localmake(curd);
catch
    err=lasterr;
end
if nargin>0
    cd(curd);
end
if ~isempty(err)
    error(err)
end


function err=localmake(rootdir)
err='';
if ~exist('progsrc','dir')
    err='Cannot locate source'; return
end
V=version; V=str2num(V(1));

tdir = 'delft3d_matlab';
sourcedir=[pwd,filesep,'progsrc'];
targetdir=[pwd,filesep,tdir];
qpversion=read_identification(sourcedir,'d3d_qp.m');
disp(['Delft3D-MATLAB interface version: ' qpversion]);
T=round(clock);
TStr=[datestr(datenum(T(1),T(2),T(3),T(4),T(5),T(6)),3) sprintf(' %2.2i %i %2.2i:%2.2i:%2.2i',T([3 1 4 5 6]))];
disp(['Current date and time           : ' TStr]);

disp(['Creating ',tdir,' directory ...']);
if ~exist(tdir,'dir')
    [success,message] = mkdir(tdir);
    if ~success
        err=message;
        return
    end
end

disp('Copying files ...');
exportsrc(sourcedir,targetdir)

disp('Modifying files ...');
fstrrep([targetdir,filesep,'d3d_qp.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'Contents.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'Contents.m'],'<CREATIONDATE>',TStr)

disp('Stripping files ...');
svnstripfile(targetdir)

%disp('Pcoding files ...');
%pmfile('dir',targetdir,targetdir,'-verbose')

disp('Cleaning up directory ...');
cd(tdir)
addpath(rootdir)
X={ '*.asv'
    '*.bak'
    '*.scc'
    'bin'
    'compileonly'};
cleanup(X)

disp('Removing unneeded subdirectories ...');
X={'org'};
cleanup(X)

rmpath(rootdir)
cd ..
disp('Finished.');


function exportsrc(sourcedir,targetdir)
d = dir(sourcedir);
for i = 1:length(d)
    source = [sourcedir filesep d(i).name];
    target = [targetdir filesep d(i).name];
    if d(i).isdir
        switch d(i).name
            case {'.','..','.svn'}
                % skip
            otherwise
                mkdir(target);
                exportsrc(source,target)
        end
    else
        copyfile(source,target)
    end
end