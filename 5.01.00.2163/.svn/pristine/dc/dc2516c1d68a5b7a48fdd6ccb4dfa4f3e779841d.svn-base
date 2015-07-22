function [revmin,revmax,changed]=determine_revision(dirname,dbid)
%DETERMINE_REVISION Determine subversion update revision numbers.
%   [REVMIN,REVMAX,CHANGED] = DETERMINE_REVISION(DIR) determines the
%   subversion update revision numbers for the specified directory DIR. The
%   values returned are the minimum/oldest update number REVMIN, the
%   maximum/youngest update number REVMAX and a flag CHANGED indicating
%   whether the local code has been modified since these updates.

%   $Id$

iter = 1;
found = 0;
if nargin<2
    dbid = 1;
end
while ~found
    switch iter
        case 1
            SvnVersion = '../../../../third_party_open/subversion/bin/win32/svnversion.exe';
        case 2
            SvnVersion = SvnVersion(4:end); % one level less deep
        case 2
            svnbin = getenv('SVN_BIN_PATH');
            SvnVersion = [svnbin filesep 'svnversion.exe'];
        case 3
            svnbin = 'c:\Program Files\Subversion\bin';
            SvnVersion = [svnbin filesep 'svnversion.exe'];
        case 4
            [s,SvnVersion] = system('which svnversion');
            if s~=0
                SvnVersion = 'The WHICH command failed';
            end
        case 5
            dprintf(dbid,'Unable to locate SVNVERSION program.\nUsing built-in implementation of svnversion.\n')
            [revmin,revmax,changed] = svnversion(dirname,dbid);
            return
    end
    %
    if exist(SvnVersion,'file')
        break
    end
    iter = iter+1;
end

[s,revstring] = system(['"' SvnVersion '" ' dirname]);
if s==0
    changed = ismember('M',revstring);
    rev = sscanf(revstring,'%i:%i');
    if isempty(rev) %exported
        revmin = 0;
        revmax = 0;
        changed = 1;
    elseif length(rev)==1
        revmin = rev;
        revmax = rev;
    else
        revmin = rev(1);
        revmax = rev(2);
    end
else
    dprintf(dbid,'Unable to execute SVNVERSION program.\nUsing built-in implementation of svnversion.\n')
    [revmin,revmax,changed] = svnversion(dirname,dbid);
end


function [min_update,max_update,changed] = svnversion(dirname,dbid)
min_update = inf;
max_update = -inf;
changed = 0;
d = dir(dirname);
for i = 1:length(d)
    if ismember(d(i).name,{'.','..','.svn'})
        % do nothing
    elseif d(i).isdir
        [min1,max1,changed1] = svnversion(fullfile(dirname,d(i).name),dbid);
        min_update = min(min_update,min1);
        max_update = max(max_update,max1);
        changed = changed | changed1;
    end
end
entries = get_svn_entries(dirname);
ref = fullfile(dirname,'.svn/text-base');
for i = 1:length(entries)
    reffile = fullfile(ref,[entries(i).filename '.svn-base']);
    newfile = fullfile(dirname,entries(i).filename);
    min_update = min(min_update,entries(i).last_updated);
    max_update = max(max_update,entries(i).last_updated);
    if ~exist(newfile,'file')
        % file has been removed
        dprintf(dbid,'File removed: "%s"\n',newfile);
        changed = 1;
    else
        changed = changed | is_file_modified(reffile,newfile,dbid);
    end
end

function entries = get_svn_entries(dirname)
entries = [];
fid = fopen(fullfile(dirname,'.svn','entries'),'r');
if fid<0
    % no subversion directory
    return
end
str = fread(fid,[1 inf],'*char');
fclose(fid);
entry = strfind(str,char(12));
%
substr = str(1:entry(1)-1);
lines = strfind(substr,char(10));
updatestr = substr(lines(3)+1:lines(4)-1);
updatenr = str2double(updatestr);
%
j = 0;
for i=1:length(entry)-1
    substr = str(entry(i)+2:entry(i+1)-1);
    lines = strfind(substr,char(10));
    if strcmp(substr(lines(1)+1:lines(2)-1),'file')
        j = j+1;
        entries(j).filename = substr(1:lines(1)-1);
        revstr = substr(lines(9)+1:lines(10)-1);
        entries(j).last_revised = str2double(revstr);
        updatestr = substr(lines(2)+1:lines(3)-1);
        if isempty(updatestr)
            entries(j).last_updated = updatenr;
        else
            entries(j).last_updated = str2double(updatestr);
        end
    end
end

function changed = is_file_modified(reffile,newfile,dbid)
fid = fopen(reffile,'r');
file1 = fread(fid,[1 inf],'*char');
fclose(fid);
%
fid = fopen(newfile,'r');
file2 = fread(fid,[1 inf],'*char');
fclose(fid);
%
changed = 1;
if isequal(file1,file2)
    changed = 0;
else
    Ids = [];
    for keyw = {'Id','Date','Author','Revision','HeadURL'}
        kw = keyw{1};
        Ids = cat(2,Ids,strfind(file1,['$' kw '$']));
    end
    Ids = sort(Ids);
    for i = 1:length(Ids)
        kw = sscanf(file1(Ids(i)+1:end),'%[A-Za-z]');
        if ~strcmp(file2(Ids(i)+(0:length(kw))),['$' kw])
            break
        else
            Amp = strfind(file2(Ids(i)+1:end),'$');
            if isempty(Amp)
                break
            else
                file2 = cat(2,file2(1:Ids(i)),kw,file2(Ids(i)+Amp(1):end));
            end
        end
    end
    if isequal(file1,file2)
        changed = 0;
    end
end

if changed
    dprintf(dbid,'File changed: "%s"\n',newfile);
end


function dprintf(fid,varargin)
if fid~=0
    fprintf(fid,varargin{:});
end