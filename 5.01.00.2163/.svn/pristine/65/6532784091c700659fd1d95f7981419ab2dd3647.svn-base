function DirectoryName = ui_getdir(StartPath,Title)
%UI_GETDIR Compileable platform independent open directory dialog box.
%
%   See UIGETDIR.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2012 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

if nargin<1
    StartPath = pwd;
elseif ~ischar(StartPath)
    error('Invalid first argument.');
end
if nargin<2
    Title = 'Browse For Folder';
elseif ~ischar(Title)
    error('Invalid second argument.');
end

if ~isstandalone
    %
    % Use FEVAL formulation to exclude uigetdir from compilation
    % (function _mlfNUigetdir not available).
    %
    DirectoryName = feval('uigetdir',StartPath,Title);
    return
elseif ispc
    %
    % Standalone Windows alternative: UIGETFOLDER
    %
    DirectoryName = uigetfolder(Title,StartPath);
    return
end

%
% Standalone Linux alternative: MATLAB implementation
%

mfig = findall(0,'tag','UIGETDIR');
switch StartPath
    case 'ui_getdir:cancel'
        set(mfig,'userdata',0,'visible','off')
        return
    case 'ui_getdir:ok'
        set(mfig,'visible','off')
        return
    case 'ui_getdir:newfolder'
        dirlist = findobj(mfig,'tag','dirlist');
        line = get(dirlist,'value');
        list = get(dirlist,'string');
        full = get(dirlist,'userdata');
        %
        parentdir = full{line};
        %
        prompt={'Name of directory:'};
        name='Name of directory';
        numlines=1;
        defaultanswer={'New Folder'};
        answer=inputdlg(prompt,name,numlines,defaultanswer);
        %
        if ~isempty(answer)
            newdir = answer{1};
            mkdir(parentdir,newdir);
            %
            switch strtok(list{line})
                case '+'
                    %
                    % Directory with a + sign: expand
                    %
                    changelist(mfig)
                case '-'
                    %
                    % Directory with a - sign: collapse and expand
                    %
                    changelist(mfig)
                    changelist(mfig)
                otherwise
                    %
                    % Directory without a subdirectory
                    %
                    dn = min(find(list{line}~=' '));
                    list{line}(dn-2)='+';
                    set(dirlist,'string',list)
                    changelist(mfig)
            end
            %
        end
        %
        return
    case 'ui_getdir:select'
        dirlist = findobj(mfig,'tag','dirlist');
        line = get(dirlist,'value');
        list = get(dirlist,'string');
        full = get(dirlist,'userdata');
        set(mfig,'userdata',full{line})
        %
        seltype = get(mfig,'SelectionType');
        if strcmp(seltype,'open') | isequal(strtok(list{line}),'+')
            changelist(mfig);
        end
        return
    otherwise
        %
        % Unknown command, so, it should be a directory name.
        %
        if ~exist(StartPath)
            error('Invalid start directory')
        elseif isequal(StartPath(end),filesep)
            StartPath = StartPath(1:end-1);
        end
end

%
% Initialize
%
screensize=get(0,'screensize');
Active = [1 1 1];
%
width = 400;
pos = [width 350];
pos = [screensize(3:4)/2-pos/2 pos];
delete(mfig)
mfig = qp_uifigure(Title,'ui_getdir:cancel','UIGETDIR',pos,'ui_getdir');
set(mfig,'userdata',StartPath)
%
voffset = pos(4)-10-18;
uicontrol('Parent',mfig, ...
    'Position',[11 voffset width-20 18], ...
    'String','Select Directory to Open', ...
    'Style','text', ...
    'HorizontalAlignment','left');
%
voffset = voffset-282;
uicontrol('Parent',mfig, ...
    'Position',[11 voffset width-20 280], ...
    'String',{'','Please wait:','  Scanning directories ...'}, ...
    'Style','listbox', ...
    'Callback','ui_getdir ui_getdir:select', ...
    'FontName',get(0,'FixedWidthFontName'), ...
    'BackgroundColor',Active, ...
    'Tag','dirlist', ...
    'HorizontalAlignment','left');
%
voffset = voffset-30;
if ~isstandalone
    uicontrol('Parent',mfig, ...
        'Position',[11 voffset 100 20], ...
        'String','Make New Folder', ...
        'Callback','ui_getdir ui_getdir:newfolder', ...
        'Style','pushbutton');
end
uicontrol('Parent',mfig, ...
    'Position',[width-139 voffset 60 20], ...
    'String','OK', ...
    'Callback','ui_getdir ui_getdir:ok', ...
    'Style','pushbutton');
uicontrol('Parent',mfig, ...
    'Position',[width-69 voffset 60 20], ...
    'String','Cancel', ...
    'Callback','ui_getdir ui_getdir:cancel', ...
    'Style','pushbutton');
%
%try
set(mfig,'visible','on')
drawnow
populatelist(mfig,StartPath);
waitfor(mfig,'visible','off')
%catch
%end
if ishandle(mfig)
    DirectoryName = get(mfig,'userdata');
    delete(mfig)
else
    DirectoryName = 0;
end


function changelist(mfig)
dirlist = findobj(mfig,'tag','dirlist');
line = get(dirlist,'value');
list = get(dirlist,'string');
full = get(dirlist,'userdata');
%
switch strtok(list{line})
    case '+'
        %
        % Directory with a + sign: expand
        %
        d = dir(full{line});
        d = d([d.isdir]);
        d = d(3:end);
        %
        pls = strfind(list{line},'+');
        pls = pls(1);
        if ~isempty(d)
            list{line}(pls) = '-';
            spaces = repmat(' ',1,pls+1);
            %
            inlist = {};
            infull = {};
            for i = 1:length(d)
                fullname = [full{line} filesep d(i).name];
                infull{end+1,1} = fullname;
                d2 = dir(fullname);
                d2 = d2([d2.isdir]);
                if length(d2)>2 % excluding '.' and '..'
                    subdir = '+ ';
                else
                    subdir = '  ';
                end
                inlist{end+1,1} = [spaces subdir d(i).name];
            end
            list = cat(1,list(1:line),inlist,list(line+1:end));
            full = cat(1,full(1:line),infull,full(line+1:end));
        else
            list{line}(pls) = ' ';
        end
    case '-'
        %
        % Directory with a - sign: collapse
        %
        isub = strmatch(full{line},full);
        full(isub(2:end)) = [];
        list(isub(2:end)) = [];
        %
        pls = strfind(list{line},'-');
        pls = pls(1);
        list{line}(pls) = '+';
    otherwise
        %
        % Directory without a + or - sign: directory without a subdirectory.
        %
end
%
set(findobj(mfig,'tag','dirlist'),'string',list,'userdata',full)


function populatelist(mfig,StartPath)
list = {};
full = {};
if ispc
    drives = finddrives;
    for i=1:length(drives)
        Path = [drives{i} ':'];
        d = dir(Path);
        if sum([d.isdir])>2 % excluding '.' and '..'
            sign = '+';
        else
            sign = ' ';
        end
        list{end+1,1} = [sign ' ' Path];
        full{end+1,1} = Path;
    end
else
    list = {filesep};
end
%
dirlist = findobj(mfig,'tag','dirlist');
set(dirlist,'string',list,'userdata',full)
expand = 1;
while expand
    if ispc
        expand = ustrcmpi(full,StartPath);
    else
        expand = 0;
    end
    if expand>0
        set(dirlist,'value',expand)
        changelist(mfig)
        full = get(dirlist,'userdata');
        if length(full{expand})==length(StartPath)
            break
        end
    else
        break
    end
end


function drives = finddrives
drives = {};
for i = 'A':'Z'
    drive = [char(i) ':'];
    if exist(drive,'dir')
        drives{end+1} = char(i);
    end
end
