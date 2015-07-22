function FI = shipma(cmd,varargin)
%SHIPMA Read Shipma project (and embedded) files. (BETA VERSION)
%   STRUCT = SHIPMA('open',FILENAME) opens the specified Shipma file, and
%   (partly) reads the associated embedded data files.
%
%   See also TEKAL, SAMPLES, DELWAQ.

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

switch lower(cmd)
    case 'open'
        FI = LocalShipmaOpen(varargin{:});
    case 'openpar'
        FI = LocalShipmaParameterOpen(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end


function FI = LocalShipmaParameterOpen(FileName,SubType)
FI.FileName = FileName;
[p,f,e] = fileparts(FileName);
if nargin<2
    e = lower(e);
    switch e
        case '.cur'
            SubType = 'current';
        case '.wav'
            SubType = 'waves';
        case '.wnd'
            SubType = 'wind';
        otherwise
            SubType = 'unknown';
    end
end
FI.SubType = SubType;
fid = fopen(FileName,'r');
i = 0;
while ~feof(fid)
    Line = fgetl(fid);
    if ~isempty(Line) && Line(1)~='*'
        if i==0
            i = i+1;
            switch SubType
                case 'current'
                    FI.WaterLevel = sscanf(Line,'%f',1);
                    continue
                case 'waves'
                    FI.WavePeriod = sscanf(Line,'%f',1);
                    continue
            end
        end
        ScaleFactor = sscanf(Line,'%f',1);
        break
    end
end
Data = fscanf(fid,'%f',[4 inf])';
FI.XY = Data(:,1:2);
switch SubType
    case 'current'
        FI.CurrentMagnitude = Data(:,3)*ScaleFactor;
        FI.CurrentToDir     = Data(:,4); % 0 = current to north
    case 'waves'
        FI.WaveHeight = Data(:,3)*ScaleFactor;
        FI.WaveToDir  = Data(:,4);
    case 'wind'
        FI.WindMagnitude = Data(:,3)*ScaleFactor;
        FI.WindFromDir   = Data(:,4); % 0 = wind from north
end
fclose(fid);


function ShipmaUnzipFolderDelete(unzipDir)
rmdir(unzipDir,'s')


function FI = LocalShipmaOpen(FileName)
[p,f,e]=fileparts(FileName);
%
% First check whether this is an XML file to prevent the error message:
% [Fatal Error] FileName:1:1: Content is not allowed in prolog.
fid = fopen(FileName,'r');
firstchar = fread(fid,[1 2],'*char');
fclose(fid);
if isequal(firstchar,'<?')
    % OK, this might be an XML file
elseif isequal(firstchar,'PK')
    % OK, this might be a zipped file. Create a temp folder for its
    % contents.
    unzipDir = tempname;
    %
    % Create a cleanup function for the temp folder. This function will be
    % called automatically if an error occurs during any of the file
    % reading below, or if reading is successful the cleanup action is
    % postponed until the FI object is deleted upon file closure or program
    % end.
    %
    CleanUp = onCleanup(@()ShipmaUnzipFolderDelete(unzipDir));
    %
    % try to unzip ... this may result in error if FileName isn't a zipfile
    % after all. The resulting unzipDir is created always which will be
    % deleted by the cleanup function created above.
    %
    unzip(FileName,unzipDir)
    FileName = [unzipDir filesep f e];
    if ~exist(FileName,'file')
        FileName = [unzipDir filesep 'scenario.xml'];
    end
    FI = LocalShipmaOpen(FileName);
    FI.CleanUp = CleanUp;
    return
else
    error('Shipma Project File should start with "<?" or "PK".')
end
%
FI.FileName = FileName;
FI.UnzipFolder = [p filesep strrep(f,'.','_') '_' e(2:end) '.emb'];
if ~exist(FI.UnzipFolder,'dir') % for the active scenario before an official save
    FI.UnzipFolder = p;
end
%
FI.XML = xmlread(FileName);
Doc = FI.XML.getFirstChild;
if FI.XML.getLength~=1 || ...
        ~isequal(char(Doc.getNodeName),'documentTag') || ...
        ~isequal(char(Doc.getAttribute('key')),'Shipma Projects')
    error('First level of Shipma XML file should be a single documentTag with key=Shipma Projects')
end
Children = getChildren(Doc);
Name = getNodeName(Children,true);
IntProp = Children(strcmp(Name,'internal_properties'));
Proj = Children(strcmp(Name,'shipmaProject'));
nProj = length(Proj);
FI.TempFilePath = char(IntProp.getFirstChild.getTextContent);
%
FI.Project(nProj).Name = '';
nCasesTot = 0;
for p = 1:nProj
    Children = getChildren(Proj(p));
    FI.Project(p).Name = getName(Proj(p));
    ProjFolder = fullfile(FI.UnzipFolder,['shi_' FI.Project(p).Name]);
    FI.Project(p).Ships = getMembers(Children(1));
    FI.Project(p).Ships.Data = getShipData(FI.Project(p).Ships.XML);
    FI.Project(p).Sceneries = getMembers(Children(2));
    FI.Project(p).Sceneries.Data = getSceneryData(FI.Project(p).Sceneries.XML,ProjFolder);
    FI.Project(p).Environments = getMembers(Children(3));
    FI.Project(p).Environments = getEnvironmentData(FI.Project(p).Environments,ProjFolder);
    FI.Project(p).Manoeuvres = getMembers(Children(4));
    FI.Project(p).Pilots = getMembers(Children(5));
    FI.Project(p).TugScenarios = getMembers(Children(6));
    FI.Project(p).Cases = getMembers(Children(7));
    FI.Project(p).Cases.Data = getCaseData(FI.Project(p).Cases.XML,ProjFolder);
    Data = FI.Project(p).Cases.Data;
    nCases = length(Data);
    for i=1:nCases
        Data(i).shipNr    = ustrcmpi(Data(i).shipId,FI.Project(p).Ships.Names);
        Data(i).windNr    = ustrcmpi(Data(i).windId,FI.Project(p).Environments.Winds.Names);
        Data(i).wavesNr   = ustrcmpi(Data(i).wavesId,FI.Project(p).Environments.Waves.Names);
        Data(i).currentNr = ustrcmpi(Data(i).currentId,FI.Project(p).Environments.Currents.Names);
        Data(i).swellNr   = ustrcmpi(Data(i).swellId,FI.Project(p).Environments.Swells.Names);
        Data(i).sceneryNr = ustrcmpi(Data(i).sceneryId,FI.Project(p).Sceneries.Names);
    end
    FI.Project(p).Cases.Data = Data;
    nCasesTot = nCasesTot + nCases;
end
%
FI.Case.Name = cell(nCasesTot,1);
FI.Case.Project = zeros(1,nCasesTot);
FI.Case.Case = zeros(1,nCasesTot);
j = 0;
for p=1:length(FI.Project)
    nCases = length(FI.Project(p).Cases.Data);
    for i=1:nCases
        j = j+1;
        FI.Case.Name{j}    = sprintf('%s\\%s',FI.Project(p).Name,FI.Project(p).Cases.Names{i});
        FI.Case.Project(j) = p;
        FI.Case.Case(j)    = i;
    end
end


function S = getMembers(Node)
S.XML = getChildren(Node);
S.Names = getName(S.XML,true);


function Name = getName(Node,forceCell)
nNode = length(Node);
if nNode==1
    Name = char(Node.getAttribute('key'));
    if nargin>1 && forceCell
        Name = {Name};
    end
else
    Name = cell(1,nNode);
    for i = 1:nNode
        Name{i} = char(Node(i).getAttribute('key'));
    end
end


function Name = getNodeName(Node,forceCell)
nNode = length(Node);
if nNode==1
    Name = char(Node.getNodeName);
    if nargin>1 && forceCell
        Name = {Name};
    end
else
    Name = cell(1,nNode);
    for i = 1:nNode
        Name{i} = char(Node(i).getNodeName);
    end
end

function Children = getChildren(Node)
nChild = Node.getLength;
c = cell(1,nChild);
c{1} = Node.getFirstChild;
for i = 2:nChild
    c{i} = c{i-1}.getNextSibling;
end
Children = [c{:}];


function Env = getEnvironmentData(Env,UnzipFolder)
for i = 1:length(Env.Names)
    envInstances = getChildren(Env.XML(i));
    if isempty(envInstances)
        envNames = {};
    else
        envNames = getName(envInstances,true);
    end
    %
    envData = [];
    nInstances = length(envInstances);
    for j = 1:nInstances
        instanceParam = getChildren(envInstances(j));
        instanceParamName = getName(instanceParam,true);
        p = 0;
        for ip = 1:length(instanceParamName)
            iPN = instanceParamName{ip};
            switch iPN
                case {'simpleSelected','fileSelected'}
                    envData(j).(iPN) = getbool(instanceParam(ip));
                case 'file'
                    FileDir = fullfile(UnzipFolder,'shi_Environment',['shi_' Env.Names{i}],['shi_' envNames{j}],'Emb_file','embCtnt');
                    embFiles = dir(FileDir);
                    if length(embFiles)>2
                        FileName = fullfile(FileDir,embFiles(3).name);
                    else
                        FileName = '';
                    end
                    envData(j).file = FileName;
                case 'originalPath'
                    % skip
                case 'description'
                    envData(j).description = char(instanceParam(ip).getTextContent);
                otherwise
                    p = p+1;
                    envData(j).Props(p).Quant = instanceParamName{ip};
                    unit = char(instanceParam(ip).getNodeName);
                    val = char(instanceParam(ip).getTextContent);
                    switch unit
                        case 'Double'
                            envData(j).Props(p).Unit = '';
                            envData(j).Props(p).Value = str2double(val);
                        otherwise
                            envData(j).Props(p).Unit = unit;
                            envData(j).Props(p).Value = str2double(val);
                    end
            end
        end
    end
    %
    Env.(Env.Names{i}).XML   = envInstances;
    Env.(Env.Names{i}).Names = envNames;
    Env.(Env.Names{i}).Data  = envData;
end


function bool = getbool(XML)
bool = char(XML.getTextContent);
switch bool
    case 'true'
        bool = true;
    case 'false'
        bool = false;
end

function Data = getSceneryData(Sceneries,UnzipFolder)
nSceneries = length(Sceneries);
Data(nSceneries).fairwayContourFile = [];
Data(nSceneries).banksuctionFile = [];
Data(nSceneries).bottomFile  = [];
Data(nSceneries).description = [];
for i = 1:nSceneries
    SceneryName = char(Sceneries(i).getAttribute('key'));
    %
    SceneProps = getChildren(Sceneries(i));
    ScenePropNames = getName(SceneProps,true);
    for ip = 1:length(ScenePropNames)
        switch ScenePropNames{ip}
            case {'fairwayContourFile','banksuctionFile','bottomFile'}
                % ### data is contained in an embedded file ###
                FileDir = fullfile(UnzipFolder,'shi_Sceneries',['shi_' SceneryName],['Emb_' ScenePropNames{ip}],'embCtnt');
                embFiles = dir(FileDir);
                if length(embFiles)>2
                    FileName = fullfile(FileDir,embFiles(3).name);
                else
                    FileName = '';
                end
                Data(i).(ScenePropNames{ip}) = FileName;
            case 'description'
                Data(i).description = char(SceneProps(ip).getTextContent);
        end
    end
end


function Data = getCaseData(Cases,UnzipFolder)
nCases = length(Cases);
if nCases==0
    Data = [];
    return
end
Data(nCases).Props = [];
for i = 1:nCases
    CaseName = char(Cases(i).getAttribute('key'));
    %
    CaseDef = getNamedChild(Cases(i),'CaseDefinition');
    CaseComposition = getNamedChild(CaseDef,'CaseComposition');
    CaseCompItems = getChildren(CaseComposition);
    CaseCompositionNames = getName(CaseCompItems,true);
    for ic = 1:length(CaseCompositionNames)
        switch CaseCompositionNames{ic}
            case {'shipId','windId','wavesId','currentId','swellId','sceneryId'}
                Data(i).(CaseCompositionNames{ic}) = char(CaseCompItems(ic).getTextContent);
            case {'windIsSelected','wavesIsSelected', ...
                    'currentIsSelected','swellIsSelected', ...
                    'sceneryIsSelected'}
                Data(i).(CaseCompositionNames{ic}) = getbool(CaseCompItems(ic));
        end
    end
    %
    CaseDir = fullfile(UnzipFolder,'shi_Cases',['shi_' CaseName],'Shi_results','Wor_workDir','embCtnt','containedFiles');
    FileName = fullfile(CaseDir,'track.his');
    if exist(FileName)
        Data(i).TimeSeries = delwaq('open',FileName);
        Data(i).TimeSeries.T0 = 0; % overrule header information
    else
        Data(i).TimeSeries = [];
    end
    Data(i).trackFile  = fullfile(CaseDir,'track.trk');
    Data(i).bottomFile = fullfile(CaseDir,'shipma.BOT');
end


function Data = getShipData(Ships)
nShips = length(Ships);
Data(nShips).Props = [];
for i = 1:nShips
    Data(i).Props = getShipProps(Ships(i));
end


function ShipProps = getShipProps(Ship)
Props = getChildren(Ship);
nProp = length(Props);
ShipProps(nProp).Unit = [];
for p = 1:nProp
    ShipProps(p).Unit = char(Props(p).getTagName);
    ShipProps(p).Quant = char(Props(p).getAttribute('key'));
    switch ShipProps(p).Quant
        case {'frontalArea','lateralArea','liftArea','yawRefLength', ...
                'heelRefLength','trimRefLength','loa','lpp','beam', ...
                'draught','mass','momOfInertia','xCog', ...
                'manoeuvringSpeed','maxRudderAngle','maxRudderRate', ...
                'maxRevAhead','maxRevAstern','maxVelocityChangeRPM', ...
                'maxPropellerAcceleration'}
            ShipProps(p).Value = str2double(char(Props(p).getTextContent));
        case 'contour'
            checkName(Props(p),'Contour')
            Contour = getChildren(Props(p)); % String and Points
            if checkName(Contour(1),'String')
                checkName(Contour(1),'String')
                checkName(Contour(2),'Points')
                Points = getChildren(Contour(2));
            else
                Points = Contour;
            end
            if length(Points)>0
                checkName(Points(1),'collectionElement')
                nPoints = length(Points);
                contour = zeros(nPoints,2);
                for i=1:nPoints
                    Point = getChildren(Points(i));
                    contour(i,1) = str2double(char(Point(1).getTextContent));
                    contour(i,2) = str2double(char(Point(2).getTextContent));
                end
            else
                contour = zeros(0,2);
            end
            ShipProps(p).Value = contour;
        otherwise
            if isequal(class(Props(p).getFirstChild),'org.apache.xerces.dom.DeferredTextImpl')
                ShipProps(p).Value = char(Props(p).getTextContent);
            else
                ShipProps(p).Value = getChildren(Props(p));
            end
    end
end

function OK = checkName(Item,name)
nodeName = char(Item.getNodeName);
ok = isequal(nodeName,name);
if nargout==0
    if ~ok
        error('Encountered tag <%s> while expecting tag <%s>',nodeName,name)
    end
else
    OK = ok;
end

function Item = getNamedChild(Parent,name)
Items = getChildren(Parent);
for i = 1:length(Items)
    if checkName(Items(i),name)
        Item = Items(i);
        return
    end
end
error('Tag <%s> does not include child tag <%s>',char(Parent.getNodeName),name)