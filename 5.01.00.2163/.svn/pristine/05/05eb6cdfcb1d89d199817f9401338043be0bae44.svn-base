function qp_validate(val_dir)
%QP_VALIDATE Helper function to validate Delft3D-QUICKPLOT.

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

baseini='validation.ini';
if nargin<1
   %
   % Would prefer to use uigetdir, but that doesn't compile.
   %
   [dummy,val_dir]=uigetfile(baseini,'Select directory containing validation cases');
   if ~ischar(val_dir)
      return
   end
end
if ~isstandalone
   qp_path=which('d3d_qp');
   qp_path=fileparts(qp_path);
   addpath(qp_path)
end
currdir=pwd;
logid=-1;
logname='validation_log.html';
sc={'<font color=FF0000>Failed</font> (open <a href="reference">reference folder</a>)','<font color=00AA00>Successful</font>'};
AnyFail=0;
NTested=0;
NFailed=0;
fs=filesep;
sdata=['..',fs,'data',fs];
sref=['..',fs,'reference',fs];
swrk=['..',fs,'work',fs];
slog=['..',fs,'logfiles',fs];
T_=1; ST_=2; M_=3; N_=4; K_=5;
%
Hpb=progressbar('cancel','delete(gcbf)','pause','on');
pHpb=get(Hpb,'position');
ssz=get(0,'screensize');
set(Hpb,'position',[10 ssz(4)-pHpb(4)-30 pHpb(3) pHpb(4)])
%
UserInterrupt=0;
if matlabversionnumber>=7.02
   saveops={'-v6' '-mat'};
elseif matlabversionnumber>=7
   saveops={'-NOUNICODE' '-mat'};
else
   saveops={'-mat'};
end
%
% Allow for a large number of messages
%
ui_message('max',10000)
set(findall(0,'tag','UI_MESSAGE window'),'position',[10 40 pHpb(3) ssz(4)-pHpb(4)-120])
DefFigProp.defaultfigure = qp_settings('defaultfigure',-999);
qp_settings('defaultfigure','')
DefFigProp.defaultfigurecolor = qp_settings('defaultfigurecolor',-999);
qp_settings('defaultfigurecolor',get(0,'factoryuicontrolbackgroundcolor')*255)
DefFigProp.defaultaxescolor = qp_settings('defaultaxescolor',-999);
qp_settings('defaultaxescolor',[255 255 255])
DefFigProp.boundingbox = qp_settings('boundingbox',-999);
qp_settings('boundingbox',0)
try
   full_ln=fullfile(val_dir,logname);
   if ~localexist(fullfile(val_dir,baseini))
      ui_message('error','Invalid validation directory: ''%s'' is missing.',baseini)
      if ishandle(Hpb)
         delete(Hpb);
      end
      return
   end
   logid=fopen(full_ln,'w');
   if logid<0
      ui_message('error',{'Cannot open logfile for validation report.','Stopping validation process.'})
      return
   else
      writeheader(logid)
      fprintf(logid,'<table bgcolor=CCCCFF>\n<tr bgcolor=AAAAFF><td><b>Validation case</b></td><td><b>Result<br>file read</b></td><td><b>Result<br>log files</b></td><td><b>View Log</b></td></tr>\n');
   end
   cd(val_dir)
   d=dir('*');
   for i=length(d):-1:1
      if ~d(i).isdir || strcmp(d(i).name(1),'.') || strcmp(d(i).name(1),'..')
         d(i)=[];
      end
   end
   %
   % Sorting required for MATLAB 5.3 ...
   %
   [sorteddirnames,I]=sort(upper({d.name}));
   d=d(I);
   %
   pbfrac=1/max(1,length(d));
   for i=1:length(d)
      progressbar((i-1)*pbfrac,Hpb,'title',d(i).name);
      ui_message('',['Case: ',d(i).name])
      NTested=NTested+1;
      DiffFound=0;
      color='';      frcolor='00AA00';      lgcolor='000000';
      result='';     frresult='PASSED';     lgresult='N/A';
      logid2=[];
      CrashMsg='';
      try
         cd(fullfile(val_dir,d(i).name));
         CaseInfo='case.ini';
         if isempty(dir('data'))
            ensure_directory('data');
            movefile('*','data');
         end
         dd=dir(['data' fs '*']);
         if ~localexist(CaseInfo) && length(dd)>2
            CID=inifile('new');
            CID=inifile('set',CID,'','FileName',dd(3).name);
            CID=inifile('set',CID,'logfilecheck','example.qplog','example.png');
            inifile('write',CaseInfo,CID);
         end
         if localexist(CaseInfo)
            CaseInfo=inifile('open',CaseInfo);
            logid2=fopen(logname,'w');
            writeheader(logid2,d(i).name)
            %
            % check for log files to run ...
            %
            if isempty(dir('logfiles'))
               ensure_directory('logfiles');
            end
            logs=dir(['logfiles' fs '*.qplog']);
            logm=dir(['logfiles' fs '*.m']);
            logs=cat(1,logs,logm);
            %
            % check for reference directory...
            %
            if isempty(dir('reference'))
               ensure_directory('reference');
            end
            %
            % Switch to work directory...
            %
            if isempty(dir('work'))
               ensure_directory('work');
            end
            cd('work')
            workdir=pwd;
            %
            % Delete all files in work directory (but make sure that we actually are in the work directory) ...
            %
            if ~isequal(workdir(end-3:end),'work')
               fprintf(logid2,'No work directory?<br>/n');
               error('No work directory?')
            end
            delete('*')
            FileName=inifile('get',CaseInfo,'*','FileName','');
            FileName2=inifile('get',CaseInfo,'*','FileName2','');
            if isempty(FileName2)
               FileName2={};
            else
               FileName2={[sdata,FileName2]};
            end
            if length(FileName)<7 || ~isequal(lower(FileName(1:7)),'http://')
                FileName = [sdata,FileName];
            end
            d3d_qp('openfile',FileName,FileName2{:});
            FI=qpfile;
            if ~isstruct(FI)
               error('Error opening file.');
            end
            [ChkOK,Dms]=qp_getdata(FI,'domains');
            fprintf(logid2,'Reading domains: %s<br>\n',sc{ChkOK+1});
            if isempty(Dms)
               Dms={'no name'};
            end
            dmx=length(Dms);
            Props={};
            for dm=1:dmx
               [Chk,P]=qp_getdata(FI,dm);
               fprintf(logid2,'Reading fields of domain ''%s'': %s<br>\n',Dms{dm},sc{Chk+1});
               ChkOK=Chk&ChkOK;
               for p=1:length(P)
                  if strcmp(P(p).Name,'-------')
                     P(p).Size=[0 0 0 0 0];
                  else
                     [Chk,P(p).Size]=qp_getdata(FI,dm,P(p),'size');
                     drawnow
                     ChkOK=Chk&ChkOK;
                  end
               end
               Props{dm}=P;
            end
            fprintf(logid2,'<hr>\n');
            if ChkOK
               CmpFile='datafields.mat';
               RefFile=[sref CmpFile];
               WrkFile=[swrk CmpFile];
               if localexist(RefFile)
                  cmpFile=localload(RefFile);
                  if isfield(cmpFile,'Props')
                     PrevProps=cmpFile.Props;
                  else
                     PrevProps=cmpFile.Data;
                  end
                  DiffFound=vardiff(Props,PrevProps)>1;
                  fprintf(logid2,'Comparing new and old fields:<br>\n');
                  if DiffFound
                     localsave(WrkFile,Props,saveops);
                     if length(Props)~=length(PrevProps)
                        fprintf(logid2,'Number of domains differs.<br>\n')
                        fprintf(logid2,'Reference data set contains %i domains.<br>\n',length(PrevProps));
                        fprintf(logid2,'New data set contains %i domains.<br>\n',length(Props));
                     else
                        JustAddedData=1;
                        for dm=1:length(Props)
                           Prop = Props{dm};
                           PropRef = PrevProps{dm};
                           pn={Prop.Name};
                           ppn={PropRef.Name};
                           dpn=setdiff(pn,ppn);
                           dppn=setdiff(ppn,pn);
                           if dm>1
                              fprintf(logid2,'<b>Domain ''%s''</b><br>\n',Dms{dm});
                           end
                           if ~isempty(dpn)
                              fprintf(logid2,'New datafields:<br>\n');
                              fprintf(logid2,'<li>%s<br></li>\n',dpn{:});
                              if ~isempty(dppn)
                                 fprintf(logid2,'Deleted datafields:<br>\n');
                                 fprintf(logid2,'<li>%s<br></li>\n',dppn{:});
                              end
                              [common,ipn,ippn]=intersect(pn,ppn);
                              Prop=Prop(ipn);
                              PropRef=PropRef(ippn);
                           elseif ~isempty(dppn)
                              JustAddedData=0;
                              fprintf(logid2,'Deleted datafields:<br>\n');
                              fprintf(logid2,'%s<br>\n',dppn{:});
                              [common,ipn,ippn]=intersect(pn,ppn);
                              Prop=Prop(ipn);
                              PropRef=PropRef(ippn);
                           end
                           %
                           if vardiff(Prop,PropRef)>1
                              JustAddedData=0;
                              vardiff(Prop,PropRef,logid2,'html','Current Data','Reference Data');
                           end
                           drawnow
                        end
                        if JustAddedData
                           DiffFound=0;
                           if strcmp(frcolor,'00AA00')
                              frcolor='000000';
                           end
                        end
                     end
                  end
                  fprintf(logid2,'Conclusion: %s<br>\n',sc{2-DiffFound});
               else
                  localsave(RefFile,Props,saveops);
               end
               if DiffFound
                  frcolor='FF0000';
                  frresult='FAILED: Data fields differ.';
                  ChkOK=0;
               end
            else
               frcolor='FF0000';
               frresult='FAILED: Error while reading data.';
               ChkOK=0;
            end
            %
            NP=length(P);
            NL=length(logs);
            NT=NP+NL;
            if 1%ChkOK
               fprintf(logid2,'<table bgcolor=CCCCFF><tr>%s%s%s</tr>\n', ...
                  '<td bgcolor=AAAAFF><b>Data field</b></td><td bgcolor=AAAAFF><b>Read</b></td><td bgcolor=AAAAFF><b>Compare</b></td></tr>');
               datacheck=inifile('get',CaseInfo,'datacheck','default',1);
               P=Props{dmx};
               for p=1:length(P)
                  if progressbar((i-1+p/NT)*pbfrac,Hpb)<0
                     fprintf(logid2,'</table>\n');
                     UserInterrupt=1;
                     error('User interrupt');
                  end
                  if ~strcmp(P(p).Name,'-------')
                     if P(p).NVal<0
                        fprintf(logid2,'<tr><td>%s</td><td>Check n/a</td><td></td></tr>\n',P(p).Name);
                     elseif ~inifile('get',CaseInfo,'datacheck',P(p).Name,datacheck)
                        fprintf(logid2,'<tr><td>%s</td><td>Skipped</td><td>Skipped</td></tr>\n',P(p).Name);
                     else
                        PName=P(p).Name;
                        PName_double = strmatch(PName,{P(1:p-1).Name},'exact');
                        PName=str2file(PName);
                        CmpFile=[PName '.mat'];
                        if PName_double
                           CmpFile=[PName sprintf('.(%i).mat',PName_double+1)];
                        end
                        RefFile=[sref CmpFile];
                        WrkFile=[swrk CmpFile];
                        idx={};
                        subf={};
                        if P(p).DimFlag(ST_)
                           idx={1};
                           if P(p).DimFlag(T_)
                               if P(p).DimFlag(M_) || P(p).DimFlag(N_)
                                   idx={P(p).Size(T_) idx{:}};
                               else
                                   idx={1:min(10,P(p).Size(T_)) idx{:}};
                               end
                           end
                        end
                        [Chk,subfields]=qp_getdata(FI,dm,P(p),'subfields');
                        if Chk && ~isempty(subfields)
                           subf={length(subfields)};
                        end
                        [Chk,Data]=qp_getdata(FI,dm,P(p),'griddata',subf{:},idx{:});
                        fprintf(logid2,'<tr><td>%s</td><td>%s</td>',P(p).Name,sc{Chk+1});
                        if ~Chk
                           frcolor='FF0000';
                           frresult=sprintf('FAILED: Error retrieving data for ''%s''.',P(p).Name);
                           ChkOK=0;
                        elseif localexist(RefFile)
                           cmpFile=localload(RefFile);
                           PrevData=cmpFile.Data;
                           %
                           if isfield(Data,'TRI')
                              Data.TRI = sortrows(sort(Data.TRI')'); %#ok<TRSRT>
                           end
                           if isfield(PrevData,'TRI')
                              PrevData.TRI = sortrows(sort(PrevData.TRI')'); %#ok<TRSRT>
                           end
                           %
                           DiffFound=vardiff(Data,PrevData);
                           addedfields = {};
                           if DiffFound==2
                              newfields=fields(Data);
                              oldfields=fields(PrevData);
                              if all(ismember(oldfields,newfields))
                                 addedfields = setdiff(newfields,oldfields);
                                 newData = Data;
                                 for f = 1:length(addedfields)
                                    newData = rmfield(newData,addedfields{f});
                                 end
                                 DiffFound=vardiff(newData,PrevData)>1;
                                 if ~DiffFound
                                    DiffFound = -1;
                                 end
                              else
                                 % new data structure misses some fields
                                 % that were included in the old
                                 % (reference) data structure
                                 DiffFound = 1;
                              end
                           else
                              newfields='';
                              DiffFound=DiffFound>1;
                           end
                           if DiffFound
                              localsave(WrkFile,Data,saveops);
                              fprintf(logid2,'<td>');
                              if ~isempty(addedfields)
                                 fprintf(logid2,'New Fields:<br>\n');
                                 fprintf(logid2,'%s<br>\n',addedfields{:});
                                 if isequal(frcolor,'00AA00') % switch to black colour only if no real error has occurred
                                    frcolor='000000';
                                 end
                                 if DiffFound>0
                                    fprintf(logid2,'<br>\n');
                                 end
                              end
                              if DiffFound>0
                                 fprintf(logid2,'%s<br>\n',sc{2-DiffFound});
                                 if ~isempty(addedfields)
                                    vardiff(newData,PrevData,logid2,'html','Current Data','Reference Data');
                                 else
                                    vardiff(Data,PrevData,logid2,'html','Current Data','Reference Data');
                                 end
                                 frcolor='FF0000';
                              end
                              fprintf(logid2,'</td></tr>\n');
                              ChkOK = DiffFound<=0;
                              if ~ChkOK
                                 frresult=sprintf('FAILED: Data changed for ''%s''.',P(p).Name);
                              end
                              localsave([PName,'.mat'],Data,saveops);
                           else
                              fprintf(logid2,'<td>%s</td></tr>\n',sc{2-DiffFound});
                           end
                        else
                           localsave(RefFile,Data,saveops);
                           fprintf(logid2,'<td>Created</td></tr>\n');
                           if isequal(frresult,'PASSED')
                              frcolor='000000';
                              frresult='CREATED';
                           end
                        end
                     end
                  else
                     fprintf(logid2,'<tr><td colspan=3 bgcolor=AAAAFF></td></tr>\n');
                  end
                  flush(logid2)
               end
               fprintf(logid2,'</table>\n');
               drawnow
            end
            %
            if isempty(logs)
               fprintf(logid2,'<hr>\nNo log files to run for validation.<br>\n');
            else
               lgcolor='00AA00';
               lgresult='PASSED';
               for lg=1:length(logs)
                  if progressbar((i-1+(NP+lg)/NT)*pbfrac,Hpb)<0
                     UserInterrupt=1;
                     error('User interrupt');
                  end
                  logf=logs(lg).name;
                  fprintf(logid2,'<hr>\nRunning log file: %s<br>\n',logf);
                  d3d_qp('reset');
                  d1=dir; d1={d1.name};
                  d3d_qp('run',[slog,logf]);
                  d2=dir; d2={d2.name};
                  checkfs=setdiff(d2,d1);
                  if isempty(checkfs)
                     fprintf(logid2,'No file to check.');
                     lgcolor='FF0000';
                     lgresult=sprintf('FAILED: No check on ''%s''.',logf);
                  else
                     %checkf=inifile('get',CaseInfo,'logfilecheck',logf,'');
                     for icheck=1:length(checkfs)
                        checkf=checkfs{icheck};
                        fprintf(logid2,'Checking File ''%s'': ',checkf);
                        showfig=0;
                        [dummypath,dummyfile,ext]=fileparts(checkf);
                        reffile=[sref,checkf];
                        args={};
                        switch lower(ext)
                           case '.png'
                              % matching DPI for default size: 53, 54, 56, 57 61, 62, 64, 65, 66, 69
                              args={'skip',256};
                              showfig=1;
                           case '.mat'
                              args={'skip',128};
                           case '.dbf'
                              args={'skip',16};
                        end
                        if ~localexist(reffile)
                           copyfile(checkf,reffile);
                           fprintf(logid2,'reference file created<br>\n');
                           if isequal(lgresult,'PASSED')
                              lgcolor='000000';
                              lgresult='CREATED';
                           end
                           if showfig
                              fprintf(logid2,'<img src=''reference/%s''><br>\n',checkf);
                           end
                        else
                           [Eql,Msg]=filesequal(checkf,reffile,args{:});
                           hasdiff=0;
                           diffimg={};
                           if ~Eql
                              try
                                 switch lower(ext)
                                    case '.png'
                                       I1=imread(checkf);
                                       I2=imread(reffile);
                                       if ~isequal(size(I1),size(I2))
                                          Msg='The bitmap sizes are different.';
                                       else
                                          I1=double(I1);
                                          I2=double(I2);
                                          diffimg=[checkf(1:end-4) '_diff.png'];
                                          imwrite(1-abs(I1-I2)/255,diffimg);
                                          diffimg={['work/' diffimg]};
                                          hasdiff=1;
                                          Msg='The bitmap images are different.';
                                       end
                                 end
                              catch
                              end
                           end
                           if ~isempty(Msg)
                              Msg=cat(2,': ',Msg);
                           end
                           fprintf(logid2,'%s%s<br>\n',sc{1+Eql},Msg);
                           if Eql
                              if showfig
                                 fprintf(logid2,'<img src=''reference/%s''><br>\n',checkf);
                              end
                           else
                              if showfig
                                 fprintf(logid2,'<table bgcolor=CCCCFF>\n');
                                 files={['reference/' checkf],['work/' checkf],diffimg{:}};
                                 nfiles=length(files);
                                 fprintf(logid2,['<tr>' repmat('<td width=300 bgcolor=AAAAFF>%s</td>',1,nfiles) '</tr>\n'],files{:});
                                 fprintf(logid2,['<tr>',repmat('<td><img src=''%s''></td>',1,nfiles),'</tr>\n'],files{:});
                                 fprintf(logid2,'</table>\n');
                              end
                              lgcolor='FF0000';
                              lgresult='FAILED: Log file results differ.';
                           end
                        end
                     end
                  end
                  flush(logid2)
                  drawnow
               end
            end
         else
            frcolor='000000';
            frresult='FAILED: case.ini missing.';
         end
      catch
         CrashMsg=lasterr;
         color='FF0000';
         AnyFail=1;
         if UserInterrupt
            result='FAILED: User interrupt.';
         else
            result='FAILED: Validation test crashed.';
         end
      end
      d3d_qp('closefile');
      if ~isempty(logid2)
         if ~isempty(CrashMsg)
            fprintf(logid2,'<font color=FF0000><b>%s</b></font><br>\n',CrashMsg);
         end
         fprintf(logid2,'</body>');
         fclose(logid2);
      end
      CaseFailed = ~isempty(strmatch('FAILED',frresult)) | ~isempty(strmatch('FAILED',lgresult)) | ~isempty(strmatch('FAILED',result));
      NFailed=NFailed + double(CaseFailed);
      AnyFail=AnyFail | CaseFailed;
      if AnyFail && ishandle(Hpb)
         progressbar(Hpb,'color','r')
      elseif ~ishandle(Hpb)
         UserInterrupt=1;
      end
      if ~isempty(result)
         fprintf(logid,'<tr><td>%s</td><td colspan=2><font color=%s><b>%s</b></font></td><td><a href="%s/%s">Click</a></tr>\n',d(i).name,color,result,d(i).name,logname);
      else
         fprintf(logid,'<tr><td>%s</td><td><font color=%s><b>%s</b></font></td><td><font color=%s><b>%s</b></font></td><td><a href="%s/%s">Click</a></tr>\n',d(i).name,frcolor,frresult,lgcolor,lgresult,d(i).name,logname);
      end
      flush(logid);
      if UserInterrupt
         break
      end
   end
catch
   if logid>0
      fprintf(logid,'<tr><td colspan=3><font color=FF0000><b>Testbank execution failed unexpectedly.</b></font></td></tr>\n');
      AnyFail=1;
   end
end
if ishandle(Hpb)
   delete(Hpb);
end
cd(currdir)
if logid>0
   fprintf(logid,'</table>\n</body>');
   fclose(logid);
end
if AnyFail
   ui_message('error','Testbank failed on %i out of %i cases! Check log file.\n',NFailed,NTested)
else
   ui_message('','Testbank completed successfully (%i cases).\n',NTested)
end
qp_settings('defaultfigure',DefFigProp.defaultfigure)
qp_settings('defaultfigurecolor',DefFigProp.defaultfigurecolor)
qp_settings('defaultaxescolor',DefFigProp.defaultaxescolor)
qp_settings('boundingbox',DefFigProp.boundingbox)


function writeheader(logid,casename)
fprintf(logid,'<html>\n<title>Delft3D-QUICKPLOT validation report</title>\n<body bgcolor=DDDDFF>\n');
fprintf(logid,'<table bgcolor=CCCCFF><tr><td colspan=2 bgcolor=AAAAFF><b>Deltares validation report</b></td></tr>\n<tr><td>Program:</td><td>Delft3D-QUICKPLOT</td></tr>\n');
stalone=' ';
if isstandalone
   stalone=' (standalone)';
end
fprintf(logid,'<tr><td>Version:</td><td>%s%s</td></tr>\n<tr><td>Date:</td><td>%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%02.0f</td></tr>\n',d3d_qp('version'),stalone,clock);
fprintf(logid,'<tr><td>MATLAB version:</td><td>%s</td></tr>\n',version);
fprintf(logid,'<tr><td>Computer type:</td><td>%s</td></tr>\n',computer);
if nargin>1
   fprintf(logid,'<tr><td>Case:</td><td>%s</td></tr>\n',casename);
end
fprintf(logid,'</table><br>\n');
flush(logid)


function X=localexist(file)
%X=exist(file);
X=fopen(file);
if X>0, fclose(X); end
X=X>0;


function flush(logid)
fseek(logid,0,-1);
fseek(logid,0,1);


function ensure_directory(dirname)
if isempty(dir(dirname))
   [parent, thisdir, ext] = fileparts(dirname);
   thisdir = [thisdir ext];
   if ~isempty(parent)
      ensure_directory(parent);
      cd(parent)
   end
   c = computer;
   if strcmp(c(1:2),'PC')
      s=dos(['mkdir "',thisdir,'"']);
   else
      s=unix(['mkdir -p ',thisdir]);
   end
end


function localsave(filename,Data,saveops)
if isstandalone
   save(filename,'Data');
else
   save(filename,'Data',saveops{:});
end


function Data = localload(filename,loadops)
if isstandalone
   Data = load(filename);
else
   Data = load(filename,'-mat');
end
