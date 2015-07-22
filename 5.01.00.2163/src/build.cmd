del build.log
call "%VS90COMNTOOLS%\vsvars32.bat"
  rem the path to devenv.exe is now added to PATH: no full path specificitation needed on next line.
devenv.exe ds.sln /Build Release /Out build.log
open_third_party\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build.log 
