@echo off

echo "Converting 'new' directory structure (win32) to 'old' directory structure (w32) ..."

if not exist win32 (
   echo "Directory 'win32' does not exist. Can not convert."
   goto :end
)

if exist w32 (
   echo "Deleting target directory w32..."
   rmdir /s /q w32
)

mkdir w32\flow\bin
mkdir w32\flow\default
mkdir w32\lib
mkdir w32\util
mkdir w32\wave\bin
mkdir w32\wave\default

copy win32\flow2d3d\bin\*     w32\flow\bin
copy win32\flow2d3d\lib\*     w32\flow\bin
copy win32\flow2d3d\scripts\* w32\flow\bin
copy win32\flow2d3d\default\* w32\flow\default

copy win32\swan\bin\*     w32\wave\bin
copy win32\swan\lib\*     w32\wave\bin
copy win32\swan\scripts\* w32\lib
copy win32\wave\bin\*     w32\wave\bin
copy win32\wave\lib\*     w32\wave\bin
copy win32\wave\default\* w32\wave\default

copy win32\util\bin\*     w32\util


:end
echo "Finished converting."

pause
