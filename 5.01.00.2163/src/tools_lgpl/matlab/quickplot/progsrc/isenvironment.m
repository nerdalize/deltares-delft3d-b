function OK = ISENVIRONMENT(NAME)
%ISENVIRONMENT  Checks the code evaluation environment.
%   NAME = ISENVIRONMENT returns the name of the environment in which the
%   code is running.
%
%   ISENVIRONMENT(NAME) returns true if the operating environment matches
%   the specified environment.
%
%   The following environments are supported:
%      MATLAB, Octave, Freemat
%
%   See also VER, VERSION, VERSTRING.

if exist('OCTAVE_VERSION')
   Name = 'Octave';
elseif exist('verstring') && ~isempty(strfind(verstring,'FreeMat'))
   Name = 'FreeMat';
else
   Name = 'MATLAB';
end

if nargin==0
   OK = Name;
else
   OK = strcmpi(NAME,Name);
end
