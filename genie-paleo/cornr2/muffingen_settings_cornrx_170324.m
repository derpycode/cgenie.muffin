% make_topo_settings
%
%   ***********************************************************************
%   *** DEFAULT PARAMETER SETTINGS FOR make_topo TOPO GENERATOR ***********
%   ***********************************************************************
%
%   Edit this file directly for additional user settings:
%
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAMATER                % DEFAULT BRIEF DESCRIPTION [SEE ABOVE]
%
% *** ENVIRONMENT SETTINGS ********************************************** %
par_dpath_source='source'; % relative path to muffingen source
opt_outputdir=false;   % ask for output directory
%
% *** EXPERIMENT INPUT AND OUTPUT *************************************** %
par_gcm='mask';         % [ 'hadcm3l'] GCM model name
par_expid='cornr2';         % [ 'xbomg'] experiment name
par_pathin='DATA.k1';        % [    'DATA'] path to HadCM3L netCDf files
par_pathout='RESULTS'; % relative (ONLY) path to output files
par_wor_name='cornr2';
%
% *** GRID RESOLUTION & REGRIDDING CONTROLS ***************************** %
% resolution
par_max_i = 36;
par_max_j = 36;
par_max_k = 16;
par_max_D = 5000.0;        % max grid depth (m)
par_lon_off = -180.0;         % longitude offset (e.g. -260.0)
par_min_Dk = 2;            % restrict shallow ocean to at least Dk levels 
par_A_frac_threshold=0.45;  % fractional area threshold for 'land'
opt_equalarea=true;        % otherwise, equal lat spacing
opt_highresseds=false; 
par_sedsopt=0; % 0==DEFAULT (k1 DEPTHS) / 1==RAW (GCM) / 2==RANDOM
%
% *** OPTIONS *********************************************************** %
% primary options
opt_makeall=false;      % [     true] 
% basic options
opt_makemask=true;      % [     true] 
opt_maketopo=true;      % [     true] RE_GRID BATHYMETRY?
opt_makeocean=true;   % [      true] CREATE OCEAN FILES
opt_makerunoff=true;   % [      true] 
opt_makewind=true;       % [     true] RE_GRID WINDS?
opt_makealbedo=false;
opt_makeseds=false;
% output options
opt_debug=false;
%
% *** FILTERING ********************************************************* %
% grid filtering
opt_user=false;      % [     true] 
opt_filtermask=false;       % [      true] 
opt_filtertopo=false;       % [      true]
opt_makepoleswide=true;       % [      true]
par_min_oceann = 20; % smallest isolated water body allowed
%
% *********************************************************************** %
