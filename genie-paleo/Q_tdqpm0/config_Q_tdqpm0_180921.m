% muffingen_settings
%
%   ***********************************************************************
%   *** PARAMETER SETTINGS FOR muffingen CONFIG GENERATOR *****************
%   ***********************************************************************
%
%   Edit this file directly for additional user settings
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAM NAME & DEFAULT VALUE   % [FORMAT] BRIEF DESCRIPTION
%
% *** EXPERIMENT INPUT AND OUTPUT *************************************** %
%
par_wor_name='Q_tdqpm0';       % ['worxbowl'] 8-char (output) config name
par_gcm='hadcm3l';             % ['hadcm3l'] input format/GCM name
par_expid='tdqpm';             % ['xbowl'] experiment name
par_pathin='INPUT';   % ['EXAMPLES.INPUT'] path to input dir
par_pathout='OUTPUT'; % ['EXAMPLES.OUTPUT'] path to output dir
opt_outputdir=false;           % [false/true] ask for output directory?
%
% *** GRID RESOLUTION & REGRIDDING CONTROLS ***************************** %
%
par_max_i=36;                  % [36] # grid cells in longitude dir (i)
par_max_j=36;                  % [36] # grid cells in latitude  dir (j)
par_max_k=16;                  % [36] # depth leves in ocean
par_max_D=5000.0;              % [5000.0] max grid depth (m)
par_lon_off=-180.0;            % [-180.0] longitude offset of grid start
par_min_Dk=2;                  % [2] minimum ocean depth (as # levels)
par_min_k=1;                   % [1] maximum ocean depth (k value)
par_A_frac_threshold=0.45;     % [0.5] fractional area threshold for 'land'
opt_equalarea=true;            % [false/true] equal area grid?
opt_highresseds=false;         % [false/true] create 2x res sediment grid
par_runoffopt=0;               % [0/1] run-off generation option
par_sedsopt=1;                 % [0/1/2] sediment re-gridding option
par_tauopt=0;                  % [0/1/2] zonal windstress generation option
par_age=13.82;                   % [0.0] optional age of paleo configuration
%
% *** OPTIONS -- MAIN *************************************************** %
%
opt_makeall=true;              % [false/true] apply all common options?
opt_user=true;                 % [false/true] enable user input to grid
%
% *** OPTIONS -- OTHER ************************************************** %
%
opt_makemask=true;             % [false/true] re-grid mask?
opt_maketopo=true;             % [false/true] re-grid bathymetry?
opt_makeocean=true;            % [false/true] create ocean files?
opt_makerunoff=true;           % [false/true] create runoff pattern?
opt_makewind=true;             % [false/true] re-grid wind products?
opt_makealbedo=true;           % [false/true] make albedo file
opt_makeseds=true;             % [false/true] make sediment files
%
% *** GRID FILTERING **************************************************** %
%
opt_filtermask=true;           % [false/true] filter land-sea mask?
opt_filtertopo=true;           % [false/true] filter topography?
opt_makepoleswide=true;        % [false/true] force wide polar island zone
par_min_oceann=20;             % [20] minimum allowed lake size (# cells)
%
% *** ENVIRONMENT SETTINGS ********************************************** %
%
par_dpath_source='source';     % ['source'] relative path to muffingen code
opt_debug=false;               % [false/true] debug output?
%
% *********************************************************************** %
