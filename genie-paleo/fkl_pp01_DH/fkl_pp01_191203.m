% muffingen_settings
%
%   ***********************************************************************
%   *** PARAMETER SETTINGS FOR muffingen CONFIG GENERATOR *****************
%   ***********************************************************************
%
%   (edit this file directly)
%
%   ***********************************************************************

% *********************************************************************** %
% *** USER SETTINGS ***************************************************** %
% *********************************************************************** %
%
% PARAM NAME & DEFAULT VALUE   % [FORMAT] BRIEF DESCRIPTION
%
% *** CONFIG NAME AND MAIN INPUT SETTINGS ******************************* %
%
par_wor_name='fkl_pp01';       % ['STRING'] 8-char (output) config name
par_gcm='k2';                  % ['STRING'] input format/GCM name
par_expid='fkl_pp01';          % ['STRING'] input experiment/data name
%
% *** FILE PATHS ******************************************************** %
%
par_pathin='../INPUT.fake';    % ['STRING'] path to input dir
par_pathout='../OUTPUT.fake';  % ['STRING'] path to output dir
opt_outputdir=false;           % [false/true] ask for output directory?
%
% *** GCM netCDF FILENAMES ********************************************** %
%
par_nc_topo_name  = '';        % ['STRING'] optional .nc name
par_nc_mask_name  = '';        % ['STRING'] optional .nc name
par_nc_axes_name  = '';        % ['STRING'] optional .nc name
par_nc_atmos_name = '';        % ['STRING'] optional .nc name
par_nc_ocean_name = '';        % ['STRING'] optional .nc name
par_nc_coupl_name = '';        % ['STRING'] optional .nc name
%
% *** GRID RESOLUTION *************************************************** %
%
par_max_i=18;                  % [1-72] # grid cells in longitude dir (i)
par_max_j=18;                  % [1-72] # grid cells in latitude  dir (j)
par_max_k=16;                  % [1-99] # depth leves in ocean
opt_equalarea=true;            % [false/true] equal area grid?
%
% *** REGRIDDING SETTINGS *********************************************** %
%
par_max_D=5000.0;              % [0.0-99999.9] max grid depth (m)
par_sur_D=0.0;                 % [0.0-99999.9] reference surface depth
par_min_Dk=2;                  % [1-99] minimum ocean depth (as # levels)
par_min_k=3;                   % [1-99] maximum ocean depth (k value)
par_lon_off=-0.0;            % [-360-0] longitude offset of grid start
par_A_frac_threshold=0.50;     % [0.0-1.0] land fractional area threshold  
par_mask_mask_name = '';       % ['STRING'] mask of land/ocean features
par_sedsopt=2;                 % [0/1/2] sediment re-gridding option
opt_highresseds=false;         % [false/true] create 2x res sediment grid
%
% *** BOUNDARY CONDITION SETTINGS *************************************** %
%
par_runoffopt=0;               % [0/1] run-off generation option
par_tauopt=0;                  % [0/1/2] zonal windstress generation option
par_age=0.0;                   % [0.0-4570.0] optional age (Myr)
%
% *** OPTIONS -- MAIN *************************************************** %
%
opt_makeall=false;             % [false/true] apply all common options?
opt_user=false;                % [false/true] enable user input to grid
opt_plots=false;                % [false/true] plot all input and output?
%
% *** OPTIONS -- DATA GENERATION **************************************** %
%
opt_makemask=true;             % [false/true] re-grid mask?
opt_maketopo=true;             % [false/true] re-grid bathymetry?
opt_makeocean=true;            % [false/true] create ocean files?
opt_makerunoff=true;           % [false/true] create runoff pattern?
opt_makewind=true;             % [false/true] re-grid wind products?
opt_makezonalwind=false;       % [false/true] force zonal wind generation
opt_makealbedo=true;           % [false/true] make albedo file
opt_makeseds=true;             % [false/true] make sediment files
%
% *** OPTIONS -- GRID FILTERING ***************************************** %
%
opt_filtermask=false;          % [false/true] filter land-sea mask?
opt_filtertopo=false;          % [false/true] filter topography?
opt_makepoleswide=false;       % [false/true] force wide polar island zone
par_min_oceann=20;             % [0-9999] minimum lake size (# cells)
%
% *** ENVIRONMENT/OTHER SETTINGS **************************************** %
%
par_dpath_source='source';     % ['source'] relative path to muffingen code
opt_debug=false;               % [false/true] debug output?
%
% *********************************************************************** %
