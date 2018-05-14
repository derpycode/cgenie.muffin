ccc
ccc	Subroutine for passing ENTS variable names and attributes (common database)
ccc

        subroutine netcdf_db_ents(label,var_name,var_att)
        implicit none

        character*(*) var_name, label, var_att(2,2)

        select case (label)

cccccccccccccc  FOR FINAL ANNUAL 2D AVERAGES

                case ('lqavg')
                        var_name='water'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average amount of water'
                        var_att(2,1)='units'
                        var_att(2,2)='metres'
                case ('ltavg')
                        var_name='land_temp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average temperature of land'
                        var_att(2,1)='units'
                        var_att(2,2)='C degrees'
                case ('palbavg')
                        var_name='effective_albedo'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average effective albedo'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'
                case ('snowavg')
                        var_name='snow'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average fractional snow cover'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'
                case ('albsavg')
                        var_name='surface_albedo'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average surface albedo'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'
                case ('runavg')
                        var_name='runoff'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average runoff'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'
                case ('pptnavg')
                        var_name='precipitation'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average precipitation'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'
                case ('relhavg')
                        var_name='humidity'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average relative humidity'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'
                case ('z0avg')
                        var_name='roughness'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average roughness length'
                        var_att(2,1)='units'
                        var_att(2,2)='metres'
                case ('evaplavg')
                        var_name='evaporation'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average evaporation'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'
                case ('bcapavg')
                        var_name='bcapavg'
                        var_att(1,1)='long_name'
                        var_att(1,2)='average bucket capacity'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

cccccccccccccc FOR 2D restarts

                case ('photo')
                        var_name='photosynthesis'
                        var_att(1,1)='long_name'
                        var_att(1,2)='photosynthesis'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('respveg')
                        var_name='veg_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('leaf')
                        var_name='leaf'
                        var_att(1,1)='long_name'
                        var_att(1,2)='leaf litter'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('respsoil')
                        var_name='soil_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='soil respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('Cveg')
                        var_name='vegC'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('Csoil')
                        var_name='soilC'
                        var_att(1,1)='long_name'
                        var_att(1,2)='soil carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('fv')
                        var_name='fv'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation fraction'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('tqld1')
                        var_name='land_temp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='land temperature'
                        var_att(2,1)='units'
                        var_att(2,2)='C degrees'

                case ('tqld2')
                        var_name='land_water'
                        var_att(1,1)='long_name'
                        var_att(1,2)='land water'
                        var_att(2,1)='units'
                        var_att(2,2)='metres'

                case ('snow')
                        var_name='snow'
                        var_att(1,1)='long_name'
                        var_att(1,2)='snow fraction'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

cccccccccccccc FOR 2D restart averages

                case ('sphoto')
                        var_name='photosynthesis'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged photosynthesis'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('srespveg')
                        var_name='veg_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged vegetation respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('sleaf')
                        var_name='leaf'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged leaf litter'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('srespsoil')
                        var_name='soil_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged soil respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('sCveg')
                        var_name='vegC'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged vegetation carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('sCsoil')
                        var_name='soilC'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged soil carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='kgC/m2/yr'

                case ('sfv')
                        var_name='fv'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged vegetation fraction'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('sepsv')
                        var_name='land_epsv'
                        var_att(1,1)='long_name'
                        var_att(1,2)='averaged epsv'
                        var_att(2,1)='units'
                        var_att(2,2)='C degrees'

cccccccccccccc FOR .slandt TIME SERIES (10) - /simpleland/carbt_diags.F

                case ('diagtime1')
                        var_name='time'
                        var_att(1,1)='long_name'
                        var_att(1,2)='years from the start of the run'
                        var_att(2,1)='units'
                        var_att(2,2)='years'

                case ('Gtveg')
                        var_name='veg_carb'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC'

                case ('Gtsoil')
                        var_name='soil_carb'
                        var_att(1,1)='long_name'
                        var_att(1,2)='soil carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC'

                case ('Gtatm')
                        var_name='atm_carb'
                        var_att(1,1)='long_name'
                        var_att(1,2)='atmospheric carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC'

                case ('Gfv')
                        var_name='veg_frac'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation fraction'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('pco2ld')
                        var_name='CO2'
                        var_att(1,1)='long_name'
                        var_att(1,2)='CO2 concentration'
                        var_att(2,1)='units'
                        var_att(2,2)='ppm'

                case ('Gtphoto')
                        var_name='photosynthesis'
                        var_att(1,1)='long_name'
                        var_att(1,2)='photosynthesis'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('Gtrveg')
                        var_name='veg_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='vegetation respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('Gtleaf')
                        var_name='leaf'
                        var_att(1,1)='long_name'
                        var_att(1,2)='leaf litter'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('Gtrsoil')
                        var_name='soil_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='soil respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/year'

cccccccccccccc FOR .pslandt TIME SERIES (14) - /simpleland/physt_diags.f

                case ('diagtime2')
                        var_name='time2'
                        var_att(1,1)='long_name'
                        var_att(1,2)='years from the start of the run'
                        var_att(2,1)='units'
                        var_att(2,2)='years'

                case ('avgs1')
                        var_name='land_temp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='land temperature'
                        var_att(2,1)='units'
                        var_att(2,2)='C degrees'

                case ('avgs2')
                        var_name='land_water'
                        var_att(1,1)='long_name'
                        var_att(1,2)='land water'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

                case ('avgs3')
                        var_name='rad_flux_atm'
                        var_att(1,1)='long_name'
                        var_att(1,2)="radiation flux to the atmosphere"
     &                       //"over land"
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('avgs4')
                        var_name='rad_flux_land'
                        var_att(1,1)='long_name'
                        var_att(1,2)='radiation flux to the land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('avgs5')
                        var_name='sens_heat'
                        var_att(1,1)='long_name'
                        var_att(1,2)="sensible heat flux to the"
     &                       //"atmosphere over land"
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('avgs6')
                        var_name='net_long'
                        var_att(1,1)='long_name'
                        var_att(1,2)='net long wave heat flux to the'
     &                       //' atmosphere over land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('avgs7')
                        var_name='evap'
                        var_att(1,1)='long_name'
                        var_att(1,2)='total evaporation'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'

                case ('avgs8')
                        var_name='precip'
                        var_att(1,1)='long_name'
                        var_att(1,2)='precipitation over land'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'

                case ('avgs9')
                        var_name='humidity'
                        var_att(1,1)='long_name'
                        var_att(1,2)='relative humidity'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'


                case ('avgs10')
                        var_name='soil_field_cap'
                        var_att(1,1)='long_name'
                        var_att(1,2)='soil field capacity'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

                case ('avgs11')
                        var_name='land_alb'
                        var_att(1,1)='long_name'
                        var_att(1,2)='land surface albedo'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('avgs12')
                        var_name='snow'
                        var_att(1,1)='long_name'
                        var_att(1,2)='fractional snow cover'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('avgs13')
                        var_name='roughness'
                        var_att(1,1)='long_name'
                        var_att(1,2)='roughness length'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

cccccccccccccc FOR .sealevel TIME SERIES (3) - /simpleland/sealevel.f

                case ('diagtime3')
                        var_name='time3'
                        var_att(1,1)='long_name'
                        var_att(1,2)='years from the start of the run'
                        var_att(2,1)='units'
                        var_att(2,2)='years'

                case ('deltah')
                        var_name='height_change'
                        var_att(1,1)='long_name'
                        var_att(1,2)='change in sea-level height'
     &                       //' relative to rhoref'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

                case ('avrho')
                        var_name='ocean_density'
                        var_att(1,1)='long_name'
                        var_att(1,2)='gloval average ocean density'
                        var_att(2,1)='units'
                        var_att(2,2)='kg/m3'

cccccccccccccc FOR .slavgt TIME SERIES (10) - /simpleland/annav_diags.f

                case ('year1')
                        var_name='time'
                        var_att(1,1)='long_name'
                        var_att(1,2)='years from the start of the run'
                        var_att(2,1)='units'
                        var_att(2,2)='years'

                case ('avgsl1')
                        var_name='net_photo'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average net'
     &                       //' photosynthesis'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl2')
                        var_name='veg_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average vegetation'
     &                       //' respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl3')
                        var_name='leaf_litter'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average leaf litter'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl4')
                        var_name='soil_resp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average soil'
     &                       //' respiration'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl5')
                        var_name='veg_carbon'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average vegetation'
     &                       //' carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl6')
                        var_name='soil_carbon'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average soil carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'

                case ('avgsl7')
                        var_name='veg_frac_cover'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average vegetation'
     &                       //' fractional cover'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('avgsl8')
                        var_name='self_shading'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average self'
     &                       //' shading fraction'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('Gtatm2')
                        var_name='atm_carbon'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average atmospheric'
     &                       //' carbon'
                        var_att(2,1)='units'
                        var_att(2,2)='GtC/yr'


cccccccccccccc FOR .pslavgt TIME SERIES (14) - /simpleland/annav_diags.f

                case ('year2')
                        var_name='time'
                        var_att(1,1)='long_name'
                        var_att(1,2)='years from the start of the run'
                        var_att(2,1)='units'
                        var_att(2,2)='years'

                case ('1avgs1')
                        var_name='land_temp'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average land'
     &                       //' temperature'
                        var_att(2,1)='units'
                        var_att(2,2)='C degrees'

                case ('2avgs2')
                        var_name='land_water'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average land water'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

                case ('3avgs3')
                        var_name='rad_flux_atm'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average radiation'
     &                       //' flux to the atmosphere 
     :                  over land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('4avgs4')
                        var_name='rad_flux_land'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average radiation'
     &                       //' flux to the land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('5avgs5')
                        var_name='sens_heat'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average sensible'
     &                       //' heat flux to the 
     :                  atmosphere over land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('6avgs6')
                        var_name='net_long'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average net long'
     &                       //' wave heat flux to the 
     :                  atmosphere over land'
                        var_att(2,1)='units'
                        var_att(2,2)='W/m2'

                case ('7avgs7')
                        var_name='evap'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average '
     &                       //' evaporation'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'

                case ('8avgs8')
                        var_name='precip'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average'
     &                       //' precipitation over land'
                        var_att(2,1)='units'
                        var_att(2,2)='m/s'

                case ('9avgs9')
                        var_name='humidity'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average relative'
     &                       //' humidity'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('10avgs10')
                        var_name='soil_field_cap'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average soil field'
     &                       //' capacity'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

                case ('11avgs11')
                        var_name='land_alb'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average land'
     &                       //' surface albedo'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('12avgs12')
                        var_name='snow'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average fractional'
     &                       //' snow cover'
                        var_att(2,1)='units'
                        var_att(2,2)='dimensionless'

                case ('13avgs13')
                        var_name='roughness'
                        var_att(1,1)='long_name'
                        var_att(1,2)='global annual average roughness'
     &                       //' length'
                        var_att(2,1)='units'
                        var_att(2,2)='m'

        end select

        end
