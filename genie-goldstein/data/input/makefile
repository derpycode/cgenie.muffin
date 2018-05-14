
#
# Preprocess input datasets which have previously been downloaded to
# ../download/ to facilitate the use of the datasets by the model
#
# this makefile requires NetCDF Operators (NCO;
# http://nco.sourceforge.net) to be installed on the system.
#

# NCO commands
NCKS=ncks
NCWA=ncwa

PREPROCESSED_DATASETS_WOA05=WOA05_an_TS.nc
DATASETS_WOA05=t00an1.nc s00an1.nc
vpath t00an1.nc ../download
vpath s00an1.nc ../download

PREPROCESSED_DATASETS=$(PREPROCESSED_DATASETS_WOA05)

all: $(PREPROCESSED_DATASETS)

.PHONY: all cleanall

cleanall:
	\rm -f $(PREPROCESSED_DATASETS_WOA05)

# preprocess previously downloaded objectively-analysed annual-mean
# climatology of ocean temperature [Locarnini et al., 2006] and
# objectively-analysed annual-mean climatology of ocean salinity
# [Antonov et al., 2006] by collating the original temperature and
# salinity fields into one file and remove dummy temporal axis. The
# two original datasets are from the World Ocean Atlas 2005 (WOA05;
# http://www.nodc.noaa.gov/OC5/WOA05/pr_woa05.html) from the National
# Oceanographic Data Center (NODC; http://www.nodc.noaa.gov/).
#
# References:
#
# Locarnini R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, and
# H. E. Garcia (2006), World Ocean Atlas 2005, Volume 1:
# Temperature. S. Levitus, Ed. NOAA Atlas NESDIS 61, U.S. Government
# Printing Office, Washington, D.C., 182 pp.
#
# Antonov J. I., R. A. Locarnini, T. P. Boyer, A. V. Mishonov, and
# H. E. Garcia (2006), World Ocean Atlas 2005, Volume 2:
# Salinity. S. Levitus, Ed. NOAA Atlas NESDIS 62, U.S. Government
# Printing Office, Washington, D.C., 182 pp.
#
$(PREPROCESSED_DATASETS_WOA05): $(DATASETS_WOA05)
	$(NCKS) -O -v t00an1 ../download/t00an1.nc ./$@
	$(NCKS) -A -v s00an1 ../download/s00an1.nc ./$@
	$(NCWA) -O -a time -v t00an1,s00an1 ./$@ ./$@
