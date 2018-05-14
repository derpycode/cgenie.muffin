# ================== TESTING RULES ===================

define EBGOGS_ACTIONS
$(MAKE) cleanall;
echo "testebgogs" > 'current_config.dat'
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testebgogs.out;
./genie.job \
-f $(CONFIG_DIR)/eb_go_gs_restartmake_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> testebgogs.out
./genie.job \
-t -z \
-f $(CONFIG_DIR)/eb_go_gs_restartread_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> testebgogs.out
endef

define BIOGEM_ACTIONS
###$(MAKE) cleanall;
echo "testbiogem" > 'current_config.dat'
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_ac_bg_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" 2>&1 | tee testbiogem.out;
endef

# NB now compares against a "knowngood" file held in SVN
define ENTS_ACTIONS
$(MAKE) cleanall;
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_el_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testents.out;
endef

# ebgogs test separately
testebgogs :
	@ echo "**** TESTING - EBGOGS ****"
	$(EBGOGS_ACTIONS)
	@ grep -B1 "**TEST" testebgogs.out;

# ebgogs/biogem test separately
testbiogem :
	@ echo "**** TESTING - BIOGEM ****"
	$(BIOGEM_ACTIONS)
	@ grep -B1 "**TEST" testbiogem.out;

