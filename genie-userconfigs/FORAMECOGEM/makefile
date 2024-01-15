CONFIG_DIR := $(CURDIR)
HOME_DIR := $(HOME)

.PHONY: update_userconfig update_grazing_file update_size_file update_foramecogem

update_grazing_file:
	cp $(CONFIG_DIR)/FORAMECOGEM.zoo $(HOME_DIR)/cgenie.muffin/genie-ecogem/data/input
	@echo "Update done!"

update_size_file:
	cp  $(CONFIG_DIR)/8P7Z4F.eco $(HOME_DIR)/cgenie.muffin/genie-ecogem/data/input
	@echo "Update done!"

update_userconfig:
	@echo "Running Python script to update user-config files..."
	@python3 update_userconfig.py $(CONFIG_DIR)
	@echo "Update done!"

update_foramecogem: update_grazing_file update_size_file update_userconfig
	@echo "All updates completed successfully!"
