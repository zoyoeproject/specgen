PRE_SPEC_DIR := ./output
SPEC_DIR := ./spec
PRE_SPEC_FILES := $(wildcard $(PRE_SPEC_DIR)/*.fun)
TEMPLATES := ../templates/
SPEC_FILES := $(patsubst $(PRE_SPEC_DIR)/%.fun,$(SPEC_DIR)/%.v,$(PRE_SPEC_FILES))
RAW_SPEC_FILES := $(patsubst $(PRE_SPEC_DIR)/%.fun,%.v,$(PRE_SPEC_FILES))
PLUGIN_DIR := ../../theories

spec: $(SPEC_FILES) $(SPEC_DIR)/type.v $(SPEC_DIR)/Makefile
	echo "all spec files generated."

$(SPEC_DIR)/Makefile: $(SPEC_FILES)
	cd $(SPEC_DIR) && coq_makefile -R $(PLUGIN_DIR) Core -I $(PLUGIN_DIR) $(RAW_SPEC_FILES) -o Makefile

$(SPEC_DIR)/type.v: $(PRE_SPEC_DIR)/type.spec
	gpp -I $(TEMPLATES) $< -o $@

$(SPEC_DIR)/%.v: $(PRE_SPEC_DIR)/%.fun
	gpp -I $(TEMPLATES) $< -o $@

clean:
	rm -rf spec/*.v
	rm -rf spec/*.vo
