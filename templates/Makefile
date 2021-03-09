PRE_SPEC_DIR := ./output
SPEC_DIR := ./spec
PRE_SPEC_FILES := $(wildcard $(PRE_SPEC_DIR)/*.fun)
TEMPLATES := ../templates/
SPEC_FILES:= $(patsubst $(PRE_SPEC_DIR)/%.fun,$(SPEC_DIR)/%.v,$(PRE_SPEC_FILES))

spec: $(SPEC_FILES) $(SPEC_DIR)/type.v
	echo $(SPEC_FILES)
	echo "all spec files"

$(SPEC_DIR)/type.v: $(PRE_SPEC_DIR)/type.spec
	gpp -I $(TEMPLATES) $< -o $@

$(SPEC_DIR)/%.v: $(PRE_SPEC_DIR)/%.fun
	gpp -I $(TEMPLATES) $< -o $@
