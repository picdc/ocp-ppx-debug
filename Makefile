OCPBUILD=ocp-build

all:
	$(OCPBUILD) init
	$(OCPBUILD)

clean-ocp:
	$(OCPBUILD) clean

clean: clean-ocp
