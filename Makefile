OCPBUILD=ocp-build

all:
	$(OCPBUILD) init
	$(OCPBUILD) -njobs 1

clean-ocp:
	$(OCPBUILD) clean

clean: clean-ocp
