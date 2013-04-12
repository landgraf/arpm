BUILDER ?= gprbuild
FLAGS ?= -p -f 
BUILD = $(BUILDER) ${FLAGS}


all:
	${BUILD} -P gnat/arpm  -gnata -ggdb -g


