BUILDER ?= gprbuild
FLAGS ?= -p -f -gnat12
BUILD = $(BUILDER) ${FLAGS}
DBNAME = db/arpm.db


all: clean build
debug :clean build_debug
warn: clean build_all_warnings
strip: clean build_strip

build: 
	${BUILD} -P gnat/arpm 

build_debug:
	${BUILD} -P gnat/arpm  -gnata -ggdb -g

build_all_warnings: 
	${BUILD} -Pgnat/arpm -gnata -gnatwu -cargs -O0 -g -v 

clean_db:
	rm -rf db/*
		
create_db: clean_db
	@mkdir -p db/
	gnatcoll_db2ada -dbmodel dbmodel -dbtype sqlite -dbname ${DBNAME} -createdb

orm: create_db clean
	@mkdir -p src/db/generated/
	cp dbmodel src/db/generated/
	cd src/db/generated/ &&  gnatcoll_db2ada -dbmodel dbmodel -api Database -orm Orm && rm dbmodel

clean:
	rm -rf bin/ obj/ lib/
