PROGRAMS := \
	snaps

DIR_BUILD := _obuild


MAX_BUILD_WORKERS := \
	$(shell nproc             2> /dev/null \
	     || gnproc            2> /dev/null \
	     || sysctl -n hw.ncpu 2> /dev/null \
	 )


.PHONY:\
	build \
	clean \
	deps \
	programs \
	purge


programs: build bin
	@for p in $(PROGRAMS); do \
		src="$(DIR_BUILD)/$$p/$$p.asm" ; \
		dst="bin/$$p" ; \
		cp $$src $$dst ; \
	done

deps:
	@opam install --yes ezjsonm ocp-build core

bin:
	@mkdir -p bin

build: ocp-build.root
	@ocp-build build -njobs $(MAX_BUILD_WORKERS)

ocp-build.root:
	@ocp-build -init -njobs $(MAX_BUILD_WORKERS)

clean: clean_bin
	@ocp-build clean
	@rm -f ocp-build.root*

clean_manually: clean_bin
	@rm -rf $(DIR_BUILD)
	@find \
		. \
			-name '*.o' \
		-or -name '*.cmi' \
		-or -name '*.cmo' \
		-or -name '*.cmx' \
	| xargs rm -f

clean_bin:
	@rm -rf bin
