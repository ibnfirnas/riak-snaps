OASIS_GENERATED_FILES :=  \
	_tags \
	configure \
	setup.data \
	setup.log \
	setup.ml \
	src/git/META \
	src/git/git.mldylib \
	src/git/git.mllib \
	src/riak/META \
	src/riak/riak.mldylib \
	src/riak/riak.mllib

default: oasis_setup build

oasis_setup:
	@oasis setup

oasis_clean:
	@for file in $(OASIS_GENERATED_FILES); do \
		rm $$file || true; \
	done

deps:
	@opam install --yes \
		oasis \
		core \
		async \
		async_shell \
		ezjsonm

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
