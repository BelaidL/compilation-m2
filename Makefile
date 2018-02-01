VERSION	  := $(shell cut -d\" -f2 src/version.ml)
TARGET	  := flap

BTARGET	 = $(TARGET).byte
OTARGET	 = $(TARGET).native
BLTARGET = $(TARGET).cma
BNTARGET = $(TARGET).cmxa
STARGET	 = $(OTARGET)

#########################
## Tools configuration ##
#########################

# Menhir can be told to produce a parser that explains what
# it is doing.
ifeq ($(DEBUGPARSING), yes)
  MENHIROPT=-yaccflag --explain -yaccflag --trace
else
  MENHIROPT=-yaccflag --explain
endif

# In Emacs, use classic display to enable error jumping.
TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb)
 OCAMLBUILDFLAGS = -cflag "-dtypes" -tag debug -classic-display -no-hygiene $(MENHIROPT)
else
 OCAMLBUILDFLAGS = -no-hygiene $(MENHIROPT)
endif

OCAMLBUILD = ocamlbuild -use-menhir $(OCAMLBUILDFLAGS)

OCAMLDOC = ocamldoc

#########
# Rules #
#########

.PHONY: all-generic byte opt doc clean dist install uninstall headers	\
	clear tests

all-generic: clear $(STARGET) $(TARGET)

$(TARGET):
	ln -sf $(STARGET) $(TARGET)

clear:
	rm -f $(STARGET)

opt: $(OTARGET)

byte: $(BTARGET)

%:
	@ $(OCAMLBUILD) src/$@

byte-debug:
	$(OCAMLBUILD) -tag debug src/$(BTARGET)
	rm -f $(STARGET)
	ln -s $(BTARGET) $(STARGET)

clean:
	@ $(OCAMLBUILD) -clean
	find . -name '*~' -exec rm '{}' \;
	rm -fr *~ $(TARGET) $(OTARGET) $(BTARGET) $(TARGET).odocl $(TARGET).docdir

doc: byte
	find src -name \*.ml | cut -d"." -f1 > $(TARGET).odocl
	$(OCAMLBUILD) $(TARGET).docdir/index.html

tests:
	$(MAKE) -C tests check
