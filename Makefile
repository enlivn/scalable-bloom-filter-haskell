.PHONY: dirs all clean cl

GHC = ghc
GHCFLAGS = --make
CLASSDIR = obj
SRCDIR = src

.SUFFIXES: .hs .o

$(CLASSDIR)/%.o : $(SRCDIR)/%.hs
	@$(GHC) $(GHCFLAGS) -isrc -outputdir $(CLASSDIR) $<

all: dirs classestocompile

SOURCEFILES := \
				Hash/Hash.hs \
				ImmutableBloomFilter.hs \
				MutableBloomFilter.hs \
				Types.hs \

classestocompile: $(addprefix $(CLASSDIR)/, $(SOURCEFILES:.hs=.o))

dirs:
	@mkdir -p $(CLASSDIR)

cl: clean

clean:
	@rm -rf $(CLASSDIR)
