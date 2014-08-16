.PHONY: makedirs all test clean cl

GHC = ghc
GHCFLAGS = -w -O2
CLASSDIR = obj
SRCDIR = src
TESTDIR = test

.SUFFIXES: .hs .o

# -----------------
#  Common targets
# -----------------
all: makedirs source test

makedirs:
	@mkdir -p $(CLASSDIR)

cl: clean

clean:
	@rm -rf $(CLASSDIR)
	@find . -name '*.hi' | xargs rm -f
	@find . -name '*.o' | xargs rm -f

# -----------------
# Source Files
# -----------------
SOURCEFILES := \
				ScalableBloomFilter.hs \
				Hash/Hash.hs \
				ImmutableBloomFilter.hs \

SOURCEFILES_C := \
				$(SRCDIR)/Hash/lookup3.c \

$(CLASSDIR)/%.o : $(SRCDIR)/%.hs
	@echo ""
	@echo "--------------------------------------------------------------------"
	@echo "Compiling source file:\t" $<
	@echo "--------------------------------------------------------------------"
	@$(GHC) --make $(GHCFLAGS) -i$(SRCDIR):$(TESTDIR) -outputdir $(CLASSDIR) $<

SOURCEFILESTOCOMPILE = $(addprefix $(CLASSDIR)/, $(SOURCEFILES:.hs=.o))

source: makedirs $(SOURCEFILESTOCOMPILE)

# -----------------
# Test Files
# -----------------
$(CLASSDIR)/%.o : $(TESTDIR)/%.hs
	@echo ""
	@echo "--------------------------------------------------------------------"
	@echo "Compiling test file:\t" $<
	@echo "--------------------------------------------------------------------"
	@$(GHC) -rtsopts -i$(SRCDIR):$(TESTDIR) -prof -auto-all $(GHCFLAGS) --make -outputdir $(CLASSDIR) $(SOURCEFILES_C) $<

TESTFILES := \
				MutableBloomCheck.hs \
				MutableBloomProfiling.hs \

TESTFILESTOCOMPILE = $(addprefix $(CLASSDIR)/, $(TESTFILES:.hs=.o))

test: makedirs $(TESTFILESTOCOMPILE)
