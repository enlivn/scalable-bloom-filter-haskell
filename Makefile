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

# -----------------
# Source Files
# -----------------
SOURCEFILES := \
				Hash/Hash.hs \
				ImmutableBloomFilter.hs \

SOURCEFILES_C := \
				$(SRCDIR)/Hash/lookup3.c \

$(CLASSDIR)/%.o : $(SRCDIR)/%.hs
	@echo ""
	@echo "--------------------------------------------------------------------"
	@echo "Compiling source file:\t" $<
	@echo "--------------------------------------------------------------------"
	@$(GHC) --make $(GHCFLAGS) -isrc -outputdir $(CLASSDIR) $<

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
	@$(GHC) --make $(GHCFLAGS) -isrc -outputdir $(CLASSDIR) $(SOURCEFILES_C) $<

TESTFILES := \
				MutableBloomCheck.hs \

TESTFILESTOCOMPILE = $(addprefix $(CLASSDIR)/, $(TESTFILES:.hs=.o))

test: makedirs $(TESTFILESTOCOMPILE)
