# no trailing slash on DEST. Don't want a "//" in FASL and ASD
SBCL?=sbcl
DEST?=obj/sbcl-home/contrib
FASL=$(DEST)/$(SYSTEM).fasl
ASD=$(DEST)/$(SYSTEM).asd

all: $(FASL)

# The explicit use of $wildcard is necessary here. While rules do expand
# wildcards implicitly (so that just "$(FASL): *.lisp" mostly works),
# that specification would fail on the contribs which have no .lisp file
# in the current directory.
# This produces $(ASD) as a side-effect.
$(FASL): $(wildcard *.lisp) $(wildcard */*.lisp)
	$(SBCL)	--load make-contrib.lisp "$(SYSTEM)" $(MODULE_REQUIRES) < /dev/null

.PHONY: clean
clean:
	$(RM) -rf $(DEST)
