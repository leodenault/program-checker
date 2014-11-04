TARGETS = program-checker

# We need to include the external libraries we used
ASDF_TREE = --asdf-tree ~/quicklisp/dists/quicklisp/software/
SYSTEM = --load-system cl-ppcre

all: $(TARGETS)

program-checker: parser.lisp
	buildapp --output program-checker \
	$(ASDF_TREE) \
	$(SYSTEM) \
	--load parser.lisp \
	--entry main
