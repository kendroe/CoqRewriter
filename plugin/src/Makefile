# Set OCamlMakefile to use
export OCAMLMAKEFILE = OCamlMakefile

# Export some common variable settings
export THREADS = yes

# Define project "rewrite"
define PROJ_rewrite
  SOURCES = mylist.ml rtrie.ml intern.ml getfile.ml lex.ml parser.ml rtype.ml exp.ml expIntern.ml disc.ml rcontext.ml rsubst.ml prettyPrint.ml renv.ml rtrace.ml cache.ml match.ml derive.ml crewrite.ml kbrewrite.ml builtin.ml rule_app.ml inner.ml test1.ml
  RESULT = rewrite_test
endef
export PROJ_rewrite

# If the environment does not define subprojects to handle,
# then use "ex1 ex2" as default
ifndef SUBPROJS
  export SUBPROJS = rewrite
endif

# Default target to use
all:	bc

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

