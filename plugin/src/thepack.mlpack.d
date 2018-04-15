src/thepack_MLPACK_DEPENDENCIES:=src/lib_coq src/mylist src/rtrie src/intern src/getfile src/lex src/parser src/rtype src/exp src/expIntern src/disc src/rcontext src/rsubst src/prettyPrint src/renv src/rtrace src/cache src/match src/derive src/crewrite src/kbrewrite src/builtin src/rule_app src/inner src/plugin
src/thepack.cmo:$(addsuffix .cmo,$(src/thepack_MLPACK_DEPENDENCIES))
src/thepack.cmx:$(addsuffix .cmx,$(src/thepack_MLPACK_DEPENDENCIES))
src/lib_coq.cmx : FOR_PACK=-for-pack Thepack
src/mylist.cmx : FOR_PACK=-for-pack Thepack
src/rtrie.cmx : FOR_PACK=-for-pack Thepack
src/intern.cmx : FOR_PACK=-for-pack Thepack
src/getfile.cmx : FOR_PACK=-for-pack Thepack
src/lex.cmx : FOR_PACK=-for-pack Thepack
src/parser.cmx : FOR_PACK=-for-pack Thepack
src/rtype.cmx : FOR_PACK=-for-pack Thepack
src/exp.cmx : FOR_PACK=-for-pack Thepack
src/expIntern.cmx : FOR_PACK=-for-pack Thepack
src/disc.cmx : FOR_PACK=-for-pack Thepack
src/rcontext.cmx : FOR_PACK=-for-pack Thepack
src/rsubst.cmx : FOR_PACK=-for-pack Thepack
src/prettyPrint.cmx : FOR_PACK=-for-pack Thepack
src/renv.cmx : FOR_PACK=-for-pack Thepack
src/rtrace.cmx : FOR_PACK=-for-pack Thepack
src/cache.cmx : FOR_PACK=-for-pack Thepack
src/match.cmx : FOR_PACK=-for-pack Thepack
src/derive.cmx : FOR_PACK=-for-pack Thepack
src/crewrite.cmx : FOR_PACK=-for-pack Thepack
src/kbrewrite.cmx : FOR_PACK=-for-pack Thepack
src/builtin.cmx : FOR_PACK=-for-pack Thepack
src/rule_app.cmx : FOR_PACK=-for-pack Thepack
src/inner.cmx : FOR_PACK=-for-pack Thepack
src/plugin.cmx : FOR_PACK=-for-pack Thepack
