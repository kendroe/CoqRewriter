src/AdvancedRewritePlugin_MLLIB_DEPENDENCIES:= src/mylist src/rtrie src/intern src/getfile src/lex src/parser src/rtype src/exp src/expIntern src/disc src/rcontext src/rsubst src/prettyPrint src/renv src/rtrace src/cache src/match src/derive src/crewrite src/kbrewrite src/builtin src/rule_app src/inner src/advanced_rewrite
src/AdvancedRewritePlugin.cma:$(addsuffix .cmo,$(src/AdvancedRewritePlugin_MLLIB_DEPENDENCIES))
src/AdvancedRewritePlugin.cmxa src/AdvancedRewritePlugin.cmxs:$(addsuffix .cmx,$(src/AdvancedRewritePlugin_MLLIB_DEPENDENCIES))
