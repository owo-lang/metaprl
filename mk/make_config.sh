#!/bin/sh

# If you want to add a new variable to this file (mk/make_config.sh):
# 
# 1) add a default value to mk/preface (possibly via mk/default)
#    and add this value to the mk/config target in the top-level Makefile
#
# 2) add a default value to the top-level OMakefile (possibly via mk/default),
#    add this value to all the mk/config .INCLUDE directives in the
#    top-level OMakefile
#
# 3) generate an updated mk/config.win32

if [ "$ENSROOT"x = x -o ! -d "$ENSROOT" ]; then
   ENSROOT=undefined
fi
if [ "$OCAMLSRC"x = x -o ! -d "$OCAMLSRC" ]; then
   OCAMLSRC=undefined
fi
if [ "$PREBUILT_CLIBS"x = x -o ! -d "$PREBUILT_CLIBS" ]; then
   PREBUILT_CLIBS=undefined
fi

if [ -f $ROOT/mk/config ]; then
   mv -f $ROOT/mk/config $ROOT/mk/config.old
   CONFIG_EXISTED=yes
fi

cat > $ROOT/mk/config << end_of_cat
# Main MetaPRL configuration file.

# This file (mk/config) is generated by make using mk/make_config.sh
# If you want to change anything except for the variable values,
# put it into mk/config.local or edit mk/make_config.sh.

# Term module to use: ds or std
# See doc/htmlman/developer-guide/term_modules.html or
# http://cvs.metaprl.org:12000/metaprl/developer-guide/term_modules.html
# for more information.
# If not sure, use ds
#
TERMS=$TERMS

#
# What representation to use for hypothesis and conclusion lists
# Possible values: Lm_array, Lm_splay (for splay trees)
# If not sure, use Lm_array
#
SEQ_SET=$SEQ_SET

#
# Refiner verbosity: VERBOSE or SIMPLE
# See doc/htmlman/developer-guide/refiner_verb_and_simp.html or
# http://cvs.metaprl.org:12000/metaprl/developer-guide/refiner_verb_and_simp.html
# for more information.
# If not sure, use VERBOSE
#
REFINER=$REFINER

#
# This is the list of theory directories theory/*
# that you want to compile.  You want to include at least
#    THEORIES = base
# Include itt if you want to use the Nuprl type theory,
# and add any additional theory directories after that.
#
# Alternatively, use THEORIES = all, or THEORIES = default
#
THEORIES=$THEORIES

#
# Use GNU readline package (available on Linux at least) (yes/no).
#
READLINE=$READLINE

#
# The GNU ncurses package (available in Linux at least) (yes/no)
#
NCURSES=$NCURSES

#
# C compiler
#
CCC=$CCC

#
# Extra make options
#
MAKE_OPTS=$MAKE_OPTS

#
# Whether to compile in various test theories and files (yes/no)
#
TESTS=$TESTS

#
# If ENSROOT is defined, it should point
# to the root of the Ensemble source tree
# In this case Ensemble support would be compiled into MetaPRL
#
ENSROOT=$ENSROOT

#
# If OCAMLSRC is defined, it should point
# to the root of the OCaml source tree
# In this case Jason's marshaller debugging code
# would be compiled into MetaPRL
# Do not enable this unless you know what you are doing!
#
OCAMLSRC=$OCAMLSRC

#
# The remaining options are omake only.
#
# Do you want to use sloppy dependencies?  If enabled, then updating
# the refiner will not force theory files to be recompiled.  If
# in doubt, you should use "false".
#
SLOPPY_DEPENDENCIES=$SLOPPY_DEPENDENCIES

#
# Do you want native and/or byte code?
# By default, we make native code.  If you want
# byte code, set it to true.
#
NATIVE_ENABLED=$NATIVE_ENABLED
BYTE_ENABLED=$BYTE_ENABLED

#
# Do you want to use precompiled versions of the C libraries?  This may
# be necessary on Windows if you don't have a C compiler.  If unsure,
# set to undefined.
#
# If set, must point to a directory (relative to the MetaPRL root) with
# precompiled C libraries.
#
PREBUILT_CLIBS=$PREBUILT_CLIBS

# This file (mk/config) is generated by make using mk/make_config.sh
# If you want to change anything except for the variable values,
# put it into mk/config.local or edit mk/make_config.sh.

end_of_cat

if [ "$CONFIG_EXISTED" != "yes" ]; then
   cat << end_of_cat

A new config file mk/config was created for you.

You should edit it before continuing.

end_of_cat
   exit 1
fi
