#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(1.0.0)
AC_PACKAGE_NAME(Erlang librsync wrapper)
AC_INIT(Erlang librsync wrapper, 1.0.0, [], rsync)

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

AC_CHECK_HEADERS([librsync.h], [], [
    AC_MSG_ERROR([librsync header file librsync.h was not found])])

AC_SEARCH_LIBS([rs_sig_begin], [rsync], [], [
    AC_MSG_ERROR([librsync library was not found])])

AC_CONFIG_FILES([vars.config
		 doc/overview.edoc])
AC_OUTPUT
