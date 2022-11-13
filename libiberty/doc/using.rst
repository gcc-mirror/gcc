..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: using libiberty, libiberty usage, how to use

.. _using:

Using
-----

.. THIS SECTION IS CRAP AND NEEDS REWRITING BADLY.

To date, ``libiberty`` is generally not installed on its own.  It has evolved
over years but does not have its own version number nor release schedule.

Possibly the easiest way to use ``libiberty`` in your projects is to drop the
``libiberty`` code into your project's sources, and to build the library along
with your own sources; the library would then be linked in at the end.  This
prevents any possible version mismatches with other copies of libiberty
elsewhere on the system.

Passing :option:`--enable-install-libiberty` to the :command:`configure`
script when building ``libiberty`` causes the header files and archive library
to be installed when make install is run.  This option also takes
an (optional) argument to specify the installation location, in the same
manner as :option:`--prefix`.

For your own projects, an approach which offers stability and flexibility
is to include ``libiberty`` with your code, but allow the end user to optionally
choose to use a previously-installed version instead.  In this way the
user may choose (for example) to install ``libiberty`` as part of GCC, and use
that version for all software built with that compiler.  (This approach
has proven useful with software using the GNU ``readline`` library.)

Making use of ``libiberty`` code usually requires that you include one or more
header files from the ``libiberty`` distribution.  (They will be named as
necessary in the function descriptions.)  At link time, you will need to
add :option:`-liberty` to your link command invocation.