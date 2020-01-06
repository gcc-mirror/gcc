/* GDC -- D front-end for GCC
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.
*/

/* This module provides the C main() function supplied by the user's program.  */

module __entrypoint;

extern(C):

/* The D main() function supplied by the user's program

   It always has `_Dmain` symbol name and uses C calling convention.
   But D frontend returns its type as `extern(D)` because of Issue 9028.
   As we need to deal with actual calling convention we have to mark it
   as `extern(C)` and use its symbol name.
*/

int _Dmain(char[][] args);
int _d_run_main(int argc, char **argv, void* mainFunc);

/* Substitutes for the C main() function.  Just calls into d_run_main with
   the default main function.  Applications are free to implement their own
   main function and call the _d_run_main function themselves with any main
   function.
*/

int main(int argc, char **argv)
{
    return _d_run_main(argc, argv, &_Dmain);
}

/* This is apparently needed on Solaris because the C tool chain seems to
   expect the main function to be called _main.  It needs both not just one!
*/

version (Solaris)
int _main(int argc, char** argv)
{
    return main(argc, argv);
}

