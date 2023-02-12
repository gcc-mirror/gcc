/* GDC -- D front-end for GCC
   Copyright (C) 2018-2023 Free Software Foundation, Inc.

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

/* This module provides the D main() function supplied by the user's program.  */

module __main;

version (D_BetterC)
{
    extern (C) int main(int, char**)
    {
        return 0;
    }
}
else
{
    int main(char[][])
    {
        return 0;
    }
}
