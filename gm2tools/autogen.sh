#!/bin/sh

# autogen.sh regenerate the autoconf files.
#   Copyright 2013-2021  Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

rm -rf autom4te.cache

# libtoolize
rm -f aclocal.m4
# aclocal -I . -I config -I ../config
aclocal -I . -I ../config
autoreconf -I . -I ../config
automake --include-deps

rm -rf autom4te.cache

exit 0
