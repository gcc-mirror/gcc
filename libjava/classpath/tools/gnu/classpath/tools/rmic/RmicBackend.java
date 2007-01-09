/* RmicBackend.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA. */

package gnu.classpath.tools.rmic;
public interface RmicBackend
{
  void setup(boolean keep, boolean need11Stubs, boolean need12Stubs,
             boolean iiop, boolean poa, boolean debug, boolean warnings,
             boolean noWrite, boolean verbose, boolean force, String classpath,
             String bootclasspath, String extdirs, String outputDirectory);

  boolean run(String[] inputFiles);
}
