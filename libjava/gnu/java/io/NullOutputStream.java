/* NullOutputStream.java -- OutputStream that does absolutely nothing
   Copyright (C) 1998 Free Software Foundation, Inc.

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
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.io;

import java.io.OutputStream;

/**
   This is a placeholder OutputStream that does absolutley nothing
   when written to.  It is intended to be used in the same manner as
   /dev/null.  None of this class's methods do anything at all.
*/
public class NullOutputStream extends OutputStream
{
  public NullOutputStream() {}
  public void write( int b ) {}
  public void write( byte b[] ) {}
  public void write( byte b[], int off, int len ) {}
  public void flush() {}
  public void close() {}
}
