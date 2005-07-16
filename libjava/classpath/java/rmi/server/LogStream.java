/* LogStream.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.rmi.server;

import java.io.OutputStream;
import java.io.PrintStream;

/**
 * @deprecated
 */
public class LogStream extends PrintStream
{
  public static final int SILENT = 0;
  public static final int BRIEF = 10;
  public static final int VERBOSE = 20;

  private static PrintStream defStream;

  private LogStream (OutputStream s)
  {
    super (s);
  }

  /**
   * @deprecated
   */
  public static LogStream log (String name)
  {
    throw new Error ("Not implemented");
  }

  /**
   * @deprecated
   */
  public static PrintStream getDefaultStream ()
  {
    return defStream;
  }
  
  /**
   * @deprecated
   */
  public static void setDefaultStream (PrintStream s)
  {
    defStream = s;
  }

  /**
   * @deprecated
   */
  public OutputStream getOutputStream ()
  {
    return out;
  }

  /**
   * @deprecated
   */
  public void setOutputStream (OutputStream s)
  {
    out = s;
  }

  /**
   * @deprecated
   */
  public void write (int buffer)
  {
    super.write (buffer);
  }

  /**
   * @deprecated
   */
  public void write (byte[] buffer, int offset, int len)
  {
    super.write (buffer, offset, len);
  }

  /**
   * @deprecated
   */
  public String toString ()
  {
    throw new Error ("Not implemented");
  }

  /**
   * @deprecated
   */
  public static int parseLevel (String s)
  {
    if (s.equalsIgnoreCase ("silent"))
      {
        return SILENT;
      }
    
    if (s.equalsIgnoreCase ("brief"))
      {
        return BRIEF;
      }
    
    if (s.equalsIgnoreCase ("verbose"))
      {
        return VERBOSE;
      }
    
    return SILENT;
  }
}
