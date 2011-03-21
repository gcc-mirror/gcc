/* LineTable.java -- A class representing a Line Table for a method
   Copyright (C) 2005, 2006 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.util;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * A class representing a Line Table for a method.
 *
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class LineTable
{

  private final long start;
  private final long end;
  private final int[] lineNum;
  private final long[] lineCI;

  /**
   * Construct a line table with the given parameters.
   *
   * @param start lowest code index for method, -1 if native
   * @param end highest code index for method, -1 if native
   * @param lineNum line numbers for in line tables
   * @param lineCI code indicies for entries in line tables
   */
  public LineTable(long start, long end, int[] lineNum, long[] lineCI)
  {
    if (lineNum.length != lineCI.length)
      throw new AssertionError("code index and line numbers tables "
                               + "not same length");
    this.start = start;
    this.end = end;
    this.lineNum = lineNum;
    this.lineCI = lineCI;
  }

  /**
   * Writes this line table to the given DataOutputStream.
   *
   * @param os the stream to write it to
   * @throws IOException
   */
  public void write(DataOutputStream os)
    throws IOException
  {
    os.writeLong(start);
    os.writeLong(end);
    os.writeInt(lineNum.length);
    for (int i = 0; i < lineNum.length; i++)
      {
        os.writeLong(lineCI[i]);
        os.writeInt(lineNum[i]);
      }
  }
}
