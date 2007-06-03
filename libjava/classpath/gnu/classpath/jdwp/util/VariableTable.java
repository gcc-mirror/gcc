/* VariableTable.java -- A class representing a Variable Table for a method
   Copyright (C) 2005, 2007 Free Software Foundation

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
 * A class representing a Variable Table for a method.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class VariableTable
{

  private final int argCnt;

  private final int slots;

  private final long[] lineCI;

  private int[] slot;

  private int[] lengths;

  private String[] sigs;

  private String[] names;

  /**
   * Make a new variable table with the given values.
   * 
   * @param argCnt number of words used by arguments in this frame
   * @param slots number of variables
   * @param lineCI first code index of given variable (size slots)
   * @param names name of given variable (size slots)
   * @param sigs signature of given variable (size slots)
   * @param lengths size of region where variable is active (size slots)
   * @param slot index of variable in this frame (size slots)
   */
  public VariableTable(int argCnt, int slots, long lineCI[], String names[],
                       String sigs[], int lengths[], int slot[])
  {
    this.argCnt = argCnt;
    this.slots = slots;
    this.lineCI = lineCI;
    this.names = names;
    this.sigs = sigs;
    this.lengths = lengths;
    this.slot = slot;
  }

  /**
   * Writes this line table to the given DataOutputStream.
   * 
   * @param os the stream to write it to
   * @throws IOException
   */
  public void write(DataOutputStream os) throws IOException
  {
    os.writeInt(argCnt);
    os.writeInt(slots);
    for (int i = 0; i < slots; i++)
      {
        os.writeLong(lineCI[i]);
        JdwpString.writeString(os, names[i]);
        JdwpString.writeString(os, sigs[i]);
        os.writeInt(lengths[i]);
        os.writeInt(slot[i]);
      }
  }

}
