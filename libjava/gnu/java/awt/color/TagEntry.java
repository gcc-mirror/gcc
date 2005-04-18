/* TagEntry.java -- A utility class used for storing the tags in ICC_Profile
   Copyright (C) 2004 Free Software Foundation

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

package gnu.java.awt.color;


/**
 * TagEntry - stores a profile tag.
 * These are conveniently stored in a hashtable with the tag signature
 * as a key. A legal profile can only have one tag with a given sig,
 * so we can conveniently ignore collisions.
 *
 * @author Sven de Marothy
 */
public class TagEntry
{
  // tag table entry size
  public static final int entrySize = 12;
  private int signature;
  private int size;
  private int offset;
  private byte[] data;

  public TagEntry(int sig, int offset, int size, byte[] data)
  {
    this.signature = sig;
    this.offset = offset;
    this.size = size;
    this.data = new byte[size];
    System.arraycopy(data, offset, this.data, 0, size);
  }

  public TagEntry(int sig, byte[] data)
  {
    this.signature = sig;
    this.size = data.length;
    this.data = new byte[size];
    System.arraycopy(data, offset, this.data, 0, size);
  }

  public byte[] getData()
  {
    byte[] d = new byte[size];
    System.arraycopy(this.data, 0, d, 0, size);
    return d;
  }

  public String hashKey()
  {
    return tagHashKey(signature);
  }

  public String toString()
  {
    String s = "";
    s = s + (char) ((byte) ((signature >> 24) & 0xFF));
    s = s + (char) ((byte) ((signature >> 16) & 0xFF));
    s = s + (char) ((byte) ((signature >> 8) & 0xFF));
    s = s + (char) ((byte) (signature & 0xFF));
    return s;
  }

  public int getSignature()
  {
    return signature;
  }

  public int getSize()
  {
    return size;
  }

  public int getOffset()
  {
    return offset;
  }

  public void setOffset(int offset)
  {
    this.offset = offset;
  }

  public static String tagHashKey(int sig)
  {
    return "" + sig;
  }
}
