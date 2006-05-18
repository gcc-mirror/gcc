/* MacResourceFork.java -- Parses MacOS resource forks.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.font.opentype;

import java.nio.ByteBuffer;


/**
 * A class for accessing data that is stored in the resource fork of
 * Macintosh files. Writing resource forks is currently not supported.
 *
 * <p>The gnu.java.awt.font package uses this class for accessing
 * fonts in the MacOS X ".dfont" format, which is is a file in the
 * format of a Macintosh resource fork, but stored in the normal data
 * fork of the file.
 *
 * <p>The implementation has been designed to work efficiently with
 * the virtual memory subsystem. It is recommended to pass an
 * instance of {@link java.nio.MappedByteBuffer} to the constructor.
 *
 * <p>Thread Safety: All access is synchronized on the ByteBuffer
 * that is passed to the constructor.
 *
 * @see <a href=
 * "http://developer.apple.com/documentation/mac/MoreToolbox/MoreToolbox-99.html"
 * >Apple&#x2019; developer documentation about the Resource File
 * Format</a>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
final class MacResourceFork
{
  int[] types;
  Resource[][] resources;
  ByteBuffer buf;

  public MacResourceFork(ByteBuffer buf)
  {
    int typeListOffset;
    int refListOffset;
    int nameListOffset;
    int mapOffset, mapLen;
    int dataOffset, dataLen;
    int numTypes;

    synchronized (buf)
    {
      buf = buf.duplicate();
      this.buf = buf;
      buf.position(0);
      dataOffset = buf.getInt();
      mapOffset = buf.getInt();
      dataLen = buf.getInt();
      mapLen = buf.getInt();
      buf.position(mapOffset + 24);
      refListOffset = mapOffset + buf.getChar();
      nameListOffset = mapOffset + buf.getChar();
      numTypes = buf.getChar() + 1;
      types = new int[numTypes];
      resources = new Resource[numTypes][];

      /* Parse resource type list. */
      typeListOffset = buf.position();
      for (int i = 0; i < numTypes; i++)
      {
        buf.position(typeListOffset + 8 * i);
        int resType = buf.getInt();
        int numRes = buf.getChar() + 1;

        types[i] = resType;
        resources[i] = new Resource[numRes];

        buf.position(refListOffset + buf.getChar());
        for (int j = 0; j < numRes; j++)
        {
          short resID = buf.getShort();
          int resNameOffset = nameListOffset + buf.getChar();
          int resDataOffset = buf.getInt();
          byte resAttr = (byte) (resDataOffset >> 24);
          resDataOffset = dataOffset + (resDataOffset & 0x00ffffff);
          buf.getInt(); /* skip four reserved bytes */

          Resource rsrc = new Resource(buf, resType, resID, resDataOffset,
                                       resNameOffset);
          resources[i][j] = rsrc;
        }
      }
    }
  }
  

  public Resource[] getResources(int type)
  {
    synchronized (buf)
    {
      for (int i = 0; i < types.length; i++)
      {
        if (types[i] == type)
          return resources[i];
      }
    }
    return null;
  }
  

  public Resource getResource(int type, short id)
  {
    Resource[] res;

    synchronized (buf)
    {
      for (int i = 0; i < types.length; i++)
      {
        if (types[i] != type)
          continue;
          
        res = resources[i];
        for (int j = 0; j < res.length; j++)
          if (res[j].getID() == id)
            return res[j];
      }
    }

    return null;
  }


  /**
   * A single resource that is contained in a resource fork.
   */
  public static final class Resource
  {
    int type;
    short id;
    byte attribute;
    int nameOffset;
    int dataOffset;
    ByteBuffer buf;

    private Resource(ByteBuffer buf,
                     int type, short id, int dataOffset, int nameOffset)
    {
      this.buf = buf;
      this.type = type;
      this.id = id;
      this.dataOffset = dataOffset;
      this.nameOffset = nameOffset;
    }


    /**
     * Returns the type of this resource.
     *
     * @return an <code>int</code> encoding a four-byte type tag,
     * such as <code>0x464f4e54</code> for <code>'FONT'</code>.
     */
    public int getType()
    {
      return type;
    }


    /**
     * Returns the ID of this resource.
     */
    public short getID()
    {
      return id;
    }


    /**
     * Retrieves the content of the resource. Only one page of memory
     * is touched, irrespective of the actual size of the resource.
     */
    public ByteBuffer getContent()
    {
      synchronized (buf)
      {
        buf.limit(buf.capacity());
        int len = buf.getInt(dataOffset);
        buf.position(dataOffset + 4).limit(dataOffset + 4 + len);
        return buf.slice();
      }
    }


    /**
     * Determines the length of the resource in bytes.
     */
    public int getLength()
    {
      synchronized (buf)
      {
        return buf.getInt(dataOffset);
      }
    }
  }
}
