/* MappedByteBufferImpl.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package java.nio;

import java.io.IOException;
import java.nio.channels.FileChannelImpl;
import gnu.gcj.RawData;

public class MappedByteBufferImpl extends MappedByteBuffer
{
  boolean readOnly;
  RawData map_address;
  public FileChannelImpl ch;
  
  public MappedByteBufferImpl (FileChannelImpl ch) throws IOException
  {
    super ((int) ch.size (), (int) ch.size (), 0, -1);
    
    this.ch = ch;
    map_address = ch.map_address;
    long si = ch.size () / 1;
    limit ((int) si);
  }

  public MappedByteBufferImpl (FileChannelImpl ch, int offset, int capacity, int limit, int position, int mark, boolean readOnly)
  {
    super (capacity, limit, position, mark);

    this.ch = ch;
    this.array_offset = offset;
    this.readOnly = readOnly;
  }
  
  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public static byte getImpl (FileChannelImpl ch, int index,
			      int limit, RawData map_address)
  {
    throw new Error ("Not implemented");
  }
  
  public static void putImpl (FileChannelImpl ch, int index,
			      int limit, byte value, RawData map_address)
  {
    throw new Error ("Not implemented");
  }

  public byte get ()
  {
    byte result = get (position());
    position (position() + 1);
    return result;
  }

  public ByteBuffer put (byte value)
  {
    put (position(), value);
    position (position() + 1);
    return this;
  }

  public byte get (int index)
  {
    return getImpl (ch, index, limit (), map_address);
  }

  public ByteBuffer put (int index, byte value)
  {
    putImpl (ch, index, limit (), value, map_address);
    return this;
  }

  public ByteBuffer compact ()
  {
    int copied = 0;
    
    while (remaining () > 0)
      {
	put (copied, get ());
	copied++;
      }

    position (copied);
    return this;
  }

  public boolean isDirect ()
  {
    return true;
  }

  public ByteBuffer slice ()
  {
    throw new Error ("Not implemented");
  }

  public ByteBuffer duplicate ()
  {
    throw new Error ("Not implemented");
  }

  public ByteBuffer asReadOnlyBuffer ()
  {
    throw new Error ("Not implemented");
  }

  public CharBuffer asCharBuffer ()
  {
    return new CharViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public ShortBuffer asShortBuffer ()
  {
    return new ShortViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public IntBuffer asIntBuffer ()
  {
    return new IntViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }
  
  public LongBuffer asLongBuffer ()
  {
    return new LongViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public FloatBuffer asFloatBuffer ()
  {
    return new FloatViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public DoubleBuffer asDoubleBuffer ()
  {
    return new DoubleViewBufferImpl (this, position (), remaining(), remaining (), 0, -1, isReadOnly ());
  }

  public final char getChar()
  {
    return ByteBufferHelper.getChar (this);
  }
  
  public final ByteBuffer putChar (char value)
  {
    return ByteBufferHelper.putChar (this, value);
  }
  
  public final char getChar (int index)
  {
    return ByteBufferHelper.getChar (this, index);
  }
  
  public final ByteBuffer putChar (int index, char value)
  {
    return ByteBufferHelper.putChar (this, index, value);
  }

  public final short getShort()
  {
    return ByteBufferHelper.getShort (this);
  }
  
  public final ByteBuffer putShort (short value)
  {
    return ByteBufferHelper.putShort (this, value);
  }
  
  public final short getShort (int index)
  {
    return ByteBufferHelper.getShort (this, index);
  }
  
  public final ByteBuffer putShort (int index, short value)
  {
    return ByteBufferHelper.putShort (this, index, value);
  }

  public final int getInt()
  {
    return ByteBufferHelper.getInt (this);
  }
  
  public final ByteBuffer putInt (int value)
  {
    return ByteBufferHelper.putInt (this, value);
  }
  
  public final int getInt (int index)
  {
    return ByteBufferHelper.getInt (this, index);
  }
  
  public final ByteBuffer putInt (int index, int value)
  {
    return ByteBufferHelper.putInt (this, index, value);
  }

  public final long getLong()
  {
    return ByteBufferHelper.getLong (this);
  }
  
  public final ByteBuffer putLong (long value)
  {
    return ByteBufferHelper.putLong (this, value);
  }
  
  public final long getLong (int index)
  {
    return ByteBufferHelper.getLong (this, index);
  }
  
  public final ByteBuffer putLong (int index, long value)
  {
    return ByteBufferHelper.putLong (this, index, value);
  }

  public final float getFloat()
  {
    return ByteBufferHelper.getFloat (this);
  }
  
  public final ByteBuffer putFloat (float value)
  {
    return ByteBufferHelper.putFloat (this, value);
  }
  
  public final float getFloat (int index)
  {
    return ByteBufferHelper.getFloat (this, index);
  }

  public final ByteBuffer putFloat (int index, float value)
  {
    return ByteBufferHelper.putFloat (this, index, value);
  }

  public final double getDouble()
  {
    return ByteBufferHelper.getDouble (this);
  }

  public final ByteBuffer putDouble (double value)
  {
    return ByteBufferHelper.putDouble (this, value);
  }
  
  public final double getDouble (int index)
  {
    return ByteBufferHelper.getDouble (this, index);
  }
  
  public final ByteBuffer putDouble (int index, double value)
  {
    return ByteBufferHelper.putDouble (this, index, value);
  }
}
