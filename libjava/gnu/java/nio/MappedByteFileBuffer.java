/* MappedByteFileBuffer.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package gnu.java.nio;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.nio.MappedByteBuffer;
import java.io.IOException;

final public class MappedByteFileBuffer
  extends MappedByteBuffer
{
  public long address;
  boolean readOnly;
  boolean direct;
  public FileChannelImpl ch;
  
  public MappedByteFileBuffer (FileChannelImpl ch) throws IOException
  {
    super ((int) ch.size (), (int) ch.size (), 0, -1);
    
    this.ch = ch;
    address = ch.address;
    try {
      long si = ch.size() / 1;
      limit((int)si);
    } catch (IOException e) {
      System.err.println("failed to get size of file-channel's file");
    }
  }
  
  public MappedByteFileBuffer (MappedByteFileBuffer b)
  {
    // FIXME: fix last value
    super (b.capacity (), b.limit (), b.position (), -1);
    
    this.readOnly = b.isReadOnly ();
    this.ch = b.ch;
    address = b.address;
    limit (b.limit ());
  }

  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public static native byte nio_read_Byte_file_channel (FileChannelImpl ch,
                                                        int index, int limit,
                                                        long address);
  public static native void nio_write_Byte_file_channel (FileChannelImpl ch,
                                                         int index, int limit,
                                                         byte value,
                                                         long address);
  public static native short nio_read_Short_file_channel (FileChannelImpl ch,
                                                          int index, int limit,
                                                          long address);
  public static native void nio_write_Short_file_channel (FileChannelImpl ch,
                                                          int index, int limit,
                                                          short value,
                                                          long address);
  public static native char nio_read_Char_file_channel (FileChannelImpl ch,
                                                        int index, int limit,
                                                        long address);
  public static native void nio_write_Char_file_channel (FileChannelImpl ch,
                                                         int index, int limit,
                                                         char value,
                                                         long address);
  public static native int nio_read_Int_file_channel (FileChannelImpl ch,
                                                      int index, int limit,
                                                      long address);
  public static native void nio_write_Int_file_channel (FileChannelImpl ch,
                                                        int index, int limit,
                                                        int value, long address);
  public static native long nio_read_Long_file_channel (FileChannelImpl ch,
                                                        int index, int limit,
                                                        long address);
  public static native void nio_write_Long_file_channel (FileChannelImpl ch,
                                                         int index, int limit,
                                                         long value,
                                                         long address);
  public static native float nio_read_Float_file_channel (FileChannelImpl ch,
                                                          int index, int limit,
                                                          long address);
  public static native void nio_write_Float_file_channel (FileChannelImpl ch,
                                                          int index, int limit,
                                                          float value,
                                                          long address);
  public static native double nio_read_Double_file_channel (FileChannelImpl ch,
                                                            int index, int limit,
                                                            long address);
  public static native void nio_write_Double_file_channel (FileChannelImpl ch,
                                                           int index, int limit,
                                                           double value,
                                                           long address);

  final public byte get ()
  {
    byte a = MappedByteFileBuffer.nio_read_Byte_file_channel (ch, position (),
                                                              limit (), address);
    position (position () + 1);
    return a;
  }

  final public ByteBuffer put (byte b)
  {
    MappedByteFileBuffer.nio_write_Byte_file_channel (ch, position (), limit (),
                                                      b, address);
    position (position () + 1);
    return this;
  }

  final public byte get (int index)
  {
    byte a = MappedByteFileBuffer.nio_read_Byte_file_channel (ch, index,
                                                              limit (),
                                                              address);
    return a;
  }

  final public ByteBuffer put (int index, byte b)
  {
    MappedByteFileBuffer.nio_write_Byte_file_channel (ch, index, limit (), b,
                                                      address);
    return this;
  }

  final public ByteBuffer compact ()
  {
    return this;
  }

  final public boolean isDirect ()
  {
    return direct;
  }

  final public ByteBuffer slice ()
  {
    MappedByteFileBuffer A = new MappedByteFileBuffer (this);
    return A;
  }

  public ByteBuffer duplicate ()
  {
    return new MappedByteFileBuffer (this);
  }

  public ByteBuffer asReadOnlyBuffer ()
  {
    MappedByteFileBuffer b = new MappedByteFileBuffer (this);
    b.readOnly = true;
    return b;
  }

  final public ByteBuffer asByteBuffer ()
  {
//     ByteBuffer res = new MappedByteFileBuffer (ch);
//     res.limit ((limit () * 1) / 1);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public byte getByte ()
  {
    byte a = nio_read_Byte_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putByte (byte value)
  {
    nio_write_Byte_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public byte getByte (int index)
  {
    byte a = nio_read_Byte_file_channel (ch, index, limit(), address);
    return a;
  }
  
  final public ByteBuffer putByte (int index, byte value)
  {
    nio_write_Byte_file_channel (ch, index, limit (), value, address);
    return this;
  };
  
  final public CharBuffer asCharBuffer ()
  {
//     CharBuffer res = new MappedCharFileBuffer (ch);
//     res.limit ((limit () * 1) / 2);
//     return res;
    throw new Error ("not implemented");
  }

  final public char getChar ()
  {
    char a = nio_read_Char_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putChar (char value)
  {
    nio_write_Char_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public char getChar (int index)
  {
    char a = nio_read_Char_file_channel (ch, index, limit (), address);
    return a;
  }
  
  final public ByteBuffer putChar (int index, char value)
  {
    nio_write_Char_file_channel (ch, index, limit (), value, address);
    return this;
  };

  final public ShortBuffer asShortBuffer ()
  {
//     ShortBuffer res = new MappedShortFileBuffer (ch);
//     res.limit ((limit () * 1) / 2);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public short getShort ()
  {
    short a = nio_read_Short_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putShort (short value)
  {
    nio_write_Short_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public short getShort (int index)
  {
    short a = nio_read_Short_file_channel (ch, index, limit (), address);
    return a;
  }
  
  final public ByteBuffer putShort (int index, short value)
  {
    nio_write_Short_file_channel (ch, index, limit (), value, address);
    return this;
  }

  final public IntBuffer asIntBuffer ()
  {
//     IntBuffer res = new MappedIntFileBuffer (ch);
//     res.limit ((limit () * 1) / 4);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public int getInt ()
  {
    int a = nio_read_Int_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putInt (int value)
  {
    nio_write_Int_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public int getInt (int index)
  {
    int a = nio_read_Int_file_channel (ch, index, limit (),
                                                            address);
    return a;
  }
  
  final public ByteBuffer putInt (int index, int value)
  {
    nio_write_Int_file_channel (ch, index, limit (), value, address);
    return this;
  }

  final public LongBuffer asLongBuffer ()
  {
//     LongBuffer res = new MappedLongFileBuffer (ch);
//     res.limit ((limit () * 1) / 8);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public long getLong ()
  {
    long a = nio_read_Long_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putLong (long value)
  {
    nio_write_Long_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public long getLong (int index)
  {
    long a = nio_read_Long_file_channel (ch, index, limit (), address);
    return a;
  }
  
  final public ByteBuffer putLong (int index, long value)
  {
    nio_write_Long_file_channel (ch, index, limit (), value, address);
    return this;
  }

  final public FloatBuffer asFloatBuffer ()
  {
//     FloatBuffer res = new MappedFloatFileBuffer (ch);
//     res.limit ((limit () * 1) / 4);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public float getFloat ()
  {
    float a = nio_read_Float_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putFloat (float value)
  {
    nio_write_Float_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public float getFloat (int index)
  {
    float a = nio_read_Float_file_channel (ch, index, limit (), address);
    return a;
  }
  
  final public ByteBuffer putFloat (int index, float value)
  {
    nio_write_Float_file_channel (ch, index, limit (), value, address);
    return this;
  }

  final public DoubleBuffer asDoubleBuffer ()
  {
//     DoubleBuffer res = new MappedDoubleFileBuffer (ch);
//     res.limit ((limit () * 1) / 8);
//     return res;
    throw new Error ("not implemented");
  }
  
  final public double getDouble ()
  {
    double a = nio_read_Double_file_channel (ch, position (), limit (), address);
    position (position () + 1);
    return a;
  }
  
  final public ByteBuffer putDouble (double value)
  {
    nio_write_Double_file_channel (ch, position (), limit (), value, address);
    position (position () + 1);
    return this;
  }
  
  final public double getDouble (int index)
  {
    double a = nio_read_Double_file_channel (ch, index, limit (), address);
    return a;
  }
  
  final public ByteBuffer putDouble (int index, double value)
  {
    nio_write_Double_file_channel (ch, index, limit (), value, address);
    return this;
  }
}
