/* MappedByteBufferImpl.java --
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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


package java.nio;

import gnu.classpath.Pointer;

import java.io.IOException;

final class MappedByteBufferImpl extends MappedByteBuffer
{
  private final boolean readOnly;

  /** Posix uses this for the pointer returned by mmap;
   * Win32 uses it for the pointer returned by MapViewOfFile. */
  public Pointer implPtr;
  /** Posix uses this for the actual length passed to mmap;
   * Win32 uses it for the pointer returned by CreateFileMapping. */
  public long implLen;

  public MappedByteBufferImpl(Pointer address, int size, boolean readOnly)
    throws IOException
  {
    super(size, size, 0, -1, address);
    this.readOnly = readOnly;
  }

  public boolean isReadOnly()
  {
    return readOnly;
  }

  public byte get()
  {
    checkForUnderflow();

    int pos = position();
    byte result = VMDirectByteBuffer.get(address, pos);
    position(pos + 1);
    return result;
  }

  public ByteBuffer put(byte value)
  {
    checkIfReadOnly();
    checkForOverflow();

    int pos = position();
    VMDirectByteBuffer.put(address, pos, value);
    position(pos + 1);
    return this;
  }

  public byte get(int index)
  {
    checkIndex(index);

    return VMDirectByteBuffer.get(address, index);
  }

  public ByteBuffer get(byte[] dst, int offset, int length)
  {
    checkArraySize(dst.length, offset, length);
    checkForUnderflow(length);

    int index = position();
    VMDirectByteBuffer.get(address, index, dst, offset, length);
    position(index+length);

    return this;
  }

  public ByteBuffer put(int index, byte value)
  {
    checkIfReadOnly();
    checkIndex(index);

    VMDirectByteBuffer.put(address, index, value);
    return this;
  }

  public ByteBuffer compact()
  {
    checkIfReadOnly();
    mark = -1;
    int pos = position();
    if (pos > 0)
      {
        int count = remaining();
        // Call shiftDown method optimized for direct buffers.
        VMDirectByteBuffer.shiftDown(address, 0, pos, count);
        position(count);
        limit(capacity());
      }
    else
      {
        position(limit());
        limit(capacity());
      }
    return this;
  }

  public boolean isDirect()
  {
    return true;
  }

  public ByteBuffer slice()
  {
    int rem = remaining();
    if (isReadOnly())
        return new DirectByteBufferImpl.ReadOnly
      (this, VMDirectByteBuffer.adjustAddress(address, position()),
       rem, rem, 0);
    else
        return new DirectByteBufferImpl.ReadWrite
      (this, VMDirectByteBuffer.adjustAddress(address, position()),
       rem, rem, 0);
  }

  private ByteBuffer duplicate(boolean readOnly)
  {
    int pos = position();
    reset();
    int mark = position();
    position(pos);
    DirectByteBufferImpl result;
    if (readOnly)
        result = new DirectByteBufferImpl.ReadOnly(this, address, capacity(),
                                                   limit(), pos);
    else
        result = new DirectByteBufferImpl.ReadWrite(this, address, capacity(),
                                                    limit(), pos);

    if (mark != pos)
      {
        result.position(mark);
        result.mark();
        result.position(pos);
      }
    return result;
  }

  public ByteBuffer duplicate()
  {
    return duplicate(isReadOnly());
  }

  public ByteBuffer asReadOnlyBuffer()
  {
    return duplicate(true);
  }

  public CharBuffer asCharBuffer()
  {
    return new CharViewBufferImpl(this, remaining() >> 1);
  }

  public ShortBuffer asShortBuffer()
  {
    return new ShortViewBufferImpl(this, remaining() >> 1);
  }

  public IntBuffer asIntBuffer()
  {
    return new IntViewBufferImpl(this, remaining() >> 2);
  }

  public LongBuffer asLongBuffer()
  {
    return new LongViewBufferImpl(this, remaining() >> 3);
  }

  public FloatBuffer asFloatBuffer()
  {
    return new FloatViewBufferImpl(this, remaining() >> 2);
  }

  public DoubleBuffer asDoubleBuffer()
  {
    return new DoubleViewBufferImpl(this, remaining() >> 3);
  }

  public char getChar()
  {
    return ByteBufferHelper.getChar(this, order());
  }

  public ByteBuffer putChar(char value)
  {
    ByteBufferHelper.putChar(this, value, order());
    return this;
  }

  public char getChar(int index)
  {
    return ByteBufferHelper.getChar(this, index, order());
  }

  public ByteBuffer putChar(int index, char value)
  {
    ByteBufferHelper.putChar(this, index, value, order());
    return this;
  }

  public short getShort()
  {
    return ByteBufferHelper.getShort(this, order());
  }

  public ByteBuffer putShort(short value)
  {
    ByteBufferHelper.putShort(this, value, order());
    return this;
  }

  public short getShort(int index)
  {
    return ByteBufferHelper.getShort(this, index, order());
  }

  public ByteBuffer putShort(int index, short value)
  {
    ByteBufferHelper.putShort(this, index, value, order());
    return this;
  }

  public int getInt()
  {
    return ByteBufferHelper.getInt(this, order());
  }

  public ByteBuffer putInt(int value)
  {
    ByteBufferHelper.putInt(this, value, order());
    return this;
  }

  public int getInt(int index)
  {
    return ByteBufferHelper.getInt(this, index, order());
  }

  public ByteBuffer putInt(int index, int value)
  {
    ByteBufferHelper.putInt(this, index, value, order());
    return this;
  }

  public long getLong()
  {
    return ByteBufferHelper.getLong(this, order());
  }

  public ByteBuffer putLong(long value)
  {
    ByteBufferHelper.putLong(this, value, order());
    return this;
  }

  public long getLong(int index)
  {
    return ByteBufferHelper.getLong(this, index, order());
  }

  public ByteBuffer putLong(int index, long value)
  {
    ByteBufferHelper.putLong(this, index, value, order());
    return this;
  }

  public float getFloat()
  {
    return ByteBufferHelper.getFloat(this, order());
  }

  public ByteBuffer putFloat(float value)
  {
    ByteBufferHelper.putFloat(this, value, order());
    return this;
  }

  public float getFloat(int index)
  {
    return ByteBufferHelper.getFloat(this, index, order());
  }

  public ByteBuffer putFloat(int index, float value)
  {
    ByteBufferHelper.putFloat(this, index, value, order());
    return this;
  }

  public double getDouble()
  {
    return ByteBufferHelper.getDouble(this, order());
  }

  public ByteBuffer putDouble(double value)
  {
    ByteBufferHelper.putDouble(this, value, order());
    return this;
  }

  public double getDouble(int index)
  {
    return ByteBufferHelper.getDouble(this, index, order());
  }

  public ByteBuffer putDouble(int index, double value)
  {
    ByteBufferHelper.putDouble(this, index, value, order());
    return this;
  }

  // NOTE: In libgcj these methods are implemented in natFileChannelXxx.cc,
  // because they're small, and to put them next to FileChannelImpl::mapImpl.
  native void unmapImpl();
  native boolean isLoadedImpl();
    // FIXME: Try to load all pages into memory.
  native void loadImpl();

  native void forceImpl();
}
