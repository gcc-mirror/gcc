/* CharBuffer.java -- 
   Copyright (C) 2007  Free Software Foundation, Inc.

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

/**
 * A CharBuffer that wraps a {@link CharSequence}.
 */
final class CharSequenceBuffer
  extends CharBuffer
{

  /**
   * The wrapped char sequence.
   */
  private final CharSequence charSequence;

  /**
   * Creates a new CharSequenceBuffer.
   *
   * @param charSeq the CharSequence to wrap
   * @param capacity the capacity
   * @param limit the limit
   * @param position the position
   * @param mark the mark
   * @param offs the offset
   */
  CharSequenceBuffer(CharSequence charSeq, int capacity, int limit,
                     int position, int mark, int offs)
  {
    super(capacity, limit, position, mark, null, null, offs);
    this.charSequence = charSeq;
  }

  /**
   * Creates a new instance of CharSequenceBuffer, wrapping the specified
   * {@link CharSequence}.
   *
   * @param charSeq the char sequence to wrap
   * @param start the start index in the char sequence
   * @param end the end index in the char sequence
   */
  CharSequenceBuffer(CharSequence charSeq, int start, int end)
  {
    this(charSeq, charSeq.length(), end, start, -1, 0);
  }

  /**
   * Returns a read-only view of this buffer.
   */
  public CharBuffer asReadOnlyBuffer()
  {
    return duplicate();
  }    

  /**
   * This buffer class is not writable by definition and thus throws
   * a ReadOnlyBufferException here.
   */
  public CharBuffer compact()
  {
    throw new ReadOnlyBufferException();
  }

  /**
   * Returns a duplicate of this buffer.
   *
   * @return a duplicate of this buffer
   */
  public CharBuffer duplicate()
  {
    return new CharSequenceBuffer(charSequence, capacity(), limit, pos, mark, 0);
  }

  /**
   * Returns the character at the current position.
   *
   * @return the character at the current position
   */
  public char get()
  {
    if (pos >= limit)
      throw new BufferUnderflowException();

    return charSequence.charAt(array_offset + pos++);
  }

  /**
   * Returns the character at the specified position.
   *
   * @return the character at the specified position
   */
  public char get(int index)
  {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException();

    return charSequence.charAt(array_offset + index);
  }

  /**
   * Cannot be direct, return <code>false</code> here.
   *
   * @return false
   */
  public boolean isDirect()
  {
    return false;
  }

  /**
   * Returns the byte order of this buffer. This is always the native byte
   * order.
   *
   * @return the byte order of this buffer
   */
  public ByteOrder order()
  {
    return ByteOrder.nativeOrder();
  }

  /**
   * This buffer class is not writable by definition and thus throws
   * a ReadOnlyBufferException here.
   */
  public CharBuffer put(char b)
  {
    throw new ReadOnlyBufferException();
  }

  /**
   * This buffer class is not writable by definition and thus throws
   * a ReadOnlyBufferException here.
   */
  public CharBuffer put(int index, char b)
  {
    throw new ReadOnlyBufferException();
  }

  /**
   * Returns a slice of this buffer, exposing the current position and limit.
   */
  public CharBuffer slice()
  {
    int newCapacity = limit - pos;
    return new CharSequenceBuffer(charSequence, newCapacity, newCapacity, 0,
                                  -1, pos);
  }

  /**
   * Returns a sub sequence from the specified start index and with the
   * specified length.
   *
   * @param start the start index
   * @param length the length of the sub sequence
   */
  public CharSequence subSequence(int start, int length)
  {
    int begin = array_offset + start + pos;
    return charSequence.subSequence(begin, begin + length);
  }

  /**
   * This kind of CharBuffer is read-only, so we return <code>true</code>
   * here.
   */
  public boolean isReadOnly()
  {
    return true;
  }

}
