/* FloatBufferImpl.java -- 
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
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ReadOnlyBufferException;

/**
 * This is a Heap memory implementation
 */
public final class FloatBufferImpl extends FloatBuffer
{
  private boolean readOnly;
  
  public FloatBufferImpl(int cap, int off, int lim)
  {
    super (cap, lim, off, 0);
    this.backing_buffer = new float [cap];
    readOnly = false;
  }
  
  public FloatBufferImpl(float[] array, int offset, int length)
  {
    super (array.length, length, offset, 0);
    this.backing_buffer = array;
    readOnly = false;
  }
  
  public FloatBufferImpl(FloatBufferImpl copy)
  {
    super (copy.capacity (), copy.limit (), copy.position (), 0);
    backing_buffer = copy.backing_buffer;
    readOnly = copy.isReadOnly ();
  }
  
  private static native float[] nio_cast (byte[] copy);
  
  FloatBufferImpl (byte[] copy)
  {
    super (copy.length, copy.length, 0, 0);
    this.backing_buffer = copy != null ? nio_cast  (copy) : null;
    readOnly = false;
  }

  private static native byte nio_get_Byte (FloatBufferImpl b, int index, int limit);

  private static native void nio_put_Byte (FloatBufferImpl b, int index, int limit, byte value);

  public ByteBuffer asByteBuffer()
  {
    ByteBufferImpl res = new ByteBufferImpl (backing_buffer);
    res.limit ((limit () * 1) / 4);
    return res;
  }
  
  public boolean isReadOnly ()
  {
    return readOnly;
  }
  
  public FloatBuffer slice()
  {
    return new FloatBufferImpl (this);
  }
  
  public FloatBuffer duplicate()
  {
    return new FloatBufferImpl(this);
  }
  
  public FloatBuffer asReadOnlyBuffer()
  {
    FloatBufferImpl result = new FloatBufferImpl (this);
    result.readOnly = true;
    return result;
  }
  
  public FloatBuffer compact()
  {
    return this;
  }
  
  public boolean isDirect()
  {
    return false;
  }
  
  final public float get()
  {
    float e = backing_buffer[position()];
    position(position()+1);
    return e;
  }
  
  final public FloatBuffer put(float b)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    backing_buffer[position()] = b;
    position(position()+1);
    return this;
  }
  
  final public float get(int index)
  {
    return backing_buffer[index];
  }
  
  final public FloatBuffer put(int index, float b)
  {
    if (readOnly)
      throw new ReadOnlyBufferException ();
    
    backing_buffer[index] = b;
    return this;
  }
  
  final public ByteOrder order ()
  {
    return ByteOrder.BIG_ENDIAN;
  }
}
