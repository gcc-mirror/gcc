/* CoderResult.java -- 
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package java.nio.charset;

import java.lang.ref.WeakReference;
import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.util.HashMap;

/**
 * @author Jesse Rosenstock
 * @since 1.4
 */
public class CoderResult
{ 
  private static final int TYPE_MALFORMED  = 0;
  private static final int TYPE_OVERFLOW   = 1;
  private static final int TYPE_UNDERFLOW  = 2;
  private static final int TYPE_UNMAPPABLE = 3;

  public static final CoderResult OVERFLOW
    = new CoderResult (TYPE_OVERFLOW, 0);
  public static final CoderResult UNDERFLOW
    = new CoderResult (TYPE_UNDERFLOW, 0);
  
  private static final String[] names
    = { "MALFORMED", "OVERFLOW", "UNDERFLOW", "UNMAPPABLE" };

  private static final Cache malformedCache
    = new Cache ()
      {
        protected CoderResult make (int length)
        {
          return new CoderResult (TYPE_MALFORMED, length);
        }
      };

  private static final Cache unmappableCache
    = new Cache ()
      {
        protected CoderResult make (int length)
        {
          return new CoderResult (TYPE_UNMAPPABLE, length);
        }
      };

  private final int type;
  private final int length;

  // Package-private to avoid a trampoline constructor.
  CoderResult (int type, int length)
  {
    this.type = type;
    this.length = length;
  }

  public boolean isError ()
  {
    return length > 0;
  }

  public boolean isMalformed ()
  {
    return type == TYPE_MALFORMED;
  }

  public boolean isOverflow ()
  {
    return type == TYPE_OVERFLOW;
  }

  public boolean isUnderflow ()
  {
    return type == TYPE_UNDERFLOW;
  }

  public boolean isUnmappable ()
  {
    return type == TYPE_UNMAPPABLE;
  }

  public int length ()
  {
    if (length <= 0)
      throw new UnsupportedOperationException ();
    else
      return length;
  }

  public static CoderResult malformedForLength (int length)
  {
    return malformedCache.get (length);
  }
    
  public void throwException ()
    throws CharacterCodingException
  {
    switch (type)
      {
        case TYPE_MALFORMED:
          throw new MalformedInputException (length);
        case TYPE_OVERFLOW:
          throw new BufferOverflowException ();
        case TYPE_UNDERFLOW:
          throw new BufferUnderflowException ();
        case TYPE_UNMAPPABLE:
          throw new UnmappableCharacterException (length);
      }
  }

  public String toString ()
  {
    String name = names[type];
    return (length > 0) ? name + '[' + length + ']' : name;
  }

  public static CoderResult unmappableForLength (int length)
  {
    return unmappableCache.get (length);
  }    

  private abstract static class Cache
  {
    private final HashMap cache;

    // Package-private to avoid a trampoline constructor.
    Cache ()
    {
      cache = new HashMap ();
    }

    // Package-private to avoid a trampoline.
    synchronized CoderResult get (int length)
    {
      if (length <= 0)
        throw new IllegalArgumentException ("Non-positive length");

      Integer len = new Integer (length);
      CoderResult cr = null;
      Object o;
      if ((o = cache.get (len)) != null)
        cr = (CoderResult) ((WeakReference) o).get ();
      if (cr == null)
        {
          cr = make (length);
          cache.put (len, new WeakReference (cr));
        }

      return cr;
    }

    protected abstract CoderResult make (int length);
  }
}
