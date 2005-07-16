/* IllegalAccessException.java -- thrown on attempt to reflect on
   inaccessible data
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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


package java.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * Thrown whenever a reflective method tries to do something that the
 * compiler would not allow.  For example, using reflection to set a private
 * variable that belongs to a class in another package is bad.
 *
 * @author Brian Jones
 * @author Warren Levy (warrenl@cygnus.com)
 * @see Class#newInstance()
 * @see Field#set(Object, Object)
 * @see Field#setBoolean(Object, boolean)
 * @see Field#setByte(Object, byte)
 * @see Field#setShort(Object, short)
 * @see Field#setChar(Object, char)
 * @see Field#setInt(Object, int)
 * @see Field#setLong(Object, long)
 * @see Field#setFloat(Object, float)
 * @see Field#setDouble(Object, double)
 * @see Field#get(Object)
 * @see Field#getBoolean(Object)
 * @see Field#getByte(Object)
 * @see Field#getShort(Object)
 * @see Field#getChar(Object)
 * @see Field#getInt(Object)
 * @see Field#getLong(Object)
 * @see Field#getFloat(Object)
 * @see Field#getDouble(Object)
 * @see Method#invoke(Object, Object[])
 * @see Constructor#newInstance(Object[])
 * @status updated to 1.4
 */
public class IllegalAccessException extends Exception
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 6616958222490762034L;

  /**
   * Create an exception without a message.
   */
  public IllegalAccessException()
  {
  }

  /**
   * Create an exception with a message.
   *
   * @param s the message
   */
  public IllegalAccessException(String s)
  {
    super(s);
  }
}
