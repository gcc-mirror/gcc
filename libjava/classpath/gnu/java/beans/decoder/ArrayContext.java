/* gnu.java.beans.decoder.ArrayContext
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.java.beans.decoder;

import java.lang.reflect.Array;

/** A Context implementation for a fixed size array. The array
 * elements have to be set using IndexContext instances.
 *
 * @author Robert Schuster
 */
class ArrayContext extends AbstractContext
{
  private Object array;

  ArrayContext(String id, Class klass, int length)
  {
    setId(id);
    array = Array.newInstance(klass, length);
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
   */
  public void addParameterObject(Object o) throws AssemblyException
  {
    throw new AssemblyException(new IllegalStateException("Adding objects without an index to a fixed array is not possible."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#reportStatement()
   */
  public void notifyStatement(Context outerContext)
  {
    // method call intentionally ignored because there is not any useful effect
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
   */
  public Object endContext(Context outerContext) throws AssemblyException
  {
    return array;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#subContextFailed()
   */
  public boolean subContextFailed()
  {
    // returns false to indicate that assembling the array does not fail only because
    // a subelement failed.
    return false;
  }

  public void set(int index, Object o) throws AssemblyException
  {
    try
      {
        Array.set(array, index, o);
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
        throw new AssemblyException(aioobe);
      }
  }

  public Object get(int index) throws AssemblyException
  {
    try
      {
        return Array.get(array, index);
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
        throw new AssemblyException(aioobe);
      }
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#getResult()
   */
  public Object getResult()
  {
    return array;
  }
}
