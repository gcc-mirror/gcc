/* gnu.java.beans.decoder.GrowableArrayContext
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

/** A Context implementation for a growable array. The array
 * elements have to be set using expressions.
 *
 * @author Robert Schuster
 */
class GrowableArrayContext extends AbstractContext
{
    private static final int INITIAL_SIZE = 16;
    
    private Class klass;
    private Object array;
    private int length;
    
    GrowableArrayContext(String id, Class newClass)
    {
        setId(id);
        klass = newClass;
        array = Array.newInstance(klass, INITIAL_SIZE);
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
     */
    public void addParameterObject(Object o) throws AssemblyException
    {
      if (length == Array.getLength(array))
        {
          Object tmp = Array.newInstance(klass, length * 2);
          System.arraycopy(array, 0, tmp, 0, length);
          array = tmp;
        }
        
      try {
        Array.set(array, length++, o);
      } catch(IllegalArgumentException iae) {
        throw new AssemblyException(iae);
      }
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#reportStatement()
     */
    public void notifyStatement(Context outerContext) throws AssemblyException
    {
        throw new AssemblyException(
            new IllegalArgumentException("Statements inside a growable array are not allowed."));
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
     */
    public Object endContext(Context outerContext) throws AssemblyException
    {
        if (length != Array.getLength(array))
          {
            Object tmp = Array.newInstance(klass, length);
            System.arraycopy(array, 0, tmp, 0, length);
            array = tmp;
          }
        
        return array;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#subContextFailed()
     */
    public boolean subContextFailed()
    {
        // returns false to indicate that assembling the array does not fail only because
        // a subelement failed
        return false;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#set(int, java.lang.Object)
     */
    public void set(int index, Object o) throws AssemblyException
    {
      try {
        Array.set(array, index, o);
      } catch(IllegalArgumentException iae) {
        throw new AssemblyException(iae);   
      }
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#get(int)
     */
    public Object get(int index) throws AssemblyException
    {
      return Array.get(array, index);
    }

    public Object getResult()
    {
        return array;
    }
}
