/* gnu.java.beans.decoder.AbstractObjectContext
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
package gnu.java.beans.decoder;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/** AbstractObjectContext is the base for all Context implementations which
 * create or provide a result object during their lifetime.
 *
 * <p>This class provides the implementation for an indexed get and set method.
 * But this does not mean that the result object supports these operation.</p>
 *
 * @author Robert Schuster
 *
 */
abstract class AbstractObjectContext extends AbstractContext
{
    protected Object object;

    AbstractObjectContext()
    {}

    /** Sets the result object of the Context.
     *
     * @param obj The result object to be set.
     */
    protected final void setObject(Object obj)
    {
        object = obj;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#set(int, java.lang.Object)
     */
    public final void set(int index, Object o) throws AssemblyException
    {
        try
        {
            Method method =
                object.getClass().getMethod(
                    "set",
                    new Class[] { Integer.TYPE, Object.class });

            method.invoke(object, new Object[] { new Integer(index), o });
        }
        catch (NoSuchMethodException nsme)
        {
            throw new AssemblyException(nsme);
        }
        catch (InvocationTargetException ite)
        {
            throw new AssemblyException(ite.getCause());
        }
        catch (IllegalAccessException iae)
        {
            throw new AssemblyException(iae);
        }
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#get(int)
     */
    public final Object get(int index) throws AssemblyException
    {
        try
        {
            Method method =
                object.getClass().getMethod(
                    "get",
                    new Class[] { Integer.TYPE });

            return method.invoke(object, new Object[] { new Integer(index)});
        }
        catch (NoSuchMethodException nsme)
        {
            throw new AssemblyException(nsme);
        }
        catch (InvocationTargetException ite)
        {
            throw new AssemblyException(ite.getCause());
        }
        catch (IllegalAccessException iae)
        {
            throw new AssemblyException(iae);
        }
    }

    public final Object getResult()
    {
        return object;
    }
}
