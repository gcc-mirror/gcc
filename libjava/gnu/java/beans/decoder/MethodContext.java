/* gnu.java.beans.decoder.MethodContext
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;

/** MethodContext collects arguments for a method call and creates the result object
 * using it. The method is called using the result object of the parent Context.
 *
 * <p>When the result object is available methods can be called on it using sub-Contexts.</p>
 *
 * @author Robert Schuster
 */
class MethodContext extends AbstractCreatableObjectContext
{
    private ArrayList arguments = new ArrayList();
    private String methodName;

    MethodContext(String id, String newMethodName)
    {
        setId(id);
        setStatement(true);
        methodName = newMethodName;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
     */
    public void addParameterObjectImpl(Object o)
    {
        arguments.add(o);
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
     */
    protected Object createObject(Context outerContext)
        throws AssemblyException
    {
        Object outerObject = outerContext.getResult();

        if (outerObject == null)
            throw new AssemblyException(
                new NullPointerException(
                    "No object to invoke method " + methodName));

        Object[] args = arguments.toArray();

        try
        {
            Method method =
                MethodFinder.getMethod(
                    outerObject.getClass(),
                    methodName,
                    args);
            return method.invoke(outerObject, args);
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
}
