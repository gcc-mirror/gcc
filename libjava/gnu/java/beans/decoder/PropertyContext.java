/* gnu.java.beans.decoder.PropertyContext
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

/** PropertyContext is a Context implementation that is very similar to MethodContext
 * and IndexContext. The sole purpose of PropertyContext to find out whether it should
 * 'set' or 'get' a certain property. This decision is made using the number of
 * arguments.
 * <p>When the method call has to be made and there is no argument we 'get' the property.
 * With one argument it is 'set'.</p>
 *
 * @author Robert Schuster
 */
class PropertyContext extends AbstractObjectContext
{
  private Object argument;
  private String propertyName;
  private String prefix = "get";
  private boolean methodCalled;

  PropertyContext(String id, String newPropertyName)
  {
    setId(id);
    propertyName = newPropertyName;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
   */
  public void addParameterObject(Object o) throws AssemblyException
  {
    if (methodCalled)
      throw new AssemblyException(new IllegalArgumentException("Cannot add parameter object when method was already called."));

    if (argument != null)
      throw new AssemblyException(new IllegalArgumentException("Property attribut allows zero or one argument only."));

    argument = o;
	setStatement(true);
    prefix = "set";
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
   */
  public void notifyStatement(Context outerContext) throws AssemblyException
  {
    if (methodCalled)
      return;
    methodCalled = true;

    Object outerObject = outerContext.getResult();

    if (outerObject == null)
      throw new AssemblyException(new NullPointerException("No object to access property "
	  + propertyName));
    

    // converts property name into a method name
    String methodName = prefix + propertyName.substring(0, 1).toUpperCase()
                        + propertyName.substring(1);

    // prepares the argument
    Object[] args = (argument != null) ? new Object[] { argument } : null;

    try
      {
	Method method = MethodFinder.getMethod(outerObject.getClass(),
	                                       methodName, args);

	// stores the result whether it is available or not
	setObject(method.invoke(outerObject, args));
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

  public Object endContext(Context outerContext) throws AssemblyException
  {
    notifyStatement(outerContext);

    return getResult();
  }

  public boolean subContextFailed()
  {
    return ! methodCalled;
  }
}
