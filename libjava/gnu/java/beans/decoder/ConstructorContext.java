/* gnu.java.beans.decoder.ConstructorContext
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

/** A ConstructorContext is a {@link Context} implementation which collects the parameters for a constructor
 * call and instantiates the result object using that constructor. After that sub-contexts can invoke
 * methods on the result object.
 *
 * <p>The constructor is invoked when a sub-context is a statement or the Context ends.</p>
 *
 * @author Robert Schuster
 */
class ConstructorContext extends AbstractCreatableObjectContext
{
  private ArrayList arguments = new ArrayList();
  private Class klass;

  ConstructorContext(String id, Class newClass)
  {
    setId(id);
    // sets superclass field 
    klass = newClass;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
   */
  protected void addParameterObjectImpl(Object o)
  {
    arguments.add(o);
  }

  protected Object createObject(Context outerContext)
    throws AssemblyException
  {
    Object[] args = arguments.toArray();

    try
      {
	Constructor constructor = MethodFinder.getConstructor(klass, args);

	// instantiates object (klass field gets re-set by superclass)
	return constructor.newInstance(args);
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
    catch (InstantiationException ie)
      {
	throw new AssemblyException(ie);
      }
  }
  
}
