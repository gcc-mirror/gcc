/* gnu.java.beans.decoder.ObjectHandler
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

import java.beans.ExceptionListener;

import org.xml.sax.Attributes;

/** An ObjectHandler parses the &lt;object&gt; tag and thereby creates various
 * Context implementations.
 *
 * @author Robert Schuster
 *
 */
public class ObjectHandler extends AbstractElementHandler
{
  /**
   * XXX: Can all results be stored with an object id?
   *
   *
   * @param PersistenceParser
   */
  ObjectHandler(ElementHandler parent)
  {
    super(parent, true);
  }

  protected Context startElement(Attributes attributes, ExceptionListener exceptionListener)
    throws AssemblyException
  {
    String className = attributes.getValue("class");
    String methodName = attributes.getValue("method");
    String fieldName = attributes.getValue("field");
    String index = attributes.getValue("index");
    String propertyName = attributes.getValue("property");
    String id = attributes.getValue("id");
    String idRef = attributes.getValue("idref");

    /* first check if we just want to access an existing object (idref present)
     *
     * note: <object idref="foo" method="bar"/> is not valid to call method "bar"
     * on the object with id "foo". Instead this should return the object "foo"
     * itself. The right way to this is:
     * <object idref="foo">
     *         <object method="bar"/>
     * </object>
     *
     * This means that if idref is present class, method, field, index and
     * property are obsolete.
     */
    if (idRef != null)
      // reactivates an existing object and giving it another name if id exists
      return new ObjectContext(id, getObject(idRef));

    // decides whether we are in a static (className present) or dynamic context
    if (className != null)
      {
	try
	  {
	    Class klass = instantiateClass(className);

	    // class name exists which means that we are in a static context.
	    // so we may want to ...
	    // access a static field if the fieldName exists
	    if (fieldName != null)
	      {
		try
		  {
		    return new ObjectContext(id,
		                             klass.getField(fieldName).get(null));
		  }
		catch (NoSuchFieldException nsfe)
		  {
		    throw new AssemblyException(nsfe);
		  }
		catch (IllegalAccessException iae)
		  {
		    throw new AssemblyException(iae);
		  }
	      }

	    // (falling through is important!)
	    // run a constructor if methodName is "new" or null
	    if (methodName == null || methodName.equals("new"))
	      return new ConstructorContext(id, klass);

	    // (falling through is important!)
	    // run a static method on the given class (if methodName exists, which is implied already) 
	    return new StaticMethodContext(id, klass, methodName);
	    // XXX: should fail if unexpected attributes are present?
	  }
	catch (ClassNotFoundException cnfe)
	  {
	    throw new AssemblyException(cnfe);
	  }
      }
    else
      {
	// className does not exist which means we are in the context of
	// some object and want to ...
	// access the get(int index) method if index != null
	if (index != null)
	  {
	    try
	      {
		// Note: http://java.sun.com/products/jfc/tsc/articles/persistence3/ says
		// that <void index="4"/> will make up a get()-call. But this is wrong because
		// <void/> tags never return values (to the surrounding context)
		return new IndexContext(id, Integer.parseInt(index));
	      }
	    catch (NumberFormatException nfe)
	      {
		throw new AssemblyException(nfe);
	      }
	  }

	// access a method if methodName exists
	if (methodName != null)
	  return new MethodContext(id, methodName);

	// (falling through is important!)
	// access a property if a propertyName exists
	if (propertyName != null && propertyName.length() > 0)
	  // this is reported as an ordinary method access where the propertyName is
	  // converted into a 'getter'-method name: convert first character of property name
	  // to upper case and prepend 'get'
	  // Note: This will be a getter-method because the <object> tag implies that a return
	  // value is expected.
	  return new PropertyContext(id, propertyName);
      }

    throw new AssemblyException(new IllegalArgumentException("Wrong or missing attributes for <object> tag."));
  }
}
