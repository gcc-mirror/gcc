/* gnu.java.beans.decoder.ArrayHandler
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
import java.util.HashMap;

import org.xml.sax.Attributes;

/** ArrayHandler processes the &lt;array&gt; tag. Depending on the existance of the 'length' attribute a Context for
 * a fixed-size or growable array is created.
 *
 * @author Robert Schuster
 */
class ArrayHandler extends AbstractElementHandler
{
  /** Contains a mapping between a textual description of a primitive type (like "byte") and
   * its corresponding wrapper class. This allows it to easily construct Array objects for
   * primitive data types.
   */
  private static HashMap typeMap = new HashMap();

  static
    {
      typeMap.put("byte", Byte.TYPE);
      typeMap.put("short", Short.TYPE);
      typeMap.put("int", Integer.TYPE);
      typeMap.put("long", Long.TYPE);

      typeMap.put("float", Float.TYPE);
      typeMap.put("double", Double.TYPE);

      typeMap.put("boolean", Boolean.TYPE);

      typeMap.put("char", Character.TYPE);
    }

  /**
   * @param PersistenceParser
   */
  ArrayHandler(ElementHandler parent)
  {
    super(parent, true);
  }

  protected Context startElement(Attributes attributes, ExceptionListener exceptionListener)
    throws AssemblyException, AssemblyException
  {
    String id = attributes.getValue("id");
    String className = attributes.getValue("class");

    if (className != null)
      {
	try
	  {
	    Class klass;

	    if (typeMap.containsKey(className))
	      klass = (Class) typeMap.get(className);
	    else
	      klass = instantiateClass(className);

	    String length = attributes.getValue("length");
	    if (length != null)
	      // creates Array with predefined length
	      return new ArrayContext(id, klass, Integer.parseInt(length));
	    else
	      // creates Array without length restriction
	      return new GrowableArrayContext(id, klass);
	  }
	catch (ClassNotFoundException cnfe)
	  {
	    throw new AssemblyException(cnfe);
	  }
	catch (NumberFormatException nfe)
	  {
	    throw new AssemblyException(nfe);
	  }
      }

    throw new AssemblyException(new IllegalArgumentException("Missing 'class' attribute in <array> tag."));
  }
}
