/* gnu.java.beans.decoder.VoidHandler
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

public class VoidHandler extends AbstractElementHandler
{
    /**
     * @param PersistenceParser
     */
    VoidHandler(ElementHandler parent)
    {
        super(parent, true);
    }

    protected Context startElement(
        Attributes attributes,
        ExceptionListener exceptionListener)
        throws AssemblyException
    {
        Context ctx = startElementImpl(attributes);
        ctx.setStatement(true);

        return ctx;
    }

    private Context startElementImpl(Attributes attributes)
        throws AssemblyException
    {
        String id = attributes.getValue("id");
        String className = attributes.getValue("class");
        String methodName = attributes.getValue("method");
        String propertyName = attributes.getValue("property");
        String index = attributes.getValue("index");

        if (className != null)
        {
            try
            {
                Class klass = instantiateClass(className);

                // class name exists which means that we are in a static context.
                // so we may want to ...
                // run a constructor if methodName is "new" or null
                if (methodName == null || methodName.equals("new"))
                    // if the id is null the result cannot be by the decoder accessed but the
                    // constructor may have side effects (e.g. registering itself in a global registry)
                    return new ConstructorContext(id, klass);

                // (falling through is important!)
                // run a static method on the given class (if methodName exists, which is implied already) 
                return new StaticMethodContext(id, klass, methodName);
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
            // access an element by index
            if (index != null)
            {
                // note: whether this resolves into get(i) or set(i, o) depends on the
                // number of arguments and is decided by the ObjectAssembler
                try
                {
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
                // this is reported as an ordinary method invocation where the propertyName is
                // converted into a 'setter'-method name: convert first character of property name
                // to upper case and prepend 'set'
                // Note: This will be a setter-method because the <void> tag implies that no return
                // value is expected (but a side effect)
                return new PropertyContext(id, propertyName);
        }

        // if code reaches this point the tag has wrong attributes. The following test
        // does not make it better but can provide are more specific error message for
        // a common mistake: <void> tags are not allowed to have an idref attribute
        throw new AssemblyException(
            new IllegalArgumentException(
                (attributes.getValue("idref") == null)
                    ? "Missing attributes for <void> tag"
                    : "<void> does not support 'idref' attribute."));
    }
}
