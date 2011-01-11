/* DomDocumentConfiguration.java --
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

package gnu.xml.dom;

import java.util.Arrays;
import java.util.List;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMErrorHandler;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMStringList;

/**
 * Document configuration, used to store normalization and other parameters.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class DomDocumentConfiguration
  implements DOMConfiguration, DOMStringList
{

  private static final List SUPPORTED_PARAMETERS =
    Arrays.asList(new String[] { "cdata-sections",
                  "comments",
                  "element-content-whitespace",
                  "entities",
                  "error-handler",
                  "namespace-declarations",
                  "split-cdata-sections",
                  "infoset"});

  boolean cdataSections = true;
  boolean comments = true;
  boolean elementContentWhitespace = true;
  boolean entities = true;
  DOMErrorHandler errorHandler;
  boolean namespaceDeclarations = true;
  boolean splitCdataSections = true;

  public void setParameter(String name, Object value)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("cdata-sections".equals(name))
      {
        cdataSections = "true".equals(value.toString());
      }
    else if ("comments".equals(name))
      {
        comments = "true".equals(value.toString());
      }
    else if ("element-content-whitespace".equals(name))
      {
        elementContentWhitespace = "true".equals(value.toString());
      }
    else if ("entities".equals(name))
      {
        entities = "true".equals(value.toString());
      }
    else if ("error-handler".equals(name))
      {
        try
          {
            errorHandler = (DOMErrorHandler) value;
          }
        catch (ClassCastException e)
          {
            throw new DomDOMException(DOMException.TYPE_MISMATCH_ERR,
                                      value.getClass().getName(), null, 0);
          }
      }
    else if ("namespace-declarations".equals(name))
      {
        namespaceDeclarations = "true".equals(value.toString());
      }
    else if ("split-cdata-sections".equals(name))
      {
        comments = "true".equals(value.toString());
      }
    else if ("infoset".equals(name))
      {
        if ("true".equals(value.toString()))
          {
            entities = false;
            cdataSections = false;
            namespaceDeclarations = true;
            elementContentWhitespace = true;
            comments = true;
          }
      }
    else if (("canonical-form".equals(name) ||
              "check-character-normalization".equals(name) ||
              "datatype-normalization".equals(name) ||
              "normalize-characters".equals(name) ||
              "validate".equals(name) ||
              "validate-if-schema".equals(name)) &&
             "false".equals(value.toString()))
      {
        // NOOP
      }
    else if (("namespaces".equals(name) ||
              "well-formed".equals(name)) &&
             "true".equals(value.toString()))
      {
        // NOOP
      }
    else
      {
        throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR,
                                  name, null, 0);
      }
  }

  public Object getParameter(String name)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("cdata-sections".equals(name))
      {
        return cdataSections ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("comments".equals(name))
      {
        return comments ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("element-content-whitespace".equals(name))
      {
        return elementContentWhitespace ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("entities".equals(name))
      {
        return entities ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("error-handler".equals(name))
      {
        return errorHandler;
      }
    else if ("namespace-declarations".equals(name))
      {
        return namespaceDeclarations ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("split-cdata-sections".equals(name))
      {
        return comments ? Boolean.TRUE : Boolean.FALSE;
      }
    else if ("canonical-form".equals(name) ||
             "check-character-normalization".equals(name) ||
             "datatype-normalization".equals(name) ||
             "normalize-characters".equals(name) ||
             "validate".equals(name) ||
             "validate-if-schema".equals(name))
      {
        return Boolean.FALSE;
      }
    else if ("namespaces".equals(name) ||
             "well-formed".equals(name))
      {
        return Boolean.TRUE;
      }
    else if ("infoset".equals(name))
      {
        return (entities == false &&
            cdataSections == false &&
            namespaceDeclarations == true &&
            comments == true) ? Boolean.TRUE : Boolean.FALSE;
      }
    throw new DomDOMException(DOMException.NOT_SUPPORTED_ERR, name, null, 0);
  }

  public boolean canSetParameter(String name, Object value)
  {
    name = name.toLowerCase();
    if ("error-handler".equals(name))
      {
        return (value == null || value instanceof DOMErrorHandler);
      }
    else if (contains(name))
      {
        return true;
      }
    else if ("canonical-form".equals(name) ||
             "check-character-normalization".equals(name) ||
             "datatype-normalization".equals(name) ||
             "normalize-characters".equals(name) ||
             "validate".equals(name) ||
             "validate-if-schema".equals(name))
      {
        return "false".equals(value.toString());
      }
    else if ("namespaces".equals(name) ||
             "well-formed".equals(name))
      {
        return "true".equals(value.toString());
      }
    return false;
  }

  public DOMStringList getParameterNames()
  {
    return this;
  }

  public String item(int i)
  {
    try
      {
        return (String) SUPPORTED_PARAMETERS.get(i);
      }
    catch (IndexOutOfBoundsException e)
      {
        return null;
      }
  }

  public int getLength()
  {
    return SUPPORTED_PARAMETERS.size();
  }

  public boolean contains(String str)
  {
    str = str.toLowerCase();
    return SUPPORTED_PARAMETERS.contains(str);
  }

}
