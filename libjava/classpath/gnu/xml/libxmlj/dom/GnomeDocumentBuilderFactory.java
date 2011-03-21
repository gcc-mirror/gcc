/* GnomeDocumentBuilderFactory.java -
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

package gnu.xml.libxmlj.dom;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

/**
 * Factory for JAXP document builders using the libxml2 implementation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class GnomeDocumentBuilderFactory
  extends DocumentBuilderFactory
{

  private boolean secureProcessing;

  public GnomeDocumentBuilderFactory ()
  {
    setNamespaceAware (true);
  }

  public Object getAttribute (String name)
  {
    // TODO
    return null;
  }

  public DocumentBuilder newDocumentBuilder ()
    throws ParserConfigurationException
  {
    /*
    if (!isNamespaceAware ())
      {
        String msg = "Parser must be namespace-aware";
        throw new ParserConfigurationException (msg);
      }
    if (isIgnoringComments ())
      {
        String msg = "Ignoring comments not supported";
        throw new ParserConfigurationException (msg);
      }
    if (isIgnoringElementContentWhitespace ())
      {
        String msg = "Ignoring element content whitespace not supported";
        throw new ParserConfigurationException (msg);
      }
      */
    return new GnomeDocumentBuilder (isValidating (),
                                     isCoalescing (),
                                     isExpandEntityReferences ());
  }

  public void setAttribute (String name, Object value)
  {
    // TODO
  }

  public void setFeature(String name, boolean value)
    throws ParserConfigurationException
  {
    if (name == null)
      throw new NullPointerException();
    if (XMLConstants.FEATURE_SECURE_PROCESSING.equals(name))
      {
        secureProcessing = true;
        return;
      }
    throw new ParserConfigurationException(name);
  }

  public boolean getFeature(String name)
    throws ParserConfigurationException
  {
    if (XMLConstants.FEATURE_SECURE_PROCESSING.equals(name))
      return secureProcessing;
    throw new ParserConfigurationException(name);
  }

}
