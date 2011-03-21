/* DomDocumentBuilderFactory.java --
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

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSParser;

/**
 * Document builder factory that uses a DOM Level 3 Load &amp; Save
 * implementation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomDocumentBuilderFactory
  extends DocumentBuilderFactory
{

  final DOMImplementation impl;
  final DOMImplementationLS ls;
  private boolean secureProcessing;

  public DomDocumentBuilderFactory()
  {
    try
      {
        DOMImplementationRegistry reg =
          DOMImplementationRegistry.newInstance();
        impl = reg.getDOMImplementation("LS 3.0");
        if (impl == null)
          {
            throw new FactoryConfigurationError("no LS implementations found");
          }
        ls = (DOMImplementationLS) impl;
      }
    catch (Exception e)
      {
        throw new FactoryConfigurationError(e);
      }
  }

  public DocumentBuilder newDocumentBuilder()
    throws ParserConfigurationException
  {
    LSParser parser = null;
    try
      {
        parser = ls.createLSParser(DOMImplementationLS.MODE_ASYNCHRONOUS,
                                   "http://www.w3.org/TR/REC-xml");
      }
    catch (DOMException e)
      {
        if (e.code == DOMException.NOT_SUPPORTED_ERR)
          {
            // Fall back to synchronous parser
            try
              {
                parser = ls.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS,
                                           "http://www.w3.org/TR/REC-xml");
              }
            catch (DOMException e2)
              {
                ParserConfigurationException pce =
                    new ParserConfigurationException();
                pce.initCause(e2);
                throw pce;
              }
          }
        else
          {
            ParserConfigurationException pce =
                new ParserConfigurationException();
            pce.initCause(e);
            throw pce;
          }
      }
    DOMConfiguration config = parser.getDomConfig();
    setParameter(config, "namespaces",
                 isNamespaceAware() ? Boolean.TRUE : Boolean.FALSE);
    setParameter(config, "element-content-whitespace",
                 isIgnoringElementContentWhitespace() ? Boolean.FALSE :
                 Boolean.TRUE);
    setParameter(config, "comments",
                 isIgnoringComments() ? Boolean.FALSE : Boolean.TRUE);
    setParameter(config, "expand-entity-references",
                 isExpandEntityReferences() ? Boolean.TRUE : Boolean.FALSE);
    setParameter(config, "coalescing",
                 isCoalescing() ? Boolean.TRUE : Boolean.FALSE);
    setParameter(config, "validating",
                 isValidating() ? Boolean.TRUE : Boolean.FALSE);
    setParameter(config, "xinclude-aware",
                 isXIncludeAware() ? Boolean.TRUE : Boolean.FALSE);
    return new DomDocumentBuilder(impl, ls, parser);
  }

  void setParameter(DOMConfiguration config, String name, Object value)
    throws ParserConfigurationException
  {
    if (!config.canSetParameter(name, value))
      {
        throw new ParserConfigurationException(name);
      }
    config.setParameter(name, value);
  }

  public Object getAttribute(String name)
  {
    // TODO
    return null;
  }

  public void setAttribute(String name, Object value)
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
