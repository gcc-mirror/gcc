/* DomDocumentBuilder.java --
   Copyright (C) 2004,2006,2007 Free Software Foundation, Inc.

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

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import javax.xml.parsers.DocumentBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSException;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSParser;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Document builder using the GNU DOM Load &amp; Save implementation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class DomDocumentBuilder
  extends DocumentBuilder
{

  final DOMImplementation impl;
  final DOMImplementationLS ls;
  final LSParser parser;

  DomDocumentBuilder(DOMImplementation impl,
                     DOMImplementationLS ls,
                     LSParser parser)
  {
    this.impl = impl;
    this.ls = ls;
    this.parser = parser;
  }

  public boolean isNamespaceAware()
  {
    DOMConfiguration config = parser.getDomConfig();
    return ((Boolean) config.getParameter("namespaces")).booleanValue();
  }

  public boolean isValidating()
  {
    DOMConfiguration config = parser.getDomConfig();
    return ((Boolean) config.getParameter("validating")).booleanValue();
  }

  public boolean isXIncludeAware()
  {
    DOMConfiguration config = parser.getDomConfig();
    return ((Boolean) config.getParameter("xinclude-aware")).booleanValue();
  }

  public void setEntityResolver(EntityResolver resolver)
  {
    DOMConfiguration config = parser.getDomConfig();
    config.setParameter("entity-resolver", resolver);
  }

  public void setErrorHandler(ErrorHandler handler)
  {
    DOMConfiguration config = parser.getDomConfig();
    config.setParameter("error-handler", handler);
  }

  public DOMImplementation getDOMImplementation()
  {
    return impl;
  }

  public Document newDocument()
  {
    return impl.createDocument(null, null, null);
  }

  public Document parse(InputStream in)
    throws SAXException, IOException
  {
    LSInput input = ls.createLSInput();
    input.setByteStream(in);
    try
      {
        return parser.parse(input);
      }
    catch (LSException e)
      {
        Throwable e2 = e.getCause();
        if (e2 instanceof IOException)
          throw (IOException) e2;
        else
          throw e;
      }
  }

  public Document parse(InputStream in, String systemId)
    throws SAXException, IOException
  {
    LSInput input = ls.createLSInput();
    input.setByteStream(in);
    input.setSystemId(systemId);
    try
      {
        return parser.parse(input);
      }
    catch (LSException e)
      {
        Throwable e2 = e.getCause();
        if (e2 instanceof IOException)
          throw (IOException) e2;
        else
          throw e;
      }
  }

  public Document parse(String systemId)
    throws SAXException, IOException
  {
    try
      {
        return parser.parseURI(systemId);
      }
    catch (LSException e)
      {
        Throwable e2 = e.getCause();
        if (e2 instanceof IOException)
          throw (IOException) e2;
        else
          throw e;
      }
  }

  public Document parse(InputSource is)
    throws SAXException, IOException
  {
    LSInput input = ls.createLSInput();
    String systemId = is.getSystemId();
    InputStream in = is.getByteStream();
    if (in != null)
      {
        input.setByteStream(in);
      }
    else
      {
        Reader reader = is.getCharacterStream();
        if (reader != null)
          {
            input.setCharacterStream(reader);
          }
        else
          {
            try
              {
                URL url = new URL(systemId);
                input.setByteStream(url.openStream());
              }
            catch (MalformedURLException e)
              {
                // Maybe this is a relative file URL
                File cwd = new File(System.getProperty("user.dir"));
                URL url = new URL(cwd.toURL(), systemId);
                input.setByteStream(url.openStream());
              }
          }
      }
    input.setPublicId(is.getPublicId());
    input.setSystemId(systemId);
    input.setEncoding(is.getEncoding());
    try
      {
        return parser.parse(input);
      }
    catch (LSException e)
      {
        Throwable e2 = e.getCause();
        if (e2 instanceof IOException)
          throw (IOException) e2;
        else
          throw e;
      }
  }

}
