/* XMLInputFactoryImpl.java -- 
   Copyright (C) 2005,2006  Free Software Foundation, Inc.

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

package gnu.xml.stream;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.stream.EventFilter;
import javax.xml.stream.StreamFilter;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLReporter;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.util.XMLEventAllocator;

/**
 * Factory for creating parsers from various kinds of XML source.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLInputFactoryImpl
  extends XMLInputFactory
{

  protected XMLResolver resolver;
  protected XMLReporter reporter;
  protected XMLEventAllocator allocator;

  protected boolean validating;
  protected boolean namespaceAware = true;
  protected boolean coalescing;
  protected boolean replacingEntityReferences = true;
  protected boolean externalEntities = true;
  protected boolean supportDTD = true;
  protected boolean xIncludeAware = false;
  protected boolean baseAware = true;
  protected boolean stringInterning = true;

  public XMLInputFactoryImpl()
  {
    allocator = new XMLEventAllocatorImpl();
  }

  public XMLStreamReader createXMLStreamReader(Reader reader)
    throws XMLStreamException
  {
    return createXMLStreamReader(null, reader);
  }
  
  public XMLStreamReader createXMLStreamReader(Source source)
    throws XMLStreamException
  {
    String systemId = source.getSystemId();
    InputStream in = getInputStream(source);
    XMLParser ret = new XMLParser(in, systemId,
                                  validating,
                                  namespaceAware,
                                  coalescing,
                                  replacingEntityReferences,
                                  externalEntities,
                                  supportDTD,
                                  baseAware,
                                  stringInterning,
                                  false,
                                  reporter,
                                  resolver);
    if (xIncludeAware)
      return new XIncludeFilter(ret, systemId, namespaceAware, validating,
                                replacingEntityReferences);
    return ret;
  }
  
  public XMLStreamReader createXMLStreamReader(InputStream in)
    throws XMLStreamException
  {
    return createXMLStreamReader(null, in);
  }
  
  public XMLStreamReader createXMLStreamReader(InputStream in, String encoding)
    throws XMLStreamException
  {
    return createXMLStreamReader(in);
  }

  public XMLStreamReader createXMLStreamReader(String systemId, InputStream in)
    throws XMLStreamException
  {
    XMLParser ret = new XMLParser(in, systemId,
                                  validating,
                                  namespaceAware,
                                  coalescing,
                                  replacingEntityReferences,
                                  externalEntities,
                                  supportDTD,
                                  baseAware,
                                  stringInterning,
                                  false,
                                  reporter,
                                  resolver);
    if (xIncludeAware)
      return new XIncludeFilter(ret, null, namespaceAware, validating,
                                replacingEntityReferences);
    return ret;
  }

  public XMLStreamReader createXMLStreamReader(String systemId, Reader reader)
    throws XMLStreamException
  {
    XMLParser ret = new XMLParser(reader, systemId,
                                  validating,
                                  namespaceAware,
                                  coalescing,
                                  replacingEntityReferences,
                                  externalEntities,
                                  supportDTD,
                                  baseAware,
                                  stringInterning,
                                  false,
                                  reporter,
                                  resolver);
    if (xIncludeAware)
      return new XIncludeFilter(ret, null, namespaceAware, validating,
                                replacingEntityReferences);
    return ret;
  }

  public XMLEventReader createXMLEventReader(Reader reader)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(reader);
    return new XMLEventReaderImpl(sr, allocator, null);
  }
  
  public XMLEventReader createXMLEventReader(String systemId, Reader reader)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(systemId, reader);
    return new XMLEventReaderImpl(sr, allocator, null);
  }
  
  public XMLEventReader createXMLEventReader(XMLStreamReader reader)
    throws XMLStreamException
  {
    return new XMLEventReaderImpl(reader, allocator, null);
  }
  
  public XMLEventReader createXMLEventReader(Source source)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(source);
    return new XMLEventReaderImpl(sr, allocator, null);
  }
  
  public XMLEventReader createXMLEventReader(InputStream in)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(in);
    return new XMLEventReaderImpl(sr, allocator, null);
  }
  
  public XMLEventReader createXMLEventReader(InputStream in, String encoding)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(in, encoding);
    return new XMLEventReaderImpl(sr, allocator, null);
  }

  public XMLEventReader createXMLEventReader(String systemId, InputStream in)
    throws XMLStreamException
  {
    XMLStreamReader sr = createXMLStreamReader(systemId, in);
    return new XMLEventReaderImpl(sr, allocator, null);
  }
  
  public XMLStreamReader createFilteredReader(XMLStreamReader reader,
                                              StreamFilter filter)
    throws XMLStreamException
  {
    return new FilteredStreamReader(reader, filter);
  }

  public XMLEventReader createFilteredReader(XMLEventReader reader,
                                             EventFilter filter)
    throws XMLStreamException
  {
    return new FilteredEventReader(reader, filter);
  }

  public XMLResolver getXMLResolver()
  {
    return resolver;
  }

  public void setXMLResolver(XMLResolver resolver)
  {
    this.resolver = resolver;
  }
  
  public XMLReporter getXMLReporter()
  {
    return reporter;
  }

  public void setXMLReporter(XMLReporter reporter)
  {
    this.reporter = reporter;
  }

  public void setProperty(String name, Object value)
    throws IllegalArgumentException
  {
    if (name.equals(IS_NAMESPACE_AWARE))
      namespaceAware = ((Boolean) value).booleanValue();
    else if (name.equals(IS_VALIDATING))
      validating = ((Boolean) value).booleanValue();
    else if (name.equals(IS_COALESCING))
      coalescing = ((Boolean) value).booleanValue();
    else if (name.equals(IS_REPLACING_ENTITY_REFERENCES))
      replacingEntityReferences = ((Boolean) value).booleanValue();
    else if (name.equals(IS_SUPPORTING_EXTERNAL_ENTITIES))
      externalEntities = ((Boolean) value).booleanValue();
    else if (name.equals(SUPPORT_DTD))
      supportDTD = ((Boolean) value).booleanValue();
    else if (name.equals(REPORTER))
      reporter = (XMLReporter) value;
    else if (name.equals(RESOLVER))
      resolver = (XMLResolver) value;
    else if (name.equals(ALLOCATOR))
      allocator = (XMLEventAllocator) value;
    else if (name.equals("gnu.xml.stream.stringInterning"))
      stringInterning = ((Boolean) value).booleanValue();
    else if (name.equals("gnu.xml.stream.baseAware"))
      baseAware = ((Boolean) value).booleanValue();
    else if (name.equals("gnu.xml.stream.xIncludeAware"))
      xIncludeAware = ((Boolean) value).booleanValue();
    else
      throw new IllegalArgumentException(name);
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    if (name.equals(IS_NAMESPACE_AWARE))
      return namespaceAware ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(IS_VALIDATING))
      return validating ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(IS_COALESCING))
      return coalescing ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(IS_REPLACING_ENTITY_REFERENCES))
      return replacingEntityReferences ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(IS_SUPPORTING_EXTERNAL_ENTITIES))
      return externalEntities ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(SUPPORT_DTD))
      return supportDTD ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals(REPORTER))
      return reporter;
    if (name.equals(RESOLVER))
      return resolver;
    if (name.equals(ALLOCATOR))
      return allocator;
    if (name.equals("gnu.xml.stream.stringInterning"))
      return stringInterning ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals("gnu.xml.stream.baseAware"))
      return baseAware ? Boolean.TRUE : Boolean.FALSE;
    if (name.equals("gnu.xml.stream.xIncludeAware"))
      return xIncludeAware ? Boolean.TRUE : Boolean.FALSE;
    throw new IllegalArgumentException(name);
  }

  public boolean isPropertySupported(String name)
  {
    return name.equals(IS_NAMESPACE_AWARE) ||
      name.equals(IS_VALIDATING) ||
      name.equals(IS_COALESCING) ||
      name.equals(IS_REPLACING_ENTITY_REFERENCES) ||
      name.equals(IS_SUPPORTING_EXTERNAL_ENTITIES) ||
      name.equals(SUPPORT_DTD) ||
      name.equals(REPORTER) ||
      name.equals(RESOLVER) ||
      name.equals(ALLOCATOR) ||
      name.equals("gnu.xml.stream.stringInterning") ||
      name.equals("gnu.xml.stream.baseAware") ||
      name.equals("gnu.xml.stream.xIncludeAware");
  }
  
  public void setEventAllocator(XMLEventAllocator allocator)
  {
    this.allocator = allocator;
  }

  public XMLEventAllocator getEventAllocator()
  {
    return allocator;
  }

  public void setCoalescing(boolean coalescing)
  {
    this.coalescing = coalescing;
  }

  public boolean isCoalescing()
  {
    return coalescing;
  }

  protected InputStream getInputStream(Source source)
    throws XMLStreamException
  {
    InputStream in = null;
    if (source instanceof StreamSource)
      {
        StreamSource streamSource = (StreamSource) source;
        in = streamSource.getInputStream();
      }
    if (in == null)
      {
        String systemId = source.getSystemId();
        try
          {
            URL url = new URL(systemId);
            try
              {
                in = url.openStream();
              }
            catch (IOException e2)
              {
                XMLStreamException e3 = new XMLStreamException(e2);
                e3.initCause(e2);
                throw e3;
              }
          }
        catch (MalformedURLException e)
          {
            // Fall back to relative file
            if (File.separatorChar != '/')
              systemId = systemId.replace('/', File.separatorChar);
            try
              {
                in = new FileInputStream(systemId);
              }
            catch (FileNotFoundException e2)
              {
                XMLStreamException e3 = new XMLStreamException(e2);
                e3.initCause(e2);
                throw e3;
              }
          }
      }
    return in;
  }
  
}

