/* XMLEventWriter.java --
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

package javax.xml.stream;

import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.events.XMLEvent;
import javax.xml.stream.util.XMLEventConsumer;

/**
 * Interface for writing XML documents from a series of events.
 */
public interface XMLEventWriter
  extends XMLEventConsumer
{

  /**
   * Ensures that any cached events are written to the underlying output
   * sink.
   */
  void flush()
    throws XMLStreamException;

  /**
   * Frees any resources used by this writer.
   */
  void close()
    throws XMLStreamException;

  /**
   * Adds the specified event to this writer.
   */
  void add(XMLEvent event)
    throws XMLStreamException;

  /**
   * Adds the specified XML stream to this writer.
   * The implementation will call <code>next</code> on the given argument
   * while <code>hasNext</code> returns true.
   */
  void add(XMLEventReader reader)
    throws XMLStreamException;

  /**
   * Returns the namespace prefix the specified URI is currently associated
   * with.
   */
  String getPrefix(String uri)
    throws XMLStreamException;

  /**
   * Associates the given namespace prefix and URI.
   */
  void setPrefix(String prefix, String uri)
    throws XMLStreamException;

  /**
   * Sets the current default namespace URI.
   */
  void setDefaultNamespace(String uri)
    throws XMLStreamException;

  /**
   * Sets the namespace context for managing namespace prefixes and URIs.
   */
  void setNamespaceContext(NamespaceContext context)
    throws XMLStreamException;

  /**
   * Returns the namespace context.
   */
  NamespaceContext getNamespaceContext();

}
