/* StreamPrintServiceFactory.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.print;

import gnu.classpath.ServiceFactory;

import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

/**
 * <code>StreamPrintServiceFactory</code> provides a static method to lookup
 * registered factories to construct <code>StreamPrintService</code> instances.
 * <p>
 * <code>StreamPrintService</code> are used to print into a provided output
 * stream in the document format provided by the stream print service
 * implementation.
 * </p><p>
 * Implementations are located and loaded automatically through the SPI JAR
 * file specification. Therefore implementation classes must provide a default
 * constructor for instantiation.
 * </p>
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public abstract class StreamPrintServiceFactory
{
  /**
   * Default public constructor.
   * Used for automatic loading and instantiation through
   * the SPI jar file specification.
   */
  public StreamPrintServiceFactory()
  {
    // nothing to do
  }

  /**
   * Searches for matching factories providing stream print services that
   * support the printing of documents with the given document flavor into
   * the given output mime type.
   *
   * @param flavor the document flavor needed, <code>null</code> doesn't
   * constrain the lookup result.
   * @param outputMimeType the mime type needed, <code>null</code> doesn't
   * constrain the lookup result.
   *
   * @return The matching <code>StreamPrintServiceFactory</code> instances.
   */
  public static StreamPrintServiceFactory[] lookupStreamPrintServiceFactories(
    DocFlavor flavor, String outputMimeType)
  {
    HashSet set = new HashSet();

    Iterator it =
      ServiceFactory.lookupProviders(StreamPrintServiceFactory.class);

    while (it.hasNext())
      {
        StreamPrintServiceFactory tmp = (StreamPrintServiceFactory) it.next();
        if (tmp.getOutputFormat().equals(outputMimeType)
            && Arrays.asList(tmp.getSupportedDocFlavors()).contains(flavor))
          set.add(tmp);
      }

    StreamPrintServiceFactory[] tmp = new StreamPrintServiceFactory[set.size()];
    return (StreamPrintServiceFactory[]) set.toArray(tmp);
  }

  /**
   * Returns the output format supported by this factory.
   *
   * @return The mime type of the output format as string representation.
   */
  public abstract String getOutputFormat();

  /**
   * Returns the document flavors this factory supports as flavors
   * for the input documents.
   *
   * @return The array of supported document flavors.
   */
  public abstract DocFlavor[] getSupportedDocFlavors();

  /**
   * Constructs a <code>StreamPrintService</code> which directs its output
   * the given output stream.
   *
   * @param out the output stream for the produced document.
   * @return The constructed stream print service.
   */
  public abstract StreamPrintService getPrintService(OutputStream out);
}
