/* DocPrintJob.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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

import javax.print.attribute.PrintJobAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.event.PrintJobAttributeListener;
import javax.print.event.PrintJobListener;

/**
 * <code>DocPrintJob</code> represents a print job which supports printing
 * of a single document.
 * <p>
 * An instance can be obtained from every <code>PrintService</code> available
 * by calling the {@link javax.print.PrintService#createPrintJob()} method.
 * A print job is bound to the print service it is created from.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface DocPrintJob
{
  /**
   * Registers a listener for changes in the specified attribute set
   * during processing of this print job.
   * <p>
   * If the given attribute set is empty no changes will be reported.
   * If the set is <code>null</code> all attributes are monitored.
   * </p>
   *
   * @param listener the listener to register.
   * @param attributes the attributes to observe.
   *
   * @see #removePrintJobAttributeListener(PrintJobAttributeListener)
   */
  void addPrintJobAttributeListener(PrintJobAttributeListener listener,
                                    PrintJobAttributeSet attributes);

  /**
   * Registers a listener for events occuring during processing
   * of this print job.
   *
   * @param listener the listener to add, if <code>null</code> nothing is done.
   *
   * @see #removePrintJobListener(PrintJobListener)
   */
  void addPrintJobListener(PrintJobListener listener);

  /**
   * Returns the print job's attributes.
   * <p>
   * The returned set of attributes is a snapshot at the time of calling this
   * method and will not be updated if changes to the print job's attributes
   * happens. To monitor changes register a print job listener.
   * </p>
   *
   * @return The attributes of this print job,
   * may be empty but never <code>null</code>.
   */
  PrintJobAttributeSet getAttributes();

  /**
   * Returns the <code>PrintService</code> object this print job is bound to.
   *
   * @return The print service.
   */
  PrintService getPrintService();

  /**
   * Prints a document with the specified print job attributes.
   *
   * <p>
   * If the doc flavor provided by the <code>Doc</code> implementation is
   * not supported by this print service a <code>PrintException</code>
   * implementing the <code>FlavorException</code> interface will be thrown.
   * </p>
   *
   * @param doc the document to print
   * @param attributes the job attributes to use. If <code>null</code> the
   * default attribute values of the print service will be used.
   *
   * @throws PrintException if an error occurs. The thrown exception may
   * implement refining print exception interface to provide more detail of
   * the error.
   *
   * @see AttributeException
   * @see FlavorException
   */
  void print(Doc doc, PrintRequestAttributeSet attributes) throws PrintException;

  /**
   * Removes the given listener from the listeners registered for changes
   * in their provided attribute set during processing of this print job.
   *
   * @param listener the listener to remove, if <code>null</code> or not
   * registered nothing will be done.
   *
   * @see #addPrintJobAttributeListener(PrintJobAttributeListener, PrintJobAttributeSet)
   */
  void removePrintJobAttributeListener(PrintJobAttributeListener listener);

  /**
   * Removes the given listener from the listeners registered for events
   * occuring during processing of this print job.
   *
   * @param listener the listener to remove, if <code>null</code> or not
   * registered nothing will be done.
   *
   * @see #addPrintJobListener(PrintJobListener)
   */
  void removePrintJobListener(PrintJobListener listener);
}
