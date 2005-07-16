/* DocPrintJob.java --
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


package javax.print;

import javax.print.attribute.PrintJobAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.event.PrintJobAttributeListener;
import javax.print.event.PrintJobListener;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface DocPrintJob
{
  /**
   * Registers a listener for changes in the specified attributes.
   * 
   * @param listener the listener to add
   * @param attributes the attributes to observe
   */
  void addPrintJobAttributeListener(PrintJobAttributeListener listener,
				    PrintJobAttributeSet attributes);

  /**
   * Registers a listener for events occuring during this print job.
   * 
   * @param listener the listener to add
   */
  void addPrintJobListener(PrintJobListener listener);

  /**
   * Returns the print job's attributes.
   * 
   * @return the attributes of this print job
   */
  PrintJobAttributeSet getAttributes();

  /**
   * Returns the <code>PrintService</code> object this print job is bound to.
   * 
   * @return the print service
   */
  PrintService getPrintService();

  /**
   * Prints a document with the specified print job attributes.
   * 
   * @param doc the document to print
   * @param attributes the attributes to use
   * 
   * @throws PrintException if an error occurs
   */
  void print(Doc doc, PrintRequestAttributeSet attributes) throws PrintException;

  /**
   * De-registers an attribute listener.
   * 
   * @param listener the listener to remove
   */
  void removePrintJobAttributeListener(PrintJobAttributeListener listener);

  /**
   * De-registers a print job listener.
   * 
   * @param listener the listener to remove
   */
  void removePrintJobListener(PrintJobListener listener);
}