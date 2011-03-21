/* MultiDoc.java --
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

import java.io.IOException;


/**
 * <code>MultiDoc</code> defines the interface for objects providing multiple
 * documents for use in a print job.
 * <p>
 * Implementations of this interface are used to pass multiple documents, to be
 * printed as one print job, to the <code>MultiDocPrintJob</code> instance.
 * </p><p>
 * There exists no implementation of this interface in the Java Print Service
 * API. Implementors may assume the following usage in print jobs and the needed
 * behaviour for implementations: The print job fetches the single documents via
 * iteration by consecutive calls of the {@link #getDoc()} method to obtain the
 * current document follwing calls of the {@link #next()} method to get the next
 * multidoc object for the next <code>getDoc()</code> method call (if returned
 * multidoc object is not <code>null</code>). The print service will fetch the
 * document object and then retrieve the print data from the document before it
 * proceeds with the next call for the next MultiDoc object in the sequence.
 * </p><p>
 * Implementations of this interface have to be multiple thread-safe.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface MultiDoc
{
  /**
   * Returns the current document.
   *
   * @return The current document.
   *
   * @throws IOException if an error occurs
   */
  Doc getDoc() throws IOException;

  /**
   * Returns the next <code>MultiDoc</code> object that contains the
   * next document for retrieval.
   *
   * @return The next <code>MultiDoc</code> object, or <code>null</code>
   * if no more documents are available.
   *
   * @throws IOException if an error occurs
   */
  MultiDoc next() throws IOException;
}
