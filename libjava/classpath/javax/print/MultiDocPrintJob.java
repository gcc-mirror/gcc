/* MultiDocPrintJob.java --
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

import javax.print.attribute.PrintRequestAttributeSet;


/**
 * <code>MultiDocPrintJob</code> represents a print job which supports
 * printing of multiple documents as one print job.
 * <p>
 * An instance can be obtained from every <code>MultiDocPrintService</code>
 * available by calling the
 * {@link javax.print.MultiDocPrintService#createMultiDocPrintJob()} method.
 * A print job is bound to the print service it is created from.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface MultiDocPrintJob extends DocPrintJob
{
  /**
   * Prints the documents supplied in the given <code>MultiDoc</code> object
   * as one print job with the given printing attributes.
   *
   * @param multiDoc the documents to print. Every document must have a
   * flavor supported by the bound print service.
   * @param attributes the printing attributes to apply to the print job. If
   * <code>null</code> the default attribute values will be used.
   *
   * @throws PrintException if an error occurs. The thrown exception may
   * implement refining print exception interface to provide more detail of
   * the error.
   *
   * @see FlavorException
   * @see AttributeException
   */
  void print(MultiDoc multiDoc, PrintRequestAttributeSet attributes)
    throws PrintException;
}
