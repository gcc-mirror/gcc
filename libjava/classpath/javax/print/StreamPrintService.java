/* StreamPrintService.java --
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

import java.io.OutputStream;


/**
 * <code>StreamPrintService</code> is a special print service capable of
 * printing into a supplied output stream.
 * <p>
 * Beside providing the same functionality as a print service it additionally
 * allows to specify the output stream for the print data. A stream print 
 * service is obtained via the {@link javax.print.StreamPrintServiceFactory} 
 * by looking for services supporting a given output format type.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class StreamPrintService implements PrintService
{
  private boolean disposed;
  private OutputStream out;
  
  /**
   * Constructs a <code>StreamPrintService</code> object.
   * 
   * @param out the <code>OutputStream</code> to use
   */
  protected StreamPrintService(OutputStream out)
  {
    this.out = out;
  }

  /**
   * Dispose this <code>StreamPrintService</code> object.
   */
  public void dispose()
  {
    disposed = true;
  }

  /**
   * Returns the document format emitted by this print service.
   * The returned string is a MIME type compatible with the 
   * {@link DocFlavor} class.
   * 
   * @return The document format of the output.
   */
  public abstract String getOutputFormat();

  /**
   * Returns the <code>OutputStream</code> of this object.
   * 
   * @return The <code>OutputStream</code>
   */
  public OutputStream getOutputStream()
  {
    return out;
  }

  /**
   * Determines if this <code>StreamPrintService</code> object is disposed.
   * 
   * @return <code>true</code> if disposed already,
   * otherwise <code>false</code>
   */
  public boolean isDisposed()
  {
    return disposed;
  }
}