/* PrintEvent.java -- 
   Copyright (C) 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package javax.print.event;

import javax.print.DocPrintJob;


/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class PrintJobEvent extends PrintEvent
{
  private static final long serialVersionUID = -1711656903622072997L;

  public static final int DATA_TRANSFER_COMPLETE = 106;
  public static final int JOB_CANCELED = 101;
  public static final int JOB_COMPLETE = 102;
  public static final int JOB_FAILED = 103;
  public static final int NO_MORE_EVENTS = 105;
  public static final int REQUIRES_ATTENTION = 104;
  
  private int reason;
  
  /**
   * Constructs a <code>PrintJobEvent</code> object.
   * 
   * @param source the source generating this event
   * @param reason the reason for this event
   */
  public PrintJobEvent(DocPrintJob source, int reason)
  {
    super(source);
    this.reason = reason;
  }

  /**
   * Returns the reason for this event.
   * 
   * @return the reason
   */
  public int getPrintEventType()
  {
    return reason;
  }

  /**
   * Returns the print job that generated this event.
   * 
   * @return the print job
   */ 
  public DocPrintJob getPrintJob()
  {
    return (DocPrintJob) getSource();
  }
}
