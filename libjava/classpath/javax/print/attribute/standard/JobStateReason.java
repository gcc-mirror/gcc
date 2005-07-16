/* JobStateReason.java --
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

package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;


/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class JobStateReason extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = -8765894420449009168L;

  public static final JobStateReason JOB_INCOMING = new JobStateReason(0);
  public static final JobStateReason JOB_DATA_INSUFFICIENT = new JobStateReason(1);
  public static final JobStateReason DOCUMENT_ACCESS_ERROR = new JobStateReason(2);
  public static final JobStateReason SUBMISSION_INTERRUPTED = new JobStateReason(3);
  public static final JobStateReason JOB_OUTGOING = new JobStateReason(4);
  public static final JobStateReason JOB_HOLD_UNTIL_SPECIFIED = new JobStateReason(5);
  public static final JobStateReason RESOURCES_ARE_NOT_READY = new JobStateReason(6);
  public static final JobStateReason PRINTER_STOPPED_PARTLY = new JobStateReason(7);
  public static final JobStateReason PRINTER_STOPPED = new JobStateReason(8);
  public static final JobStateReason JOB_INTERPRETING = new JobStateReason(9);
  public static final JobStateReason JOB_QUEUED = new JobStateReason(10);
  public static final JobStateReason JOB_TRANSFORMING = new JobStateReason(11);
  public static final JobStateReason JOB_QUEUED_FOR_MARKER = new JobStateReason(12);
  public static final JobStateReason JOB_PRINTING = new JobStateReason(13);
  public static final JobStateReason JOB_CANCELED_BY_USER = new JobStateReason(14);
  public static final JobStateReason JOB_CANCELED_BY_OPERATOR = new JobStateReason(15);
  public static final JobStateReason JOB_CANCELED_AT_DEVICE = new JobStateReason(16);
  public static final JobStateReason ABORTED_BY_SYSTEM = new JobStateReason(17);
  public static final JobStateReason UNSUPPORTED_COMPRESSION = new JobStateReason(18);
  public static final JobStateReason COMPRESSION_ERROR = new JobStateReason(19);
  public static final JobStateReason UNSUPPORTED_DOCUMENT_FORMAT = new JobStateReason(20);
  public static final JobStateReason DOCUMENT_FORMAT_ERROR = new JobStateReason(21);
  public static final JobStateReason PROCESSING_TO_STOP_POINT = new JobStateReason(22);
  public static final JobStateReason SERVICE_OFF_LINE = new JobStateReason(23);
  public static final JobStateReason JOB_COMPLETED_SUCCESSFULLY = new JobStateReason(24);
  public static final JobStateReason JOB_COMPLETED_WITH_WARNINGS = new JobStateReason(25);
  public static final JobStateReason JOB_COMPLETED_WITH_ERRORS = new JobStateReason(26);
  public static final JobStateReason JOB_RESTARTABLE = new JobStateReason(27);
  public static final JobStateReason QUEUED_IN_DEVICE = new JobStateReason(28);

  /**
   * Constructs a <code>JobStateReason</code> object.
   */
  protected JobStateReason(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>JobStateReason</code> itself
   */
  public Class getCategory()
  {
    return JobStateReason.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return the name
   */
  public String getName()
  {
    return "job-state-reason";
  }
}
