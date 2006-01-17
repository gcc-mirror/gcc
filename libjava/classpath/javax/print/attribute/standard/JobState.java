/* JobState.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;


/**
 * The <code>JobState</code> printing attribute reports
 * the current state of a job.
 * <p>
 * The {@link javax.print.attribute.standard.JobStateReasons}
 * attribute provides further detailed information about
 * the given job state. Detailed information about the job
 * state and job state reasons can be found in the RFC 2911.
 * </p> 
 * <p>
 * <b>IPP Compatibility:</b> JobState is an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class JobState extends EnumSyntax
  implements PrintJobAttribute
{
  private static final long serialVersionUID = 400465010094018920L;

  /**
   * The job state is currently unknown.
   */
  public static final JobState UNKNOWN = new JobState(0);
  
  /**
   * The job is pending processing.
   */
  public static final JobState PENDING = new JobState(3);
  
  /**
   * The job is currently not a candidate for printing because
   * of reasons reported by the job-state-reasons attribute. If
   * the reasons are no longer present it will return to the
   * pending state.
   */
  public static final JobState PENDING_HELD = new JobState(4);
  
  /**
   * The job is currently processed.
   */
  public static final JobState PROCESSING = new JobState(5);
  
  /**
   * The job's processing has stopped. The job-state-reasons
   * attribute may indicate the reason(s). The job will return
   * to the processing state if the reasons are no longer present.
   */
  public static final JobState PROCESSING_STOPPED = new JobState(6);
  
  /**
   * The job has been canceled by the client.
   */
  public static final JobState CANCELED = new JobState(7);
  
  /**
   * The job has been aborted by the system.
   */
  public static final JobState ABORTED = new JobState(8);
  
  /**
   * The job has completed successfully.
   */
  public static final JobState COMPLETED = new JobState(9);


  private static final String[] stringTable = { "unknown", null, null, 
                                                "pending", "pending-held",
                                                "processing", 
                                                "processing-stopped",
                                                "canceled", "aborted", 
                                                "completed"};
  
  private static final JobState[] enumValueTable = { UNKNOWN, null, null,
                                                     PENDING, PENDING_HELD,
                                                     PROCESSING,
                                                     PROCESSING_STOPPED,
                                                     CANCELED, ABORTED,
                                                     COMPLETED };
  
  /**
   * Constructs a <code>JobState</code> object.
   * 
   * @param value the enum value.
   */
  protected JobState(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobState</code> itself.
   */
  public Class getCategory()
  {
    return JobState.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-state".
   */
  public String getName()
  {
    return "job-state";
  }
  
  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}
