/* JobStateReason.java --
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

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;


/**
 * The <code>JobStateReason</code> attribute provides additional
 * information about the current state of a job. Its always part
 * of the {@link javax.print.attribute.standard.JobStateReasons}
 * printing attribute. 
 * <p>
 * <b>IPP Compatibility:</b> JobStateReason is not an IPP 1.1 
 * attribute itself but used inside the <code>JobStateReasons</code>
 * attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class JobStateReason extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = -8765894420449009168L;

  /**
   * The printer has accepted the job or is retrieving document
   * data for processing.
   */
  public static final JobStateReason JOB_INCOMING = new JobStateReason(0);
  
  /**
   * The printer is expecting additional document data before it
   * can move this job into the processing state.
   */
  public static final JobStateReason JOB_DATA_INSUFFICIENT = new JobStateReason(1);
  
  /**
   * The printer is unable to access one or more documents provided
   * by reference in the print job.
   */
  public static final JobStateReason DOCUMENT_ACCESS_ERROR = new JobStateReason(2);
  
  /**
   * The printer has not received the whole job submission. This
   * indicates no reason for the interruption.
   */
  public static final JobStateReason SUBMISSION_INTERRUPTED = new JobStateReason(3);
  
  /**
   * The printer transfers the job to the actual output device.
   */
  public static final JobStateReason JOB_OUTGOING = new JobStateReason(4);
  
  /**
   * The job was submitted with a <code>JobHoldUntil</code> attribute which 
   * specifies a time period still in the future and causes the job to 
   * be on hold.
   */
  public static final JobStateReason JOB_HOLD_UNTIL_SPECIFIED = new JobStateReason(5);
  
  /**
   * One or more resources needed by the job are not ready. E.g. needed
   * media type.
   */
  public static final JobStateReason RESOURCES_ARE_NOT_READY = new JobStateReason(6);
  
  /**
   * The printer stopped partly.
   */
  public static final JobStateReason PRINTER_STOPPED_PARTLY = new JobStateReason(7);
  
  /**
   * The printer stopped complete.
   */
  public static final JobStateReason PRINTER_STOPPED = new JobStateReason(8);
  
  /**
   * The printer is currently interpreting the jobs document data.
   * Detailed state of the job's processing state.
   */
  public static final JobStateReason JOB_INTERPRETING = new JobStateReason(9);
  
  /**
   * The printer has queued the document data.
   * Detailed state of the job's processing state.
   */
  public static final JobStateReason JOB_QUEUED = new JobStateReason(10);
  
  /**
   * The printer is transforming the document data to another representation.
   * Detailed state of the job's processing state.
   */
  public static final JobStateReason JOB_TRANSFORMING = new JobStateReason(11);
  
  /**
   * The job is queued for marking.
   */
  public static final JobStateReason JOB_QUEUED_FOR_MARKER = new JobStateReason(12);
  
  /**
   * The job is currently printing.
   */
  public static final JobStateReason JOB_PRINTING = new JobStateReason(13);
  
  /**
   * The job was canceled by the user (the owner of the job).
   */
  public static final JobStateReason JOB_CANCELED_BY_USER = new JobStateReason(14);
  
  /**
   * The job was canceled by the operator.
   */
  public static final JobStateReason JOB_CANCELED_BY_OPERATOR = new JobStateReason(15);
  
  /**
   * The job was canceled by an unidentified local user at the device.
   */
  public static final JobStateReason JOB_CANCELED_AT_DEVICE = new JobStateReason(16);
  
  /**
   * The job has been aborted by the system.
   */
  public static final JobStateReason ABORTED_BY_SYSTEM = new JobStateReason(17);
  
  /**
   * The printer aborted the job because of an unsupported compression while
   * trying to decompress the document data.
   */
  public static final JobStateReason UNSUPPORTED_COMPRESSION = new JobStateReason(18);
  
  /**
   * The printer aborted the job because of a compression error while
   * trying to decompress the document data. If this state is given the test
   * for supported compression has already been passed.
   */
  public static final JobStateReason COMPRESSION_ERROR = new JobStateReason(19);
  
  /**
   * The printer aborted the job because of the document format is not supported.
   * This may happen if a job is specified as application/octet-stream format. 
   */
  public static final JobStateReason UNSUPPORTED_DOCUMENT_FORMAT = new JobStateReason(20);
  
  /**
   * The printer aborted the job because of an error in the document data. 
   */
  public static final JobStateReason DOCUMENT_FORMAT_ERROR = new JobStateReason(21);
  
  /**
   * The client has either canceled the job or the printer aborted the job.
   * However the printer still performs some action on the job e.g. to cleanup. 
   */
  public static final JobStateReason PROCESSING_TO_STOP_POINT = new JobStateReason(22);
  
  /**
   * The printer is offline and therefore is not accepting jobs.
   */
  public static final JobStateReason SERVICE_OFF_LINE = new JobStateReason(23);
  
  /**
   * The printer completed the job successfully. 
   */
  public static final JobStateReason JOB_COMPLETED_SUCCESSFULLY = new JobStateReason(24);
  
  /**
   * The printer completed the job with warnings.
   */
  public static final JobStateReason JOB_COMPLETED_WITH_WARNINGS = new JobStateReason(25);
  
  /**
   * The printer completed the job with errors.
   */
  public static final JobStateReason JOB_COMPLETED_WITH_ERRORS = new JobStateReason(26);
  
  /**
   * The job is retained and is able to be restared. 
   */
  public static final JobStateReason JOB_RESTARTABLE = new JobStateReason(27);
  
  /**
   * The printer has forwarded the job to the actual output device. This device
   * is not capable of reporting the state back so that the job state is set
   * to completed by the printer. 
   */
  public static final JobStateReason QUEUED_IN_DEVICE = new JobStateReason(28);

  private static final String[] stringTable = 
    { "job-incoming", "job-data-insufficient", "document-access-error", 
      "submission-interrupted", "job-outgoing", "job-hold-until-specified",
      "resources-are-not-ready", "printer-stopped-partly", "printer-stopped",
      "job-interpreting", "job-queued", "job-transforming", 
      "job-queued-for-marker", "job-printing", "job-canceled-by-user",
      "job-canceled-by-operator", "job-canceled-at-device", 
      "aborted-by-system", "unsupported-compression", "compression-error",
      "unsupported-document-format", "document-format-error", 
      "processing-to-stop-point", "service-off-line", 
      "job-completed-successfully", "job-completed-with-warnings", 
      "job-completed-with-errors", "job-restartable", "queued-in-device" };
  
  private static final JobStateReason[] enumValueTable = 
    { JOB_INCOMING, JOB_DATA_INSUFFICIENT, DOCUMENT_ACCESS_ERROR, 
      SUBMISSION_INTERRUPTED, JOB_OUTGOING, JOB_HOLD_UNTIL_SPECIFIED, 
      RESOURCES_ARE_NOT_READY, PRINTER_STOPPED_PARTLY, PRINTER_STOPPED,
      JOB_INTERPRETING, JOB_QUEUED, JOB_TRANSFORMING, JOB_QUEUED_FOR_MARKER,
      JOB_PRINTING, JOB_CANCELED_BY_USER, JOB_CANCELED_BY_OPERATOR,
      JOB_CANCELED_AT_DEVICE, ABORTED_BY_SYSTEM, UNSUPPORTED_COMPRESSION,
      COMPRESSION_ERROR, UNSUPPORTED_DOCUMENT_FORMAT, DOCUMENT_FORMAT_ERROR,
      PROCESSING_TO_STOP_POINT, SERVICE_OFF_LINE, JOB_COMPLETED_SUCCESSFULLY,
      JOB_COMPLETED_WITH_WARNINGS, JOB_COMPLETED_WITH_ERRORS, JOB_RESTARTABLE,
      QUEUED_IN_DEVICE };
  
  /**
   * Constructs a <code>JobStateReason</code> object.
   * 
   * @param value the enum value.
   */
  protected JobStateReason(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobStateReason</code> itself.
   */
  public Class getCategory()
  {
    return JobStateReason.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-state-reason".
   */
  public String getName()
  {
    return "job-state-reason";
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
