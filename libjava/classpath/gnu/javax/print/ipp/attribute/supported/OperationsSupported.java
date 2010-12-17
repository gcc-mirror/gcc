/* OperationsSupported.java --
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


package gnu.javax.print.ipp.attribute.supported;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;

/**
 * <code>OperationsSupported</code> specifies the enums of the operations
 * supported by a given printer or job object. The attribute is further
 * specified in RFC 2911 section 4.4.15.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class OperationsSupported extends EnumSyntax
  implements SupportedValuesAttribute
{
  /*
   * Value               Operation Name
     -----------------   -------------------------------------
     0x0000              reserved, not used
     0x0001              reserved, not used
     0x0002              Print-Job
     0x0003              Print-URI
     0x0004              Validate-Job
     0x0005              Create-Job
     0x0006              Send-Document
     0x0007              Send-URI
     0x0008              Cancel-Job
     0x0009              Get-Job-Attributes
     0x000A              Get-Jobs
     0x000B              Get-Printer-Attributes
     0x000C              Hold-Job
     0x000D              Release-Job
     0x000E              Restart-Job
     0x000F              reserved for a future operation
     0x0010              Pause-Printer
     0x0011              Resume-Printer
     0x0012              Purge-Jobs
     0x0013-0x3FFF       reserved for future IETF standards track operations
     0x4000-0x8FFF       reserved for vendor extensions
   */

  // standard ipp 1.1 operations

  /**
   * Operation to print a job in one request/response. */
  public static final OperationsSupported PRINT_JOB =
    new OperationsSupported(0x02);

  /** Operation to print a document from an URI */
  public static final OperationsSupported PRINT_URI =
    new OperationsSupported(0x03);

  /** Operation to validate a job before submission. */
  public static final OperationsSupported VALIDATE_JOB =
    new OperationsSupported(0x04);

  /**
   * Operation to create an initial job for use with multiple document per job.
   */
  public static final OperationsSupported CREATE_JOB =
    new OperationsSupported(0x05);

  /**
   * Operation to send a document to a multidoc job created via CREATE_JOB
   */
  public static final OperationsSupported SEND_DOCUMENT =
    new OperationsSupported(0x06);

  /**
   * Operation to send a document uri to a multidoc job created
   * via CREATE_JOB. The document accessible from this URI will be printed.
   */
  public static final OperationsSupported SEND_URI =
    new OperationsSupported(0x07);

  /** Operation to cancel a job by its ID or name.  */
  public static final OperationsSupported CANCEL_JOB =
    new OperationsSupported(0x08);

  /** Operation to get job attributes of a current job. */
  public static final OperationsSupported GET_JOB_ATTRIBUTES =
    new OperationsSupported(0x09);

  /** Operation to pause a printer. */
  public static final OperationsSupported PAUSE_PRINTER =
    new OperationsSupported(0x10);

  /** Operation to get all currently queued or processed jobs. */
  public static final OperationsSupported GET_JOBS =
    new OperationsSupported(0x0A);

  /** Operation to get the attributes of a printer. */
  public static final OperationsSupported GET_PRINTER_ATTRIBUTES =
    new OperationsSupported(0x0B);

  /** Operation to put a job on hold by its ID or name. */
  public static final OperationsSupported HOLD_JOB =
    new OperationsSupported(0x0C);

  /** Operation to release a job by its ID or name. */
  public static final OperationsSupported RELEASE_JOB =
    new OperationsSupported(0x0D);

  /** Operation to restart a job by its ID or name. */
  public static final OperationsSupported RESTART_JOB =
    new OperationsSupported(0x0E);

  /** Not yet an operation - reserved for futher use. */
  public static final OperationsSupported RESERVED =
    new OperationsSupported(0x0F);

  /** Operation to resume a printer. */
  public static final OperationsSupported RESUME_PRINTER =
    new OperationsSupported(0x11);

  /** Operation to remove all jobs from a printer regardless of state. */
  public static final OperationsSupported PURGE_JOBS =
    new OperationsSupported(0x12);


  private static final String[] stringTable = { "print-job", "print-uri",
                                                "validate-job", "create-job",
                                                "send-document", "send-uri",
                                                "cancel-job", "get-job-attributes",
                                                "pause-printer", "get-jobs",
                                                "get-printer-attributes", "hold-job",
                                                "release-job", "restart-job", "reserved",
                                                "resume-printer", "purge-job"};

  private static final OperationsSupported[] enumValueTable =
    { PRINT_JOB, PRINT_URI, VALIDATE_JOB, CREATE_JOB, SEND_DOCUMENT, SEND_URI,
      CANCEL_JOB, GET_JOB_ATTRIBUTES, PAUSE_PRINTER, GET_JOBS, GET_PRINTER_ATTRIBUTES,
      HOLD_JOB, RELEASE_JOB, RESTART_JOB, RESERVED, RESUME_PRINTER, PURGE_JOBS};


  /**
   * Constructs a <code>OperationsSupported</code> object.
   *
   * @param value the enum value
   */
  protected OperationsSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>OperationsSupported</code> itself.
   */
  public Class<? extends Attribute> getCategory()
  {
    return OperationsSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "operations-supported".
   */
  public String getName()
  {
    return "operations-supported";
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

  // we start with 2
  protected int getOffset()
  {
    return 2;
  }
}
