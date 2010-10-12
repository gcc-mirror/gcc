/* IppStatusCode.java --
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


package gnu.javax.print.ipp;

/**
 * IPP Status codes as described in RFC 2911 APPENDIX B
 * (Status Codes and Suggested Status Code Messages)
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class IppStatusCode
{
  /**
   * Indicates a successful request with no attributes being
   * ignored or substituted.
   */
  public static final int SUCCESSFUL_OK = 0x0000;

  /**
   * Indicates a successful request, however some of the supplied
   * attributes are ignored or substituted.
   */
  public static final int SUCCESSFUL_OK_IGNORED_OR_SUBSTITUED_ATTRIBUTES = 0x0001;

  /**
   * Indicates a successful request, however some of the supplied
   * attributes conflicted and therefore were ignored or substituted.
   */
  public static final int SUCCESSFUL_OK_CONFLICTING_ATTRIBUTES = 0x0002;

  // Client Error Status Codes
  // Indicates that the client has done something wrong in its
  // requests send to the IPP server object

  /** Indicates a bad request e.g. malformed syntax. */
  public static final int CLIENT_ERROR_BAD_REQUEST = 0x0400;

  /** Indicates that the client is forbidden to access the server. */
  public static final int CLIENT_ERROR_FORBIDDEN = 0x0401;

  /** Indicates that the client needs to authenticate. */
  public static final int CLIENT_ERROR_NOT_AUTHENTICATED = 0x0402;

  /** Indicates that the client is not authorized. */
  public static final int CLIENT_ERROR_NOT_AUTHORIZED = 0x0403;

  /**
   * Indicates a request which is not possible to process.
   * For example if the request is directed at a job already finished.
   */
  public static final int CLIENT_ERROR_NOT_POSSIBLE = 0x0404;

  /** Indicates that the client got a timeout for additional action. */
  public static final int CLIENT_ERROR_TIMEOUT = 0x0405;

  /** Indicates that nothing was found for the request uri. */
  public static final int CLIENT_ERROR_NOT_FOUND = 0x0406;

  /** Indicates that the requested object is gone. */
  public static final int CLIENT_ERROR_GONE = 0x0407;

  /** Indicates that the request entities are too long. */
  public static final int CLIENT_ERROR_REQUEST_ENTITY_TOO_LONG = 0x0408;

  /** Indicates that a request value is too long. */
  public static final int CLIENT_ERROR_REQUEST_VALUE_TOO_LONG = 0x0409;

  /** Indicates that the supplied document format is not supported. */
  public static final int CLIENT_ERROR_DOCUMENT_FORMAT_NOT_SUPPORTED = 0x040A;

  /**
   * Indicates that the supplied attributes or values of attributes are not
   * supported by the printer object. Returning this code depends on the
   * given "ipp-attribute-fidelity" operation attribute value.
   */
  public static final int CLIENT_ERROR_ATTRIBUTES_OR_VALUES_NOT_SUPPORTED
    = 0x040B;

  /**
   * Indicates the the URI scheme in a supplied print-uri or send-uri attribute
   * is not supported.
   */
  public static final int CLIENT_ERROR_URI_SCHEME_NOT_SUPPORTED = 0x040C;

  /** Indicates that a supplied attributes-charset is not supported. */
  public static final int CLIENT_ERROR_CHARSET_NOT_SUPPORTED = 0x040D;

  /** Indicates that conflicting attributes are in the request. */
  public static final int CLIENT_ERROR_CONFLICTING_ATTRIBUTES =  0x040E;

  /** Indicates that the specified algorithm is not supported. */
  public static final int CLIENT_ERROR_COMPRESSION_NOT_SUPPORTED =  0x040F;

  /**
   * Indicates that the document cannot be decompressed with the client
   * compression algorithm specified by the client.
   */
  public static final int CLIENT_ERROR_COMPRESSION_ERROR =  0x0410;

  /** Indicates an error in the document format of the document. */
  public static final int CLIENT_ERROR_DOCUMENT_FORMAT_ERROR =  0x0411;

  /**
   * Indicates that the document supplied via print-uri or send-uri cannot be
   * accessed by the printer object.
   */
  public static final int CLIENT_ERROR_DOCUMENT_ACCESS_ERROR =  0x0412;


  /** Indicates an internal server error. */
  public static final int SERVER_ERROR_INTERNAL_ERROR = 0x0500;

  /** Indicates that the server does not support the operation. */
  public static final int SERVER_ERROR_OPERATION_NOT_SUPPORTED =  0x0501;

  /** Indicates that the server' service is not available. */
  public static final int SERVER_ERROR_SERVICE_UNAVAILABLE =  0x0502;

  /** Indicates that the server does not support the IPP version. */
  public static final int SERVER_ERROR_VERSION_NOT_SUPPORTED =  0x0503;

  /** Indicates that the server has a device error e.g. paper jam. */
  public static final int SERVER_ERROR_DEVICE_ERROR = 0x0504;

  /** Indicates that the server has a temporary error. */
  public static final int SERVER_ERROR_TEMPORARY_ERROR = 0x0505;

  /** Indicates that the server is currently not accepting jobs. */
  public static final int SERVER_ERROR_NOT_ACCEPTING_JOBS = 0x0506;

  /**
   * Indicates that the server is currently busy with processing.
   * Requests may be tried later again.
   */
  public static final int SERVER_ERROR_BUSY = 0x0507;

  /** Indicates that the server has canceled the job for various reasons. */
  public static final int SERVER_ERROR_JOB_CANCELED = 0x0508;

  /** Indicates that the server does not support multidocument jobs. */
  public static final int SERVER_ERROR_MULTIPLE_DOCUMENT_JOBS_NOT_SUPPORTED
    = 0x0509;

  private IppStatusCode()
  {
    // not to be instantiated
  }

}
