/* GSSException.java -- a general exception in GSS.
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

/* The documentation comments of this class are derived from the text
   of RFC 2853:  Generic Security Service API Version 2: Java Bindings.
   That document is covered under the following license notice:

Copyright (C) The Internet Society (2000).  All Rights Reserved.

This document and translations of it may be copied and furnished to
others, and derivative works that comment on or otherwise explain it
or assist in its implementation may be prepared, copied, published and
distributed, in whole or in part, without restriction of any kind,
provided that the above copyright notice and this paragraph are
included on all such copies and derivative works.  However, this
document itself may not be modified in any way, such as by removing
the copyright notice or references to the Internet Society or other
Internet organizations, except as needed for the purpose of developing
Internet standards in which case the procedures for copyrights defined
in the Internet Standards process must be followed, or as required to
translate it into languages other than English.

The limited permissions granted above are perpetual and will not be
revoked by the Internet Society or its successors or assigns.

This document and the information contained herein is provided on an
"AS IS" basis and THE INTERNET SOCIETY AND THE INTERNET ENGINEERING
TASK FORCE DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION HEREIN
WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. */


package org.ietf.jgss;

import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

/**
 * This exception is thrown whenever a fatal GSS-API error occurs
 * including mechanism specific errors.  It may contain both, the major
 * and minor, GSS-API status codes.  The mechanism implementers are
 * responsible for setting appropriate minor status codes when throwing
 * this exception.  Aside from delivering the numeric error code(s) to
 * the caller, this class performs the mapping from their numeric values
 * to textual representations.  All Java GSS-API methods are declared
 * throwing this exception.
 */
public class GSSException extends Exception
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  // These values do not jive with the "Constant Field Values" in the J2SE
  // 1.4.1, but do follow RFC 2853. I trust the IETF, but not Sun.

  /**
   * Channel bindings mismatch error.
   */
  public static final int BAD_BINDINGS = 4;

  /**
   * Unsupported mechanism requested error.
   */
  public static final int BAD_MECH = 1;

  /**
   * Invalid name provided error.
   */
  public static final int BAD_NAME = 2;

  /**
   * Name of unsupported type provided error.
   */
  public static final int BAD_NAMETYPE = 3;

  /**
   * Invalid status code error - this is the default status value.
   */
  public static final int BAD_STATUS = 5;

  /**
   * Token had invalid integrity check error.
   */
  public static final int BAD_MIC = 6;

  /**
   * Specified security context expired error.
   */
  public static final int CONTEXT_EXPIRED = 12;

  /**
   * Expired credentials detected error.
   */
  public static final int CREDENTIALS_EXPIRED = 11;

  /**
   * Defective credential error.
   */
  public static final int DEFECTIVE_CREDENTIAL = 10;

  /**
   * Defective token error.
   */
  public static final int DEFECTIVE_TOKEN = 9;

  /**
   * General failure, unspecified at GSS-API level.
   */
  public static final int FAILURE = 13;

  /**
   * Invalid security context error.
   */
  public static final int NO_CONTEXT = 8;

  /**
   * Invalid credentials error.
   */
  public static final int NO_CRED = 7;

  /**
   * Unsupported QOP value error.
   */
  public static final int BAD_QOP = 14;

  /**
   * Operation unauthorized error.
   */
  public static final int UNAUTHORIZED = 15;

  /**
   * Operation unavailable error.
   */
  public static final int UNAVAILABLE = 16;

  /**
   * Duplicate credential element requested error.
   */
  public static final int DUPLICATE_ELEMENT = 17;

  /**
   * Name contains multi-mechanism elements error.
   */
  public static final int NAME_NOT_MN = 18;

  /**
   * The token was a duplicate of an earlier token.  This is a fatal error
   * code that may occur during context establishment.  It is not used to
   * indicate supplementary status values.  The MessageProp object is used
   * for that purpose.
   */
  public static final int DUPLICATE_TOKEN = 20;

  /**
   * The token's validity period has expired.  This is a fatal error code
   * that may occur during context establishment.  It is not used to
   * indicate supplementary status values.  The MessageProp object is used
   * for that purpose.
   */
  public static final int OLD_TOKEN = 19;

  /**
   * A later token has already been processed.  This is a fatal error code
   * that may occur during context establishment.  It is not used to
   * indicate supplementary status values.  The MessageProp object is used
   * for that purpose.
   */
  public static final int UNSEQ_TOKEN = 21;

  /**
   * An expected per-message token was not received.  This is a fatal
   * error code that may occur during context establishment.  It is not
   * used to indicate supplementary status values.  The MessageProp object
   * is used for that purpose.
   */
  public static final int GAP_TOKEN = 22;

  private final int major;
  private int minor;
  private String minorString;

  private ResourceBundle messages;

  // Constructors.
  // -------------------------------------------------------------------------

  /**
   * Create a new GSS exception with the given major code.
   *
   * @param major The major GSS error code.
   */
  public GSSException(int major)
  {
    this(major, 0, null);
  }

  /**
   * Create a new GSS exception with the given major and minor codes, and a
   * minor explanation string.
   *
   * @param major The major GSS error code.
   * @param minor The minor application-specific error code.
   * @param minorString An explanation of the minor error code.
   */
  public GSSException(int major, int minor, String minorString)
  {
    this.major = major;
    this.minor = minor;
    this.minorString = minorString;
    try
      {
        messages = PropertyResourceBundle.getBundle("org/ietf/jgss/MessagesBundle");
      }
    catch (Exception e)
      {
        messages = null;
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the major code representing the GSS error code that caused
   * this exception to be thrown.
   *
   * @return The major error code.
   */
  public int getMajor()
  {
    return major;
  }

  /**
   * Returns the mechanism error code that caused this exception.  The
   * minor code is set by the underlying mechanism.  Value of 0 indicates
   * that mechanism error code is not set.
   *
   * @return The minor error code, or 0 if not set.
   */
  public int getMinor()
  {
    return minor;
  }

  /**
   * Returns a string explaining the GSS major error code causing this
   * exception to be thrown.
   *
   * @return The major error string.
   */
  public String getMajorString()
  {
    switch (major)
      {
      case BAD_MECH:
        return getMsg("GSSException.BAD_MECH",
                      "An unsupported mechanism was requested.");
      case BAD_NAME:
        return getMsg("GSSException.BAD_NAME",
                      "An invalid name was supplied.");
      case BAD_NAMETYPE:
        return getMsg("GSSException.BAD_NAMETYPE",
                      "A supplied name was of an unsupported type.");
      case BAD_BINDINGS:
        return getMsg("GSSException.BAD_BINDINGS",
                      "Incorrect channel bindings were supplied.");
      case BAD_STATUS:
        return getMsg("GSSException.BAD_STATUS",
                      "An invalid status code was supplied.");
      case BAD_MIC:
        return getMsg("GSSException.BAD_MIC",
                      "A token had an invalid MIC.");
      case NO_CRED:
        return getMsg("GSSException.NO_CRED",
                      "No credentials were supplied, or the credentials were "+
                      "unavailable or inaccessible.");
      case NO_CONTEXT:
        return getMsg("GSSException.NO_CONTEXT",
                      "Invalid context has been supplied.");
      case DEFECTIVE_TOKEN:
        return getMsg("GSSException.DEFECTIVE_TOKEN",
                      "A supplied token was invalid.");
      case DEFECTIVE_CREDENTIAL:
        return getMsg("GSSException.DEFECTIVE_CREDENTIAL",
                      "A supplied credential was invalid.");
      case CREDENTIALS_EXPIRED:
        return getMsg("GSSException.CREDENTIALS_EXPIRED",
                      "The referenced credentials have expired.");
      case CONTEXT_EXPIRED:
        return getMsg("GSSException.CONTEXT_EXPIRED",
                      "The context has expired.");
      case FAILURE:
        return getMsg("GSSException.FAILURE",
                      "Miscellaneous failure.");
      case BAD_QOP:
        return getMsg("GSSException.BAD_QOP",
                      "The quality-of-protection requested could not be provided.");
      case UNAUTHORIZED:
        return getMsg("GSSException.UNAUTHORIZED",
                      "The operation is forbidden by local security policy.");
      case UNAVAILABLE:
        return getMsg("GSSException.UNAVAILABLE",
                      "The operation or option is unavailable.");
      case DUPLICATE_ELEMENT:
        return getMsg("GSSException.DUPLICATE_ELEMENT",
                      "The requested credential element already exists.");
      case NAME_NOT_MN:
        return getMsg("GSSException.NAME_NOT_MN",
                      "The provided name was not a mechanism name.");
      case OLD_TOKEN:
        return getMsg("GSSException.OLD_TOKEN",
                      "The token's validity period has expired.");
      case DUPLICATE_TOKEN:
        return getMsg("GSSException.DUPLICATE_TOKEN",
                      "The token was a duplicate of an earlier version.");
      case UNSEQ_TOKEN:
        return getMsg("GSSException.UNSEQ_TOKEN",
                      "A later token has already been processed.");
      case GAP_TOKEN:
        return getMsg("GSSException.GAP_TOKEN",
                      "An expected per-message token was not received.");
      default: return "Unknown or invalid error code.";
      }
  }

  /**
   * Returns a string explaining the mechanism specific error code.
   * <code>null</code> will be returned when no mechanism error code has
   * been set.
   *
   * @return The minor error string, or <code>null</code>.
   */
  public String getMinorString()
  {
    return minorString;
  }

  /**
   * Used internally by the GSS-API implementation and the underlying
   * mechanisms to set the minor code and its textual representation.
   *
   * @param minorCode The mechanism specific error code.
   * @param message   A textual explanation of the mechanism error code.
   */
  public void setMinor(int minorCode, String message)
  {
    this.minor = minorCode;
    this.minorString = message;
  }

  /**
   * Returns a textual representation of both the major and minor status
   * codes.
   *
   * @return The textual representation.
   */
  public String toString()
  {
    return GSSException.class.getName() + ": " + getMessage();
  }

  /**
   * Returns a detailed message of this exception.  Overrides {@link
   * Throwable#getMessage()}. It is customary in Java to use this method to
   * obtain exception information.
   *
   * @return The detail message.
   */
  public String getMessage()
  {
    if (minor == 0)
      return getMajorString();
    else
      return getMajorString() + " (" + minorString + ")";
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private String getMsg(String key, String defaultText)
  {
    if (messages != null)
      {
        try
          {
            return messages.getString(key);
          }
        catch (Exception e)
          {
          }
      }
    return defaultText;
  }
}
