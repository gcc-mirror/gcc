/* MessageProp.java -- GSS-API message property.
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

/**
 * <p>This is a utility class used within the per-message {@link
 * GSSContext} methods to convey per-message properties.</p>
 *
 * <p>When used with the GSSContext interface's {@link
 * GSSContext#wrap(byte[],int,int,org.ietf.jgss.MessageProp)} and {@link
 * GSSContext#getMIC(byte[],int,int,org.ietf.jgss.MessageProp)} methods, an
 * instance of this class is used to indicate the desired QOP and to
 * request if confidentiality services are to be applied to caller
 * supplied data (wrap only).  To request default QOP, the value of 0
 * should be used for QOP.</p>
 *
 * <p>When used with the {@link
 * GSSContext#unwrap(byte[],int,int,org.ietf.jgss.MessageProp)} and {@link
 * GSSContext#verifyMIC(byte[],int,int,byte[],int,int,org.ietf.jgss.MessageProp)}
 * methods of the GSSContext interface, an instance of this class will be
 * used to indicate the applied QOP and confidentiality services over the
 * supplied message. In the case of verifyMIC, the confidentiality state
 * will always be "false".  Upon return from these methods, this object will
 * also contain any supplementary status values applicable to the processed
 * token.  The supplementary status values can indicate old tokens, out
 * of sequence tokens, gap tokens or duplicate tokens.</p>
 */
public class MessageProp
{

  // Fields.
  // -------------------------------------------------------------------------

  private int qopVal;
  private boolean privState;
  private boolean duplicate;
  private boolean old;
  private boolean unseq;
  private boolean gap;
  private int minorStatus;
  private String minorString;

  // Constructors.
  // -------------------------------------------------------------------------

  /**
   * <p>Constructor which sets QOP to 0 indicating that the default QOP is
   * requested.</p>
   *
   * @param privState The desired privacy state. "true" for privacy and
   *                  "false" for integrity only.
   */
  public MessageProp(boolean privState)
  {
    this(0, privState);
  }

  /**
   * <p>Constructor which sets the values for the qop and privacy state.</p>
   *
   * @param qop       The desired QOP.  Use 0 to request a default QOP.
   * @param privState The desired privacy state. "true" for privacy and
   *                  "false" for integrity only.
   */
  public MessageProp(int qop, boolean privState)
  {
    this.qopVal = qop;
    this.privState = privState;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Retrieves the QOP value.
   *
   * @return The QOP value.
   */
  public int getQOP()
  {
    return qopVal;
  }

  /**
   * Retrieves the privacy state.
   *
   * @return The privacy state.
   */
  public boolean getPrivacy()
  {
    return privState;
  }

  /**
   * Retrieves the minor status that the underlying mechanism might have
   * set.
   *
   * @return The minor status.
   */
  public int getMinorStatus()
  {
    return minorStatus;
  }

  /**
   * Returns a string explaining the mechanism specific error code.
   * <code>null</code> will be returned when no mechanism error code has
   * been set.
   *
   * @return The minor status string.
   */
  public String getMinorString()
  {
    return minorString;
  }

  /**
   * Sets the QOP value.
   *
   * @param qopVal The QOP value to be set.  Use 0 to request a default
   *               QOP value.
   */
  public void setQOP(int qopVal)
  {
    this.qopVal = qopVal;
  }

  /**
   * Sets the privacy state.
   *
   * @param privState The privacy state to set.
   */
  public void setPrivacy(boolean privState)
  {
    this.privState = privState;
  }

  /**
   * Returns "true" if this is a duplicate of an earlier token.
   *
   * @return True if this is a duplicate of an earlier token.
   */
  public boolean isDuplicateToken()
  {
    return duplicate;
  }

  /**
   * Returns "true" if the token's validity period has expired.
   *
   * @return True if the token's validity period has expired.
   */
  public boolean isOldToken()
  {
    return old;
  }

  /**
   * Returns "true" if a later token has already been processed.
   *
   * @return True if a later token has already been processed.
   */
  public boolean isUnseqToken()
  {
    return unseq;
  }

  /**
   * Returns "true" if an expected per-message token was not received.
   *
   * @return True if an expected per-message token was not received.
   */
  public boolean isGapToken()
  {
    return gap;
  }

  /**
   * This method sets the state for the supplementary information flags
   * and the minor status in MessageProp.  It is not used by the
   * application but by the GSS implementation to return this information
   * to the caller of a per-message context method.
   *
   * @param duplicate   True if the token was a duplicate of an earlier
   *                    token, false otherwise.
   * @param old         True if the token's validity period has expired,
   *                    false otherwise.
   * @param unseq       True if a later token has already been processed,
   *                    false otherwise.
   * @param gap         True if one or more predecessor tokens have not yet
   *                    been successfully processed, false otherwise.
   * @param minorStatus The integer minor status code that the underlying
   *                    mechanism wants to set.
   * @param minorString The textual representation of the minorStatus
   *                    value.
   */
  public void setSupplementaryStates(boolean duplicate, boolean old,
                                     boolean unseq, boolean gap,
                                     int minorStatus, String minorString)
  {
    this.duplicate = duplicate;
    this.old = old;
    this.unseq = unseq;
    this.gap = gap;
    this.minorStatus = minorStatus;
    this.minorString = minorString;
  }
}
