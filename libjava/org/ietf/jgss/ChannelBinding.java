/* ChannelBinding.java -- a channel binding in the GSS-API.
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

import java.net.InetAddress;
import java.util.Arrays;

/**
 * <p>The GSS-API accommodates the concept of caller-provided channel
 * binding information.  Channel bindings are used to strengthen the
 * quality with which peer entity authentication is provided during
 * context establishment.  They enable the GSS-API callers to bind the
 * establishment of the security context to relevant characteristics
 * like addresses or to application specific data.</p>
 *
 * <p>The caller initiating the security context must determine the
 * appropriate channel binding values to set in the {@link GSSContext}
 * object. The acceptor must provide an identical binding in order to
 * validate that received tokens possess correct channel-related
 * characteristics.</p>
 *
 * <p>Use of channel bindings is optional in GSS-API.  Since channel-binding
 * information may be transmitted in context establishment tokens,
 * applications should therefore not use confidential data as
 * channel-binding components.</p>
 */
public class ChannelBinding
{

  // Fields.
  // -------------------------------------------------------------------------

  private final byte[] appData;
  private final InetAddress initAddr;
  private final InetAddress acceptAddr;

  // Constructor.
  // -------------------------------------------------------------------------

  /**
   * Create a ChannelBinding object with user supplied address information
   * and data. <code>null</code> values can be used for any fields which the
   * application does not want to specify.
   *
   * @param initAddr   The address of the context initiator. <code>null</code>
   *                   value can be supplied to indicate that the application
   *                   does not want to set this value.
   * @param acceptAddr The address of the context acceptor. <code>null</code>
   *                   value can be supplied to indicate that the application
   *                   does not want to set this value.
   * @param appData    Application supplied data to be used as part of the
   *                   channel bindings. <code>null</code> value can be
   *                   supplied to indicate that the application does not
   *                   want to set this value.
   */
  public ChannelBinding(InetAddress initAddr, InetAddress acceptAddr,
                        byte[] appData)
  {
    this.appData = (appData != null) ? (byte[]) appData.clone() : null;
    this.initAddr = initAddr;
    this.acceptAddr = acceptAddr;
  }

  /**
   * Creates a ChannelBinding object without any addressing information.
   *
   * @param appData Application supplied data to be used as part of the
   *                channel bindings.
   */
  public ChannelBinding(byte[] appData)
  {
    this(null, null, appData);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the initiator's address for this channel binding.
   * <code>null</code> is returned if the address has not been set.
   *
   * @return The initiator's address, or <code>null</code>.
   */
  public InetAddress getInitiatorAddress()
  {
    return initAddr;
  }

  /**
   * Returns the acceptor's address for this channel binding.
   * <code>null</code> is returned if the address has not been set.
   *
   * @return The acceptor's address, or <code>null</code>.
   */
  public InetAddress getAcceptorAddress()
  {
    return acceptAddr;
  }

  /**
   * Returns application data being used as part of the ChannelBinding.
   * <code>null</code> is returned if no application data has been
   * specified for the channel binding.
   *
   * @return The application data, or <code>null</code>.
   */
  public byte[] getApplicationData()
  {
    if (appData != null)
      return (byte[]) appData.clone();
    return null;
  }

  /**
   * Returns <code>true</code> if two channel bindings match.
   *
   * @param obj Another channel binding to compare with.
   * @return True if this channel binding equals the other.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof ChannelBinding))
      return false;
    ChannelBinding cb = (ChannelBinding) obj;
    boolean b1 = Arrays.equals(appData, cb.appData);
    boolean b2 = (initAddr == null && cb.initAddr == null)
      || (cb.initAddr != null && initAddr.equals(cb.initAddr));
    boolean b3 = (acceptAddr == null && cb.acceptAddr == null)
      || (cb.acceptAddr != null && acceptAddr.equals(cb.acceptAddr));
    return b1 && b2 && b3;
  }

  /**
   * Returns the hash code for this channel binding.
   *
   * @return The hash code.
   */
  public int hashCode()
  {
    int code = 0;
    if (appData != null)
      for (int i = 0; i < appData.length; i++)
        code ^= appData[i] << ((8 * i) & 31);
    if (initAddr != null)
      code ^= initAddr.hashCode();
    if (acceptAddr != null)
      code ^= acceptAddr.hashCode();
    return code;
  }
}
