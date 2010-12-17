/* KerberosTicket.java -- a kerberos ticket
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


package javax.security.auth.kerberos;

import gnu.classpath.NotImplementedException;

import java.io.Serializable;
import java.net.InetAddress;
import java.util.Date;

import javax.crypto.SecretKey;
import javax.security.auth.DestroyFailedException;
import javax.security.auth.Destroyable;
import javax.security.auth.RefreshFailedException;
import javax.security.auth.Refreshable;

/**
 * This class represents a Kerberos ticket.  See the Kerberos
 * authentication RFC for more information:
 * <a href="http://www.ietf.org/rfc/rfc1510.txt">RFC 1510</a>.
 *
 * @since 1.4
 */
public class KerberosTicket
    implements Destroyable, Serializable, Refreshable
{
  private static final long serialVersionUID = 7395334370157380539L;

  // Indices of the various flags.  From the kerberos spec.
  // We only list the ones we use.
  private static final int FORWARDABLE = 1;
  private static final int FORWARDED = 2;
  private static final int PROXIABLE = 3;
  private static final int PROXY = 4;
  private static final int POSTDATED = 6;
  private static final int RENEWABLE = 8;
  private static final int INITIAL = 9;
  private static final int NUM_FLAGS = 12;

  private byte[] asn1Encoding;
  private KeyImpl sessionKey;
  private boolean[] flags;
  private Date authTime;
  private Date startTime;
  private Date endTime;
  private Date renewTill;
  private KerberosPrincipal client;
  private KerberosPrincipal server;
  private InetAddress[] clientAddresses;

  /**
   * Create a new ticket given all the facts about it.
   *
   * Note that flags may be null or "short"; any flags not specified
   * will be taken to be false.
   *
   * If the key is not renewable, then renewTill may be null.
   *
   * If authTime is null, then it is taken to be the same as startTime.
   *
   * If clientAddresses is null, then the ticket can be used anywhere.
   *
   * @param asn1Encoding the contents of the ticket, as ASN1
   * @param client the client principal
   * @param server the server principal
   * @param key the contents of the session key
   * @param type the type of the key
   * @param flags an array of flags, as specified by the RFC
   * @param authTime when the client was authenticated
   * @param startTime starting time at which the ticket is valid
   * @param endTime ending time, after which the ticket is invalid
   * @param renewTill for a rewewable ticket, the time before which it must
   * be renewed
   * @param clientAddresses a possibly-null array of addresses where this
   * ticket may be used
   */
  public KerberosTicket(byte[] asn1Encoding, KerberosPrincipal client,
                        KerberosPrincipal server, byte[] key, int type,
                        boolean[] flags, Date authTime, Date startTime,
                        Date endTime, Date renewTill,
                        InetAddress[] clientAddresses)
  {
    this.asn1Encoding = (byte[]) asn1Encoding.clone();
    this.sessionKey = new KeyImpl(key, type);
    this.flags = new boolean[NUM_FLAGS];
    if (flags != null)
      System.arraycopy(flags, 0, this.flags, 0,
                       Math.min(flags.length, NUM_FLAGS));
    this.flags = (boolean[]) flags.clone();
    this.authTime = (Date) authTime.clone();
    this.startTime = (Date) ((startTime == null)
                              ? authTime : startTime).clone();
    this.endTime = (Date) endTime.clone();
    this.renewTill = (Date) renewTill.clone();
    this.client = client;
    this.server = server;
    this.clientAddresses = (clientAddresses == null
                            ? null
                            : (InetAddress[]) clientAddresses.clone());
  }

  /**
   * Destroy this ticket.  This discards secret information.  After this
   * method is called, other methods will throw IllegalStateException.
   */
  public void destroy() throws DestroyFailedException
  {
    if (sessionKey == null)
      throw new DestroyFailedException("already destroyed");
    sessionKey = null;
    asn1Encoding = null;
  }

  /**
   * Return true if this ticket has been destroyed.
   */
  public boolean isDestroyed()
  {
    return sessionKey == null;
  }

  /**
   * Return true if the ticket is currently valid.  This is true if
   * the system time is between the ticket's start and end times.
   */
  public boolean isCurrent()
  {
    long now = System.currentTimeMillis();
    return startTime.getTime() <= now && now <= endTime.getTime();
  }

  /**
   * If the ticket is renewable, and the renewal time has not yet elapsed,
   * attempt to renew the ticket.
   * @throws RefreshFailedException if the renewal fails for any reason
   */
  public void refresh() throws RefreshFailedException, NotImplementedException
  {
    if (! isRenewable())
      throw new RefreshFailedException("not renewable");
    if (renewTill != null
        && System.currentTimeMillis() >= renewTill.getTime())
      throw new RefreshFailedException("renewal time elapsed");
    // FIXME: must contact the KDC.
    // Use the java.security.krb5.kdc property...
    throw new RefreshFailedException("not implemented");
  }

  /**
   * Return the client principal for this ticket.
   */
  public final KerberosPrincipal getClient()
  {
    return client;
  }

  /**
   * Return the server principal for this ticket.
   */
  public final KerberosPrincipal getServer()
  {
    return server;
  }

  /**
   * Return true if this ticket is forwardable.
   */
  public final boolean isForwardable()
  {
    return flags[FORWARDABLE];
  }

  /**
   * Return true if this ticket has been forwarded.
   */
  public final boolean isForwarded()
  {
    return flags[FORWARDED];
  }

  /**
   * Return true if this ticket is proxiable.
   */
  public final boolean isProxiable()
  {
    return flags[PROXIABLE];
  }

  /**
   * Return true if this ticket is a proxy ticket.
   */
  public final boolean isProxy()
  {
    return flags[PROXY];
  }

  /**
   * Return true if this ticket was post-dated.
   */
  public final boolean isPostdated()
  {
    return flags[POSTDATED];
  }

  /**
   * Return true if this ticket is renewable.
   */
  public final boolean isRenewable()
  {
    return flags[RENEWABLE];
  }

  /**
   * Return true if this ticket was granted by an application
   * server, and not via a ticket-granting ticket.
   */
  public final boolean isInitial()
  {
    return flags[INITIAL];
  }

  /**
   * Return the flags for this ticket as a boolean array.
   * See the RFC to understand what the different entries mean.
   */
  public final boolean[] getFlags()
  {
    return (boolean[]) flags.clone();
  }

  /**
   * Return the authentication time for this ticket.
   */
  public final Date getAuthTime()
  {
    return (Date) authTime.clone();
  }

  /**
   * Return the start time for this ticket.
   */
  public final Date getStartTime()
  {
    return (Date) startTime.clone();
  }

  /**
   * Return the end time for this ticket.
   */
  public final Date getEndTime()
  {
    return (Date) endTime.clone();
  }

  /**
   * Return the renewal time for this ticket.  For a non-renewable
   * ticket, this will return null.
   */
  public final Date getRenewTill()
  {
    return flags[RENEWABLE] ? ((Date) renewTill.clone()) : null;
  }

  /**
   * Return the allowable client addresses for this ticket.  This will
   * return null if the ticket can be used anywhere.
   */
  public final InetAddress[] getClientAddresses()
  {
    return (clientAddresses == null
            ? null
            : (InetAddress[]) clientAddresses.clone());
  }

  /**
   * Return the encoded form of this ticket.
   */
  public final byte[] getEncoded()
  {
    checkDestroyed();
    return (byte[]) sessionKey.key.clone();
  }

  /**
   * Return the secret key associated with this ticket.
   */
  public final SecretKey getSessionKey()
  {
    checkDestroyed();
    return sessionKey;
  }

  private void checkDestroyed()
  {
    if (sessionKey == null)
      throw new IllegalStateException("key is destroyed");
  }

  public String toString()
  {
    return getClass().getName() +
      "[client=" + client +
      ",server=" + server +
      ",sessionKey=" + sessionKey +
      ",flags=" + flags +
      ",authTime=" + authTime +
      ",startTime= " + startTime +
      ",endTime=" + endTime +
      ",renewTill=" + renewTill +
      ",clientAddresses=" + clientAddresses +
      "]";
  }

  /**
   * <p>
   * Returns the type of the session key in accordance with
   * RFC1510.  This usually corresponds to the encryption
   * algorithm used by the key, though more than one algorithm
   * may use the same key type (e.g. DES with different checksum
   * mechanisms and chaining modes). Negative values are reserved
   * for local use.  Non-negative values are for officially assigned
   * type fields.  The RFC defines:
   * </p>
   * <ul>
   * <li>0 &mdash; null</li>
   * <li>1 &mdash; DES (in CBC mode with either MD4 or MD5 checksums)</li>
   * </ul>
   *
   * @return the type of session key used by this ticket.
   */
  public final int getSessionKeyType()
  {
    return sessionKey.type;
  }

}
