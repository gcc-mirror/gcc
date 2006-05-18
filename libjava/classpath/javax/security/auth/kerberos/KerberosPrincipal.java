/* KerberosPrincipal.java -- a kerberos principal
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
import gnu.classpath.SystemProperties;

import java.io.Serializable;
import java.security.Principal;

/**
 * This represents a Kerberos principal.  See the Kerberos
 * authentication RFC for more information:
 * <a href="http://www.ietf.org/rfc/rfc1510.txt">RFC 1510</a>.
 *
 * @since 1.4
 */
public final class KerberosPrincipal
    implements Serializable, Principal
{
  // Uncomment when serialization is correct.
  // private static final long serialVersionUID = -7374788026156829911L;

  /**
   * Constant from the RFC: "Just the name of the principal as in DCE, or
   * for users".
   */
  public static final int KRB_NT_PRINCIPAL = 1;

  /**
   * Constant from the RFC: "Service and other unique instance (krbtgt)".
   */
  public static final int KRB_NT_SRV_HST = 3;

  /**
   * Constant from the RFC: "Service with host name as instance (telnet,
   * rcommands)".
   */
  public static final int KRB_NT_SRV_INST = 2;

  /**
   * Constant from the RFC: "Service with host as remaining components".
   */
  public static final int KRB_NT_SRV_XHST = 4;

  /**
   * Constant from the RFC: "Unique ID".
   */
  public static final int KRB_NT_UID = 5;

  /**
   * Constant from the RFC: "Name type not known".
   */
  public static final int KRB_NT_UNKNOWN = 0;

  private String name;
  private int type;
  private String realm;

  /**
   * Create a new instance with the given name and a type of
   * {@link #KRB_NT_PRINCIPAL}.
   * @param name the principal's name
   */
  public KerberosPrincipal(String name)
  {
    this(name, KRB_NT_PRINCIPAL);
  }

  /**
   * Create a new instance with the given name and type.  The name is
   * parsed according to the rules in the RFC.  If there is no realm,
   * then the local realm is used instead.
   * 
   * @param name the principal's name
   * @param type the principal's type
   */
  public KerberosPrincipal(String name, int type)
  // Marked as unimplemented because we don't look for the realm as needed.
    throws NotImplementedException
  {
    if (type < KRB_NT_UNKNOWN || type > KRB_NT_UID)
      throw new IllegalArgumentException("unknown type: " + type);
    this.name = name;
    this.type = type;
    this.realm = parseRealm();
  }

  private String parseRealm()
  {
    // Handle quoting as specified by the Kerberos RFC.
    int i, len = name.length();
    boolean quoted = false;
    for (i = 0; i < len; ++i)
      {
        if (quoted)
          {
            quoted = false;
            continue;
          }
        char c = name.charAt(i);
        if (c == '\\')
          {
            quoted = true;
            continue;
          }
        if (c == '@')
          break;
      }
    if (quoted || i == len - 1)
      throw new IllegalArgumentException("malformed principal: " + name);
    if (i < len)
      {
        // We have the realm.  FIXME: verify its syntax?
        return name.substring(i + 1);
      }
    // Try to find the default realm.
    String def = SystemProperties.getProperty("java.security.krb5.realm");
    if (def != null)
      return def;
    // Now ask the system.
    // FIXME: use java.security.krb5.conf,
    // or $JAVA_HOME/lib/security/krb5.conf to find the krb config file.
    // Then pass to native code using krb5_set_config_files() and
    // krb5_get_default_realm().  But... what about /etc/krb5.conf?
    throw new IllegalArgumentException("default realm can't be found");
  }

  /**
   * Return the name of this principal.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Return the realm of this principal.
   */
  public String getRealm()
  {
    return realm;
  }

  /**
   * Return the type of this principal.
   */
  public int getNameType()
  {
    return type;
  }

  public int hashCode()
  {
    return name.hashCode();
  }

  public boolean equals(Object other)
  {
    if (! (other instanceof KerberosPrincipal))
      return false;
    KerberosPrincipal kp = (KerberosPrincipal) other;
    return name.equals(kp.name) && type == kp.type;
  }

  public String toString()
  {
    // This is what came to mind.
    return name + ":" + type; 
  }
}
