/* Cookie.java --
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


package gnu.java.net.protocol.http;

import java.util.Date;

/**
 * An HTTP cookie, as specified in RFC 2109.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class Cookie
{

  /**
   * The name of the cookie.
   */
  protected final String name;

  /**
   * The value of the cookie.
   */
  protected final String value;

  /**
   * Optional documentation of the intended use of the cookie.
   */
  protected final String comment;

  /**
   * The domain for which the cookie is valid.
   */
  protected final String domain;

  /**
   * Optional subset of URL paths within the domain for which the cookie is
   * valid.
   */
  protected final String path;

  /**
   * Indicates that the user-agent should only use secure means to transmit
   * this cookie to the server.
   */
  protected final boolean secure;

  /**
   * The date at which this cookie expires.
   */
  protected final Date expires;

  public Cookie(String name, String value, String comment, String domain,
                String path, boolean secure, Date expires)
  {
    this.name = name;
    this.value = value;
    this.comment = comment;
    this.domain = domain;
    this.path = path;
    this.secure = secure;
    this.expires = expires;
  }

  public String getName()
  {
    return name;
  }

  public String getValue()
  {
    return value;
  }

  public String getComment()
  {
    return comment;
  }

  public String getDomain()
  {
    return domain;
  }

  public String getPath()
  {
    return path;
  }

  public boolean isSecure()
  {
    return secure;
  }

  public Date getExpiryDate()
  {
    return expires;
  }

  public String toString()
  {
    return toString(true, true);
  }
  
  public String toString(boolean showPath, boolean showDomain)
  {
    StringBuffer buf = new StringBuffer();
    buf.append(name);
    buf.append('=');
    buf.append(value);
    if (showPath)
      {
        buf.append("; $Path=");
        buf.append(path);
      }
    if (showDomain)
      {
        buf.append("; $Domain=");
        buf.append(domain);
      }
    return buf.toString();
  }

}

