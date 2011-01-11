/* CookieManager.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package gnu.java.net.protocol.http;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * A simple non-persistent cookie manager. This class can be extended to
 * provide cookie persistence.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class SimpleCookieManager
  implements CookieManager
{

  /**
   * The cookie cache.
   * This is a dictionary mapping domains to maps of cookies by name.
   */
  protected Map<String, Map<String, Cookie>> cookies;

  /**
   * Constructor.
   */
  public SimpleCookieManager()
  {
    cookies = new HashMap<String, Map<String, Cookie>>();
  }

  public void setCookie(Cookie cookie)
  {
    String domain = cookie.getDomain();
    Map<String, Cookie> map = cookies.get(domain);
    if (map == null)
      {
        map = new HashMap<String, Cookie>();
        cookies.put(domain, map);
      }
    String name = cookie.getName();
    map.put(name, cookie); // will replace a cookie of the same name
  }

  public Cookie[] getCookies(String host, boolean secure, String path)
  {
    ArrayList<Cookie> matches = new ArrayList<Cookie>();
    Date now = new Date();
    if (Character.isLetter(host.charAt(0)))
      {
        int di = host.indexOf('.');
        while (di != -1)
          {
            addCookies(matches, host, secure, path, now);
            host = host.substring(di);
            di = host.indexOf('.', 1);
          }
      }
    addCookies(matches, host, secure, path, now);
    Cookie[] ret = new Cookie[matches.size()];
    matches.toArray(ret);
    return ret;
  }

  private void addCookies(ArrayList<Cookie> matches, String domain,
                          boolean secure, String path, Date now)
  {
    Map<String, Cookie> map = cookies.get(domain);
    if (map != null)
      {
        ArrayList<String> expired = new ArrayList<String>();
        for (Map.Entry<String, Cookie> entry : map.entrySet())
          {
            Cookie cookie = entry.getValue();
            Date expires = cookie.getExpiryDate();
            if (expires != null && expires.before(now))
              {
                expired.add(entry.getKey());
                continue;
              }
            if (secure && !cookie.isSecure())
              {
                continue;
              }
            if (path.startsWith(cookie.getPath()))
              {
                matches.add(cookie);
              }
          }
        // Good housekeeping
        for (Iterator<String> i = expired.iterator(); i.hasNext(); )
          {
            map.remove(i.next());
          }
      }
  }

}
