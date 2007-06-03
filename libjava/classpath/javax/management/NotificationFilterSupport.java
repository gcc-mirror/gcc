/* NotificationFilterSupport.java -- Filter on notification type.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package javax.management;

import java.util.Vector;

/**
 * Performs filtering of {@link Notification}s
 * based on a list of type prefixes.  The type of a notification
 * is compared with each member of the list using
 * {@link String#startsWith(String)} and, if one matches,
 * the notification is allowed to pass through the filter.
 * Matching on the beginning of the string is used in
 * preference to wildcards, so <code>type.*</code> will
 * match only notifications with a type beginning with
 * code>type.*</code>, not <code>type.</code> as
 * expected.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class NotificationFilterSupport
  implements NotificationFilter
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 6579080007561786969L;

  /**
   * Lists the types that may pass through the filter.
   */
  private final Vector<String> enabledTypes = new Vector<String>();

  /**
   * Blocks all types by emptying the list of enabled attributes.
   */
  public void disableAllTypes()
  {
    enabledTypes.clear();
  }

  /**
   * Removes the specified type prefix from the list
   * of enabled types, thus preventing matching types
   * from passing through the filter.  If the specified
   * type prefix is not enabled, this operation has no
   * effect.
   *
   * @param prefix the prefix to disable.
   */
  public void disableType(String prefix)
  {
    enabledTypes.remove(prefix);
  }

  /**
   * Adds the specified type prefix to the list
   * of enabled types, thus allowing
   * types starting with this string to pass through
   * the filter.  If the type prefix is already
   * enabled, this has no effect.
   *
   * @param prefix the prefix to enable.
   * @throws IllegalArgumentException if <code>prefix</code>
   *                                  is <code>null</code>.
   */
  public void enableType(String prefix)
  {
    if (prefix == null)
      throw new IllegalArgumentException("A null prefix was supplied.");
    if (!enabledTypes.contains(prefix))
      enabledTypes.add(prefix);
  }
  
  /**
   * Returns the list of enabled types for this
   * filter.
   *
   * @return the list of enabled types.
   */
  public Vector<String> getEnabledTypes()
  {
    return enabledTypes;
  }

  /**
   * Returns true if the type of the specified notification
   * begins with one of the enabled type prefixes.
   *
   * @param notif the notification being filtered.
   * @return true if the notification's type is enabled.
   */
  public boolean isNotificationEnabled(Notification notif)
  {
    String nType = notif.getType();
    for (String type : enabledTypes)
      if (nType.startsWith(type))
	return true;
    return false;
  }

}
