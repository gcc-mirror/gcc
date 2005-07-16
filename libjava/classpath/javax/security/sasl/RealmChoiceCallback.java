/* RealmChoiceCallback.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package javax.security.sasl;

import javax.security.auth.callback.ChoiceCallback;

/**
 * This callback is used by {@link SaslClient} and {@link SaslServer} to obtain
 * a realm given a list of realm choices.
 */
public class RealmChoiceCallback extends ChoiceCallback
{

  /**
   * Constructs a <code>RealmChoiceCallback</code> with a prompt, a list of
   * choices and a default choice.
   *
   * @param prompt the non-null prompt to use to request the realm.
   * @param choices the non-null list of realms to choose from.
   * @param defaultChoice the choice to be used as the default when the list of
   * choices is displayed. It is an index into the <code>choices</code> array.
   * @param multiple <code>true</code> if multiple choices allowed;
   * <code>false</code> otherwise.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>
   * or empty, if <code>choices</code> has a length of <code>0</code>, if any
   * element from <code>choices</code> is <code>null</code> or empty, or if
   * <code>defaultChoice</code> does not fall within the array boundary of
   * <code>choices</code>.
   */
  public RealmChoiceCallback(String prompt, String[] choices, int defaultChoice,
                             boolean multiple)
  {
    super(prompt, choices, defaultChoice, multiple);
  }
}
