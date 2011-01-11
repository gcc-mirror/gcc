/* DefaultFocusTraversalPolicy.java --
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package java.awt;

/**
 * DefaultFocusTraversalPolicy is the default focus traversal policy
 * used by Containers.
 *
 * This policy sharpens ContainerOrderFocusTraversalPolicy's
 * acceptance criteria, to reject those Components that have
 * unfocusable peers.  Despite this extra strictness, this policy will
 * always accept a Component that has explicitly been set focusable by
 * any means.
 *
 * This AWT implementation assumes that the peers of the following
 * Components are not focusable: Canvas, Panel, Label, ScrollPane,
 * Scrollbar, Window, and any lightweight Component.
 *
 * A Component's focusability is independent of the focusability of
 * its peer.
 *
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 * @since 1.4
 */
public class DefaultFocusTraversalPolicy
  extends ContainerOrderFocusTraversalPolicy
{
  private static final long serialVersionUID = 8876966522510157497L;

  /**
   * Construct a default focus traversal policy.
   */
  public DefaultFocusTraversalPolicy ()
  {
  }

  /**
   * Check whether a given Component would be acceptable as a focus
   * owner.  The Component must be displayable, visible and enabled to
   * be acceptable.  If the Component's focus traversability has been
   * overridden, by overriding Component.isFocusTraversable or
   * Component.isFocusable, or by calling Component.setFocusable, then
   * the Component will be accepted if it is focusable.  If the
   * Component uses the default focus traversable behaviour, then
   * <code>comp</code> will always be rejected if it is a Canvas,
   * Panel, Label, ScrollPane, Scrollbar, Window or lightweight
   * Component.
   *
   * @param comp the Component to check
   *
   * @return true if the Component is an acceptable target for
   * keyboard input focus, false otherwise
   */
  protected boolean accept (Component comp)
  {
    if (comp.visible
        && comp.isDisplayable ()
        && comp.enabled)
      {
        if (comp.isFocusTraversableOverridden != 0
            && (comp.isFocusTraversable () || comp.isFocusable()))
          return true;

        if (!(comp instanceof Canvas
              || comp instanceof Panel
              || comp instanceof Label
              || comp instanceof ScrollPane
              || comp instanceof Scrollbar
              || comp instanceof Window
              || comp.isLightweight ()))
          return true;
      }
    return false;
  }
}
