/* Panel.java -- Simple container object
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * A panel is a simple container class. It's default layout is the
 * <code>FlowLayout</code> manager.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see FlowLayout
 * @since 1.0
 * @status updated to 1.4
 */
public class Panel extends Container implements Accessible
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -2728009084054400034L;

  /** The cached accessible context. */
  private transient AccessibleContext context;

  /** Flag set when the first system-requested paint event is
      dispatched. */
  private transient boolean initialSystemUpdateDone;

  /** Flag set when the first application-requested paint event is
      consumed. */
  private transient boolean initialUpdateConsumed;

  /*
   * The number used to generate the name returned by getName.
   */
  private static transient long next_panel_number;

  /**
   * Initializes a new instance of <code>Panel</code> that has a default
   * layout manager of <code>FlowLayout</code>.
   */
  public Panel()
  {
    this(new FlowLayout());
  }

  /**
   * Initializes a new instance of <code>Panel</code> with the specified
   * layout manager.
   *
   * @param layoutManager the layout manager for this object
   * @since 1.1
   */
  public Panel(LayoutManager layoutManager)
  {
    setLayout(layoutManager);
  }

  /**
   * Notifies this object to create its native peer.
   *
   * @see #isDisplayable()
   * @see #removeNotify()
   */
  public void addNotify()
  {
    if (peer == null)
      peer = getToolkit().createPanel(this);
    super.addNotify();
  }

  /**
   * Gets the AccessibleContext associated with this panel, creating one if
   * necessary. This always returns an instance of {@link AccessibleAWTPanel}.
   *
   * @return the accessibility context of this panel
   * @since 1.3
   */
  public AccessibleContext getAccessibleContext()
  {
    if (context == null)
      context = new AccessibleAWTPanel();
    return context;
  }

  /**
   * This class provides accessibility support for Panels, and is the
   * runtime type returned by {@link #getAccessibleContext()}.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.3
   */
  protected class AccessibleAWTPanel extends AccessibleAWTContainer
  {
    /**
     * Compatible with JDK 1.4+.
     */
    private static final long serialVersionUID = -6409552226660031050L;

    /**
     * The default constructor.
     */
    protected AccessibleAWTPanel()
    {
    }

    /**
     * Get the role of this accessible object, a panel.
     *
     * @return the role of the object
     * @see AccessibleRole#PANEL
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PANEL;
    }
  }

  /**
   * Generate a unique name for this panel.
   *
   * @return A unique name for this panel.
   */
  String generateName ()
  {
    return "panel" + getUniqueLong ();
  }

  private static synchronized long getUniqueLong ()
  {
    return next_panel_number++;
  }
}
