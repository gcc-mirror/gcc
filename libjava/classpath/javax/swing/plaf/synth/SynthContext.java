/* SynthContext.java -- Contextual information about a region
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


package javax.swing.plaf.synth;

import javax.swing.JComponent;

/**
 * Contains some contextual information about a region. The information passed
 * in objects of this class can only be considered valid during the method call
 * that it was passed to.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class SynthContext
{

  /**
   * The component.
   */
  private JComponent component;

  /**
   * The region of the component.
   */
  private Region region;

  /**
   * The style of the component.
   */
  private SynthStyle style;

  /**
   * The state of the component.
   */
  private int state;

  /**
   * Creates a new <code>SynthContext</code> object.
   *
   * @param component the component for which this context is used
   * @param region the region of the component
   * @param style the style associated with the component
   * @param state a or'ed bitmask of the constants from {@link SynthConstants}
   */
  public SynthContext(JComponent component, Region region, SynthStyle style,
                      int state)
  {
    this.component = component;
    this.region = region;
    this.style = style;
    this.state = state;
  }

  /**
   * Returns the component that contains the region.
   *
   * @return the component that contains the region
   */
  public JComponent getComponent()
  {
    return component;
  }

  /**
   * Returns the region that identifies this state.
   *
   * @return the region that identifies this state
   */
  public Region getRegion()
  {
    return region;
  }

  /**
   * Returns the style of the region.
   *
   * @return the style of the region
   */
  public SynthStyle getStyle()
  {
    return style;
  }

  /**
   * Returns the state of the component. This is a or'ed bitmask of the
   * constants defined in {@link SynthConstants}.
   *
   * @return the state of the component
   *
   * @see SynthConstants
   */
  public int getComponentState()
  {
    return state;
  }
}
