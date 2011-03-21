/* SynthLookAndFeel.java -- A skinnable Swing look and feel
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

import gnu.classpath.NotImplementedException;

import java.awt.Component;
import java.io.InputStream;
import java.text.ParseException;

import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicLookAndFeel;


/**
 * A look and feel that can be customized either by providing a file to
 * {@link #load} or by setting a {@link SynthStyleFactory} using
 * {@link #setStyleFactory}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class SynthLookAndFeel
  extends BasicLookAndFeel
{

  /**
   * The style factory that will be used by the UI classes to load their
   * style sets from.
   */
  private static SynthStyleFactory styleFactory;

  /**
   * Creates a new instance of <code>SynthLookAndFeel</code>. In order to use
   * the Synth look and feel you either need to call {@link #load} to load a
   * set of styles from an XML file, or you need to call
   * {@link #setStyleFactory} to provide your own style factory.
   */
  public SynthLookAndFeel()
  {
    // FIXME: What to do here, if anything?
  }

  /**
   * Sets the style factory that the UI classes of Synth will use to load their
   * sets of styles.
   *
   * @param sf the style factory to set
   */
  public static void setStyleFactory(SynthStyleFactory sf)
  {
    styleFactory = sf;
  }

  /**
   * Returns the current style factory that the UI classes of Synth will use to
   * load their sets of styles.
   *
   * @return the current style factory
   */
  public static SynthStyleFactory getStyleFactory()
  {
    return styleFactory;
  }

  /**
   * Returns the style for the specified component and region.
   *
   * @param c the component for which to return the style
   * @param r the region of the component for which to return the style
   *
   * @return the style for the specified component and region
   */
  public static SynthStyle getStyle(JComponent c, Region r)
  {
    return getStyleFactory().getStyle(c, r);
  }

  /**
   * Updates all style information of the component and it's children.
   *
   * @param c the componenent for which to update the style
   */
  public static void updateStyles(Component c)
    throws NotImplementedException
  {
    // FIXME: Implement this properly.
  }

  /**
   * Returns the region for a given Swing component.
   *
   * @param c the Swing component for which to fetch the region
   *
   * @return the region for a given Swing component
   */
  public static Region getRegion(JComponent c)
    throws NotImplementedException
  {
    // FIXME: This can be implemented as soon as we have the component UI
    // classes in place, since this region will be matched via the UI classes.
    return null;
  }

  /**
   * Creates the Synth look and feel component UI instance for the given
   * component.
   *
   * @param c the component for which to create a UI instance
   *
   * @return the Synth look and feel component UI instance for the given
   *         component
   */
  public static ComponentUI createUI(JComponent c)
    throws NotImplementedException
  {
    // FIXME: This can be implemented as soon as we have the component UI
    // classes in place.
    return null;
  }

  /**
   * Initializes this look and feel.
   */
  public void initialize()
    throws NotImplementedException
  {
    super.initialize();
    // TODO: Implement at least the following here:
    // if (styleFactory != null)
    //   styleFactory = new DefaultStyleFactory();
  }

  /**
   * Uninitializes the look and feel.
   */
  public void uninitialize()
    throws NotImplementedException
  {
    super.uninitialize();
    // TODO: What to do here?
  }

  /**
   * Returns the UI defaults of this look and feel.
   *
   * @return the UI defaults of this look and feel
   */
  public UIDefaults getDefaults()
    throws NotImplementedException
  {
    // FIXME: This is certainly wrong. The defaults should be fetched/merged
    // from the file from which the l&f is loaded.
    return super.getDefaults();
  }

  /**
   * FIXME: DOCUMENT ME!
   *
   * @return FIXME
   */
  public boolean shouldUpdateStyleOnAncestorChanged()
    throws NotImplementedException
  {
    return false;
  }

  /**
   * Loads a set of {@link SynthStyle}s that are used for the look and feel of
   * the components. The <code>resourceBase</code> parameter is used to resolve
   * references against, like icons and other files.
   *
   * @param in the input stream from where to load the styles
   * @param resourceBase the base against which references are resolved.
   *
   * @throws ParseException if the input stream cannot be parsed
   * @throws IllegalArgumentException if one of the parameters is
   *         <code>null</code>
   */
  public void load(InputStream in, Class<?> resourceBase)
    throws ParseException, IllegalArgumentException, NotImplementedException
  {
    // FIXME: Implement this correctly.
  }

  /**
   * Returns a textual description of the Synth look and feel. This returns
   * &quot;Synth look and feel&quot;.
   *
   * @return a textual description of the Synth look and feel
   */
  public String getDescription()
  {
    return "Synth look and feel";
  }

  /**
   * Returns the ID of the Synth look and feel. This returns &quot;Synth&quot;.
   *
   * @return the ID of the Synth look and feel
   */
  public String getID()
  {
    return "Synth";
  }

  /**
   * Returns the name of the Synth look and feel. This returns
   * &quot;Synth look and feel&quot;.
   *
   * @return the name of the Synth look and feel
   */
  public String getName()
  {
    return "Synth look and feel";
  }

  /**
   * Returns <code>false</code> since the Synth look and feel is not a native
   * look and feel.
   *
   * @return <code>false</code>
   */
  public boolean isNativeLookAndFeel()
  {
    return false;
  }

  /**
   * Returns <code>true</code> since the Synth look and feel is always a
   * supported look and feel.
   *
   * @return <code>true</code>
   */
  public boolean isSupportedLookAndFeel()
  {
    return true;
  }

}
