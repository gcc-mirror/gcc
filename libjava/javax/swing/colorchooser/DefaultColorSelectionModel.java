/* DefaultColorSelectionModel.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing.colorchooser;

import java.awt.Color;
import java.io.Serializable;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * This is the default implementation of the ColorSelectionModel interface
 * that JColorChoosers use.
 *
 * @author Andrew Selkirk
 * @version 1.0
 */
public class DefaultColorSelectionModel implements ColorSelectionModel,
                                                   Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = -8117143602864778804L;

  /** The currently selected color. */
  private Color selectedColor;

  /** The ChangeEvent fired to all ChangeListeners. */
  protected transient ChangeEvent changeEvent = new ChangeEvent(this);

  /** The list of listeners. */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * Creates a new color selection model with the default white color.
   */
  public DefaultColorSelectionModel()
  {
    this(Color.white);
  }

  /**
   * Creates a new color selection model with a given selected color.
   *
   * @param color The initial color.
   *
   * @throws Error If the color is null.
   */
  public DefaultColorSelectionModel(Color color)
  {
    super();
    if (color == null)
      throw new Error("ColorSelectionModel cannot be set to have null color.");
    this.selectedColor = color;
  }

  /**
   * Returns the selected color.
   *
   * @return The selected color.
   */
  public Color getSelectedColor()
  {
    return selectedColor;
  }

  /**
   * This method sets the color.
   *
   * @param color The color to set.
   *
   * @throws Error If the color is set.
   */
  public void setSelectedColor(Color color)
  {
    if (color == null)
      throw new Error("ColorSelectionModel cannot be set to have null color.");
    if (color != selectedColor)
      {
	this.selectedColor = color;
	fireStateChanged();
      }
  }

  /**
   * Adds a listener to this model.
   *
   * @param listener The listener to add.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Removes a listener from this model.
   *
   * @param listener The listener to remove.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Returns all currently added <code>ChangeListener</code> objects.
   *
   * @return Array of <code>ChangeListener</code> objects.
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Calls all the <code>stateChanged()</code> method of all added
   * <code>ChangeListener</code> objects with <code>changeEvent</code> as
   * argument.
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].stateChanged(changeEvent);
  }
}
