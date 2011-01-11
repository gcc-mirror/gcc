/* Caret.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Graphics;
import java.awt.Point;

import javax.swing.event.ChangeListener;

/**
 * Defines the method to be implemented by a caret that can be used in Swing
 * text components.
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public interface Caret
{
  /**
   * Registers a {@link ChangeListener} that is notified whenever that state
   * of this <code>Caret</code> changes.
   *
   * @param l the listener to register to this caret
   */
  void addChangeListener(ChangeListener l);

  /**
   * Removes a {@link ChangeListener} from the list of registered listeners.
   *
   * @param l the listener to remove
   */
  void removeChangeListener(ChangeListener l);

  /**
   * Installs this <code>Caret</code> on the specified text component. This
   * usually involves setting up listeners.
   *
   * This method is called by {@link JTextComponent#setCaret(Caret)} after
   * this caret has been set on the text component.
   *
   * @param c the text component to install this caret to
   */
  void install(JTextComponent c);

  /**
   * Deinstalls this <code>Caret</code> from the specified text component.
   * This usually involves removing listeners from the text component.
   *
   * This method is called by {@link JTextComponent#setCaret(Caret)} before
   * this caret is removed from the text component.
   *
   * @param c the text component to deinstall this caret from
   */
  void deinstall(JTextComponent c);

  /**
   * Returns the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @return the blink rate of this <code>Caret</code> or <code>0</code> if
   *         this caret does not blink
   */
  int getBlinkRate();

  /**
   * Sets the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @param rate the new blink rate to set
   */
  void setBlinkRate(int rate);

  /**
   * Returns the current position of this <code>Caret</code> within the
   * <code>Document</code>.
   *
   * @return the current position of this <code>Caret</code> within the
   *         <code>Document</code>
   */
  int getDot();

  /**
   * Sets the current position of this <code>Caret</code> within the
   * <code>Document</code>. This also sets the <code>mark</code> to the
   * new location.
   *
   * @param dot the new position to be set
   *
   * @see #moveDot(int)
   */
  void setDot(int dot);

  /**
   * Moves the <code>dot</code> location without touching the
   * <code>mark</code>. This is used when making a selection.
   *
   * @param dot the location where to move the dot
   *
   * @see #setDot(int)
   */
  void moveDot(int dot);

  /**
   * Returns the current position of the <code>mark</code>. The
   * <code>mark</code> marks the location in the <code>Document</code> that
   * is the end of a selection. If there is no selection, the <code>mark</code>
   * is the same as the <code>dot</code>.
   *
   * @return the current position of the mark
   */
  int getMark();

  /**
   * Returns the current visual position of this <code>Caret</code>.
   *
   * @return the current visual position of this <code>Caret</code>
   *
   * @see #setMagicCaretPosition
   */
  Point getMagicCaretPosition();

  /**
   * Sets the current visual position of this <code>Caret</code>.
   *
   * @param p the Point to use for the saved location. May be <code>null</code>
   *        to indicate that there is no visual location
   */
  void setMagicCaretPosition(Point p);

  /**
   * Returns <code>true</code> if the selection is currently visible,
   * <code>false</code> otherwise.
   *
   * @return <code>true</code> if the selection is currently visible,
   *         <code>false</code> otherwise
   */
  boolean isSelectionVisible();

  /**
   * Sets the visiblity state of the selection.
   *
   * @param v <code>true</code> if the selection should be visible,
   *        <code>false</code> otherwise
   */
  void setSelectionVisible(boolean v);

  /**
   * Returns <code>true</code> if this <code>Caret</code> is currently visible,
   * and <code>false</code> if it is not.
   *
   * @return <code>true</code> if this <code>Caret</code> is currently visible,
   *         and <code>false</code> if it is not
   */
  boolean isVisible();

  /**
   * Sets the visibility state of the caret. <code>true</code> shows the
   * <code>Caret</code>, <code>false</code> hides it.
   *
   * @param v the visibility to set
   */
  void setVisible(boolean v);

  /**
   * Paints this <code>Caret</code> to the specified <code>Graphics</code>
   * context.
   *
   * @param g the graphics context to render to
   */
  void paint(Graphics g);
}
