/* TextUI.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.text.BadLocationException;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.View;


/**
 * An abstract base class for delegates that provide the user
 * interface for text editors.
 *
 * @see javax.swing.text.JTextComponent
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class TextUI
  extends ComponentUI
{
  /**
   * Constructs a new <code>TextUI</code>.
   */  
  public TextUI()
  {
  }


  /**
   * Calculates the geometric extent of the character at the
   * given offset.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param pos the zero-based index of the character into the
   *        document model.
   *
   * @return the bounding box of the character at index
   *         <code>pos</code>, in view coordinates.
   *
   * @throws BadLocationException if <code>pos</code> does not
   *         designate a valid position in the document model.
   *
   * @see javax.swing.text.View#modelToView(int,
   *      javax.swing.text.Position.Bias, int,
   *      javax.swing.text.position.Bias, java.awt.Shape)
   */
  public abstract Rectangle modelToView(JTextComponent tc, int pos)
    throws BadLocationException;


  /**
   * Calculates the geometric extent of the character at the
   * given offset.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param pos the zero-based index of the character into the
   *        document model.
   *
   * @param bias whether to take the character before or after the
   *        caret position indicated by <code>pos</code>.  The value
   *        must be either {@link
   *        javax.swing.text.Position.Bias#Backward} or {@link
   *        javax.swing.text.Position.Bias#Forward}.
   *
   * @return the bounding box of the character at index
   *         <code>pos</code>, in view coordinates.
   *
   * @throws BadLocationException if <code>pos</code> does not
   *         designate a valid position in the document model.
   *
   * @see javax.swing.text.View#modelToView(int,
   *      javax.swing.text.Position.Bias, int,
   *      javax.swing.text.position.Bias, java.awt.Shape)
   */
  public abstract Rectangle modelToView(JTextComponent tc, int pos,
                                        Position.Bias bias)
    throws BadLocationException;


  /**
   * Finds the caret position which is closest to the specified visual
   * location.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param loc the position in view coordinates.
   *
   * @return the caret position which is closest to <code>loc</code>.
   *
   * @see #viewToModel(JTextComponent, Point, Position.Bias[])
   */
  public abstract int viewToModel(JTextComponent t, Point pt);


  /**
   * Finds the caret position which is closest to the specified visual
   * location.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param loc the position in view coordinates.
   *
   * @param outBias an array whose size must be at least one.
   *        After the call, <code>outBias[0]</code> will indicate
   *        whether <code>loc</code> is in the glyph before
   *        (<code>Position.Bias.Backward</code>) or after
   *        (<code>Position.Bias.Forward</code>) the returned
   *        caret position.
   *
   * @return the caret position which is closest to <code>loc</code>.
   */
  public abstract int viewToModel(JTextComponent tc, Point loc,
                                  Position.Bias[] outBias);
 


  /**
   * Calculates the caret position that is visually next to the given
   * position. This is useful to determine where to move the caret
   * after the user has pressed an arrow key.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param pos the current caret position, a zero-based index
   *        into the document model.
   *
   * @param bias whether to take the character before or after the
   *        caret position indicated by <code>pos</code>.  The value
   *        must be either {@link
   *        javax.swing.text.Position.Bias#Backward} or {@link
   *        javax.swing.text.Position.Bias#Forward}.
   *
   * @param direction the visual direction. Pass
   *        {@link javax.swing.SwingConstants#WEST} for the left
   *        arrow key, {@link javax.swing.SwingConstants#EAST}
   *        for the right arrow key, {@link
   *        javax.swing.SwingConstants#NORTH} for the up arrow
   *        key, or {@link javax.swing.SwingConstants#SOUTH}
   *        for the down arrow key.
   *
   * @throws BadLocationException if <code>pos</code> does not
   *         designate a valid position in the document model.
   *
   * @throws IllegalArgumentException if <code>direction</code>
   *         is not one of <code>Position.Bias.Forward</code>
   *         or <code>Position.Biad.Backward</code>.
   */
  public abstract int getNextVisualPositionFrom(JTextComponent tc,
                                                int pos,
                                                Position.Bias bias,
                                                int direction,
                                                Position.Bias[] outBias)
    throws BadLocationException;


  /**
   * Repaints a range of characters.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param start the first character in the range that needs
   *        painting, indicated as an index into the document model.
   *
   * @param end the last character in the range that needs
   *        painting, indicated as an index into the document model.
   *        <code>end</code> must be greater than or equal to
   *        <code>start</code>.
   */
  public abstract void damageRange(JTextComponent tc, int start, int end);


  /**
   * Repaints a range of characters, also specifying the bias for the
   * start and end of the range.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param start the first character in the range that needs
   *        painting, indicated as an index into the document model.
   *
   * @param end the last character in the range that needs
   *        painting, indicated as an index into the document model.
   *        <code>end</code> must be greater than or equal to
   *        <code>start</code>.
   */
  public abstract void damageRange(JTextComponent tc,
                                   int start, int end,
                                   Position.Bias startBias,
                                   Position.Bias endBias);

  
  /**
   * Retrieves the <code>EditorKit</code> managing policies and
   * persistent state.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @return the <code>EditorKit</code> used by <code>tc</code>.
   */
  public abstract EditorKit getEditorKit(JTextComponent tc);
  
  
  /**
   * Retrieves the root of the view tree that visually presents
   * the text.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @return the root <code>View</code> used by <code>tc</code>.
   */
  public abstract View getRootView(JTextComponent tc);


  /**
   * Returns a String for presenting a tool tip at the specified
   * location.
   *
   * @param tc the <code>JTextComponent</code> for which this
   *        delegate object provides the user interface.
   *
   * @param loc the location for which the tool tip is requested.
   *
   * @return the text for the tool tip, or <code>null</code> to
   *         display no tool tip.
   *
   * @since 1.4
   */
  public String getToolTipText(JTextComponent tc, Point loc)
  {
    return null;
  }
}
