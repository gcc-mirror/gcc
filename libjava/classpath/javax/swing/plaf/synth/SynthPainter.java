/* SynthPainter.java -- An abstract painter for synth components
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

import java.awt.Graphics;

/**
 * The abstract definition of a delegate that takes the responsibility of
 * painting for the components.
 *
 * This class is defined to be abstract and all methods are no-ops.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public abstract class SynthPainter
{

  /**
   * Creates a new <code>SynthPainter</code> object.
   */
  public SynthPainter()
  {
    // Nothing to do here.
  }

  /**
   * Paints the foreground of an arrow button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param dir the orientation of the arrow
   */
  public void paintArrowButtonForeground(SynthContext ctx, Graphics g, int x,
                                         int y, int w, int h, int dir)
  {
    // Nothing to do here.
  }

  /**
   * Paints the foreground of a progress bar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param dir the orientation of the progress bar
   */
  public void paintProgressBarForeground(SynthContext ctx, Graphics g,
                                         int x, int y, int w, int h,
                                         int dir)
  {
    // Nothing to do here.
  }

  /**
   * Paints the foreground of a separator.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param dir the orientation of the separator
   */
  public void paintSeparatorForeground(SynthContext ctx, Graphics g,
                                       int x, int y, int w, int h,
                                       int dir)
  {
    // Nothing to do here.
  }

  /**
   * Paints the foreground of a split pane's divider.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param dir the orientation of the divider
   */
  public void paintSplitPaneDividerForeground(SynthContext ctx, Graphics g,
                                              int x, int y, int w, int h,
                                              int dir)
  {
    // Nothing to do here.
  }

  /**
   * Paints a split pane's divider, when it is being dragged.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param dir the orientation of the divider
   */
  public void paintSplitPaneDragDivider(SynthContext ctx, Graphics g,
                                        int x, int y, int w, int h,
                                        int dir)
  {
    // Nothing to do here.
  }

  /**
   * Paints the indicator for a tree cell which has the focus.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTreeCellFocus(SynthContext ctx, Graphics g,
                                 int x, int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of an arrow button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintArrowButtonBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of an arrow button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintArrowButtonBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintButtonBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintButtonBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a check box.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintCheckBoxBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a check box.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintCheckBoxBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a check box menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintCheckBoxMenuItemBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a check box menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintCheckBoxMenuItemBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a color chooser.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintColorChooserBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a color chooser.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintColorChooserBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a combo box.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintComboBoxBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a combo box.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintComboBoxBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a desktop icon.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintDesktopIconBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a desktop icon.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintDesktopIconBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a desktop pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintDesktopPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a desktop pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintDesktopPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of an editor pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintEditorPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of an editor pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintEditorPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a file chooser.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintFileChooserBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a file chooser.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintFileChooserBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a formatted text field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintFormattedTextFieldBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a formatted text field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintFormattedTextFieldBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of an internal frame.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintInternalFrameBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of an internal frame.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintInternalFrameBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of an internal frame's title pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintInternalFrameTitlePaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of an internal frame's title pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintInternalFrameTitlePaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a label.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintLabelBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a label.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintLabelBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a list.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintListBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a list.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintListBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a menu.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a menu.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a menu bar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuBarBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a menu bar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuBarBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuItemBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintMenuItemBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of an option pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintOptionPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of an option pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintOptionPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a panel.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPanelBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a panel.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPanelBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a password field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPasswordFieldBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a password field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPasswordFieldBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a popup menu.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPopupMenuBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a popup menu.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintPopupMenuBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a progress bar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintProgressBarBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a progress bar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintProgressBarBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a radio button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRadioButtonBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a radio button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRadioButtonBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a radio button menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRadioButtonMenuItemBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a radio button menu item.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRadioButtonMenuItemBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a root pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRootPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a root pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintRootPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a scrollbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollBarBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a scrollbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollBarBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a scrollbar's thumb.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param orientation orientation of the scrollbar
   */
  public void paintScrollBarThumbBackground(SynthContext ctx, Graphics g, int x,
                                            int y, int w, int h, int orientation)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a scrollbar's thumb.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param orientation orientation of the scrollbar
   */
  public void paintScrollBarThumbBorder(SynthContext ctx, Graphics g, int x,
                                        int y, int w, int h, int orientation)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a scrollbar's track.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollBarTrackBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a scrollbar's track.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollBarTrackBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a scroll pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a scroll pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintScrollPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a separator.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSeparatorBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a separator.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSeparatorBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a slider.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSliderBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a slider.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSliderBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a slider's thumb.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param orientation orientation of the slider
   */
  public void paintSliderThumbBackground(SynthContext ctx, Graphics g, int x,
                                         int y, int w, int h, int orientation)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a slider's thumb.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param orientation orientation of the slider
   */
  public void paintSliderThumbBorder(SynthContext ctx, Graphics g, int x,
                                     int y, int w, int h, int orientation)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a slider's track.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSliderTrackBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a slider's track.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSliderTrackBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a spinner.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSpinnerBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a spinner.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSpinnerBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a split pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSplitPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a split pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSplitPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a split pane's divider.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintSplitPaneDividerBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of the contents of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneContentBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of the contents of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneContentBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of the tab area of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneTabAreaBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of the tab area of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTabbedPaneTabAreaBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a tab of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param index the index of the tab to paint
   */
  public void paintTabbedPaneTabBackground(SynthContext ctx, Graphics g, int x,
                                           int y, int w, int h, int index)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a tab of a tabbed pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   * @param index the index of the tab to paint
   */
  public void paintTabbedPaneTabBorder(SynthContext ctx, Graphics g, int x,
                                       int y, int w, int h, int index)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a table.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTableBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a table.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTableBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a table's header.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTableHeaderBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a table's header.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTableHeaderBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a text area.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextAreaBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a text area.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextAreaBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a text field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextFieldBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a text field.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextFieldBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a text pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextPaneBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a text pane.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTextPaneBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a toggle button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToggleButtonBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a toggle button.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToggleButtonBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of the contents of a toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarContentBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of the contents of a toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarContentBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of the window of a detached toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarDragWindowBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of the window of a detached toolbar.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolBarDragWindowBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a tool tip.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolTipBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a tool tip.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintToolTipBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a tree.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTreeBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a tree.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTreeBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a cell in a tree.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTreeCellBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a cell in a tree.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintTreeCellBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the background of a viewport.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintViewportBackground(SynthContext ctx, Graphics g, int x,
                                   int y, int w, int h)
  {
    // Nothing to do here.
  }

  /**
   * Paints the border of a viewport.
   *
   * @param ctx the synth context identifying the component and region for
   *        painting
   * @param g the graphics context to use for painting
   * @param x the X coordinate of the area to paint
   * @param y the Y coordinate of the area to paint 
   * @param w the width of the area to paint
   * @param h the height of the area to paint
   */
  public void paintViewportBorder(SynthContext ctx, Graphics g, int x,
                               int y, int w, int h)
  {
    // Nothing to do here.
  }
}
