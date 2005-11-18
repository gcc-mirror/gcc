/* WrappedPlainView.java -- 
   Copyright (C) 2005 Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Container;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.text.Position.Bias;

/**
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 */
public class WrappedPlainView extends BoxView implements TabExpander
{
  /** The color for selected text **/
  Color selectedColor;
  
  /** The color for unselected text **/
  Color unselectedColor;
  
  /** The color for disabled components **/
  Color disabledColor;
  
  /** Stores the font metrics **/
  protected FontMetrics metrics;
  
  /** Whether or not to wrap on word boundaries **/
  boolean wordWrap;
  
  /** A ViewFactory that creates WrappedLines **/
  ViewFactory viewFactory = new WrappedLineCreator();
  
  /** The start of the selected text **/
  int selectionStart;
  
  /** The end of the selected text **/
  int selectionEnd;
  
  /**
   * The instance returned by {@link #getLineBuffer()}.
   */
  private transient Segment lineBuffer;
  
  public WrappedPlainView (Element elem)
  {
    this (elem, false);
  }
  
  public WrappedPlainView (Element elem, boolean wordWrap)
  {
    super (elem, Y_AXIS);
    this.wordWrap = wordWrap;    
  }  
  
  /**
   * Provides access to the Segment used for retrievals from the Document.
   * @return the Segment.
   */
  protected final Segment getLineBuffer()
  {
    if (lineBuffer == null)
      lineBuffer = new Segment();
    return lineBuffer;
  }
  
  /**
   * Returns the next tab stop position after a given reference position.
   *
   * This implementation ignores the <code>tabStop</code> argument.
   * 
   * @param x the current x position in pixels
   * @param tabStop the position within the text stream that the tab occured at
   */
  public float nextTabStop(float x, int tabStop)
  {
    JTextComponent host = (JTextComponent)getContainer();
    float tabSizePixels = getTabSize()
                          * host.getFontMetrics(host.getFont()).charWidth('m');
    return (float) (Math.floor(x / tabSizePixels) + 1) * tabSizePixels;
  }
  
  /**
   * Returns the tab size for the Document based on 
   * PlainDocument.tabSizeAttribute, defaulting to 8 if this property is
   * not defined
   * 
   * @return the tab size.
   */
  protected int getTabSize()
  {
    Object tabSize = getDocument().getProperty(PlainDocument.tabSizeAttribute);
    if (tabSize == null)
      return 8;
    return ((Integer)tabSize).intValue();
  }
  
  /**
   * Draws a line of text, suppressing white space at the end and expanding
   * tabs.  Calls drawSelectedText and drawUnselectedText.
   * @param p0 starting document position to use
   * @param p1 ending document position to use
   * @param g graphics context
   * @param x starting x position
   * @param y starting y position
   */
  protected void drawLine(int p0, int p1, Graphics g, int x, int y)
  {
    try
    {
      // We have to draw both selected and unselected text.  There are
      // several cases:
      //  - entire range is unselected
      //  - entire range is selected
      //  - start of range is selected, end of range is unselected
      //  - start of range is unselected, end of range is selected
      //  - middle of range is selected, start and end of range is unselected
      
      // entire range unselected:      
      if ((selectionStart == selectionEnd) || 
          (p0 > selectionEnd || p1 < selectionStart))
        drawUnselectedText(g, x, y, p0, p1);
      
      // entire range selected
      else if (p0 >= selectionStart && p1 <= selectionEnd)
        drawSelectedText(g, x, y, p0, p1);
      
      // start of range selected, end of range unselected
      else if (p0 >= selectionStart)
        {
          x = drawSelectedText(g, x, y, p0, selectionEnd);
          drawUnselectedText(g, x, y, selectionEnd, p1);
        }
      
      // start of range unselected, end of range selected
      else if (selectionStart > p0 && selectionEnd > p1)
        {
          x = drawUnselectedText(g, x, y, p0, selectionStart);
          drawSelectedText(g, x, y, selectionStart, p1);
        }
      
      // middle of range selected
      else if (selectionStart > p0)
        {
          x = drawUnselectedText(g, x, y, p0, selectionStart);
          x = drawSelectedText(g, x, y, selectionStart, selectionEnd);
          drawUnselectedText(g, x, y, selectionEnd, p1);
        }        
    }
    catch (BadLocationException ble)
    {
      // shouldn't happen
    }
  }

  /**
   * Renders the range of text as selected text.  Just paints the text 
   * in the color specified by the host component.  Assumes the highlighter
   * will render the selected background.
   * @param g the graphics context
   * @param x the starting X coordinate
   * @param y the starting Y coordinate
   * @param p0 the starting model location
   * @param p1 the ending model location 
   * @return the X coordinate of the end of the text
   * @throws BadLocationException if the given range is invalid
   */
  protected int drawSelectedText(Graphics g, int x, int y, int p0, int p1)
      throws BadLocationException
  {
    g.setColor(selectedColor);
    Segment segment = getLineBuffer();
    getDocument().getText(p0, p1 - p0, segment);
    return Utilities.drawTabbedText(segment, x, y, g, this, p0);
  }

  /**
   * Renders the range of text as normal unhighlighted text.
   * @param g the graphics context
   * @param x the starting X coordinate
   * @param y the starting Y coordinate
   * @param p0 the starting model location
   * @param p1 the end model location
   * @return the X location of the end off the range
   * @throws BadLocationException if the range given is invalid
   */
  protected int drawUnselectedText(Graphics g, int x, int y, int p0, int p1)
      throws BadLocationException
  {    
    JTextComponent textComponent = (JTextComponent) getContainer();
    if (textComponent.isEnabled())
      g.setColor(unselectedColor);
    else
      g.setColor(disabledColor);

    Segment segment = getLineBuffer();
    getDocument().getText(p0, p1 - p0, segment);
    return Utilities.drawTabbedText(segment, x, y, g, this, p0);
  }  
  
  /**
   * Loads the children to initiate the view.  Called by setParent.
   * Creates a WrappedLine for each child Element.
   */
  protected void loadChildren (ViewFactory f)
  {
    Element root = getElement();
    int numChildren = root.getElementCount();
    if (numChildren == 0)
      return;
    
    View[] children = new View[numChildren];
    for (int i = 0; i < numChildren; i++)
      children[i] = new WrappedLine(root.getElement(i));
    replace(0, 0, children);
  }
  
  /**
   * Calculates the break position for the text between model positions
   * p0 and p1.  Will break on word boundaries or character boundaries
   * depending on the break argument given in construction of this 
   * WrappedPlainView.  Used by the nested WrappedLine class to determine
   * when to start the next logical line.
   * @param p0 the start model position
   * @param p1 the end model position
   * @return the model position at which to break the text
   */
  protected int calculateBreakPosition(int p0, int p1)
  {
    Container c = getContainer();
    Rectangle alloc = c.isValid() ? c.getBounds()
                                 : new Rectangle(c.getPreferredSize());
    updateMetrics();
    try
      {
        getDocument().getText(p0, p1 - p0, getLineBuffer());
      }
    catch (BadLocationException ble)
      {
        // this shouldn't happen
      }
    // FIXME: Should we account for the insets of the container?
    if (wordWrap)
      return p0
             + Utilities.getBreakLocation(lineBuffer, metrics, alloc.x,
                                          alloc.x + alloc.width, this, 0);
    else
      {
      return p0
             + Utilities.getTabbedTextOffset(lineBuffer, metrics, alloc.x,
                                             alloc.x + alloc.width, this, 0);
      }
  }
  
  void updateMetrics()
  {
    Container component = getContainer();
    metrics = component.getFontMetrics(component.getFont());
  }
  
  /**
   * Determines the preferred span along the given axis.  Implemented to 
   * cache the font metrics and then call the super classes method.
   */
  public float getPreferredSpan (int axis)
  {
    updateMetrics();
    return super.getPreferredSpan(axis);
  }
  
  /**
   * Determines the minimum span along the given axis.  Implemented to 
   * cache the font metrics and then call the super classes method.
   */
  public float getMinimumSpan (int axis)
  {
    updateMetrics();
    return super.getMinimumSpan(axis);
  }
  
  /**
   * Determines the maximum span along the given axis.  Implemented to 
   * cache the font metrics and then call the super classes method.
   */
  public float getMaximumSpan (int axis)
  {
    updateMetrics();
    return super.getMaximumSpan(axis);
  }
  
  /**
   * Called when something was inserted.  Overridden so that
   * the view factory creates WrappedLine views.
   */
  public void insertUpdate (DocumentEvent e, Shape a, ViewFactory f)
  {
    super.insertUpdate(e, a, viewFactory);
    // FIXME: could improve performance by repainting only the necessary area
    getContainer().repaint();
  }
  
  /**
   * Called when something is removed.  Overridden so that
   * the view factory creates WrappedLine views.
   */
  public void removeUpdate (DocumentEvent e, Shape a, ViewFactory f)
  {
    super.removeUpdate(e, a, viewFactory);
    // FIXME: could improve performance by repainting only the necessary area
    getContainer().repaint();
  }
  
  /**
   * Called when the portion of the Document that this View is responsible
   * for changes.  Overridden so that the view factory creates
   * WrappedLine views.
   */
  public void changedUpdate (DocumentEvent e, Shape a, ViewFactory f)
  {
    super.changedUpdate(e, a, viewFactory);
    // FIXME: could improve performance by repainting only the necessary area
    getContainer().repaint();
  }
    
  class WrappedLineCreator implements ViewFactory
  {
    // Creates a new WrappedLine
    public View create(Element elem)
    {
      return new WrappedLine(elem);
    }    
  }
  
  /**
   * Renders the <code>Element</code> that is associated with this
   * <code>View</code>.  Caches the metrics and then calls
   * super.paint to paint all the child views.
   *
   * @param g the <code>Graphics</code> context to render to
   * @param a the allocated region for the <code>Element</code>
   */
  public void paint(Graphics g, Shape a)
  {
    JTextComponent comp = (JTextComponent)getContainer();
    selectionStart = comp.getSelectionStart();
    selectionEnd = comp.getSelectionEnd();
    updateMetrics();
    super.paint(g, a);
  }
  
  /**
   * Sets the size of the View.  Implemented to update the metrics
   * and then call super method.
   */
  public void setSize (float width, float height)
  {
    updateMetrics();
    if (width != getWidth())
      preferenceChanged(null, true, true);
    super.setSize(width, height);
  }
  
  class WrappedLine extends View
  { 
    /** Used to cache the number of lines for this View **/
    int numLines;
    
    public WrappedLine(Element elem)
    {
      super(elem);
      determineNumLines();
    }

    /**
     * Renders this (possibly wrapped) line using the given Graphics object
     * and on the given rendering surface.
     */
    public void paint(Graphics g, Shape s)
    {
      // Ensure metrics are up-to-date.
      updateMetrics();
      JTextComponent textComponent = (JTextComponent) getContainer();

      g.setFont(textComponent.getFont());
      selectedColor = textComponent.getSelectedTextColor();
      unselectedColor = textComponent.getForeground();
      disabledColor = textComponent.getDisabledTextColor();

      // FIXME: this is a hack, for some reason textComponent.getSelectedColor
      // was returning black, which is not visible against a black background
      selectedColor = Color.WHITE;
      
      Rectangle rect = s.getBounds();
      int lineHeight = metrics.getHeight();

      int end = getEndOffset();
      int currStart = getStartOffset();
      int currEnd;      
      while (currStart < end)
        {
          currEnd = calculateBreakPosition(currStart, end);
          drawLine(currStart, currEnd, g, rect.x, rect.y);
          rect.y += lineHeight;          
          if (currEnd == currStart)
            currStart ++;
          else
            currStart = currEnd;          
        }
    }
    
    /**
     * Determines the number of logical lines that the Element
     * needs to be displayed
     * @return the number of lines needed to display the Element
     */
    int determineNumLines()
    {      
      numLines = 0;
      int end = getEndOffset();
      if (end == 0)
        return 0;
            
      int breakPoint;
      for (int i = getStartOffset(); i < end;)
        {
          numLines ++;
          // careful: check that there's no off-by-one problem here
          // depending on which position calculateBreakPosition returns
          breakPoint = calculateBreakPosition(i, end);
          if (breakPoint == i)
            i ++;
          else
            i = breakPoint;
        }
      return numLines;
    }
    
    /**
     * Determines the preferred span for this view along the given axis.
     * 
     * @param axis the axis (either X_AXIS or Y_AXIS)
     * 
     * @return the preferred span along the given axis.
     * @throws IllegalArgumentException if axis is not X_AXIS or Y_AXIS
     */
    public float getPreferredSpan(int axis)
    {
      if (axis == X_AXIS)
        return getWidth();
      else if (axis == Y_AXIS)
        return numLines * metrics.getHeight(); 
      
      throw new IllegalArgumentException("Invalid axis for getPreferredSpan: "
                                         + axis);
    }
    
    /**
     * Provides a mapping from model space to view space.
     * 
     * @param pos the position in the model
     * @param a the region into which the view is rendered
     * @param b the position bias (forward or backward)
     * 
     * @return a box in view space that represents the given position 
     * in model space
     * @throws BadLocationException if the given model position is invalid
     */
    public Shape modelToView(int pos, Shape a, Bias b)
        throws BadLocationException
    {
      Segment s = getLineBuffer();
      int lineHeight = metrics.getHeight();
      Rectangle rect = a.getBounds();
      
      // Return a rectangle with width 1 and height equal to the height 
      // of the text
      rect.height = lineHeight;
      rect.width = 1;

      int currLineStart = getStartOffset();
      int end = getEndOffset();
      
      if (pos < currLineStart || pos >= end)
        throw new BadLocationException("invalid offset", pos);
           
      while (true)
        {
          int currLineEnd = calculateBreakPosition(currLineStart, end);
          // If pos is between currLineStart and currLineEnd then just find
          // the width of the text from currLineStart to pos and add that
          // to rect.x
          if (pos >= currLineStart && pos < currLineEnd || pos == end - 1)
            {             
              try
                {
                  getDocument().getText(currLineStart, pos - currLineStart, s);
                }
              catch (BadLocationException ble)
                {
                  // Shouldn't happen
                }
              rect.x += Utilities.getTabbedTextWidth(s, metrics, rect.x,
                                                     WrappedPlainView.this,
                                                     currLineStart);
              return rect;
            }
          // Increment rect.y so we're checking the next logical line
          rect.y += lineHeight;
          
          // Increment currLineStart to the model position of the start
          // of the next logical line
          if (currLineEnd == currLineStart)
            currLineStart = end;
          else
            currLineStart = currLineEnd;
        }

    }

    /**
     * Provides a mapping from view space to model space.
     * 
     * @param x the x coordinate in view space
     * @param y the y coordinate in view space
     * @param a the region into which the view is rendered
     * @param b the position bias (forward or backward)
     * 
     * @return the location in the model that best represents the
     * given point in view space
     */
    public int viewToModel(float x, float y, Shape a, Bias[] b)
    {
      Segment s = getLineBuffer();
      Rectangle rect = a.getBounds();
      int currLineStart = getStartOffset();
      int end = getEndOffset();
      int lineHeight = metrics.getHeight();
      if (y < rect.y)
        return currLineStart;
      if (y > rect.y + rect.height)
        return end - 1;

      while (true)
        {
          int currLineEnd = calculateBreakPosition(currLineStart, end);
          // If we're at the right y-position that means we're on the right
          // logical line and we should look for the character
          if (y >= rect.y && y < rect.y + lineHeight)
            {
              // Check if the x position is to the left or right of the text
              if (x < rect.x)
                return currLineStart;
              if (x > rect.x + rect.width)
                return currLineEnd - 1;
              
              try
                {
                  getDocument().getText(currLineStart, end - currLineStart, s);
                }
              catch (BadLocationException ble)
                {
                  // Shouldn't happen
                }
              int mark = Utilities.getTabbedTextOffset(s, metrics, rect.x,
                                                       (int) x,
                                                       WrappedPlainView.this,
                                                       currLineStart);
              return currLineStart + mark;
            }
          // Increment rect.y so we're checking the next logical line
          rect.y += lineHeight;
          
          // Increment currLineStart to the model position of the start
          // of the next logical line
          if (currLineEnd == currLineStart)
            currLineStart = end;
          else
            currLineStart = currLineEnd;
        }
    }    

    /**
     * Returns the document position that is (visually) nearest to the given
     * document position <code>pos</code> in the given direction <code>d</code>.
     *
     * @param c the text component
     * @param pos the document position
     * @param b the bias for <code>pos</code>
     * @param d the direction, must be either {@link SwingConstants#NORTH},
     *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
     *        {@link SwingConstants#EAST}
     * @param biasRet an array of {@link Position.Bias} that can hold at least
     *        one element, which is filled with the bias of the return position
     *        on method exit
     *
     * @return the document position that is (visually) nearest to the given
     *         document position <code>pos</code> in the given direction
     *         <code>d</code>
     *
     * @throws BadLocationException if <code>pos</code> is not a valid offset 
     *         in the document model
     */
    public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                         Position.Bias b, int d,
                                         Position.Bias[] biasRet)
      throws BadLocationException
    {
      // TODO: Implement this properly.
      throw new AssertionError("Not implemented yet.");
    }
    
    /**
     * This method is called from insertUpdate and removeUpdate.
     * If the number of lines in the document has changed, just repaint
     * the whole thing (note, could improve performance by not repainting 
     * anything above the changes).  If the number of lines hasn't changed, 
     * just repaint the given Rectangle.
     * @param a the Rectangle to repaint if the number of lines hasn't changed
     */
    void updateDamage (Rectangle a)
    {
      int newNumLines = determineNumLines();
      if (numLines != newNumLines)
        {
          numLines = newNumLines;
          getContainer().repaint();
        }
      else
        getContainer().repaint(a.x, a.y, a.width, a.height);
    }
    
    /**
     * This method is called when something is inserted into the Document
     * that this View is displaying.
     * 
     * @param changes the DocumentEvent for the changes.
     * @param a the allocation of the View
     * @param f the ViewFactory used to rebuild
     */
    public void insertUpdate (DocumentEvent changes, Shape a, ViewFactory f)
    {
      updateDamage((Rectangle)a); 
    }
    
    /**
     * This method is called when something is removed from the Document
     * that this View is displaying.
     * 
     * @param changes the DocumentEvent for the changes.
     * @param a the allocation of the View
     * @param f the ViewFactory used to rebuild
     */
    public void removeUpdate (DocumentEvent changes, Shape a, ViewFactory f)
    {
      updateDamage((Rectangle)a); 
    }
  }
}
