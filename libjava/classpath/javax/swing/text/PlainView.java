/* PlainView.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentEvent.ElementChange;

public class PlainView extends View implements TabExpander
{
  Color selectedColor;
  Color unselectedColor;

  /**
   * The color that is used to draw disabled text fields.
   */
  Color disabledColor;

  Font font;
  
  /** The length of the longest line in the Document **/
  float maxLineLength = -1;
  
  /** The longest line in the Document **/
  Element longestLine = null;
  
  protected FontMetrics metrics;

  /**
   * The instance returned by {@link #getLineBuffer()}.
   */
  private transient Segment lineBuffer;

  public PlainView(Element elem)
  {
    super(elem);
  }

  /**
   * @since 1.4
   */
  protected void updateMetrics()
  {
    Component component = getContainer();
    Font font = component.getFont();

    if (this.font != font)
      {
	this.font = font;
	metrics = component.getFontMetrics(font);
      }
  }
  
  /**
   * @since 1.4
   */
  protected Rectangle lineToRect(Shape a, int line)
  {
    // Ensure metrics are up-to-date.
    updateMetrics();
    
    Rectangle rect = a.getBounds();
    int fontHeight = metrics.getHeight();
    return new Rectangle(rect.x, rect.y + (line * fontHeight),
			 rect.width, fontHeight);
  }

  public Shape modelToView(int position, Shape a, Position.Bias b)
    throws BadLocationException
  {
    // Ensure metrics are up-to-date.
    updateMetrics();
    
    Document document = getDocument();

    // Get rectangle of the line containing position.
    int lineIndex = getElement().getElementIndex(position);
    Rectangle rect = lineToRect(a, lineIndex);

    // Get the rectangle for position.
    Element line = getElement().getElement(lineIndex);
    int lineStart = line.getStartOffset();
    Segment segment = getLineBuffer();
    document.getText(lineStart, position - lineStart, segment);
    int xoffset = Utilities.getTabbedTextWidth(segment, metrics, rect.x,
					       this, lineStart);

    // Calc the real rectangle.
    rect.x += xoffset;
    rect.width = 1;
    rect.height = metrics.getHeight();

    return rect;
  }
  
  protected void drawLine(int lineIndex, Graphics g, int x, int y)
  {
    try
      {
	metrics = g.getFontMetrics();
	// FIXME: Selected text are not drawn yet.
	Element line = getElement().getElement(lineIndex);
	drawUnselectedText(g, x, y, line.getStartOffset(), line.getEndOffset());
	//drawSelectedText(g, , , , );
      }
    catch (BadLocationException e)
      {
	AssertionError ae = new AssertionError("Unexpected bad location");
	ae.initCause(e);
	throw ae;
      }
  }

  protected int drawSelectedText(Graphics g, int x, int y, int p0, int p1)
    throws BadLocationException
  {
    g.setColor(selectedColor);
    Segment segment = getLineBuffer();
    getDocument().getText(p0, p1 - p0, segment);
    return Utilities.drawTabbedText(segment, x, y, g, this, 0);
  }

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
    return Utilities.drawTabbedText(segment, x, y, g, this, segment.offset);
  }

  public void paint(Graphics g, Shape s)
  {
    // Ensure metrics are up-to-date.
    updateMetrics();
    
    JTextComponent textComponent = (JTextComponent) getContainer();

    g.setFont(textComponent.getFont());
    selectedColor = textComponent.getSelectedTextColor();
    unselectedColor = textComponent.getForeground();
    disabledColor = textComponent.getDisabledTextColor();

    Rectangle rect = s.getBounds();

    // FIXME: Text may be scrolled.
    Document document = textComponent.getDocument();
    Element root = document.getDefaultRootElement();
    int y = rect.y;
    
    for (int i = 0; i < root.getElementCount(); i++)
      {
	drawLine(i, g, rect.x, y);
	y += metrics.getHeight();
      }
  }

  /**
   * Returns the tab size of a tab.  Checks the Document's
   * properties for PlainDocument.tabSizeAttribute and returns it if it is
   * defined, otherwise returns 8.
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
   * Returns the next tab stop position after a given reference position.
   *
   * This implementation ignores the <code>tabStop</code> argument.
   * 
   * @param x the current x position in pixels
   * @param tabStop the position within the text stream that the tab occured at
   */
  public float nextTabStop(float x, int tabStop)
  {
    float tabSizePixels = getTabSize() * metrics.charWidth('m');
    return (float) (Math.floor(x / tabSizePixels) + 1) * tabSizePixels;
  }

  /**
   * Returns the length of the longest line, used for getting the span
   * @return the length of the longest line
   */
  float determineMaxLineLength()
  {
    // if the longest line is cached, return the cached value
    if (maxLineLength != -1)
      return maxLineLength;
    
    // otherwise we have to go through all the lines and find it
    Element el = getElement();
    Segment seg = getLineBuffer();
    float span = 0;
    for (int i = 0; i < el.getElementCount(); i++)
      {
        Element child = el.getElement(i);
        int start = child.getStartOffset();
        int end = child.getEndOffset();
        try
          {
            el.getDocument().getText(start, end - start, seg);
          }
        catch (BadLocationException ex)
          {
            AssertionError ae = new AssertionError("Unexpected bad location");
	    ae.initCause(ex);
	    throw ae;
          }
        
        if (seg == null || seg.array == null || seg.count == 0)
          continue;
        
        int width = metrics.charsWidth(seg.array, seg.offset, seg.count);
        if (width > span)
          {
            longestLine = child;
            span = width;
          }
      }
    maxLineLength = span;
    return maxLineLength;
  }
  
  public float getPreferredSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException();

    // make sure we have the metrics
    updateMetrics();

    float span = 0;
    Element el = getElement();

    switch (axis)
      {
      case X_AXIS:
        span = determineMaxLineLength();
      case Y_AXIS:
      default:
        span = metrics.getHeight() * el.getElementCount();
        break;
      }
    return span;
  }

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model.
   *
   * @param x the x coordinate in the view space
   * @param y the y coordinate in the view space
   * @param a the allocation of this <code>View</code>
   * @param b the bias to use
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code>
   */
  public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
  {
    Rectangle rec = a.getBounds();
    Document doc = getDocument();
    Element root = doc.getDefaultRootElement();
    
    // PlainView doesn't support line-wrapping so we can find out which
    // Element was clicked on just by the y-position    
    int lineClicked = (int) (y - rec.y) / metrics.getHeight();
    if (lineClicked >= root.getElementCount())
      return getEndOffset() - 1;
    
    Element line = root.getElement(lineClicked);
    Segment s = getLineBuffer();
    int start = line.getStartOffset();
    // We don't want the \n at the end of the line.
    int end = line.getEndOffset() - 1;
    try
      {
        doc.getText(start, end - start, s);
      }
    catch (BadLocationException ble)
      {
        AssertionError ae = new AssertionError("Unexpected bad location");
        ae.initCause(ble);
        throw ae;
      }
    
    int pos = Utilities.getTabbedTextOffset(s, metrics, rec.x, (int)x, this, start);
    return Math.max (0, pos);
  }     
  
  /**
   * Since insertUpdate and removeUpdate each deal with children
   * Elements being both added and removed, they both have to perform
   * the same checks.  So they both simply call this method.
   * @param changes the DocumentEvent for the changes to the Document.
   * @param a the allocation of the View.
   * @param f the ViewFactory to use for rebuilding.
   */
  protected void updateDamage(DocumentEvent changes, Shape a, ViewFactory f)
  {
    Element el = getElement();
    ElementChange ec = changes.getChange(el);
    
    // If ec is null then no lines were added or removed, just 
    // repaint the changed line
    if (ec == null)
      {
        int line = getElement().getElementIndex(changes.getOffset());
        damageLineRange(line, line, a, getContainer());
        return;
      }
    
    Element[] removed = ec.getChildrenRemoved();
    Element[] newElements = ec.getChildrenAdded();
    
    // If no Elements were added or removed, we just want to repaint
    // the area containing the line that was modified
    if (removed == null && newElements == null)
      {
        int line = getElement().getElementIndex(changes.getOffset());
        damageLineRange(line, line, a, getContainer());
        return;
      }

    // Check to see if we removed the longest line, if so we have to
    // search through all lines and find the longest one again
    if (removed != null)
      {
        for (int i = 0; i < removed.length; i++)
          if (removed[i].equals(longestLine))
            {
              // reset maxLineLength and search through all lines for longest one
              maxLineLength = -1;
              determineMaxLineLength();
              ((JTextComponent)getContainer()).repaint();
              return;
            }
      }
    
    // If we've reached here, that means we haven't removed the longest line
    if (newElements == null)
      {
        // No lines were added, just repaint the container and exit
        ((JTextComponent)getContainer()).repaint();
        return;
      }

    //  Make sure we have the metrics
    updateMetrics();
       
    // If we've reached here, that means we haven't removed the longest line
    // and we have added at least one line, so we have to check if added lines
    // are longer than the previous longest line        
    Segment seg = getLineBuffer();
    float longestNewLength = 0;
    Element longestNewLine = null;    

    // Loop through the added lines to check their length
    for (int i = 0; i < newElements.length; i++)
      {
        Element child = newElements[i];
        int start = child.getStartOffset();
        int end = child.getEndOffset();
        try
          {
            el.getDocument().getText(start, end - start, seg);
          }
        catch (BadLocationException ex)
          {
            AssertionError ae = new AssertionError("Unexpected bad location");
	    ae.initCause(ex);
	    throw ae;
          }
                
        if (seg == null || seg.array == null || seg.count == 0)
          continue;
        
        int width = metrics.charsWidth(seg.array, seg.offset, seg.count);
        if (width > longestNewLength)
          {
            longestNewLine = child;
            longestNewLength = width;
          }
      }
    
    // Check if the longest of the new lines is longer than our previous
    // longest line, and if so update our values
    if (longestNewLength > maxLineLength)
      {
        maxLineLength = longestNewLength;
        longestLine = longestNewLine;
      }
    // Repaint the container
    ((JTextComponent)getContainer()).repaint();
  }

  /**
   * This method is called when something is inserted into the Document
   * that this View is displaying.
   * 
   * @param changes the DocumentEvent for the changes.
   * @param a the allocation of the View
   * @param f the ViewFactory used to rebuild
   */
  public void insertUpdate(DocumentEvent changes, Shape a, ViewFactory f)
  {
    updateDamage(changes, a, f);
  }

  /**
   * This method is called when something is removed from the Document
   * that this View is displaying.
   * 
   * @param changes the DocumentEvent for the changes.
   * @param a the allocation of the View
   * @param f the ViewFactory used to rebuild
   */
  public void removeUpdate(DocumentEvent changes, Shape a, ViewFactory f)
  {
    updateDamage(changes, a, f);
  }
  
  /**
   * This method is called when attributes were changed in the 
   * Document in a location that this view is responsible for.
   */
  public void changedUpdate (DocumentEvent changes, Shape a, ViewFactory f)
  {
    updateDamage(changes, a, f);
  }
  
  /**
   * Repaint the given line range.  This is called from insertUpdate,
   * changedUpdate, and removeUpdate when no new lines were added 
   * and no lines were removed, to repaint the line that was 
   * modified.
   * 
   * @param line0 the start of the range
   * @param line1 the end of the range
   * @param a the rendering region of the host
   * @param host the Component that uses this View (used to call repaint
   * on that Component)
   * 
   * @since 1.4
   */
  protected void damageLineRange (int line0, int line1, Shape a, Component host)
  {
    if (a == null)
      return;

    Rectangle rec0 = lineToRect(a, line0);
    Rectangle rec1 = lineToRect(a, line1);

    if (rec0 == null || rec1 == null)
      // something went wrong, repaint the entire host to be safe
      host.repaint();
    else
      {
        Rectangle repaintRec = rec0.union(rec1);
        host.repaint();
      }    
  }

  /**
   * Provides a {@link Segment} object, that can be used to fetch text from
   * the document.
   *
   * @returna {@link Segment} object, that can be used to fetch text from
   *          the document
   */
  protected Segment getLineBuffer()
  {
    if (lineBuffer == null)
      lineBuffer = new Segment();
    return lineBuffer;
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
   * @throws BadLocationException if <code>pos</code> is not a valid offset in
   *         the document model
   */
  public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                       Position.Bias b, int d,
                                       Position.Bias[] biasRet)
    throws BadLocationException
  {
    // TODO: Implement this properly.
    throw new AssertionError("Not implemented yet.");
  }
}

