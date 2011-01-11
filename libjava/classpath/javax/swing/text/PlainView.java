/* PlainView.java --
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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

import javax.swing.SwingUtilities;
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

  /**
   * While painting this is the textcomponent's current start index
   * of the selection.
   */
  int selectionStart;

  /**
   * While painting this is the textcomponent's current end index
   * of the selection.
   */
  int selectionEnd;

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

  /**
   * The base offset for tab calculations.
   */
  private int tabBase;

  /**
   * The tab size.
   */
  private int tabSize;

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
        tabSize = getTabSize() * metrics.charWidth('m');
      }
  }

  /**
   * @since 1.4
   */
  protected Rectangle lineToRect(Shape a, int line)
  {
    // Ensure metrics are up-to-date.
    updateMetrics();

    Rectangle rect = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
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
    tabBase = rect.x;

    // Get the rectangle for position.
    Element line = getElement().getElement(lineIndex);
    int lineStart = line.getStartOffset();
    Segment segment = getLineBuffer();
    document.getText(lineStart, position - lineStart, segment);
    int xoffset = Utilities.getTabbedTextWidth(segment, metrics, tabBase,
                                               this, lineStart);

    // Calc the real rectangle.
    rect.x += xoffset;
    rect.width = 1;
    rect.height = metrics.getHeight();

    return rect;
  }

  /**
   * Draws a line of text. The X and Y coordinates specify the start of
   * the <em>baseline</em> of the line.
   *
   * @param lineIndex the index of the line
   * @param g the graphics to use for drawing the text
   * @param x the X coordinate of the baseline
   * @param y the Y coordinate of the baseline
   */
  protected void drawLine(int lineIndex, Graphics g, int x, int y)
  {
    try
      {
        Element line = getElement().getElement(lineIndex);
        int startOffset = line.getStartOffset();
        int endOffset = line.getEndOffset() - 1;

        if (selectionStart <= startOffset)
          // Selection starts before the line ...
          if (selectionEnd <= startOffset)
            {
              // end ends before the line: Draw completely unselected text.
              drawUnselectedText(g, x, y, startOffset, endOffset);
            }
          else if (selectionEnd <= endOffset)
            {
              // and ends within the line: First part is selected,
              // second is not.
              x = drawSelectedText(g, x, y, startOffset, selectionEnd);
              drawUnselectedText(g, x, y, selectionEnd, endOffset);
            }
          else
            // and ends behind the line: Draw completely selected text.
            drawSelectedText(g, x, y, startOffset, endOffset);
        else if (selectionStart < endOffset)
          // Selection starts within the line ..
          if (selectionEnd < endOffset)
            {
              // and ends within it: First part unselected, second part
              // selected, third part unselected.
              x = drawUnselectedText(g, x, y, startOffset, selectionStart);
              x = drawSelectedText(g, x, y, selectionStart, selectionEnd);
              drawUnselectedText(g, x, y, selectionEnd, endOffset);
            }
          else
            {
              // and ends behind the line: First part unselected, second
              // part selected.
              x = drawUnselectedText(g, x, y, startOffset, selectionStart);
              drawSelectedText(g, x, y, selectionStart, endOffset);
            }
        else
          // Selection is behind this line: Draw completely unselected text.
          drawUnselectedText(g, x, y, startOffset, endOffset);
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
    return Utilities.drawTabbedText(segment, x, y, g, this, segment.offset);
  }

  /**
   * Draws a chunk of unselected text.
   *
   * @param g the graphics to use for drawing the text
   * @param x the X coordinate of the baseline
   * @param y the Y coordinate of the baseline
   * @param p0 the start position in the text model
   * @param p1 the end position in the text model
   *
   * @return the X location of the end of the range
   *
   * @throws BadLocationException if <code>p0</code> or <code>p1</code> are
   *         invalid
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
    return Utilities.drawTabbedText(segment, x, y, g, this, segment.offset);
  }

  public void paint(Graphics g, Shape s)
  {
    // Ensure metrics are up-to-date.
    updateMetrics();

    JTextComponent textComponent = (JTextComponent) getContainer();

    selectedColor = textComponent.getSelectedTextColor();
    unselectedColor = textComponent.getForeground();
    disabledColor = textComponent.getDisabledTextColor();
    selectionStart = textComponent.getSelectionStart();
    selectionEnd = textComponent.getSelectionEnd();

    Rectangle rect = s instanceof Rectangle ? (Rectangle) s : s.getBounds();
    tabBase = rect.x;

    // FIXME: Text may be scrolled.
    Document document = textComponent.getDocument();
    Element root = getElement();
    int height = metrics.getHeight();

    // For layered highlighters we need to paint the layered highlights
    // before painting any text.
    LayeredHighlighter hl = null;
    Highlighter h = textComponent.getHighlighter();
    if (h instanceof LayeredHighlighter)
      hl = (LayeredHighlighter) h;

    int count = root.getElementCount();

    // Determine first and last line inside the clip.
    Rectangle clip = g.getClipBounds();
    SwingUtilities.computeIntersection(rect.x, rect.y, rect.width, rect.height,
                                       clip);
    int line0 = (clip.y - rect.y) / height;
    line0 = Math.max(0, Math.min(line0, count - 1));
    int line1 = (clip.y + clip.height - rect.y) / height;
    line1 = Math.max(0, Math.min(line1, count - 1));
    int y = rect.y + metrics.getAscent() + height * line0;
    for (int i = line0; i <= line1; i++)
      {
        if (hl != null)
          {
            Element lineEl = root.getElement(i);
            // Exclude the trailing newline from beeing highlighted.
            if (i == count)
              hl.paintLayeredHighlights(g, lineEl.getStartOffset(),
                                        lineEl.getEndOffset(), s, textComponent,
                                        this);
            else
              hl.paintLayeredHighlights(g, lineEl.getStartOffset(),
                                        lineEl.getEndOffset() - 1, s,
                                        textComponent, this);
          }
        drawLine(i, g, rect.x, y);
        y += height;
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
    float next = x;
    if (tabSize != 0)
      {
        int numTabs = (((int) x) - tabBase) / tabSize;
        next = tabBase + (numTabs + 1) * tabSize;
      }
    return next;
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
        int end = child.getEndOffset() - 1;
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

    Element el = getElement();
    float span;

    switch (axis)
      {
      case X_AXIS:
        span = determineMaxLineLength();
        break;
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
    Rectangle rec = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
    tabBase = rec.x;

    int pos;
    if ((int) y < rec.y)
      // Above our area vertically. Return start offset.
      pos = getStartOffset();
    else if ((int) y > rec.y + rec.height)
      // Below our area vertically. Return end offset.
      pos = getEndOffset() - 1;
    else
      {
        // Inside the allocation vertically. Determine line and X offset.
        Document doc = getDocument();
        Element root = doc.getDefaultRootElement();
        int line = Math.abs(((int) y - rec.y) / metrics.getHeight());
        if (line >= root.getElementCount())
          pos = getEndOffset() - 1;
        else
          {
            Element lineEl = root.getElement(line);
            if (x < rec.x)
              // To the left of the allocation area.
              pos = lineEl.getStartOffset();
            else if (x > rec.x + rec.width)
              // To the right of the allocation area.
              pos = lineEl.getEndOffset() - 1;
            else
              {
                try
                  {
                    int p0 = lineEl.getStartOffset();
                    int p1 = lineEl.getEndOffset();
                    Segment s = new Segment();
                    doc.getText(p0, p1 - p0, s);
                    tabBase = rec.x;
                    pos = p0 + Utilities.getTabbedTextOffset(s, metrics,
                                                             tabBase, (int) x,
                                                             this, p0);
                  }
                catch (BadLocationException ex)
                  {
                    // Should not happen.
                    pos = -1;
                  }
              }

          }
      }
    // Bias is always forward.
    b[0] = Position.Bias.Forward;
    return pos;
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
    // This happens during initialization.
    if (metrics == null)
      {
        updateMetrics();
        preferenceChanged(null, true, true);
        return;
      }

    Element element = getElement();

    // Find longest line if it hasn't been initialized yet.
    if (longestLine == null)
      findLongestLine(0, element.getElementCount() - 1);

    ElementChange change = changes.getChange(element);
    if (changes.getType() == DocumentEvent.EventType.INSERT)
      {
        // Handles character/line insertion.

        // Determine if lines have been added. In this case we repaint
        // differently.
        boolean linesAdded = true;
        if (change == null)
          linesAdded = false;

        // Determine the start line.
        int start;
        if (linesAdded)
          start = change.getIndex();
        else
          start = element.getElementIndex(changes.getOffset());

        // Determine the length of the updated region.
        int length = 0;
        if (linesAdded)
          length = change.getChildrenAdded().length - 1;

        // Update the longest line and length.
        int oldMaxLength = (int) maxLineLength;
        if (longestLine.getEndOffset() < changes.getOffset()
            || longestLine.getStartOffset() > changes.getOffset()
                                             + changes.getLength())
          {
            findLongestLine(start, start + length);
          }
        else
          {
            findLongestLine(0, element.getElementCount() - 1);
          }

        // Trigger a preference change so that the layout gets updated
        // correctly.
        preferenceChanged(null, maxLineLength != oldMaxLength, linesAdded);

        // Damage the updated line range.
        int endLine = start;
        if (linesAdded)
          endLine = element.getElementCount() - 1;
        damageLineRange(start, endLine, a, getContainer());

      }
    else
      {
        // Handles character/lines removals.

        // Update the longest line and length and trigger preference changed.
        int oldMaxLength = (int) maxLineLength;
        if (change != null)
          {
            // Line(s) have been removed.
            findLongestLine(0, element.getElementCount() - 1);
            preferenceChanged(null, maxLineLength != oldMaxLength, true);
          }
        else
          {
            // No line has been removed.
            int lineNo = getElement().getElementIndex(changes.getOffset());
            Element line = getElement().getElement(lineNo);
            if (longestLine == line)
              {
                findLongestLine(0, element.getElementCount() - 1);
                preferenceChanged(null, maxLineLength != oldMaxLength, false);
            }
            damageLineRange(lineNo, lineNo, a, getContainer());
        }
      }
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
        Rectangle repaintRec = SwingUtilities.computeUnion(rec0.x, rec0.y,
                                                           rec0.width,
                                                           rec0.height, rec1);
        host.repaint(repaintRec.x, repaintRec.y, repaintRec.width,
                     repaintRec.height);
      }
  }

  /**
   * Provides a {@link Segment} object, that can be used to fetch text from
   * the document.
   *
   * @returna {@link Segment} object, that can be used to fetch text from
   *          the document
   */
  protected final Segment getLineBuffer()
  {
    if (lineBuffer == null)
      lineBuffer = new Segment();
    return lineBuffer;
  }

  /**
   * Finds and updates the longest line in the view inside an interval of
   * lines.
   *
   * @param start the start of the search interval
   * @param end the end of the search interval
   */
  private void findLongestLine(int start, int end)
  {
    for (int i = start; i <= end; i++)
      {
        int w = getLineLength(i);
        if (w > maxLineLength)
          {
            maxLineLength = w;
            longestLine = getElement().getElement(i);
          }
      }
  }

  /**
   * Determines the length of the specified line.
   *
   * @param line the number of the line
   *
   * @return the length of the line in pixels
   */
  private int getLineLength(int line)
  {
    Element lineEl = getElement().getElement(line);
    Segment buffer = getLineBuffer();
    try
      {
        Document doc = getDocument();
        doc.getText(lineEl.getStartOffset(),
                    lineEl.getEndOffset() - lineEl.getStartOffset() - 1,
                    buffer);
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError("Unexpected bad location");
        err.initCause(ex);
        throw err;
      }

    return Utilities.getTabbedTextWidth(buffer, metrics, tabBase, this,
                                        lineEl.getStartOffset());
  }
}
