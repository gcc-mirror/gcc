/* DefaultCaret.java --
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
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.EventListener;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;

/**
 * The default implementation of the {@link Caret} interface.
 *
 * @author orgininal author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public class DefaultCaret extends Rectangle
  implements Caret, FocusListener, MouseListener, MouseMotionListener
{

  /**
   * Controls the blinking of the caret.
   *
   * @author Roman Kennke (kennke@aicas.com)
   * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
   */
  private class BlinkTimerListener implements ActionListener
  {
    /**
     * Forces the next event to be ignored. The next event should be ignored
     * if we force the caret to appear. We do not know how long will it take
     * to fire the comming event; this may be near immediately. Better to leave
     * the caret visible one iteration longer.
     */
    boolean ignoreNextEvent;
    
    /**
     * Receives notification when the blink timer fires and updates the visible
     * state of the caret.
     * 
     * @param event the action event
     */
    public void actionPerformed(ActionEvent event)
    {
      if (ignoreNextEvent)
        ignoreNextEvent = false;
      else
        {
          visible = !visible;
          repaint();
        }
    }
  }

  /**
   * Listens for changes in the text component's document and updates the
   * caret accordingly.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class DocumentHandler implements DocumentListener
  {
    /**
     * Receives notification that some text attributes have changed. No action
     * is taken here.
     *
     * @param event the document event
     */
    public void changedUpdate(DocumentEvent event)
    {
      // Nothing to do here.
    }

    /**
     * Receives notification that some text has been inserted from the text
     * component. The caret is moved forward accordingly.
     *
     * @param event the document event
     */
    public void insertUpdate(DocumentEvent event)
    {
      if (policy == ALWAYS_UPDATE || 
          (SwingUtilities.isEventDispatchThread() && 
           policy == UPDATE_WHEN_ON_EDT))
        {        
          int dot = getDot();
          setDot(dot + event.getLength());
        }
    }

    /**
     * Receives notification that some text has been removed into the text
     * component. The caret is moved backwards accordingly.
     *
     * @param event the document event
     */
    public void removeUpdate(DocumentEvent event)
    {
      if (policy == ALWAYS_UPDATE || 
          (SwingUtilities.isEventDispatchThread() && 
           policy == UPDATE_WHEN_ON_EDT))
        {
          int dot = getDot();
          setDot(dot - event.getLength());
        }
      else if (policy == NEVER_UPDATE)
        {
          int docLength = event.getDocument().getLength();
          if (getDot() > docLength)
            setDot(docLength);
        }
    }
  }

  /**
   * Listens for property changes on the text document. This is used to add and
   * remove our document listener, if the document of the text component has
   * changed.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class PropertyChangeHandler implements PropertyChangeListener
  {

    /**
     * Receives notification when a property has changed on the text component.
     * This adds/removes our document listener from the text component's
     * document when the document changes.
     *
     * @param e the property change event
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals("document"))
        {
          Document oldDoc = (Document) e.getOldValue();
          oldDoc.removeDocumentListener(documentListener);
          Document newDoc = (Document) e.getNewValue();
          newDoc.addDocumentListener(documentListener);
        }
    }
    
  }

  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 4325555698756477346L;
  
  /**
   * Indicates the Caret position should always be updated after Document
   * changes even if the updates are not performed on the Event Dispatching
   * thread.
   * 
   * @since 1.5
   */
  public static final int ALWAYS_UPDATE = 2;

  /**
   * Indicates the Caret position should not be changed unless the Document
   * length becomes less than the Caret position, in which case the Caret
   * is moved to the end of the Document.
   * 
   * @since 1.5
   */
  public static final int NEVER_UPDATE = 1;
  
  /** 
   * Indicates the Caret position should be updated only if Document changes
   * are made on the Event Dispatcher thread.
   *  
   * @since 1.5
   */
  public static final int UPDATE_WHEN_ON_EDT = 0;
  
  /** Keeps track of the current update policy **/
  int policy = UPDATE_WHEN_ON_EDT;
    
  /**
   * The <code>ChangeEvent</code> that is fired by {@link #fireStateChanged()}.
   */
  protected ChangeEvent changeEvent = new ChangeEvent(this);

  /**
   * Stores all registered event listeners.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * Our document listener.
   */
  DocumentListener documentListener;

  /**
   * Our property listener.
   */
  PropertyChangeListener propertyChangeListener;

  /**
   * The text component in which this caret is installed.
   */
  private JTextComponent textComponent;

  /**
   * Indicates if the selection should be visible or not.
   */
  private boolean selectionVisible = true;

  /**
   * The blink rate of this <code>Caret</code>.
   */
  private int blinkRate = 500;

  /**
   * The current dot position.
   */
  private int dot = 0;

  /**
   * The current mark position.
   */
  private int mark = 0;

  /**
   * The current visual caret position.
   */
  private Point magicCaretPosition = null;

  /**
   * Indicates if this <code>Caret</code> is currently visible or not. This is
   * package private to avoid an accessor method.
   */
  boolean visible = false;

  /**
   * The current highlight entry.
   */
  private Object highlightEntry;

  private Timer blinkTimer;
  
  private BlinkTimerListener blinkListener;

  /**
   * Creates a new <code>DefaultCaret</code> instance.
   */
  public DefaultCaret()
  {
    // Nothing to do here.
  }

  /**
   * Sets the Caret update policy.
   *    
   * @param policy the new policy.  Valid values are:
   * ALWAYS_UPDATE: always update the Caret position, even when Document
   * updates don't occur on the Event Dispatcher thread.
   * NEVER_UPDATE: don't update the Caret position unless the Document
   * length becomes less than the Caret position (then update the
   * Caret to the end of the Document).
   * UPDATE_WHEN_ON_EDT: update the Caret position when the 
   * Document updates occur on the Event Dispatcher thread.  This is the 
   * default.
   * 
   * @since 1.5
   * @throws IllegalArgumentException if policy is not one of the above.
   */
  public void setUpdatePolicy (int policy)
  {
    if (policy != ALWAYS_UPDATE && policy != NEVER_UPDATE
        && policy != UPDATE_WHEN_ON_EDT)
      throw new 
        IllegalArgumentException
        ("policy must be ALWAYS_UPDATE, NEVER__UPDATE, or UPDATE_WHEN_ON_EDT");
    this.policy = policy;
  }
  
  /**
   * Gets the caret update policy.
   * 
   * @return the caret update policy.
   * @since 1.5
   */
  public int getUpdatePolicy ()
  {
    return policy;
  }
  
  /**
   * Moves the caret position when the mouse is dragged over the text
   * component, modifying the selection accordingly.
   *
   * @param event the <code>MouseEvent</code> describing the drag operation
   */
  public void mouseDragged(MouseEvent event)
  {
    moveCaret(event);
  }

  /**
   * Indicates a mouse movement over the text component. Does nothing here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseMoved(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * When the click is received from Button 1 then the following actions
   * are performed here:
   *
   * <ul>
   * <li>If we receive a double click, the caret position (dot) is set
   *   to the position associated to the mouse click and the word at
   *   this location is selected.</li>
   * <li>If we receive a triple click, the caret position (dot) is set
   *   to the position associated to the mouse click and the line at
   *   this location is selected.</li>
   * </ul>
   *
   * @param event the <code>MouseEvent</code> describing the click operation
   */
  public void mouseClicked(MouseEvent event)
  {
    // TODO: Implement double- and triple-click behaviour here.
  }

  /**
   * Indicates that the mouse has entered the text component. Nothing is done
   * here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseEntered(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * Indicates that the mouse has exited the text component. Nothing is done
   * here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseExited(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * If the button 1 is pressed, the caret position is updated to the
   * position of the mouse click and the text component requests the input
   * focus if it is enabled. If the SHIFT key is held down, the caret will
   * be moved, which might select the text between the old and new location.
   *
   * @param event the <code>MouseEvent</code> describing the press operation
   */
  public void mousePressed(MouseEvent event)
  {
    positionCaret(event);
  }

  /**
   * Indicates that a mouse button has been released on the text component.
   * Nothing is done here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseReleased(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * Sets the caret to <code>visible</code> if the text component is editable.
   *
   * @param event the <code>FocusEvent</code>
   */
  public void focusGained(FocusEvent event)
  {
    setVisible(true);
  }

  /**
   * Sets the caret to <code>invisible</code>.
   *
   * @param event the <code>FocusEvent</code>
   */
  public void focusLost(FocusEvent event)
  {
    if (event.isTemporary() == false)
      setVisible(false);
  }

  /**
   * Moves the caret to the position specified in the <code>MouseEvent</code>.
   * This will cause a selection if the dot and mark are different.
   *
   * @param event the <code>MouseEvent</code> from which to fetch the position
   */
  protected void moveCaret(MouseEvent event)
  {
    int newDot = getComponent().viewToModel(event.getPoint());
    moveDot(newDot);
  }

  /**
   * Repositions the caret to the position specified in the
   * <code>MouseEvent</code>.
   *
   * @param event the <code>MouseEvent</code> from which to fetch the position
   */
  protected void positionCaret(MouseEvent event)
  {
    int newDot = getComponent().viewToModel(event.getPoint());
    setDot(newDot);
  }

  /**
   * Deinstalls this <code>Caret</code> from the specified
   * <code>JTextComponent</code>. This removes any listeners that have been
   * registered by this <code>Caret</code>.
   *
   * @param c the text component from which to install this caret
   */
  public void deinstall(JTextComponent c)
  {
    textComponent.removeFocusListener(this);
    textComponent.removeMouseListener(this);
    textComponent.removeMouseMotionListener(this);
    textComponent.getDocument().removeDocumentListener(documentListener);
    documentListener = null;
    textComponent.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
    textComponent = null;

    // Deinstall blink timer if present.
    if (blinkTimer != null)
      blinkTimer.stop();
    blinkTimer = null;
  }

  /**
   * Installs this <code>Caret</code> on the specified
   * <code>JTextComponent</code>. This registers a couple of listeners
   * on the text component.
   *
   * @param c the text component on which to install this caret
   */
  public void install(JTextComponent c)
  {
    textComponent = c;
    textComponent.addFocusListener(this);
    textComponent.addMouseListener(this);
    textComponent.addMouseMotionListener(this);
    propertyChangeListener = new PropertyChangeHandler();
    textComponent.addPropertyChangeListener(propertyChangeListener);
    documentListener = new DocumentHandler();
    textComponent.getDocument().addDocumentListener(documentListener);

    repaint();
  }

  /**
   * Sets the current visual position of this <code>Caret</code>.
   *
   * @param p the Point to use for the saved location. May be <code>null</code>
   *        to indicate that there is no visual location
   */
  public void setMagicCaretPosition(Point p)
  {
    magicCaretPosition = p;
  }

  /**
   * Returns the current visual position of this <code>Caret</code>.
   *
   * @return the current visual position of this <code>Caret</code>
   *
   * @see #setMagicCaretPosition
   */
  public Point getMagicCaretPosition()
  {
    return magicCaretPosition;
  }

  /**
   * Returns the current position of the <code>mark</code>. The
   * <code>mark</code> marks the location in the <code>Document</code> that
   * is the end of a selection. If there is no selection, the <code>mark</code>
   * is the same as the <code>dot</code>.
   *
   * @return the current position of the mark
   */
  public int getMark()
  {
    return mark;
  }

  private void handleHighlight()
  {
    Highlighter highlighter = textComponent.getHighlighter();
    
    if (highlighter == null)
      return;
    
    int p0 = Math.min(dot, mark);
    int p1 = Math.max(dot, mark);
    
    if (selectionVisible && p0 != p1)
      {
	try
	  {
	    if (highlightEntry == null)
	      highlightEntry = highlighter.addHighlight(p0, p1, getSelectionPainter());
	    else
	      highlighter.changeHighlight(highlightEntry, p0, p1);
	  }
	catch (BadLocationException e)
	  {
	    // This should never happen.
	    throw new InternalError();
	  }
      }
    else
      {
	if (highlightEntry != null)
	  {
	    highlighter.removeHighlight(highlightEntry);
	    highlightEntry = null;
	  }
      }
  }

  /**
   * Sets the visiblity state of the selection.
   *
   * @param v <code>true</code> if the selection should be visible,
   *        <code>false</code> otherwise
   */
  public void setSelectionVisible(boolean v)
  {
    if (selectionVisible == v)
      return;
    
    selectionVisible = v;
    handleHighlight();
    repaint();
  }

  /**
   * Returns <code>true</code> if the selection is currently visible,
   * <code>false</code> otherwise.
   *
   * @return <code>true</code> if the selection is currently visible,
   *         <code>false</code> otherwise
   */
  public boolean isSelectionVisible()
  {
    return selectionVisible;
  }

  /**
   * Causes the <code>Caret</code> to repaint itself.
   */
  protected final void repaint()
  {
    getComponent().repaint(x, y, width, height);
  }

  /**
   * Paints this <code>Caret</code> using the specified <code>Graphics</code>
   * context.
   *
   * @param g the graphics context to use
   */
  public void paint(Graphics g)
  {
    JTextComponent comp = getComponent();
    if (comp == null)
      return;

    int dot = getDot();
    Rectangle rect = null;

    try
      {
        rect = textComponent.modelToView(dot);
      }
    catch (BadLocationException e)
      {
        assert false : "Unexpected bad caret location: " + dot;
        return;
      }

    if (rect == null)
      return;

    // Check if paint has possibly been called directly, without a previous
    // call to damage(). In this case we need to do some cleanup first.
    if ((x != rect.x) || (y != rect.y))
      {
        repaint(); // Erase previous location of caret.
        x = rect.x;
        y = rect.y;
        width = 1;
        height = rect.height;
      }

    // Now draw the caret on the new position if visible.
    if (visible)
      {
        g.setColor(textComponent.getCaretColor());
        g.drawLine(rect.x, rect.y, rect.x, rect.y + rect.height);
      }
  }

  /**
   * Returns all registered event listeners of the specified type.
   *
   * @param listenerType the type of listener to return
   *
   * @return all registered event listeners of the specified type
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Registers a {@link ChangeListener} that is notified whenever that state
   * of this <code>Caret</code> changes.
   *
   * @param listener the listener to register to this caret
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Removes a {@link ChangeListener} from the list of registered listeners.
   *
   * @param listener the listener to remove
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Returns all registered {@link ChangeListener}s of this <code>Caret</code>.
   *
   * @return all registered {@link ChangeListener}s of this <code>Caret</code>
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }

  /**
   * Notifies all registered {@link ChangeListener}s that the state
   * of this <code>Caret</code> has changed.
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].stateChanged(changeEvent);
  }

  /**
   * Returns the <code>JTextComponent</code> on which this <code>Caret</code>
   * is installed.
   *
   * @return the <code>JTextComponent</code> on which this <code>Caret</code>
   *         is installed
   */
  protected final JTextComponent getComponent()
  {
    return textComponent;
  }

  /**
   * Returns the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @return the blink rate of this <code>Caret</code> or <code>0</code> if
   *         this caret does not blink
   */
  public int getBlinkRate()
  {
    return blinkRate;
  }

  /**
   * Sets the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @param rate the new blink rate to set
   */
  public void setBlinkRate(int rate)
  {
    if (blinkTimer != null)
      blinkTimer.setDelay(rate);
    blinkRate = rate;
  }

  /**
   * Returns the current position of this <code>Caret</code> within the
   * <code>Document</code>.
   *
   * @return the current position of this <code>Caret</code> within the
   *         <code>Document</code>
   */
  public int getDot()
  {
    return dot;
  }

  /**
   * Moves the <code>dot</code> location without touching the
   * <code>mark</code>. This is used when making a selection.
   *
   * @param dot the location where to move the dot
   *
   * @see #setDot(int)
   */
  public void moveDot(int dot)
  {
    if (dot >= 0)
      {
        this.dot = dot;
        handleHighlight();
        adjustVisibility(this);
        appear();
      }
  }

  /**
   * Sets the current position of this <code>Caret</code> within the
   * <code>Document</code>. This also sets the <code>mark</code> to the new
   * location.
   * 
   * @param dot
   *          the new position to be set
   * @see #moveDot(int)
   */
  public void setDot(int dot)
  {
    if (dot >= 0)
      {
        this.mark = dot;
        this.dot = dot;
        handleHighlight();
        adjustVisibility(this);
        appear();
      }
  }
  
  /**
   * Show the caret (may be hidden due blinking) and adjust the timer not to
   * hide it (possibly immediately).
   * 
   * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
   */
  void appear()
  {
    // All machinery is only required if the carret is blinking.
    if (blinkListener != null)
      {
        blinkListener.ignoreNextEvent = true;

        // If the caret is visible, erase the current position by repainting
        // over.
        if (visible)
          repaint();

        // Draw the caret in the new position.
        visible = true;

        Rectangle area = null;
        try
          {
            area = getComponent().modelToView(getDot());
          }
        catch (BadLocationException ex)
          {
            assert false : "Unexpected bad caret location: " + getDot();
          }
        if (area != null)
          damage(area);
      }
    repaint();
  }  

  /**
   * Returns <code>true</code> if this <code>Caret</code> is currently visible,
   * and <code>false</code> if it is not.
   *
   * @return <code>true</code> if this <code>Caret</code> is currently visible,
   *         and <code>false</code> if it is not
   */
  public boolean isVisible()
  {
    return visible;
  }

  /**
   * Sets the visibility state of the caret. <code>true</code> shows the
   * <code>Caret</code>, <code>false</code> hides it.
   *
   * @param v the visibility to set
   */  
  public void setVisible(boolean v)
  {
    if (v != visible)
      {
        visible = v;
        if (visible)
          if (textComponent.isEnabled() && textComponent.isEditable())
            {
              if (blinkTimer == null)
                initBlinkTimer();
              blinkTimer.start();
            }
        else
          {
            if (blinkTimer != null)
              blinkTimer.stop();
          }
        Rectangle area = null;
        try
          {            
            area = getComponent().modelToView(getDot());
          }
        catch (BadLocationException ex)
          {
            assert false: "Unexpected bad caret location: " + getDot();
          }
        if (area != null)
          damage(area);
      }
  }

  /**
   * Returns the {@link Highlighter.HighlightPainter} that should be used
   * to paint the selection.
   *
   * @return the {@link Highlighter.HighlightPainter} that should be used
   *         to paint the selection
   */
  protected Highlighter.HighlightPainter getSelectionPainter()
  {
    return DefaultHighlighter.DefaultPainter;
  }

  /**
   * Updates the carets rectangle properties to the specified rectangle and
   * repaints the caret.
   *
   * @param r the rectangle to set as the caret rectangle
   */
  protected void damage(Rectangle r)
  {
    if (r == null)
      return;
    x = r.x;
    y = r.y;
    width = 1;
    // height is normally set in paint and we leave it untouched. However, we
    // must set a valid value here, since otherwise the painting mechanism
    // sets a zero clip and never calls paint.
    if (height <= 0)
      height = getComponent().getHeight();
    repaint();
  }

  /**
   * Adjusts the text component so that the caret is visible. This default
   * implementation simply calls
   * {@link JComponent#scrollRectToVisible(Rectangle)} on the text component.
   * Subclasses may wish to change this.
   */
  protected void adjustVisibility(Rectangle rect)
  {
    getComponent().scrollRectToVisible(rect);
  }

  /**
   * Initializes the blink timer.
   */
  private void initBlinkTimer()
  {
    // Setup the blink timer.
    blinkListener = new BlinkTimerListener();
    blinkTimer = new Timer(getBlinkRate(), blinkListener);
    blinkTimer.setRepeats(true);
  }
}
