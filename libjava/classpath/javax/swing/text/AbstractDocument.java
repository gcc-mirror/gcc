/* AbstractDocument.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import java.awt.font.TextAttribute;
import java.io.PrintStream;
import java.io.Serializable;
import java.text.Bidi;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DocumentFilter;
import javax.swing.tree.TreeNode;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

/**
 * An abstract base implementation for the {@link Document} interface.
 * This class provides some common functionality for all <code>Element</code>s,
 * most notably it implements a locking mechanism to make document modification
 * thread-safe.
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public abstract class AbstractDocument implements Document, Serializable
{
  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 6842927725919637215L;

  /**
   * Standard error message to indicate a bad location.
   */
  protected static final String BAD_LOCATION = "document location failure";

  /**
   * Standard name for unidirectional <code>Element</code>s.
   */
  public static final String BidiElementName = "bidi level";

  /**
   * Standard name for content <code>Element</code>s. These are usually
   * {@link LeafElement}s.
   */
  public static final String ContentElementName = "content";

  /**
   * Standard name for paragraph <code>Element</code>s. These are usually
   * {@link BranchElement}s.
   */
  public static final String ParagraphElementName = "paragraph";

  /**
   * Standard name for section <code>Element</code>s. These are usually
   * {@link DefaultStyledDocument.SectionElement}s.
   */
  public static final String SectionElementName = "section";

  /**
   * Attribute key for storing the element name.
   */
  public static final String ElementNameAttribute = "$ename";

  /**
   * Standard name for the bidi root element.
   */
  private static final String BidiRootName = "bidi root";

  /**
   * Key for storing the asynchronous load priority.
   */
  private static final String AsyncLoadPriority = "load priority";

  /**
   * Key for storing the I18N state.
   */
  private static final String I18N = "i18n";

  /**
   * The actual content model of this <code>Document</code>.
   */
  Content content;

  /**
   * The AttributeContext for this <code>Document</code>.
   */
  AttributeContext context;

  /**
   * The currently installed <code>DocumentFilter</code>.
   */
  DocumentFilter documentFilter;

  /**
   * The documents properties.
   */
  Dictionary properties;

  /**
   * Manages event listeners for this <code>Document</code>.
   */
  protected EventListenerList listenerList = new EventListenerList();
  
  /**
   * Stores the current writer thread.  Used for locking.
   */ 
  private Thread currentWriter = null;
  
  /**
   * The number of readers.  Used for locking.
   */
  private int numReaders = 0;
  
  /**
   * The number of current writers. If this is > 1 then the same thread entered
   * the write lock more than once.
   */
  private int numWriters = 0;  

  /** An instance of a DocumentFilter.FilterBypass which allows calling
   * the insert, remove and replace method without checking for an installed
   * document filter.
   */
  private DocumentFilter.FilterBypass bypass;

  /**
   * The bidi root element.
   */
  private BidiRootElement bidiRoot;

  /**
   * True when we are currently notifying any listeners. This is used
   * to detect illegal situations in writeLock().
   */
  private transient boolean notifyListeners;

  /**
   * Creates a new <code>AbstractDocument</code> with the specified
   * {@link Content} model.
   *
   * @param doc the <code>Content</code> model to be used in this
   *        <code>Document<code>
   *
   * @see GapContent
   * @see StringContent
   */
  protected AbstractDocument(Content doc)
  {
    this(doc, StyleContext.getDefaultStyleContext());
  }

  /**
   * Creates a new <code>AbstractDocument</code> with the specified
   * {@link Content} model and {@link AttributeContext}.
   *
   * @param doc the <code>Content</code> model to be used in this
   *        <code>Document<code>
   * @param ctx the <code>AttributeContext</code> to use
   *
   * @see GapContent
   * @see StringContent
   */
  protected AbstractDocument(Content doc, AttributeContext ctx)
  {
    content = doc;
    context = ctx;

    // FIXME: Fully implement bidi.
    bidiRoot = new BidiRootElement();

    // FIXME: This is determined using a Mauve test. Make the document
    // actually use this.
    putProperty(I18N, Boolean.FALSE);

    // Add one child to the bidi root.
    writeLock();
    try
      {
        Element[] children = new Element[1];
        children[0] = new BidiElement(bidiRoot, 0, 1, 0);
        bidiRoot.replace(0, 0, children);
      }
    finally
      {
        writeUnlock();
      }
  }
  
  /** Returns the DocumentFilter.FilterBypass instance for this
   * document and create it if it does not exist yet.
   *  
   * @return This document's DocumentFilter.FilterBypass instance.
   */
  private DocumentFilter.FilterBypass getBypass()
  {
    if (bypass == null)
      bypass = new Bypass();
    
    return bypass;
  }

  /**
   * Returns the paragraph {@link Element} that holds the specified position.
   *
   * @param pos the position for which to get the paragraph element
   *
   * @return the paragraph {@link Element} that holds the specified position
   */
  public abstract Element getParagraphElement(int pos);

  /**
   * Returns the default root {@link Element} of this <code>Document</code>.
   * Usual <code>Document</code>s only have one root element and return this.
   * However, there may be <code>Document</code> implementations that
   * support multiple root elements, they have to return a default root element
   * here.
   *
   * @return the default root {@link Element} of this <code>Document</code>
   */
  public abstract Element getDefaultRootElement();

  /**
   * Creates and returns a branch element with the specified
   * <code>parent</code> and <code>attributes</code>. Note that the new
   * <code>Element</code> is linked to the parent <code>Element</code>
   * through {@link Element#getParentElement}, but it is not yet added
   * to the parent <code>Element</code> as child.
   *
   * @param parent the parent <code>Element</code> for the new branch element
   * @param attributes the text attributes to be installed in the new element
   *
   * @return the new branch <code>Element</code>
   *
   * @see BranchElement
   */
  protected Element createBranchElement(Element parent,
					AttributeSet attributes)
  {
    return new BranchElement(parent, attributes);
  }

  /**
   * Creates and returns a leaf element with the specified
   * <code>parent</code> and <code>attributes</code>. Note that the new
   * <code>Element</code> is linked to the parent <code>Element</code>
   * through {@link Element#getParentElement}, but it is not yet added
   * to the parent <code>Element</code> as child.
   *
   * @param parent the parent <code>Element</code> for the new branch element
   * @param attributes the text attributes to be installed in the new element
   *
   * @return the new branch <code>Element</code>
   *
   * @see LeafElement
   */
  protected Element createLeafElement(Element parent, AttributeSet attributes,
				      int start, int end)
  {
    return new LeafElement(parent, attributes, start, end);
  }

  /**
   * Creates a {@link Position} that keeps track of the location at the
   * specified <code>offset</code>.
   *
   * @param offset the location in the document to keep track by the new
   *        <code>Position</code>
   *
   * @return the newly created <code>Position</code>
   *
   * @throws BadLocationException if <code>offset</code> is not a valid
   *         location in the documents content model
   */
  public synchronized Position createPosition(final int offset)
    throws BadLocationException
  {
    return content.createPosition(offset);
  }

  /**
   * Notifies all registered listeners when the document model changes.
   *
   * @param event the <code>DocumentEvent</code> to be fired
   */
  protected void fireChangedUpdate(DocumentEvent event)
  {
    notifyListeners = true;
    try
      {
        DocumentListener[] listeners = getDocumentListeners();
        for (int index = 0; index < listeners.length; ++index)
          listeners[index].changedUpdate(event);
      }
    finally
      {
        notifyListeners = false;
      }
  }

  /**
   * Notifies all registered listeners when content is inserted in the document
   * model.
   *
   * @param event the <code>DocumentEvent</code> to be fired
   */
  protected void fireInsertUpdate(DocumentEvent event)
  {
    notifyListeners = true;
    try
      {
        DocumentListener[] listeners = getDocumentListeners();
        for (int index = 0; index < listeners.length; ++index)
          listeners[index].insertUpdate(event);
      }
    finally
      {
        notifyListeners = false;
      }
  }

  /**
   * Notifies all registered listeners when content is removed from the
   * document model.
   *
   * @param event the <code>DocumentEvent</code> to be fired
   */
  protected void fireRemoveUpdate(DocumentEvent event)
  {
    notifyListeners = true;
    try
      {
        DocumentListener[] listeners = getDocumentListeners();
        for (int index = 0; index < listeners.length; ++index)
          listeners[index].removeUpdate(event);
      }
    finally
      {
        notifyListeners = false;
      }
  }

  /**
   * Notifies all registered listeners when an <code>UndoableEdit</code> has
   * been performed on this <code>Document</code>.
   *
   * @param event the <code>UndoableEditEvent</code> to be fired
   */
  protected void fireUndoableEditUpdate(UndoableEditEvent event)
  {
    UndoableEditListener[] listeners = getUndoableEditListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].undoableEditHappened(event);
  }

  /**
   * Returns the asynchronous loading priority. Returns <code>-1</code> if this
   * document should not be loaded asynchronously.
   *
   * @return the asynchronous loading priority
   */
  public int getAsynchronousLoadPriority()
  {
    Object val = getProperty(AsyncLoadPriority);
    int prio = -1;
    if (val != null)
      prio = ((Integer) val).intValue(); 
    return prio;
  }

  /**
   * Returns the {@link AttributeContext} used in this <code>Document</code>.
   *
   * @return the {@link AttributeContext} used in this <code>Document</code>
   */
  protected final AttributeContext getAttributeContext()
  {
    return context;
  }

  /**
   * Returns the root element for bidirectional content.
   *
   * @return the root element for bidirectional content
   */
  public Element getBidiRootElement()
  {
    return bidiRoot;
  }

  /**
   * Returns the {@link Content} model for this <code>Document</code>
   *
   * @return the {@link Content} model for this <code>Document</code>
   *
   * @see GapContent
   * @see StringContent
   */
  protected final Content getContent()
  {
    return content;
  }

  /**
   * Returns the thread that currently modifies this <code>Document</code>
   * if there is one, otherwise <code>null</code>. This can be used to
   * distinguish between a method call that is part of an ongoing modification
   * or if it is a separate modification for which a new lock must be aquired.
   *
   * @return the thread that currently modifies this <code>Document</code>
   *         if there is one, otherwise <code>null</code>
   */
  protected final synchronized Thread getCurrentWriter()
  {
    return currentWriter;
  }

  /**
   * Returns the properties of this <code>Document</code>.
   *
   * @return the properties of this <code>Document</code>
   */
  public Dictionary<Object, Object> getDocumentProperties()
  {
    // FIXME: make me thread-safe
    if (properties == null)
      properties = new Hashtable();

    return properties;
  }

  /**
   * Returns a {@link Position} which will always mark the end of the
   * <code>Document</code>.
   *
   * @return a {@link Position} which will always mark the end of the
   *         <code>Document</code>
   */
  public final Position getEndPosition()
  {
    Position p;
    try
      {
        p = createPosition(content.length());
      }
    catch (BadLocationException ex)
      {
        // Shouldn't really happen.
        p = null;
      }
    return p;
  }

  /**
   * Returns the length of this <code>Document</code>'s content.
   *
   * @return the length of this <code>Document</code>'s content
   */
  public int getLength()
  {
    // We return Content.getLength() -1 here because there is always an
    // implicit \n at the end of the Content which does count in Content
    // but not in Document.
    return content.length() - 1;
  }

  /**
   * Returns all registered listeners of a given listener type.
   *
   * @param listenerType the type of the listeners to be queried
   *
   * @return all registered listeners of the specified type
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Returns a property from this <code>Document</code>'s property list.
   *
   * @param key the key of the property to be fetched
   *
   * @return the property for <code>key</code> or <code>null</code> if there
   *         is no such property stored
   */
  public final Object getProperty(Object key)
  {
    // FIXME: make me thread-safe
    Object value = null;
    if (properties != null)
      value = properties.get(key);

    return value;
  }

  /**
   * Returns all root elements of this <code>Document</code>. By default
   * this just returns the single root element returned by
   * {@link #getDefaultRootElement()}. <code>Document</code> implementations
   * that support multiple roots must override this method and return all roots
   * here.
   *
   * @return all root elements of this <code>Document</code>
   */
  public Element[] getRootElements()
  {
    Element[] elements = new Element[2];
    elements[0] = getDefaultRootElement();
    elements[1] = getBidiRootElement();
    return elements;
  }

  /**
   * Returns a {@link Position} which will always mark the beginning of the
   * <code>Document</code>.
   *
   * @return a {@link Position} which will always mark the beginning of the
   *         <code>Document</code>
   */
  public final Position getStartPosition()
  {
    Position p;
    try
      {
        p = createPosition(0);
      }
    catch (BadLocationException ex)
      {
        // Shouldn't really happen.
        p = null;
      }
    return p;
  }

  /**
   * Returns a piece of this <code>Document</code>'s content.
   *
   * @param offset the start offset of the content
   * @param length the length of the content
   *
   * @return the piece of content specified by <code>offset</code> and
   *         <code>length</code>
   *
   * @throws BadLocationException if <code>offset</code> or <code>offset +
   *         length</code> are invalid locations with this
   *         <code>Document</code>
   */
  public String getText(int offset, int length) throws BadLocationException
  {
    return content.getString(offset, length);
  }

  /**
   * Fetches a piece of this <code>Document</code>'s content and stores
   * it in the given {@link Segment}.
   *
   * @param offset the start offset of the content
   * @param length the length of the content
   * @param segment the <code>Segment</code> to store the content in
   *
   * @throws BadLocationException if <code>offset</code> or <code>offset +
   *         length</code> are invalid locations with this
   *         <code>Document</code>
   */
  public void getText(int offset, int length, Segment segment)
    throws BadLocationException
  {
    content.getChars(offset, length, segment);
  }

  /**
   * Inserts a String into this <code>Document</code> at the specified
   * position and assigning the specified attributes to it.
   * 
   * <p>If a {@link DocumentFilter} is installed in this document, the
   * corresponding method of the filter object is called.</p>
   * 
   * <p>The method has no effect when <code>text</code> is <code>null</code>
   * or has a length of zero.</p>
   * 
   *
   * @param offset the location at which the string should be inserted
   * @param text the content to be inserted
   * @param attributes the text attributes to be assigned to that string
   *
   * @throws BadLocationException if <code>offset</code> is not a valid
   *         location in this <code>Document</code>
   */
  public void insertString(int offset, String text, AttributeSet attributes)
    throws BadLocationException
  {
    // Bail out if we have a bogus insertion (Behavior observed in RI).
    if (text == null || text.length() == 0)
      return;

    writeLock();
    try
      {
        if (documentFilter == null)
          insertStringImpl(offset, text, attributes);
        else
          documentFilter.insertString(getBypass(), offset, text, attributes);
      }
    finally
      {
        writeUnlock();
      }
  }

  void insertStringImpl(int offset, String text, AttributeSet attributes)
    throws BadLocationException
  {
    // Just return when no text to insert was given.
    if (text == null || text.length() == 0)
      return;
    DefaultDocumentEvent event =
      new DefaultDocumentEvent(offset, text.length(),
			       DocumentEvent.EventType.INSERT);

    UndoableEdit undo = content.insertString(offset, text);
    if (undo != null)
      event.addEdit(undo);

    // Check if we need bidi layout.
    if (getProperty(I18N).equals(Boolean.FALSE))
      {
        Object dir = getProperty(TextAttribute.RUN_DIRECTION);
        if (TextAttribute.RUN_DIRECTION_RTL.equals(dir))
          putProperty(I18N, Boolean.TRUE);
        else
          {
            char[] chars = text.toCharArray();
            if (Bidi.requiresBidi(chars, 0, chars.length))
              putProperty(I18N, Boolean.TRUE);
          }
      }

    insertUpdate(event, attributes);

    fireInsertUpdate(event);

    if (undo != null)
      fireUndoableEditUpdate(new UndoableEditEvent(this, undo));
  }

  /**
   * Called to indicate that text has been inserted into this
   * <code>Document</code>. The default implementation does nothing.
   * This method is executed within a write lock.
   *
   * @param chng the <code>DefaultDocumentEvent</code> describing the change
   * @param attr the attributes of the changed content
   */
  protected void insertUpdate(DefaultDocumentEvent chng, AttributeSet attr)
  {
    if (Boolean.TRUE.equals(getProperty(I18N)))
      updateBidi(chng);
  }

  /**
   * Called after some content has been removed from this
   * <code>Document</code>. The default implementation does nothing.
   * This method is executed within a write lock.
   *
   * @param chng the <code>DefaultDocumentEvent</code> describing the change
   */
  protected void postRemoveUpdate(DefaultDocumentEvent chng)
  {
    if (Boolean.TRUE.equals(getProperty(I18N)))
      updateBidi(chng);
  }

  /**
   * Stores a property in this <code>Document</code>'s property list.
   *
   * @param key the key of the property to be stored
   * @param value the value of the property to be stored
   */
  public final void putProperty(Object key, Object value)
  {
    // FIXME: make me thread-safe
    if (properties == null)
      properties = new Hashtable();

    if (value == null)
      properties.remove(key);
    else
      properties.put(key, value);

    // Update bidi structure if the RUN_DIRECTION is set.
    if (TextAttribute.RUN_DIRECTION.equals(key))
      {
        if (TextAttribute.RUN_DIRECTION_RTL.equals(value)
            && Boolean.FALSE.equals(getProperty(I18N)))
          putProperty(I18N, Boolean.TRUE);

        if (Boolean.TRUE.equals(getProperty(I18N)))
          {
            writeLock();
            try
              {
                DefaultDocumentEvent ev =
                  new DefaultDocumentEvent(0, getLength(),
                                           DocumentEvent.EventType.INSERT);
                updateBidi(ev);
              }
            finally
              {
                writeUnlock();
              }
          }
      }
  }

  /**
   * Updates the bidi element structure.
   *
   * @param ev the document event for the change
   */
  private void updateBidi(DefaultDocumentEvent ev)
  {
    // Determine start and end offset of the paragraphs to be scanned.
    int start = 0;
    int end = 0;
    DocumentEvent.EventType type = ev.getType();
    if (type == DocumentEvent.EventType.INSERT
        || type == DocumentEvent.EventType.CHANGE)
      {
        int offs = ev.getOffset();
        int endOffs = offs + ev.getLength();
        start = getParagraphElement(offs).getStartOffset();
        end = getParagraphElement(endOffs).getEndOffset();
      }
    else if (type == DocumentEvent.EventType.REMOVE)
      {
        Element par = getParagraphElement(ev.getOffset());
        start = par.getStartOffset();
        end = par.getEndOffset();
      }
    else
      assert false : "Unknown event type";

    // Determine the bidi levels for the affected range.
    Bidi[] bidis = getBidis(start, end);

    int removeFrom = 0;
    int removeTo = 0;

    int offs = 0;
    int lastRunStart = 0;
    int lastRunEnd = 0;
    int lastRunLevel = 0;
    ArrayList newEls = new ArrayList();
    for (int i = 0; i < bidis.length; i++)
      {
        Bidi bidi = bidis[i];
        int numRuns = bidi.getRunCount();
        for (int r = 0; r < numRuns; r++)
          {
            if (r == 0 && i == 0)
              {
                if (start > 0)
                  {
                    // Try to merge with the previous element if it has the
                    // same bidi level as the first run.
                    int prevElIndex = bidiRoot.getElementIndex(start - 1);
                    removeFrom = prevElIndex;
                    Element prevEl = bidiRoot.getElement(prevElIndex);
                    AttributeSet atts = prevEl.getAttributes();
                    int prevElLevel = StyleConstants.getBidiLevel(atts);
                    if (prevElLevel == bidi.getRunLevel(r))
                      {
                        // Merge previous element with current run.
                        lastRunStart = prevEl.getStartOffset() - start;
                        lastRunEnd = bidi.getRunLimit(r);
                        lastRunLevel  = bidi.getRunLevel(r);
                      }
                    else if (prevEl.getEndOffset() > start)
                      {
                        // Split previous element and replace by 2 new elements.
                        lastRunStart = 0;
                        lastRunEnd = bidi.getRunLimit(r);
                        lastRunLevel = bidi.getRunLevel(r);
                        newEls.add(new BidiElement(bidiRoot,
                                                   prevEl.getStartOffset(),
                                                   start, prevElLevel));
                      }
                    else
                      {
                        // Simply start new run at start location.
                        lastRunStart = 0;
                        lastRunEnd = bidi.getRunLimit(r);
                        lastRunLevel = bidi.getRunLevel(r);
                        removeFrom++;
                      }
                  }
                else
                  {
                    // Simply start new run at start location.
                    lastRunStart = 0;
                    lastRunEnd = bidi.getRunLimit(r);
                    lastRunLevel = bidi.getRunLevel(r);
                    removeFrom = 0;
                  }
              }
            if (i == bidis.length - 1 && r == numRuns - 1)
              {
                if (end <= getLength())
                  {
                    // Try to merge last element with next element.
                    int nextIndex = bidiRoot.getElementIndex(end);
                    Element nextEl = bidiRoot.getElement(nextIndex);
                    AttributeSet atts = nextEl.getAttributes();
                    int nextLevel = StyleConstants.getBidiLevel(atts);
                    int level = bidi.getRunLevel(r);
                    if (lastRunLevel == level && level == nextLevel)
                      {
                        // Merge runs together.
                        if (lastRunStart + start == nextEl.getStartOffset())
                          removeTo = nextIndex - 1;
                        else
                          {
                            newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                                       nextEl.getEndOffset(), level));
                            removeTo = nextIndex;
                          }
                      }
                    else if (lastRunLevel == level)
                      {
                        // Merge current and last run.
                        int endOffs = offs + bidi.getRunLimit(r);
                        newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                                   start + endOffs, level));
                        if (start + endOffs == nextEl.getStartOffset())
                          removeTo = nextIndex - 1;
                        else
                          {
                            newEls.add(new BidiElement(bidiRoot, start + endOffs,
                                                       nextEl.getEndOffset(),
                                                       nextLevel));
                            removeTo = nextIndex;
                          }
                      }
                    else if (level == nextLevel)
                      {
                        // Merge current and next run.
                        newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                                   start + lastRunEnd,
                                                   lastRunLevel));
                        newEls.add(new BidiElement(bidiRoot, start + lastRunEnd,
                                                   nextEl.getEndOffset(), level));
                        removeTo = nextIndex;
                      }
                    else
                      {
                        // Split next element.
                        int endOffs = offs + bidi.getRunLimit(r);
                        newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                                   start + lastRunEnd,
                                                   lastRunLevel));
                        newEls.add(new BidiElement(bidiRoot, start + lastRunEnd,
                                                   start + endOffs, level));
                        newEls.add(new BidiElement(bidiRoot, start + endOffs,
                                                   nextEl.getEndOffset(),
                                                   nextLevel));
                        removeTo = nextIndex;
                      }
                  }
                else
                  {
                    removeTo = bidiRoot.getElementIndex(end);
                    int level = bidi.getRunLevel(r);
                    int runEnd = offs + bidi.getRunLimit(r);

                    if (level == lastRunLevel)
                      {
                        // Merge with previous.
                        lastRunEnd = offs + runEnd;
                        newEls.add(new BidiElement(bidiRoot,
                                                  start + lastRunStart,
                                                  start + runEnd, level));
                      }
                    else
                      {
                        // Create element for last run and current run.
                        newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                                   start + lastRunEnd,
                                                   lastRunLevel));
                        newEls.add(new BidiElement(bidiRoot,
                                                   start + lastRunEnd,
                                                   start + runEnd,
                                                   level));
                       }
                  }
              }
            else
              {
                int level = bidi.getRunLevel(r);
                int runEnd = bidi.getRunLimit(r);

                if (level == lastRunLevel)
                  {
                    // Merge with previous.
                    lastRunEnd = offs + runEnd;
                  }
                else
                  {
                    // Create element for last run and update values for
                    // current run.
                    newEls.add(new BidiElement(bidiRoot, start + lastRunStart,
                                               start + lastRunEnd,
                                               lastRunLevel));
                    lastRunStart = lastRunEnd;
                    lastRunEnd = offs + runEnd;
                    lastRunLevel = level;
                  }
              }
          }
        offs += bidi.getLength();
      }

    // Determine the bidi elements which are to be removed.
    int numRemoved = 0;
    if (bidiRoot.getElementCount() > 0)
      numRemoved = removeTo - removeFrom + 1;
    Element[] removed = new Element[numRemoved];
    for (int i = 0; i < numRemoved; i++)
      removed[i] = bidiRoot.getElement(removeFrom + i);

    Element[] added = new Element[newEls.size()];
    added = (Element[]) newEls.toArray(added);

    // Update the event.
    ElementEdit edit = new ElementEdit(bidiRoot, removeFrom, removed, added);
    ev.addEdit(edit);

    // Update the structure.
    bidiRoot.replace(removeFrom, numRemoved, added);
  }

  /**
   * Determines the Bidi objects for the paragraphs in the specified range.
   *
   * @param start the start of the range
   * @param end the end of the range
   *
   * @return the Bidi analysers for the paragraphs in the range
   */
  private Bidi[] getBidis(int start, int end)
  {
    // Determine the default run direction from the document property.
    Boolean defaultDir = null;
    Object o = getProperty(TextAttribute.RUN_DIRECTION);
    if (o instanceof Boolean)
      defaultDir = (Boolean) o;

    // Scan paragraphs and add their level arrays to the overall levels array.
    ArrayList bidis = new ArrayList();
    Segment s = new Segment();
    for (int i = start; i < end;)
      {
        Element par = getParagraphElement(i);
        int pStart = par.getStartOffset();
        int pEnd = par.getEndOffset();

        // Determine the default run direction of the paragraph.
        Boolean dir = defaultDir;
        o = par.getAttributes().getAttribute(TextAttribute.RUN_DIRECTION);
        if (o instanceof Boolean)
          dir = (Boolean) o;

        // Bidi over the paragraph.
        try
          {
            getText(pStart, pEnd - pStart, s);
          }
        catch (BadLocationException ex)
          {
            assert false : "Must not happen";
          }
        int flag = Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT;
        if (dir != null)
          {
            if (TextAttribute.RUN_DIRECTION_LTR.equals(dir))
              flag = Bidi.DIRECTION_LEFT_TO_RIGHT;
            else
              flag = Bidi.DIRECTION_RIGHT_TO_LEFT;
          }
        Bidi bidi = new Bidi(s.array, s.offset, null, 0, s.count, flag);
        bidis.add(bidi);
        i = pEnd;
      }
    Bidi[] ret = new Bidi[bidis.size()];
    ret = (Bidi[]) bidis.toArray(ret);
    return ret;
  }

  /**
   * Blocks until a read lock can be obtained.  Must block if there is
   * currently a writer modifying the <code>Document</code>.
   */
  public final synchronized void readLock()
  {
    try
      {
        while (currentWriter != null)
          {
            if (currentWriter == Thread.currentThread())
              return;
            wait();
          }
        numReaders++;
      }
    catch (InterruptedException ex)
      {
        throw new Error("Interrupted during grab read lock");
      }
  }

  /**
   * Releases the read lock. If this was the only reader on this
   * <code>Document</code>, writing may begin now.
   */
  public final synchronized void readUnlock()
  {
    // Note we could have a problem here if readUnlock was called without a
    // prior call to readLock but the specs simply warn users to ensure that
    // balance by using a finally block:
    // readLock()
    // try
    // { 
    //   doSomethingHere 
    // }
    // finally
    // {
    //   readUnlock();
    // }
    
    // All that the JDK seems to check for is that you don't call unlock
    // more times than you've previously called lock, but it doesn't make
    // sure that the threads calling unlock were the same ones that called lock

    // If the current thread holds the write lock, and attempted to also obtain
    // a readLock, then numReaders hasn't been incremented and we don't need
    // to unlock it here.
    if (currentWriter == Thread.currentThread())
      return;

    // FIXME: the reference implementation throws a 
    // javax.swing.text.StateInvariantError here
    if (numReaders <= 0)
      throw new IllegalStateException("document lock failure");
    
    // If currentWriter is not null, the application code probably had a 
    // writeLock and then tried to obtain a readLock, in which case 
    // numReaders wasn't incremented
    numReaders--;
    notify();
  }

  /**
   * Removes a piece of content from this <code>Document</code>.
   * 
   * <p>If a {@link DocumentFilter} is installed in this document, the
   * corresponding method of the filter object is called. The
   * <code>DocumentFilter</code> is called even if <code>length</code>
   * is zero. This is different from {@link #replace}.</p>
   * 
   * <p>Note: When <code>length</code> is zero or below the call is not
   * forwarded to the underlying {@link AbstractDocument.Content} instance
   * of this document and no exception is thrown.</p>
   * 
   * @param offset the start offset of the fragment to be removed
   * @param length the length of the fragment to be removed
   *
   * @throws BadLocationException if <code>offset</code> or
   *         <code>offset + length</code> or invalid locations within this
   *         document
   */
  public void remove(int offset, int length) throws BadLocationException
  {
    writeLock();
    try
      {
        DocumentFilter f = getDocumentFilter();
        if (f == null)
          removeImpl(offset, length);
        else
          f.remove(getBypass(), offset, length);
      }
    finally
      {
        writeUnlock();
      }
  }

  void removeImpl(int offset, int length) throws BadLocationException
  {
    // The RI silently ignores all requests that have a negative length.
    // Don't ask my why, but that's how it is.
    if (length > 0)
      {
        if (offset < 0 || offset > getLength())
          throw new BadLocationException("Invalid remove position", offset);

        if (offset + length > getLength())
          throw new BadLocationException("Invalid remove length", offset);

        DefaultDocumentEvent event =
          new DefaultDocumentEvent(offset, length,
                                   DocumentEvent.EventType.REMOVE);
    
        // The order of the operations below is critical!        
        removeUpdate(event);
        UndoableEdit temp = content.remove(offset, length);

        postRemoveUpdate(event);
        fireRemoveUpdate(event);
      }
  }

  /**
   * Replaces a piece of content in this <code>Document</code> with
   * another piece of content.
   * 
   * <p>If a {@link DocumentFilter} is installed in this document, the
   * corresponding method of the filter object is called.</p>
   * 
   * <p>The method has no effect if <code>length</code> is zero (and
   * only zero) and, at the same time, <code>text</code> is
   * <code>null</code> or has zero length.</p>
   *
   * @param offset the start offset of the fragment to be removed
   * @param length the length of the fragment to be removed
   * @param text the text to replace the content with
   * @param attributes the text attributes to assign to the new content
   *
   * @throws BadLocationException if <code>offset</code> or
   *         <code>offset + length</code> or invalid locations within this
   *         document
   *
   * @since 1.4
   */
  public void replace(int offset, int length, String text,
                      AttributeSet attributes)
    throws BadLocationException
  {
    // Bail out if we have a bogus replacement (Behavior observed in RI).
    if (length == 0 
        && (text == null || text.length() == 0))
      return;

    writeLock();
    try
      {
        if (documentFilter == null)
          {
            // It is important to call the methods which again do the checks
            // of the arguments and the DocumentFilter because subclasses may
            // have overridden these methods and provide crucial behavior
            // which would be skipped if we call the non-checking variants.
            // An example for this is PlainDocument where insertString can
            // provide a filtering of newlines.
            remove(offset, length);
            insertString(offset, text, attributes);
          }
        else
          documentFilter.replace(getBypass(), offset, length, text, attributes);
      }
    finally
      {
        writeUnlock();
      }
  }
  
  void replaceImpl(int offset, int length, String text,
		      AttributeSet attributes)
    throws BadLocationException
  {
    removeImpl(offset, length);
    insertStringImpl(offset, text, attributes);
  }

  /**
   * Adds a <code>DocumentListener</code> object to this document.
   *
   * @param listener the listener to add
   */
  public void addDocumentListener(DocumentListener listener)
  {
    listenerList.add(DocumentListener.class, listener);
  }

  /**
   * Removes a <code>DocumentListener</code> object from this document.
   *
   * @param listener the listener to remove
   */
  public void removeDocumentListener(DocumentListener listener)
  {
    listenerList.remove(DocumentListener.class, listener);
  }

  /**
   * Returns all registered <code>DocumentListener</code>s.
   *
   * @return all registered <code>DocumentListener</code>s
   */
  public DocumentListener[] getDocumentListeners()
  {
    return (DocumentListener[]) getListeners(DocumentListener.class);
  }

  /**
   * Adds an {@link UndoableEditListener} to this <code>Document</code>.
   *
   * @param listener the listener to add
   */
  public void addUndoableEditListener(UndoableEditListener listener)
  {
    listenerList.add(UndoableEditListener.class, listener);
  }

  /**
   * Removes an {@link UndoableEditListener} from this <code>Document</code>.
   *
   * @param listener the listener to remove
   */
  public void removeUndoableEditListener(UndoableEditListener listener)
  {
    listenerList.remove(UndoableEditListener.class, listener);
  }

  /**
   * Returns all registered {@link UndoableEditListener}s.
   *
   * @return all registered {@link UndoableEditListener}s
   */
  public UndoableEditListener[] getUndoableEditListeners()
  {
    return (UndoableEditListener[]) getListeners(UndoableEditListener.class);
  }

  /**
   * Called before some content gets removed from this <code>Document</code>.
   * The default implementation does nothing but may be overridden by
   * subclasses to modify the <code>Document</code> structure in response
   * to a remove request. The method is executed within a write lock.
   *
   * @param chng the <code>DefaultDocumentEvent</code> describing the change
   */
  protected void removeUpdate(DefaultDocumentEvent chng)
  {
    // Do nothing here. Subclasses may wish to override this.
  }

  /**
   * Called to render this <code>Document</code> visually. It obtains a read
   * lock, ensuring that no changes will be made to the <code>document</code>
   * during the rendering process. It then calls the {@link Runnable#run()}
   * method on <code>runnable</code>. This method <em>must not</em> attempt
   * to modifiy the <code>Document</code>, since a deadlock will occur if it
   * tries to obtain a write lock. When the {@link Runnable#run()} method
   * completes (either naturally or by throwing an exception), the read lock
   * is released. Note that there is nothing in this method related to
   * the actual rendering. It could be used to execute arbitrary code within
   * a read lock.
   *
   * @param runnable the {@link Runnable} to execute
   */
  public void render(Runnable runnable)
  {
    readLock();
    try
    {
      runnable.run();
    }
    finally
    {
      readUnlock();
    }
  }

  /**
   * Sets the asynchronous loading priority for this <code>Document</code>.
   * A value of <code>-1</code> indicates that this <code>Document</code>
   * should be loaded synchronously.
   *
   * @param p the asynchronous loading priority to set
   */
  public void setAsynchronousLoadPriority(int p)
  {
    Integer val = p >= 0 ? new Integer(p) : null;
    putProperty(AsyncLoadPriority, val);
  }

  /**
   * Sets the properties of this <code>Document</code>.
   *
   * @param p the document properties to set
   */
  public void setDocumentProperties(Dictionary<Object, Object> p)
  {
    // FIXME: make me thread-safe
    properties = p;
  }

  /**
   * Blocks until a write lock can be obtained.  Must wait if there are 
   * readers currently reading or another thread is currently writing.
   */
  protected synchronized final void writeLock()
  {
    try
      {
        while (numReaders > 0 || currentWriter != null)
          {
            if (Thread.currentThread() == currentWriter)
              {
                if (notifyListeners)
                  throw new IllegalStateException("Mutation during notify");
                numWriters++;
                return;
              }
            wait();
          }
        currentWriter = Thread.currentThread();
        numWriters = 1;
      }
    catch (InterruptedException ex)
      {
        throw new Error("Interupted during grab write lock");
      }
  }

  /**
   * Releases the write lock. This allows waiting readers or writers to
   * obtain the lock.
   */
  protected final synchronized void writeUnlock()
  {
    if (--numWriters <= 0)
      {
        numWriters = 0;
        currentWriter = null;
        notifyAll();
      }
  }

  /**
   * Returns the currently installed {@link DocumentFilter} for this
   * <code>Document</code>.
   *
   * @return the currently installed {@link DocumentFilter} for this
   *         <code>Document</code>
   *
   * @since 1.4
   */
  public DocumentFilter getDocumentFilter()
  {
    return documentFilter;
  }

  /**
   * Sets the {@link DocumentFilter} for this <code>Document</code>.
   *
   * @param filter the <code>DocumentFilter</code> to set
   *
   * @since 1.4
   */
  public void setDocumentFilter(DocumentFilter filter)
  {
    this.documentFilter = filter;
  }

  /**
   * Dumps diagnostic information to the specified <code>PrintStream</code>.
   *
   * @param out the stream to write the diagnostic information to
   */
  public void dump(PrintStream out)
  {
    ((AbstractElement) getDefaultRootElement()).dump(out, 0);
    ((AbstractElement) getBidiRootElement()).dump(out, 0);
  }

  /**
   * Defines a set of methods for managing text attributes for one or more
   * <code>Document</code>s.
   *
   * Replicating {@link AttributeSet}s throughout a <code>Document</code> can
   * be very expensive. Implementations of this interface are intended to
   * provide intelligent management of <code>AttributeSet</code>s, eliminating
   * costly duplication.
   *
   * @see StyleContext
   */
  public interface AttributeContext
  {
    /**
     * Returns an {@link AttributeSet} that contains the attributes
     * of <code>old</code> plus the new attribute specified by
     * <code>name</code> and <code>value</code>.
     *
     * @param old the attribute set to be merged with the new attribute
     * @param name the name of the attribute to be added
     * @param value the value of the attribute to be added
     *
     * @return the old attributes plus the new attribute
     */
    AttributeSet addAttribute(AttributeSet old, Object name, Object value);

    /**
     * Returns an {@link AttributeSet} that contains the attributes
     * of <code>old</code> plus the new attributes in <code>attributes</code>.
     *
     * @param old the set of attributes where to add the new attributes
     * @param attributes the attributes to be added
     *
     * @return an {@link AttributeSet} that contains the attributes
     *         of <code>old</code> plus the new attributes in
     *         <code>attributes</code>
     */
    AttributeSet addAttributes(AttributeSet old, AttributeSet attributes);

    /**
     * Returns an empty {@link AttributeSet}.
     *
     * @return  an empty {@link AttributeSet}
     */
    AttributeSet getEmptySet();

    /**
     * Called to indicate that the attributes in <code>attributes</code> are
     * no longer used.
     *
     * @param attributes the attributes are no longer used
     */
    void reclaim(AttributeSet attributes);

    /**
     * Returns a {@link AttributeSet} that has the attribute with the specified
     * <code>name</code> removed from <code>old</code>.
     *
     * @param old the attribute set from which an attribute is removed
     * @param name the name of the attribute to be removed
     *
     * @return the attributes of <code>old</code> minus the attribute
     *         specified by <code>name</code>
     */
    AttributeSet removeAttribute(AttributeSet old, Object name);

    /**
     * Removes all attributes in <code>attributes</code> from <code>old</code>
     * and returns the resulting <code>AttributeSet</code>.
     *
     * @param old the set of attributes from which to remove attributes
     * @param attributes the attributes to be removed from <code>old</code>
     *
     * @return the attributes of <code>old</code> minus the attributes in
     *         <code>attributes</code>
     */
    AttributeSet removeAttributes(AttributeSet old, AttributeSet attributes);

    /**
     * Removes all attributes specified by <code>names</code> from
     * <code>old</code> and returns the resulting <code>AttributeSet</code>.
     *
     * @param old the set of attributes from which to remove attributes
     * @param names the names of the attributes to be removed from
     *        <code>old</code>
     *
     * @return the attributes of <code>old</code> minus the attributes in
     *         <code>attributes</code>
     */
    AttributeSet removeAttributes(AttributeSet old, Enumeration<?> names);
  }

  /**
   * A sequence of data that can be edited. This is were the actual content
   * in <code>AbstractDocument</code>'s is stored.
   */
  public interface Content
  {
    /**
     * Creates a {@link Position} that keeps track of the location at
     * <code>offset</code>.
     *
     * @return a {@link Position} that keeps track of the location at
     *         <code>offset</code>.
     *
     * @throw BadLocationException if <code>offset</code> is not a valid
     *        location in this <code>Content</code> model
     */
    Position createPosition(int offset) throws BadLocationException;

    /**
     * Returns the length of the content.
     *
     * @return the length of the content
     */
    int length();

    /**
     * Inserts a string into the content model.
     *
     * @param where the offset at which to insert the string
     * @param str the string to be inserted
     *
     * @return an <code>UndoableEdit</code> or <code>null</code> if undo is
     *         not supported by this <code>Content</code> model
     *
     * @throws BadLocationException if <code>where</code> is not a valid
     *         location in this <code>Content</code> model
     */
    UndoableEdit insertString(int where, String str)
      throws BadLocationException;

    /**
     * Removes a piece of content from the content model.
     *
     * @param where the offset at which to remove content
     * @param nitems the number of characters to be removed
     *
     * @return an <code>UndoableEdit</code> or <code>null</code> if undo is
     *         not supported by this <code>Content</code> model
     *
     * @throws BadLocationException if <code>where</code> is not a valid
     *         location in this <code>Content</code> model
     */
    UndoableEdit remove(int where, int nitems) throws BadLocationException;

    /**
     * Returns a piece of content.
     *
     * @param where the start offset of the requested fragment
     * @param len the length of the requested fragment
     *
     * @return the requested fragment
     * @throws BadLocationException if <code>offset</code> or
     *         <code>offset + len</code>is not a valid
     *         location in this <code>Content</code> model
     */
    String getString(int where, int len) throws BadLocationException;

    /**
     * Fetches a piece of content and stores it in <code>txt</code>.
     *
     * @param where the start offset of the requested fragment
     * @param len the length of the requested fragment
     * @param txt the <code>Segment</code> where to fragment is stored into
     *
     * @throws BadLocationException if <code>offset</code> or
     *         <code>offset + len</code>is not a valid
     *         location in this <code>Content</code> model
     */
    void getChars(int where, int len, Segment txt) throws BadLocationException;
  }

  /**
   * An abstract base implementation of the {@link Element} interface.
   */
  public abstract class AbstractElement
    implements Element, MutableAttributeSet, TreeNode, Serializable
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = 1712240033321461704L;

    /** The number of characters that this Element spans. */
    int count;

    /** The starting offset of this Element. */
    int offset;

    /** The attributes of this Element. */
    AttributeSet attributes;

    /** The parent element. */
    Element element_parent;

    /** The parent in the TreeNode interface. */
    TreeNode tree_parent;

    /** The children of this element. */
    Vector tree_children;

    /**
     * Creates a new instance of <code>AbstractElement</code> with a
     * specified parent <code>Element</code> and <code>AttributeSet</code>.
     *
     * @param p the parent of this <code>AbstractElement</code>
     * @param s the attributes to be assigned to this
     *        <code>AbstractElement</code>
     */
    public AbstractElement(Element p, AttributeSet s)
    {
      element_parent = p;
      AttributeContext ctx = getAttributeContext();
      attributes = ctx.getEmptySet();
      if (s != null)
        addAttributes(s);
    }

    /**
     * Returns the child nodes of this <code>Element</code> as an
     * <code>Enumeration</code> of {@link TreeNode}s.
     *
     * @return the child nodes of this <code>Element</code> as an
     *         <code>Enumeration</code> of {@link TreeNode}s
     */
    public abstract Enumeration children();

    /**
     * Returns <code>true</code> if this <code>AbstractElement</code>
     * allows children.
     *
     * @return <code>true</code> if this <code>AbstractElement</code>
     *         allows children
     */
    public abstract boolean getAllowsChildren();

    /**
     * Returns the child of this <code>AbstractElement</code> at
     * <code>index</code>.
     *
     * @param index the position in the child list of the child element to
     *        be returned
     *
     * @return the child of this <code>AbstractElement</code> at
     *         <code>index</code>
     */
    public TreeNode getChildAt(int index)
    {
      return (TreeNode) tree_children.get(index);
    }

    /**
     * Returns the number of children of this <code>AbstractElement</code>.
     *
     * @return the number of children of this <code>AbstractElement</code>
     */
    public int getChildCount()
    {
      return tree_children.size();
    }

    /**
     * Returns the index of a given child <code>TreeNode</code> or
     * <code>-1</code> if <code>node</code> is not a child of this
     * <code>AbstractElement</code>.
     *
     * @param node the node for which the index is requested
     *
     * @return the index of a given child <code>TreeNode</code> or
     *         <code>-1</code> if <code>node</code> is not a child of this
     *         <code>AbstractElement</code>
     */
    public int getIndex(TreeNode node)
    {
      return tree_children.indexOf(node);
    }

    /**
     * Returns the parent <code>TreeNode</code> of this
     * <code>AbstractElement</code> or <code>null</code> if this element
     * has no parent.
     *
     * @return the parent <code>TreeNode</code> of this
     *         <code>AbstractElement</code> or <code>null</code> if this
     *         element has no parent
     */
    public TreeNode getParent()
    {
      return tree_parent;
    }

    /**
     * Returns <code>true</code> if this <code>AbstractElement</code> is a
     * leaf element, <code>false</code> otherwise.
     *
     * @return <code>true</code> if this <code>AbstractElement</code> is a
     *         leaf element, <code>false</code> otherwise
     */
    public abstract boolean isLeaf();

    /**
     * Adds an attribute to this element.
     *
     * @param name the name of the attribute to be added
     * @param value the value of the attribute to be added
     */
    public void addAttribute(Object name, Object value)
    {
      attributes = getAttributeContext().addAttribute(attributes, name, value);
    }

    /**
     * Adds a set of attributes to this element.
     *
     * @param attrs the attributes to be added to this element
     */
    public void addAttributes(AttributeSet attrs)
    {
      attributes = getAttributeContext().addAttributes(attributes, attrs);
    }

    /**
     * Removes an attribute from this element.
     *
     * @param name the name of the attribute to be removed
     */
    public void removeAttribute(Object name)
    {
      attributes = getAttributeContext().removeAttribute(attributes, name);
    }

    /**
     * Removes a set of attributes from this element.
     *
     * @param attrs the attributes to be removed
     */
    public void removeAttributes(AttributeSet attrs)
    {
      attributes = getAttributeContext().removeAttributes(attributes, attrs);
    }

    /**
     * Removes a set of attribute from this element.
     *
     * @param names the names of the attributes to be removed
     */
    public void removeAttributes(Enumeration<?> names)
    {
      attributes = getAttributeContext().removeAttributes(attributes, names);
    }

    /**
     * Sets the parent attribute set against which the element can resolve
     * attributes that are not defined in itself.
     *
     * @param parent the resolve parent to set
     */
    public void setResolveParent(AttributeSet parent)
    {
      attributes = getAttributeContext().addAttribute(attributes,
                                                      ResolveAttribute,
                                                      parent);
    }

    /**
     * Returns <code>true</code> if this element contains the specified
     * attribute.
     *
     * @param name the name of the attribute to check
     * @param value the value of the attribute to check
     *
     * @return <code>true</code> if this element contains the specified
     *         attribute
     */
    public boolean containsAttribute(Object name, Object value)
    {
      return attributes.containsAttribute(name, value);
    }

    /**
     * Returns <code>true</code> if this element contains all of the
     * specified attributes.
     *
     * @param attrs the attributes to check
     *
     * @return <code>true</code> if this element contains all of the
     *         specified attributes
     */
    public boolean containsAttributes(AttributeSet attrs)
    {
      return attributes.containsAttributes(attrs);
    }

    /**
     * Returns a copy of the attributes of this element.
     *
     * @return a copy of the attributes of this element
     */
    public AttributeSet copyAttributes()
    {
      return attributes.copyAttributes();
    }

    /**
     * Returns the attribute value with the specified key. If this attribute
     * is not defined in this element and this element has a resolving
     * parent, the search goes upward to the resolve parent chain.
     *
     * @param key the key of the requested attribute
     *
     * @return the attribute value for <code>key</code> of <code>null</code>
     *         if <code>key</code> is not found locally and cannot be resolved
     *         in this element's resolve parents
     */
    public Object getAttribute(Object key)
    {
      Object result = attributes.getAttribute(key);
      if (result == null)
        {
          AttributeSet resParent = getResolveParent();
          if (resParent != null)
            result = resParent.getAttribute(key);
        }
      return result;
    }

    /**
     * Returns the number of defined attributes in this element.
     *
     * @return the number of defined attributes in this element
     */
    public int getAttributeCount()
    {
      return attributes.getAttributeCount();
    }

    /**
     * Returns the names of the attributes of this element.
     *
     * @return the names of the attributes of this element
     */
    public Enumeration<?> getAttributeNames()
    {
      return attributes.getAttributeNames();
    }

    /**
     * Returns the resolve parent of this element.
     * This is taken from the AttributeSet, but if this is null,
     * this method instead returns the Element's parent's 
     * AttributeSet
     *
     * @return the resolve parent of this element
     *
     * @see #setResolveParent(AttributeSet)
     */
    public AttributeSet getResolveParent()
    {
      return attributes.getResolveParent();
    }

    /**
     * Returns <code>true</code> if an attribute with the specified name
     * is defined in this element, <code>false</code> otherwise.
     *
     * @param attrName the name of the requested attributes
     *
     * @return <code>true</code> if an attribute with the specified name
     *         is defined in this element, <code>false</code> otherwise
     */
    public boolean isDefined(Object attrName)
    {
      return attributes.isDefined(attrName);
    }

    /**
     * Returns <code>true</code> if the specified <code>AttributeSet</code>
     * is equal to this element's <code>AttributeSet</code>, <code>false</code>
     * otherwise.
     *
     * @param attrs the attributes to compare this element to
     *
     * @return <code>true</code> if the specified <code>AttributeSet</code>
     *         is equal to this element's <code>AttributeSet</code>,
     *         <code>false</code> otherwise
     */
    public boolean isEqual(AttributeSet attrs) 
    {
      return attributes.isEqual(attrs);
    }

    /**
     * Returns the attributes of this element.
     *
     * @return the attributes of this element
     */
    public AttributeSet getAttributes()
    {
      return this;
    }

    /**
     * Returns the {@link Document} to which this element belongs.
     *
     * @return the {@link Document} to which this element belongs
     */
    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    /**
     * Returns the child element at the specified <code>index</code>.
     *
     * @param index the index of the requested child element
     *
     * @return the requested element
     */
    public abstract Element getElement(int index);

    /**
     * Returns the name of this element.
     *
     * @return the name of this element
     */
    public String getName()
    {
      return (String) attributes.getAttribute(ElementNameAttribute);
    }

    /**
     * Returns the parent element of this element.
     *
     * @return the parent element of this element
     */
    public Element getParentElement()
    {
      return element_parent;
    }

    /**
     * Returns the offset inside the document model that is after the last
     * character of this element.
     *
     * @return the offset inside the document model that is after the last
     *         character of this element
     */
    public abstract int getEndOffset();

    /**
     * Returns the number of child elements of this element.
     *
     * @return the number of child elements of this element
     */
    public abstract int getElementCount();

    /**
     * Returns the index of the child element that spans the specified
     * offset in the document model.
     *
     * @param offset the offset for which the responsible element is searched
     *
     * @return the index of the child element that spans the specified
     *         offset in the document model
     */
    public abstract int getElementIndex(int offset);

    /**
     * Returns the start offset if this element inside the document model.
     *
     * @return the start offset if this element inside the document model
     */
    public abstract int getStartOffset();

    /**
     * Prints diagnostic output to the specified stream.
     *
     * @param stream the stream to write to
     * @param indent the indentation level
     */
    public void dump(PrintStream stream, int indent)
    {
      StringBuffer b = new StringBuffer();
      for (int i = 0; i < indent; ++i)
        b.append(' ');
      b.append('<');
      b.append(getName());
      // Dump attributes if there are any.
      if (getAttributeCount() > 0)
        {
          b.append('\n');
          Enumeration attNames = getAttributeNames();
          while (attNames.hasMoreElements())
            {
              for (int i = 0; i < indent + 2; ++i)
                b.append(' ');
              Object attName = attNames.nextElement();
              b.append(attName);
              b.append('=');
              Object attribute = getAttribute(attName);
              b.append(attribute);
              b.append('\n');
            }
        }
      if (getAttributeCount() > 0)
        {
          for (int i = 0; i < indent; ++i)
            b.append(' ');
        }
      b.append(">\n");

      // Dump element content for leaf elements.
      if (isLeaf())
        {
          for (int i = 0; i < indent + 2; ++i)
            b.append(' ');
          int start = getStartOffset();
          int end = getEndOffset();
          b.append('[');
          b.append(start);
          b.append(',');
          b.append(end);
          b.append("][");
          try
            {
              b.append(getDocument().getText(start, end - start));
            }
          catch (BadLocationException ex)
            {
              AssertionError err = new AssertionError("BadLocationException "
                                                      + "must not be thrown "
                                                      + "here.");
              err.initCause(ex);
	      throw err;
            }
          b.append("]\n");
        }
      stream.print(b.toString());

      // Dump child elements if any.
      int count = getElementCount();
      for (int i = 0; i < count; ++i)
        {
          Element el = getElement(i);
          if (el instanceof AbstractElement)
            ((AbstractElement) el).dump(stream, indent + 2);
        }
    }
  }

  /**
   * An implementation of {@link Element} to represent composite
   * <code>Element</code>s that contain other <code>Element</code>s.
   */
  public class BranchElement extends AbstractElement
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = -6037216547466333183L;

    /**
     * The child elements of this BranchElement.
     */
    private Element[] children;

    /**
     * The number of children in the branch element.
     */
    private int numChildren;

    /**
     * The last found index in getElementIndex(). Used for faster searching.
     */
    private int lastIndex;

    /**
     * Creates a new <code>BranchElement</code> with the specified
     * parent and attributes.
     *
     * @param parent the parent element of this <code>BranchElement</code>
     * @param attributes the attributes to set on this
     *        <code>BranchElement</code>
     */
    public BranchElement(Element parent, AttributeSet attributes)
    {
      super(parent, attributes);
      children = new Element[1];
      numChildren = 0;
      lastIndex = -1;
    }

    /**
     * Returns the children of this <code>BranchElement</code>.
     *
     * @return the children of this <code>BranchElement</code>
     */
    public Enumeration children()
    {
      if (numChildren == 0)
        return null;

      Vector tmp = new Vector();

      for (int index = 0; index < numChildren; ++index)
        tmp.add(children[index]);
      
      return tmp.elements();
    }

    /**
     * Returns <code>true</code> since <code>BranchElements</code> allow
     * child elements.
     *
     * @return <code>true</code> since <code>BranchElements</code> allow
     *         child elements
     */
    public boolean getAllowsChildren()
    {
      return true;
    }

    /**
     * Returns the child element at the specified <code>index</code>.
     *
     * @param index the index of the requested child element
     *
     * @return the requested element
     */
    public Element getElement(int index)
    {
      if (index < 0 || index >= numChildren)
        return null;

      return children[index];
    }

    /**
     * Returns the number of child elements of this element.
     *
     * @return the number of child elements of this element
     */
    public int getElementCount()
    {
      return numChildren;
    }

    /**
     * Returns the index of the child element that spans the specified
     * offset in the document model.
     *
     * @param offset the offset for which the responsible element is searched
     *
     * @return the index of the child element that spans the specified
     *         offset in the document model
     */
    public int getElementIndex(int offset)
    {
      // Implemented using an improved linear search.
      // This makes use of the fact that searches are not random but often
      // close to the previous search. So we try to start the binary
      // search at the last found index.

      int i0 = 0; // The lower bounds.
      int i1 = numChildren - 1; // The upper bounds.
      int index = -1; // The found index.

      int p0 = getStartOffset();
      int p1; // Start and end offset local variables.

      if (numChildren == 0)
        index = 0;
      else if (offset >= getEndOffset())
        index = numChildren - 1;
      else
        {
          // Try lastIndex.
          if (lastIndex >= i0 && lastIndex <= i1)
            {
              Element last = getElement(lastIndex);
              p0 = last.getStartOffset();
              p1 = last.getEndOffset();
              if (offset >= p0 && offset < p1)
                index = lastIndex;
              else
                {
                  // Narrow the search bounds using the lastIndex, even
                  // if it hasn't been a hit.
                  if (offset < p0)
                    i1 = lastIndex;
                  else
                    i0 = lastIndex;
                }
            }
          // The actual search.
          int i = 0;
          while (i0 <= i1 && index == -1)
            {
              i = i0 + (i1 - i0) / 2;
              Element el = getElement(i);
              p0 = el.getStartOffset();
              p1 = el.getEndOffset();
              if (offset >= p0 && offset < p1)
                {
                  // Found it!
                  index = i;
                }
              else if (offset < p0)
                i1 = i - 1;
              else
                i0 = i + 1;
            }

          if (index == -1)
            {
              // Didn't find it. Return the boundary index.
              if (offset < p0)
                index = i;
              else
                index = i + 1;
            }

          lastIndex = index;
        }
      return index;
    }

    /**
     * Returns the offset inside the document model that is after the last
     * character of this element.
     * This is the end offset of the last child element. If this element
     * has no children, this method throws a <code>NullPointerException</code>.
     *
     * @return the offset inside the document model that is after the last
     *         character of this element
     *
     * @throws NullPointerException if this branch element has no children
     */
    public int getEndOffset()
    {
      // This might accss one cached element or trigger an NPE for
      // numChildren == 0. This is checked by a Mauve test.
      Element child = numChildren > 0 ? children[numChildren - 1]
                                      : children[0];
      return child.getEndOffset();
    }

    /**
     * Returns the name of this element. This is {@link #ParagraphElementName}
     * in this case.
     *
     * @return the name of this element
     */
    public String getName()
    {
      return ParagraphElementName;
    }

    /**
     * Returns the start offset of this element inside the document model.
     * This is the start offset of the first child element. If this element
     * has no children, this method throws a <code>NullPointerException</code>.
     *
     * @return the start offset of this element inside the document model
     *
     * @throws NullPointerException if this branch element has no children and
     *         no startOffset value has been cached
     */
    public int getStartOffset()
    {
      // Do not explicitly throw an NPE here. If the first element is null
      // then the NPE gets thrown anyway. If it isn't, then it either
      // holds a real value (for numChildren > 0) or a cached value
      // (for numChildren == 0) as we don't fully remove elements in replace()
      // when removing single elements.
      // This is checked by a Mauve test.
      return children[0].getStartOffset();
    }

    /**
     * Returns <code>false</code> since <code>BranchElement</code> are no
     * leafes.
     *
     * @return <code>false</code> since <code>BranchElement</code> are no
     *         leafes
     */
    public boolean isLeaf()
    {
      return false;
    }

    /**
     * Returns the <code>Element</code> at the specified <code>Document</code>
     * offset.
     *
     * @return the <code>Element</code> at the specified <code>Document</code>
     *         offset
     *
     * @see #getElementIndex(int)
     */
    public Element positionToElement(int position)
    {
      // XXX: There is surely a better algorithm
      // as beginning from first element each time.
      for (int index = 0; index < numChildren; ++index)
        {
	  Element elem = children[index];

	  if ((elem.getStartOffset() <= position)
	      && (position < elem.getEndOffset()))
	    return elem;
        }

      return null;
    }

    /**
     * Replaces a set of child elements with a new set of child elemens.
     *
     * @param offset the start index of the elements to be removed
     * @param length the number of elements to be removed
     * @param elements the new elements to be inserted
     */
    public void replace(int offset, int length, Element[] elements)
    {
      int delta = elements.length - length;
      int copyFrom = offset + length; // From where to copy.
      int copyTo = copyFrom + delta;    // Where to copy to.
      int numMove = numChildren - copyFrom; // How many elements are moved. 
      if (numChildren + delta > children.length)
        {
          // Gotta grow the array.
          int newSize = Math.max(2 * children.length, numChildren + delta);
          Element[] target = new Element[newSize];
          System.arraycopy(children, 0, target, 0, offset);
          System.arraycopy(elements, 0, target, offset, elements.length);
          System.arraycopy(children, copyFrom, target, copyTo, numMove);
          children = target;
        }
      else
        {
          System.arraycopy(children, copyFrom, children, copyTo, numMove);
          System.arraycopy(elements, 0, children, offset, elements.length);
        }
      numChildren += delta;
    }

    /**
     * Returns a string representation of this element.
     *
     * @return a string representation of this element
     */
    public String toString()
    {
      return ("BranchElement(" + getName() + ") "
	      + getStartOffset() + "," + getEndOffset() + "\n");
    }
  }

  /**
   * Stores the changes when a <code>Document</code> is beeing modified.
   */
  public class DefaultDocumentEvent extends CompoundEdit
    implements DocumentEvent
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = 5230037221564563284L;

    /**
     * The threshold that indicates when we switch to using a Hashtable.
     */
    private static final int THRESHOLD = 10;
    
    /** The starting offset of the change. */
    private int offset;

    /** The length of the change. */
    private int length;

    /** The type of change. */
    private DocumentEvent.EventType type;

    /**
     * Maps <code>Element</code> to their change records. This is only
     * used when the changes array gets too big. We can use an
     * (unsync'ed) HashMap here, since changes to this are (should) always
     * be performed inside a write lock. 
     */
    private HashMap changes;

    /**
     * Indicates if this event has been modified or not. This is used to
     * determine if this event is thrown.
     */
    private boolean modified;

    /**
     * Creates a new <code>DefaultDocumentEvent</code>.
     *
     * @param offset the starting offset of the change
     * @param length the length of the change
     * @param type the type of change
     */
    public DefaultDocumentEvent(int offset, int length,
				DocumentEvent.EventType type)
    {
      this.offset = offset;
      this.length = length;
      this.type = type;
      modified = false;
    }

    /**
     * Adds an UndoableEdit to this <code>DocumentEvent</code>. If this
     * edit is an instance of {@link ElementEdit}, then this record can
     * later be fetched by calling {@link #getChange}.
     *
     * @param edit the undoable edit to add
     */
    public boolean addEdit(UndoableEdit edit)
    {
      // Start using Hashtable when we pass a certain threshold. This
      // gives a good memory/performance compromise.
      if (changes == null && edits.size() > THRESHOLD)
        {
          changes = new HashMap();
          int count = edits.size();
          for (int i = 0; i < count; i++)
            {
              Object o = edits.elementAt(i);
              if (o instanceof ElementChange)
                {
                  ElementChange ec = (ElementChange) o;
                  changes.put(ec.getElement(), ec);
                }
            }
        }

      if (changes != null && edit instanceof ElementChange)
        {
          ElementChange elEdit = (ElementChange) edit;
          changes.put(elEdit.getElement(), elEdit);
        }
      return super.addEdit(edit);
    }

    /**
     * Returns the document that has been modified.
     *
     * @return the document that has been modified
     */
    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    /**
     * Returns the length of the modification.
     *
     * @return the length of the modification
     */
    public int getLength()
    {
      return length;
    }

    /**
     * Returns the start offset of the modification.
     *
     * @return the start offset of the modification
     */
    public int getOffset()
    {
      return offset;
    }

    /**
     * Returns the type of the modification.
     *
     * @return the type of the modification
     */
    public DocumentEvent.EventType getType()
    {
      return type;
    }

    /**
     * Returns the changes for an element.
     *
     * @param elem the element for which the changes are requested
     *
     * @return the changes for <code>elem</code> or <code>null</code> if
     *         <code>elem</code> has not been changed
     */
    public ElementChange getChange(Element elem)
    {
      ElementChange change = null;
      if (changes != null)
        {
          change = (ElementChange) changes.get(elem);
        }
      else
        {
          int count = edits.size();
          for (int i = 0; i < count && change == null; i++)
            {
              Object o = edits.get(i);
              if (o instanceof ElementChange)
                {
                  ElementChange ec = (ElementChange) o;
                  if (elem.equals(ec.getElement()))
                    change = ec;
                }
            }
        }
      return change;
    }
    
    /**
     * Returns a String description of the change event.  This returns the
     * toString method of the Vector of edits.
     */
    public String toString()
    {
      return edits.toString();
    }
  }
  
  /**
   * An implementation of {@link DocumentEvent.ElementChange} to be added
   * to {@link DefaultDocumentEvent}s.
   */
  public static class ElementEdit extends AbstractUndoableEdit
    implements DocumentEvent.ElementChange
  {
    /** The serial version UID of ElementEdit. */
    private static final long serialVersionUID = -1216620962142928304L;

    /**
     * The changed element.
     */
    private Element elem;

    /**
     * The index of the change.
     */
    private int index;

    /**
     * The removed elements.
     */
    private Element[] removed;

    /**
     * The added elements.
     */
    private Element[] added;
    
    /**
     * Creates a new <code>ElementEdit</code>.
     *
     * @param elem the changed element
     * @param index the index of the change
     * @param removed the removed elements
     * @param added the added elements
     */
    public ElementEdit(Element elem, int index,
		       Element[] removed, Element[] added)
    {
      this.elem = elem;
      this.index = index;
      this.removed = removed;
      this.added = added;
    }

    /**
     * Returns the added elements.
     *
     * @return the added elements
     */
    public Element[] getChildrenAdded()
    {
      return added;
    }

    /**
     * Returns the removed elements.
     *
     * @return the removed elements
     */
    public Element[] getChildrenRemoved()
    {
      return removed;
    }

    /**
     * Returns the changed element.
     *
     * @return the changed element
     */
    public Element getElement()
    {
      return elem;
    }

    /**
     * Returns the index of the change.
     *
     * @return the index of the change
     */
    public int getIndex()
    {
      return index;
    }
  }

  /**
   * An implementation of {@link Element} that represents a leaf in the
   * document structure. This is used to actually store content.
   */
  public class LeafElement extends AbstractElement
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = -8906306331347768017L;

    /**
     * Manages the start offset of this element.
     */
    private Position startPos;

    /**
     * Manages the end offset of this element.
     */
    private Position endPos;

    /**
     * Creates a new <code>LeafElement</code>.
     *
     * @param parent the parent of this <code>LeafElement</code>
     * @param attributes the attributes to be set
     * @param start the start index of this element inside the document model
     * @param end the end index of this element inside the document model
     */
    public LeafElement(Element parent, AttributeSet attributes, int start,
                       int end)
    {
      super(parent, attributes);
      try
        {
          startPos = createPosition(start);
          endPos = createPosition(end);
        }
      catch (BadLocationException ex)
        {
          AssertionError as;
          as = new AssertionError("BadLocationException thrown "
                                  + "here. start=" + start
                                  + ", end=" + end
                                  + ", length=" + getLength());
          as.initCause(ex);
          throw as;
        }
    }

    /**
     * Returns <code>null</code> since <code>LeafElement</code>s cannot have
     * children.
     *
     * @return <code>null</code> since <code>LeafElement</code>s cannot have
     *         children
     */
    public Enumeration children()
    {
      return null;
    }

    /**
     * Returns <code>false</code> since <code>LeafElement</code>s cannot have
     * children.
     *
     * @return <code>false</code> since <code>LeafElement</code>s cannot have
     *         children
     */
    public boolean getAllowsChildren()
    {
      return false;
    }

    /**
     * Returns <code>null</code> since <code>LeafElement</code>s cannot have
     * children.
     *
     * @return <code>null</code> since <code>LeafElement</code>s cannot have
     *         children
     */
    public Element getElement(int index)
    {
      return null;
    }

    /**
     * Returns <code>0</code> since <code>LeafElement</code>s cannot have
     * children.
     *
     * @return <code>0</code> since <code>LeafElement</code>s cannot have
     *         children
     */
    public int getElementCount()
    {
      return 0;
    }

    /**
     * Returns <code>-1</code> since <code>LeafElement</code>s cannot have
     * children.
     *
     * @return <code>-1</code> since <code>LeafElement</code>s cannot have
     *         children
     */
    public int getElementIndex(int offset)
    {
      return -1;
    }

    /**
     * Returns the end offset of this <code>Element</code> inside the
     * document.
     *
     * @return the end offset of this <code>Element</code> inside the
     *         document
     */
    public int getEndOffset()
    {
      return endPos.getOffset();
    }

    /**
     * Returns the name of this <code>Element</code>. This is
     * {@link #ContentElementName} in this case.
     *
     * @return the name of this <code>Element</code>
     */
    public String getName()
    {
      String name = super.getName();
      if (name == null)
        name = ContentElementName;
      return name;
    }

    /**
     * Returns the start offset of this <code>Element</code> inside the
     * document.
     *
     * @return the start offset of this <code>Element</code> inside the
     *         document
     */
    public int getStartOffset()
    {
      return startPos.getOffset();
    }

    /**
     * Returns <code>true</code>.
     *
     * @return <code>true</code>
     */
    public boolean isLeaf()
    {
      return true;
    }

    /**
     * Returns a string representation of this <code>Element</code>.
     *
     * @return a string representation of this <code>Element</code>
     */
    public String toString()
    {
      return ("LeafElement(" + getName() + ") "
	      + getStartOffset() + "," + getEndOffset() + "\n");
    }
  }

  /**
   * The root element for bidirectional text.
   */
  private class BidiRootElement
    extends BranchElement
  {
    /**
     * Creates a new bidi root element.
     */
    BidiRootElement()
    {
      super(null, null);
    }

    /**
     * Returns the name of the element.
     *
     * @return the name of the element
     */
    public String getName()
    {
      return BidiRootName;
    }
  }

  /**
   * A leaf element for the bidi structure.
   */
  private class BidiElement
    extends LeafElement
  {
    /**
     * Creates a new BidiElement.
     *
     * @param parent the parent element
     * @param start the start offset
     * @param end the end offset
     * @param level the bidi level
     */
    BidiElement(Element parent, int start, int end, int level)
    {
      super(parent, new SimpleAttributeSet(), start, end);
      addAttribute(StyleConstants.BidiLevel, new Integer(level));
    }

    /**
     * Returns the name of the element.
     *
     * @return the name of the element
     */
    public String getName()
    {
      return BidiElementName;
    }
  }

  /** A class whose methods delegate to the insert, remove and replace methods
   * of this document which do not check for an installed DocumentFilter.
   */
  class Bypass extends DocumentFilter.FilterBypass
  {

    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    public void insertString(int offset, String string, AttributeSet attr)
    throws BadLocationException
    {
      AbstractDocument.this.insertStringImpl(offset, string, attr);
    }

    public void remove(int offset, int length)
    throws BadLocationException
    {
      AbstractDocument.this.removeImpl(offset, length);
    }

    public void replace(int offset, int length, String string,
                        AttributeSet attrs)
    throws BadLocationException
    {
      AbstractDocument.this.replaceImpl(offset, length, string, attrs);
    }
    
  }
  
}
