/* AccessibleContext.java -- the context of an accessible object
   Copyright (C) 2002 Free Software Foundation

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

package javax.accessibility;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Locale;

/**
 * The minimum information that all accessible objects return. This includes
 * name, description, role, and state of the object, parents and children,
 * and any other useful information. If a component supports further details,
 * it should implement one of the following:<ul>
 * <li>{@link AccessibleAction} - the object can perform actions</li>
 * <li>{@link AccessibleComponent} - the object has a graphical
 *     representation</li>
 * <li>{@link AccessibleSelection} - the object allows its children to be
 *     selected</li>
 * <li>{@link AccessibleText} - the object represents editable text</li>
 * <li>{@link AccessibleValue} - the object represents a numerical value</li>
 * </ul>
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class AccessibleContext
{
  /**
   * Constant used when the accessible name has changed. Both the old and new
   * values are listed in the event.
   *
   * @see #getAccessibleName()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_NAME_PROPERTY
    = "AccessibleName";

  /**
   * Constanat used when the accessible description has changed. Both the old
   * and new values are listed in the event.
   *
   * @see #getAccessibleDescription()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_DESCRIPTION_PROPERTY
    = "AccessibleDescription";

  /**
   * Constant used when the accessibleStateSet has changed. Both the old and
   * new values are listed in the event, although either may be null if a
   * state was disabled at that time.
   *
   * @see #getAccessibleStateSet()
   * @see AccessibleState
   * @see AccessibleStateSet
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_STATE_PROPERTY
    = "AccessibleState";

  /**
   * Constant used when the accessibleValue has changed. Both the old and new
   * values are listed in the event.
   *
   * @see #getAccessibleValue()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_VALUE_PROPERTY
    = "AccessibleValue";

  /**
   * Constant used when the accessibleSelection has changed. Both the old and
   * new values of the event are reserved for future use.
   *
   * @see #getAccessibleSelection()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_SELECTION_PROPERTY
    = "AccessibleSelection";

  /**
   * Constant used when the accessibleText has changed. Both the old and new
   * values of the event are reserved for future use.
   *
   * @see #getAccessibleText()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_TEXT_PROPERTY
    = "AccessibleText";

  /**
   * Constant used when the accessibleText caret has changed. Both the old and
   * new values are listed in the event.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_CARET_PROPERTY
    = "AccessibleCaret";

  /**
   * Constant used when the visible data has changed. Both the old and new
   * values of the event are reserved for future use.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_VISIBLE_DATA_PROPERTY
    = "AccessibleVisibleData";

  /**
   * Constant used when children are added or removed. On addition, the new
   * value of the event holds the new child; on removal, the old value holds
   * the removed child.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_CHILD_PROPERTY
    = "AccessibleChild";

  /**
   * Constant used when active descendent of a component has changed. Both
   * the old and new values are listed in the event.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public static final String ACCESSIBLE_ACTIVE_DESCENDANT_PROPERTY
    = "AccessibleActiveDescendant";

  /**
   * Constant used when the accessible table caption has changed. Both the
   * old and new values are listed in the event.
   *
   * @see Accessible
   * @see AccessibleTable
   */
  public static final String ACCESSIBLE_TABLE_CAPTION_CHANGED
    = "accessibleTableCaptionChanged";

  /**
   * Constant used when the accessible table summary has changed. Both the
   * old and new values are listed in the event.
   *
   * @see Accessible
   * @see AccessibleTable
   */
  public static final String ACCESSIBLE_TABLE_SUMMARY_CHANGED
    = "accessibleTableSummaryChanged";

  /**
   * Constant used when the accessible table model has changed. Only the new
   * value of the event has meaning.
   *
   * @see AccessibleTable
   * @see AccessibleTableModelChange
   */
  public static final String ACCESSIBLE_TABLE_MODEL_CHANGED
    = "accessibleTableModelChanged";

  /**
   * Constant used when the accessible table row header has changed. Only the
   * new value of the event has meaning.
   *
   * @see AccessibleTable
   * @see AccessibleTableModelChange
   */
  public static final String ACCESSIBLE_TABLE_ROW_HEADER_CHANGED
    = "accessibleTableRowHeaderChanged";

  /**
   * Constant used when the accessible table row description has changed. Only
   * the new value of the event has meaning.
   *
   * @see AccessibleTable
   */
  public static final String ACCESSIBLE_TABLE_ROW_DESCRIPTION_CHANGED
    = "accessibleTableRowDescriptionChanged";

  /**
   * Constant used when the accessible table column header has changed. Only
   * the new value of the event has meaning.
   *
   * @see AccessibleTable
   * @see AccessibleTableModelChange
   */
  public static final String ACCESSIBLE_TABLE_COLUMN_HEADER_CHANGED
    = "accessibleTableColumnHeaderChanged";

  /**
   * Constant used when the accessible table column description has changed.
   * Only the new value of the event has meaning.
   *
   * @see AccessibleTable
   */
  public static final String ACCESSIBLE_TABLE_COLUMN_DESCRIPTION_CHANGED
    = "accessibleTableColumnDescriptionChanged";

  /**
   * Constant used when supported set of actions has changed. Both the old
   * and new values are listed in the event.
   *
   * @see AccessibleAction
   */
  public static final String ACCESSIBLE_ACTION_PROPERTY
    = "accessibleActionProperty";

  /**
   * Constant used when a hypertext element received focus. Both the old
   * and new values are listed in the event, with -1 indicating that no link
   * had focus.
   *
   * @see AccessibleHyperlink
   */
  public static final String ACCESSIBLE_HYPERTEXT_OFFSET
    = "AccessibleHypertextOffset";

  /**
   * The accessible parent of this object.
   *
   * @see #getAccessibleParent()
   * @see #setAccessibleParent(Accessible)
   */
  protected Accessible accessibleParent;

  /**
   * A localized string naming this object.
   *
   * @see #getAccessibleName()
   * @see #setAccessibleName(String)
   */
  protected String accessibleName;

  /**
   * A localized string describing this object.
   *
   * @see #getAccessibleDescription()
   * @see #setAccessibleDescription(String)
   */
  protected String accessibleDescription;

  /**
   * The listener tool.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #firePropertyChange(String, Object, Object)
   */
  private final PropertyChangeSupport listeners
    = new PropertyChangeSupport(this);

  /**
   * Default constructor.
   */
  public AccessibleContext()
  {
  }

  /**
   * Get the localized name of the object. For example, a label may just
   * return the text of the label, while an entry field for city may return
   * "city" in en_US.
   *
   * @return the accessible object's name, or null if it is unnamed
   * @see #setAccessibleName(String)
   */
  public String getAccessibleName()
  {
    return accessibleName;
  }

  /**
   * Set the localized name of the object. This will fire a
   * PropertyChangeEvent with ACCESSIBLE_NAME_PROPERTY.
   *
   * @param s the new name
   * @see #getAccessibleName()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public void setAccessibleName(String s)
  {
    listeners.firePropertyChange(ACCESSIBLE_NAME_PROPERTY, accessibleName, s);
    accessibleName = s;
  }

  /**
   * Get the localized description of the object. For example, a 'Cancel'
   * button may be described as "Ignore changes and close dialog box" in
   * en_US.
   *
   * @return the accessible object's description, or null if there is none
   * @see #setAccessibleDescription(String)
   */
  public String getAccessibleDescription()
  {
    return accessibleDescription;
  }

  /**
   * Set the localized name of the object. This will fire a
   * PropertyChangeEvent with ACCESSIBLE_DESCRIPTION_PROPERTY.
   *
   * @param s the new description
   * @see #getAccessibleDescription()
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public void setAccessibleDescription(String s)
  {
    listeners.firePropertyChange(ACCESSIBLE_DESCRIPTION_PROPERTY,
                                 accessibleDescription, s);
    accessibleDescription = s;
  }

  /**
   * Gets the role of this object. For example, a button serves the role of
   * AccessibleRole.PUSH_BUTTON. This allows assistive technologies to funnel
   * similar objects into the same assistance classes. Note that the class
   * is extensible, to define new roles if necessary.
   *
   * @return the role of the object
   * @see AccessibleRole
   */
  public abstract AccessibleRole getAccessibleRole();

  /**
   * Gets the state set of this object. A change in the state of the object
   * will fire a PropertyChangeEvent for ACCESSIBLE_STATE_PROPERTY.
   *
   * @return the current state of the object
   * @see AccessibleState
   * @see AccessibleStateSet
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public abstract AccessibleStateSet getAccessibleStateSet();

  /**
   * Return the accessible parent of this object.
   *
   * @return the accessible parent, or null if there is none
   */
  public Accessible getAccessibleParent()
  {
    return accessibleParent;
  }

  /**
   * Sets the accessible parent of this object. This should only be used when
   * the current parent object should not be the accessible parent; only the
   * parent of the accessible child should call this method.
   *
   * @param a the new parent
   */
  public void setAccessibleParent(Accessible a)
  {
    accessibleParent = a;
  }

  /**
   * Gets the index of this object within its accessible parent.
   *
   * @return the 0-based index, or -1 if there is no accessible parent
   * @see #getAccessibleParent()
   * @see #getAccessibleChildrenCount()
   * @see #getAccessibleChild(int)
   */
  public abstract int getAccessibleIndexInParent();

  /**
   * Returns the number of accessible children of this object.
   *
   * @return the number of accessible children
   * @see #getAccessibleChild(int)
   */
  public abstract int getAccessibleChildrenCount();

  /**
   * Returns the specified accessible chile.
   *
   * @param i the 0-based index to get
   * @return the child, or null if out of bounds
   * @see #getAccessibleChildrenCount()
   */
  public abstract Accessible getAccessibleChild(int i);

  /**
   * Gets the component locale, deferring to the parent if one is not declared.
   *
   * @return the locale
   * @throws java.awt.IllegalComponentStateException if there is no locale
   *         or parent
   */
  public abstract Locale getLocale();

  /**
   * Add a PropertyChangeListener to the listener list. This listener will
   * be notified of all property changes to the accessible object.
   *
   * @param l the listener to add
   * @see #ACCESSIBLE_NAME_PROPERTY
   * @see #ACCESSIBLE_DESCRIPTION_PROPERTY
   * @see #ACCESSIBLE_STATE_PROPERTY
   * @see #ACCESSIBLE_VALUE_PROPERTY
   * @see #ACCESSIBLE_SELECTION_PROPERTY
   * @see #ACCESSIBLE_TEXT_PROPERTY
   * @see #ACCESSIBLE_VISIBLE_DATA_PROPERTY
   * @see #removePropertyChangeListener(PropertyChangeListener)
   */
  public void addPropertyChangeListener(PropertyChangeListener l)
  {
    listeners.addPropertyChangeListener(l);
  }

  /**
   * Remove a PropertyChangeListener from the listener list.
   *
   * @param l the listener to remove
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public void removePropertyChangeListener(PropertyChangeListener l)
  {
    listeners.removePropertyChangeListener(l);
  }

  /**
   * Get any supported accessible actions. The default implementation returns
   * null.
   *
   * @return the supported action, or null
   * @see AccessibleAction
   */
  public AccessibleAction getAccessibleAction()
  {
    return null;
  }

  /**
   * Get any supported accessible compoent. The default implementation returns
   * null.
   *
   * @return the supported component, or null
   * @see AccessibleComponent
   */
  public AccessibleComponent getAccessibleComponent()
  {
    return null;
  }

  /**
   * Get any supported accessible selection. The default implementation returns
   * null.
   *
   * @return the supported selection, or null
   * @see AccessibleSelection
   */
  public AccessibleSelection getAccessibleSelection()
  {
    return null;
  }

  /**
   * Get any supported accessible text. The default implementation returns
   * null.
   *
   * @return the supported text, or null
   * @see AccessibleText
   */
  public AccessibleText getAccessibleText()
  {
    return null;
  }

  /**
   * Get any supported accessible editable text. The default implementation
   * returns null.
   *
   * @return the supported editable text, or null
   * @see AccessibleEditableText
   */
  public AccessibleEditableText getAccessibleEditableText()
  {
    return null;
  }

  /**
   * Get any supported accessible value. The default implementation returns
   * null.
   *
   * @return the supported value, or null
   * @see AccessibleValue
   */
  public AccessibleValue getAccessibleValue()
  {
    return null;
  }

  /**
   * Get all supported accessible icons. The default implementation returns
   * null.
   *
   * @return the supported icons, or null
   * @see AccessibleIcon
   */
  public AccessibleIcon[] getAccessibleIcon()
  {
    return null;
  }

  /**
   * Get any supported accessible relation set. The default implementation
   * returns null.
   *
   * @return the supported relation set, or null
   * @see AccessibleRelationSet
   */
  public AccessibleRelationSet getAccessibleRelationSet()
  {
    return null;
  }

  /**
   * Get any supported accessible table. The default implementation returns
   * null.
   *
   * @return the supported table, or null
   * @see AccessibleTable
   */
  public AccessibleTable getAccessibleTable()
  {
    return null;
  }

  /**
   * Fire an event to report property changes. This is intended for use by
   * the accessible objects, not general application programs. If oldValue and
   * newValue differ, and the listenter list is not empty, a PropertyChange
   * event is fired to each listener.
   *
   * @param name the property name
   * @param oldValue the prior value
   * @param newValue the updated value
   * @see PropertyChangeSupport
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #ACCESSIBLE_NAME_PROPERTY
   * @see #ACCESSIBLE_DESCRIPTION_PROPERTY
   * @see #ACCESSIBLE_STATE_PROPERTY
   * @see #ACCESSIBLE_VALUE_PROPERTY
   * @see #ACCESSIBLE_SELECTION_PROPERTY
   * @see #ACCESSIBLE_TEXT_PROPERTY
   * @see #ACCESSIBLE_VISIBLE_DATA_PROPERTY
   */
  public void firePropertyChange(String name, Object oldValue, Object newValue)
  {
    listeners.firePropertyChange(name, oldValue, newValue);
  }
} // class AccessibleContext
