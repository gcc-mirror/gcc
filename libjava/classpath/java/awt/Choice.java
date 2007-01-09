/* Choice.java -- Java choice button widget.
   Copyright (C) 1999, 2000, 2001, 2002, 2004, 2006 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.peer.ChoicePeer;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * This class implements a drop down choice list.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class Choice extends Component
  implements ItemSelectable, Serializable, Accessible
{
  /**
   * The number used to generate the name returned by getName.
   */
  private static transient long next_choice_number;

  // Serialization constant
  private static final long serialVersionUID = -4075310674757313071L;

  /**
   * @serial A list of items for the choice box, which can be <code>null</code>.
   * This is package-private to avoid an accessor method.
   */
  Vector pItems = new Vector();

  /**
   * @serial The index of the selected item in the choice box.
   */
  private int selectedIndex = -1;

  /**
   * ItemListener chain
   */
  private ItemListener item_listeners;

  /**
   * This class provides accessibility support for the
   * combo box.
   *
   * @author Jerry Quinn  (jlquinn@optonline.net)
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  protected class AccessibleAWTChoice
    extends AccessibleAWTComponent
    implements AccessibleAction
  {

    /**
     * Serialization constant to match JDK 1.5
     */
    private static final long serialVersionUID = 7175603582428509322L;

    /**
     * Default constructor which simply calls the
     * super class for generic component accessibility
     * handling.
     */
    public AccessibleAWTChoice()
    {
      super();
    }

    /**
     * Returns an implementation of the <code>AccessibleAction</code>
     * interface for this accessible object.  In this case, the
     * current instance is simply returned (with a more appropriate
     * type), as it also implements the accessible action as well as
     * the context.
     *
     * @return the accessible action associated with this context.
     * @see javax.accessibility.AccessibleAction
     */
    public AccessibleAction getAccessibleAction()
    {
      return this;
    }

    /**
     * Returns the role of this accessible object.
     *
     * @return the instance of <code>AccessibleRole</code>,
     *         which describes this object.
     * @see javax.accessibility.AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.COMBO_BOX;
    }
	  
    /**
     * Returns the number of actions associated with this accessible
     * object.  In this case, it is the number of choices available.
     *
     * @return the number of choices available.
     * @see javax.accessibility.AccessibleAction#getAccessibleActionCount()
     */
    public int getAccessibleActionCount()
    {
      return pItems.size();
    }

    /**
     * Returns a description of the action with the supplied id.
     * In this case, it is the text used in displaying the particular
     * choice on-screen.
     *
     * @param i the id of the choice whose description should be
     *          retrieved.
     * @return the <code>String</code> used to describe the choice.
     * @see javax.accessibility.AccessibleAction#getAccessibleActionDescription(int)
     */
    public String getAccessibleActionDescription(int i)
    {
      return (String) pItems.get(i);
    }
	  
    /**
     * Executes the action with the specified id.  In this case,
     * calling this method provides the same behaviour as would
     * choosing a choice from the list in a visual manner.
     *
     * @param i the id of the choice to select.
     * @return true if a valid choice was specified.
     * @see javax.accessibility.AccessibleAction#doAccessibleAction(int)
     */
    public boolean doAccessibleAction(int i)
    {
      if (i < 0 || i >= pItems.size())
	return false;
	    
      Choice.this.select( i );

      return true;
    }
  }

  /**
   * Initializes a new instance of <code>Choice</code>.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true
   */
  public Choice()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException ();
  }

  /**
   * Returns the number of items in the list.
   *
   * @return The number of items in the list.
   */
  public int getItemCount()
  {
    return countItems ();
  }

  /**
   * Returns the number of items in the list.
   *
   * @return The number of items in the list.
   *
   * @deprecated This method is deprecated in favor of <code>getItemCount</code>.
   */
  public int countItems()
  {
    return pItems.size();
  }

  /**
   * Returns the item at the specified index in the list.
   *
   * @param index The index into the list to return the item from.
   *
   * @exception ArrayIndexOutOfBoundsException If the index is invalid.
   */
  public String getItem(int index)
  {
    return (String)pItems.elementAt(index);
  }

  /**
   * Adds the specified item to this choice box.
   *
   * @param item The item to add.
   *
   * @exception NullPointerException If the item's value is null
   *
   * @since 1.1
   */
  public synchronized void add(String item)
  {
    if (item == null)
      throw new NullPointerException ("item must be non-null");

    pItems.addElement(item);

    if (peer != null)
      ((ChoicePeer) peer).add(item, getItemCount() - 1);

    if (selectedIndex == -1) 
      select( 0 );
  }

  /**
   * Adds the specified item to this choice box.
   *
   * This method is oboslete since Java 2 platform 1.1. Please use 
   * {@link #add(String)} instead.
   *
   * @param item The item to add.
   *
   * @exception NullPointerException If the item's value is equal to null
   */
  public synchronized void addItem(String item)
  {
    add(item);
  }

  /** Inserts an item into this Choice.  Existing items are shifted
   * upwards.  If the new item is the only item, then it is selected.
   * If the currently selected item is shifted, then the first item is
   * selected.  If the currently selected item is not shifted, then it
   * remains selected.
   *
   * @param item The item to add.
   * @param index The index at which the item should be inserted.
   *
   * @exception IllegalArgumentException If index is less than 0
   */
  public synchronized void insert(String item, int index)
  {
    if (index < 0)
      throw new IllegalArgumentException ("index may not be less then 0");

    if (index > getItemCount ())
      index = getItemCount ();

    pItems.insertElementAt(item, index);

    if (peer != null)
      ((ChoicePeer) peer).add (item, index);

    if (selectedIndex == -1 || selectedIndex >= index)
      select(0);
  }

  /**
   * Removes the specified item from the choice box.
   *
   * @param item The item to remove.
   *
   * @exception IllegalArgumentException If the specified item doesn't exist.
   */
  public synchronized void remove(String item)
  {
    int index = pItems.indexOf(item);
    if (index == -1)
      throw new IllegalArgumentException ("item \""
					  + item + "\" not found in Choice");
    remove(index);
  }

  /**
   * Removes the item at the specified index from the choice box.
   *
   * @param index The index of the item to remove.
   *
   * @exception IndexOutOfBoundsException If the index is not valid.
   */
  public synchronized void remove(int index)
  {
    pItems.removeElementAt(index);

    if (peer != null)
      ((ChoicePeer) peer).remove( index );

    if( getItemCount() == 0 )
      selectedIndex = -1;
    else 
      {
	if( selectedIndex > index ) 
	  selectedIndex--;
	else if( selectedIndex == index )
	  selectedIndex = 0;

	if( peer != null )
	  ((ChoicePeer)peer).select( selectedIndex );
      }
  }

  /**
   * Removes all of the objects from this choice box.
   */
  public synchronized void removeAll()
  {
    if (getItemCount() <= 0)
      return;
  
    pItems.removeAllElements ();

    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	cp.removeAll ();
      }

    selectedIndex = -1;
  }

  /**
   * Returns the currently selected item, or null if no item is
   * selected.
   *
   * @return The currently selected item.
   */
  public synchronized String getSelectedItem()
  {
    return (selectedIndex == -1
	    ? null
	    : ((String)pItems.elementAt(selectedIndex)));
  }

  /**
   * Returns an array with one row containing the selected item.
   *
   * @return An array containing the selected item.
   */
  public synchronized Object[] getSelectedObjects()
  {
    if (selectedIndex == -1)
      return null;

    Object[] objs = new Object[1];
    objs[0] = pItems.elementAt(selectedIndex);

    return objs;
  }

  /**
   * Returns the index of the selected item.
   *
   * @return The index of the selected item.
   */
  public int getSelectedIndex()
  {
    return selectedIndex;
  }

  /**
   * Forces the item at the specified index to be selected.
   *
   * @param index The index of the row to make selected.
   *
   * @exception IllegalArgumentException If the specified index is invalid.
   */
  public synchronized void select(int index)
  {
    if ((index < 0) || (index >= getItemCount()))
      throw new IllegalArgumentException("Bad index: " + index);

    if( selectedIndex == index ) 
      return;

    selectedIndex = index;
    if( peer != null ) 
      ((ChoicePeer)peer).select( index );
  }

  /**
   * Forces the named item to be selected.
   *
   * @param item The item to be selected.
   *
   * @exception IllegalArgumentException If the specified item does not exist.
   */
  public synchronized void select(String item)
  {
    int index = pItems.indexOf(item);
    if( index >= 0 )
      select( index );
  }

  /**
   * Creates the native peer for this object.
   */
  public void addNotify()
  {
    if (peer == null)
      peer = getToolkit ().createChoice (this);
    super.addNotify ();
  }

  /**
   * Adds the specified listener to the list of registered listeners for
   * this object.
   *
   * @param listener The listener to add.
   */
  public synchronized void addItemListener(ItemListener listener)
  {
    item_listeners = AWTEventMulticaster.add(item_listeners, listener);
  }

  /**
   * Removes the specified listener from the list of registered listeners for
   * this object.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeItemListener(ItemListener listener)
  {
    item_listeners = AWTEventMulticaster.remove(item_listeners, listener);
  }

  /**
   * Processes this event by invoking <code>processItemEvent()</code> if the
   * event is an instance of <code>ItemEvent</code>, otherwise the event
   * is passed to the superclass.
   *
   * @param event The event to process.
   */
  protected void processEvent(AWTEvent event)
  {
    if (event instanceof ItemEvent)
      processItemEvent((ItemEvent)event);
    else
      super.processEvent(event);
  }

  void dispatchEventImpl(AWTEvent e)
  {
    super.dispatchEventImpl(e);

    if( e.id <= ItemEvent.ITEM_LAST && e.id >= ItemEvent.ITEM_FIRST && 
	( item_listeners != null || 
	  ( eventMask & AWTEvent.ITEM_EVENT_MASK ) != 0 ) )
      processEvent(e);
  }

  /**
   * Processes item event by dispatching to any registered listeners.
   *
   * @param event The event to process.
   */
  protected void processItemEvent(ItemEvent event)
  {
    int index = pItems.indexOf((String) event.getItem());
    if (item_listeners != null)
      item_listeners.itemStateChanged(event);
  }

  /**
   * Returns a debugging string for this object.
   *
   * @return A debugging string for this object.
   */
  protected String paramString()
  {
    return "selectedIndex=" + selectedIndex + "," + super.paramString();
  }

  /**
   * Returns an array of all the objects currently registered as FooListeners
   * upon this Choice. FooListeners are registered using the addFooListener
   * method.
   *
   * @exception ClassCastException If listenerType doesn't specify a class or
   * interface that implements java.util.EventListener.
   *
   * @since 1.3
   */
  public <T extends EventListener> T[] getListeners (Class<T> listenerType)
  {
    if (listenerType == ItemListener.class)
      return AWTEventMulticaster.getListeners (item_listeners, listenerType);

    return super.getListeners (listenerType);
  }

  /**
   * Returns all registered item listeners.
   *
   * @since 1.4
   */
  public ItemListener[] getItemListeners ()
  {
    return (ItemListener[]) getListeners (ItemListener.class);
  }

  /**
   * Gets the AccessibleContext associated with this <code>Choice</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTChoice();
    return accessibleContext;
  }
  
  /**
   * Generate a unique name for this <code>Choice</code>.
   *
   * @return A unique name for this <code>Choice</code>.
   */
  String generateName()
  {
    return "choice" + getUniqueLong();
  }

  private static synchronized long getUniqueLong()
  {
    return next_choice_number++;
  }
} // class Choice 
