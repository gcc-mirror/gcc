/* JToolBar.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.swing.plaf.ToolBarUI;

/**
 * JToolBar
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JToolBar extends JComponent 
  implements SwingConstants, Accessible
{
  /**
   * AccessibleJToolBar
   */
  protected class AccessibleJToolBar extends AccessibleJComponent
  {
    private static final long serialVersionUID = -5516888265903814215L;

    /**
     * Constructor AccessibleJToolBar
     */
    protected AccessibleJToolBar()
    {
    }

    /**
     * getAccessibleStateSet
     * @return AccessibleStateSet
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      return null; // TODO
    }

    /**
     * getAccessibleRole
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TOOL_BAR;
    }
  }

	/**
	 * Separator
	 */
	public static class Separator extends JSeparator {

	  private static final long serialVersionUID = -1656745644823105219L;
    
		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * separatorSize
		 */
		private Dimension size;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor Separator
		 */
		public Separator() {
			// TODO
		} // Separator()

		/**
		 * Constructor Separator
		 * @param size TODO
		 */
		public Separator(Dimension size) {
			// TODO
		} // Separator()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getUIClassID
		 * @returns String
		 */
		public String getUIClassID() {
			return null; // TODO
		} // getUIClassID()

		/**
		 * getPreferredSize
		 * @returns Dimension
		 */
		public Dimension getPreferredSize() {
			return null; // TODO
		} // getPreferredSize()

		/**
		 * getMaximumSize
		 * @returns Dimension
		 */
		public Dimension getMaximumSize() {
			return null; // TODO
		} // getMaximumSize()

		/**
		 * getMinimumSize
		 * @returns Dimension
		 */
		public Dimension getMinimumSize() {
			return null; // TODO
		} // getMinimumSize()

		/**
		 * getSeparatorSize
		 * @returns Dimension
		 */
		public Dimension getSeparatorSize() {
			return null; // TODO
		} // getSeparatorSize()

		/**
		 * setSeparatorSize
		 * @param size TODO
		 */
		public void setSeparatorSize(Dimension size) {
			// TODO
		} // setSeparatorSize()


	} // Separator

//        /**
//         * DefaultJToolBarLayout
//         */
//        private class DefaultJToolBarLayout {
//
//            private void DefaultJToolBarLayout() {
//            }
//
//            private LayoutManager getLayout() {
//                switch (JToolBar.this.getOrientation()) {
//                    case HORIZONTAL: setLayout(new GridLayout(1, 0, 4, 4));
//                                     break;
//                    case VERTICAL: setLayout(new GridLayout(0, 1, 4, 4));
//                                   break;
//                }
//            }
//        } // DefaultJToolBarLayout


    private static final long serialVersionUID = -1269915519555129643L;
    
	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "ToolBarUI";

	/**
	 * paintBorder
	 */
	private boolean paintBorder;

	/**
	 * margin
	 */
	private Insets margin;

	/**
	 * floatable
	 */
	private boolean floatable;

	/**
	 * orientation
	 */
	private int orientation = HORIZONTAL;

//        protected transient DefaultJToolBarLayout toolbarLayout;

	/** Fired in a PropertyChangeEvent when the "orientation" property changes.
	*/
	public static final String ORIENTATION_CHANGED_PROPERTY = "orientation";

	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JToolBar
	 */
	public JToolBar() {
          this(null);
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param orientation JToolBar orientation (HORIZONTAL or VERTICAL)
	 */
	public JToolBar(int orientation) {
          this(null, orientation);
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param name Name assigned to undocked tool bar.
	 */
	public JToolBar(String name) {
          this(name, HORIZONTAL);
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param name Name assigned to undocked tool bar.
	 * @param orientation JToolBar orientation (HORIZONTAL or VERTICAL)
	 */
	public JToolBar(String name, int orientation) {
	        setName(name);
		if (orientation != HORIZONTAL && orientation != VERTICAL)
			throw new IllegalArgumentException(orientation + " is not a legal orientation");
		this.orientation = orientation;
//                toolbarLayout = new DefaultJToolBarLayout();
                updateUI();	
	} // JToolBar()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * add
	 * @param action TODO
	 * @returns JButton
	 */
	public JButton add(Action action) {
		return null; // TODO
	} // add()

	/**
	 * paintBorder
	 * @param graphics TODO
	 */
	protected void paintBorder(Graphics graphics) {
		// TODO
	} // paintBorder()

	/**
	 * getUI
	 * @returns ToolBarUI
	 */
	public ToolBarUI getUI() {
	    System.out.println("ui = " + ui);
		return (ToolBarUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(ToolBarUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
          setUI((ToolBarUI)UIManager.getUI(this));
	} // updateUI()

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getComponentIndex
	 * @param component TODO
	 * @returns int
	 */
	public int getComponentIndex(Component component) {
		return 0; // TODO
	} // getComponentIndex()

	/**
	 * getComponentAtIndex
	 * @param index TODO
	 * @returns Component
	 */
	public Component getComponentAtIndex(int index) {
		return null; // TODO
	} // getComponentAtIndex()

	/**
	 * getMargin
	 * @returns Insets
	 */
	public Insets getMargin() {
		return null; // TODO
	} // getMargin()

	/**
	 * setMargin
	 * @param margin TODO
	 */
	public void setMargin(Insets margin) {
		// TODO
	} // setMargin()

	/**
	 * isBorderPainted
	 * @returns boolean
	 */
	public boolean isBorderPainted() {
		return false; // TODO
	} // isBorderPainted()

	/**
	 * setBorderPainted
	 * @param painted TODO
	 */
	public void setBorderPainted(boolean painted) {
		// TODO
	} // setBorderPainted()

	/**
	 * isFloatable
	 * @returns boolean
	 */
	public boolean isFloatable() {
		return false; // TODO
	} // isFloatable()

	/**
	 * setFloatable
	 * @param floatable TODO
	 */
	public void setFloatable(boolean floatable) {
		// TODO
	} // setFloatable()

	/**
	 * getOrientation
	 * @returns int
	 */
	public int getOrientation() {
		return this.orientation;
	} // getOrientation()

	/**
	 * setLayout
	 * @param mgr
	 */
	public void setLayout(LayoutManager mgr) {
	    super.setLayout(mgr);
	} // setLayout()

	/**
	 * setOrientation
	 * @param orientation
	 */
	public void setOrientation(int orientation) {
		if (orientation != HORIZONTAL && orientation != VERTICAL)
			throw new IllegalArgumentException(orientation + " is not a legal orientation");
	    if (orientation != this.orientation)
	    {
		int oldOrientation = this.orientation;
		this.orientation = orientation;
		firePropertyChange(ORIENTATION_CHANGED_PROPERTY, oldOrientation,
			this.orientation);
	    }
	} // setOrientation()

	/**
	 * addSeparator
	 */
	public void addSeparator() {
		// TODO
	} // addSeparator()

	/**
	 * addSeparator
	 * @param size TODO
	 */
	public void addSeparator(Dimension size) {
		// TODO
	} // addSeparator()

	/**
	 * createActionComponent
	 * @param action TODO
	 * @returns JButton
	 */
	protected JButton createActionComponent(Action action) {
		return null; // TODO
	} // createActionComponent()

	/**
	 * createActionChangeListener
	 * @param button TODO
	 * @returns PropertyChangeListener
	 */
	protected PropertyChangeListener createActionChangeListener(JButton button) {
		return null; // TODO
	} // createActionChangeListener()

	/**
	 * addImpl
	 * @param component TODO
	 * @param constraints TODO
	 * @param index TODO
	 */
  /*
	protected void addImpl(Component component, Object constraints, int index) {
		// TODO
	} // addImpl()
  */
	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

  /**
   * getAccessibleContext
   * @return AccessibleContext
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJToolBar();
    
    return accessibleContext;
  }
}
