/* JTree.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.Rectangle;
import java.io.Serializable;
import java.util.Hashtable;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.plaf.TreeUI;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class JTree extends JComponent implements Scrollable, Accessible
{
    JTree()
    {
	updateUI();
    }
    
    public TreeUI getUI()
    {
        return (TreeUI) ui;
    }

    public void setUI(TreeUI ui)
    {
        super.setUI(ui);
    }

    public void updateUI()
    {
        setUI((TreeUI)UIManager.getUI(this));
    }

    
    public String getUIClassID()
    {
	return "JTree";
    }


    public AccessibleContext getAccessibleContext()
    {
      return null;
    }

    public Dimension getPreferredScrollableViewportSize()
    {
	return null;
    }

    public int getScrollableUnitIncrement(Rectangle visibleRect,
					  int orientation,
					  int direction)
    {
	return 1;
    }

    public int getScrollableBlockIncrement(Rectangle visibleRect,
					   int orientation,
					   int direction)
    {
	return 1;
    }

    public boolean getScrollableTracksViewportWidth()
    {
	return false;
    }

    public boolean getScrollableTracksViewportHeight()
    {
	return false;
    }
}



