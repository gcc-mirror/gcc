/* JList.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionListener;
import javax.swing.plaf.ListUI;

public class JList extends JComponent implements Accessible, Scrollable
{
    Color select_back, select_fore;
    ListCellRenderer render;
    int visibles = 8;
    
    ListModel          model;
    ListSelectionModel sel_model;

    public JList()
    {	
	init();
    }

    public JList(Object[] listData)
    {
	init();
	setListData(listData);
    }


    public JList(Vector listData)
    {
	init();
	setListData(listData);
    }


    public JList(ListModel listData)
    {
	init();
	setModel(listData);
    }
    void init()
    {
	render = new DefaultCellRenderer();
	
	sel_model = new DefaultListSelectionModel();
	setModel(new DefaultListModel());

	select_back = new Color(0,0,255);
	select_fore = new Color(255,255,255);

	updateUI();
    }

    
    public int getVisibleRowCount()
    { return visibles; }
    public void setVisibleRowCount(int visibleRowCount)
    {
	visibles =  visibleRowCount;
	invalidate();
	repaint();
    }

    void addListSelectionListener(ListSelectionListener listener)
    { sel_model.addListSelectionListener(listener);    }
    void removeListSelectionListener(ListSelectionListener listener)
    { sel_model.removeListSelectionListener(listener);    }

    void setSelectionMode(int a)
    {  sel_model.setSelectionMode(a);   }
    void setSelectedIndex(int a)
    {  sel_model.setSelectionInterval(a,a); }
    int getSelectedIndex()
    {	return sel_model.getMinSelectionIndex();    }
    Object getSelectedValue()
    {  
	int index = getSelectedIndex();
	if (index == -1)
	    return null;
	return getModel().getElementAt(index);
    }

    Color getSelectionBackground()
    { return select_back;    }    
    Color getSelectionForeground()
    { return select_fore;    }


    public void setListData(final Object[] listData)
    {
	class AL extends AbstractListModel
	{
	    public int getSize()              { return listData.length; }
	    public Object getElementAt(int i) { return listData[i];     }
	};
	
	setModel (new AL());
    }
    
    public void setListData(final Vector listData)
    {
	class AL extends AbstractListModel 
	{
	    public int getSize()              { return listData.size(); }
	    public Object getElementAt(int i) { return listData.elementAt(i); }
	};
	
        setModel (new AL());
    }
    
    
    public ListCellRenderer getCellRenderer()
    {    return  render; }
    public void setCellRenderer(ListCellRenderer cellRenderer)
    {
	render = cellRenderer;
	invalidate();
	repaint();
    }
    
    public void setModel(ListModel model)
    {
	ListDataListener l = new ListDataListener()
	    {
		public void intervalAdded(ListDataEvent e) {
		    repaint();
		}
		public void intervalRemoved(ListDataEvent e) {
		    repaint();
		}
		public void contentsChanged(ListDataEvent e) {
		    repaint();
		}
	    };
	
	this.model = model;  
	model.addListDataListener(l);	
    }

    public ListModel getModel() 
    {  return model;        }
    
    
    public ListUI getUI()
    {  return (ListUI) ui;    }
    public void setUI(ListUI ui)
    {   super.setUI(ui);      }

    public void updateUI()
    {
        setUI((ListUI)UIManager.getUI(this));
    }

    public String getUIClassID()
    {
	return "JList";
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
