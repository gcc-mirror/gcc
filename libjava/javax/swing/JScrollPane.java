/* JScrollPane.java -- 
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;

import javax.accessibility.Accessible;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ScrollPaneUI;
import javax.swing.plaf.UIResource;

/**
 * <table>
 * <tr><th>Property                    </th><th>Stored in       </th><th>Bound?</th></tr>
 * <tr><td>columnHeader                </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>columnHeaderView            </td><td>columnHeader    </td><td>no    </td></tr>
 * <tr><td>componentOrientation        </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>horizontalScrollBar         </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>horizontalScrollBarPolicy   </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>layout                      </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>rowHeader                   </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>rowHeaderView               </td><td>rowHeader       </td><td>no    </td></tr>
 * <tr><td>validateRoot                </td><td>scrollPane      </td><td>no    </td></tr>
 * <tr><td>verticalScrollBar           </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>verticalScrollBarPolicy     </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>viewport                    </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>viewportBorder              </td><td>scrollPane      </td><td>yes   </td></tr>
 * <tr><td>viewportBorderBounds        </td><td>scrollPane      </td><td>no    </td></tr>
 * <tr><td>viewportView                </td><td>viewport        </td><td>no    </td></tr>
 * <tr><td>wheelScrollingEnabled       </td><td>scrollPane      </td><td>yes   </td></tr>
 * </table>
 */
public class JScrollPane 
  extends JComponent 
  implements Accessible, ScrollPaneConstants
{
  private static final long serialVersionUID = 5203525440012340014L;
  
  protected JViewport columnHeader;
  protected JViewport rowHeader;

  protected Component lowerLeft;
  protected Component lowerRight;
  protected Component upperLeft;
  protected Component upperRight;

  protected JScrollBar horizontalScrollBar;
  protected int horizontalScrollBarPolicy;
  protected JScrollBar verticalScrollBar;
  protected int verticalScrollBarPolicy;

  protected JViewport viewport;
  
  Border viewportBorder;
  boolean wheelScrollingEnabled;
  ChangeListener scrollListener;  

  public JViewport getColumnHeader()
  {
    return columnHeader;
  }

  public Component getCorner(String key) {
    if (getComponentOrientation() 
        == ComponentOrientation.LEFT_TO_RIGHT)
      {
        if (key == LOWER_LEADING_CORNER)
          key = LOWER_LEFT_CORNER;
        else if (key == LOWER_TRAILING_CORNER)
          key = LOWER_RIGHT_CORNER;
        else if (key == UPPER_LEADING_CORNER)
          key = UPPER_LEFT_CORNER;
        else if (key == UPPER_TRAILING_CORNER)
          key = UPPER_RIGHT_CORNER;
      }
    else if (getComponentOrientation() 
             == ComponentOrientation.RIGHT_TO_LEFT)
      {
        if (key == LOWER_LEADING_CORNER)
          key = LOWER_RIGHT_CORNER;
        else if (key == LOWER_TRAILING_CORNER)
          key = LOWER_LEFT_CORNER;
        else if (key == UPPER_LEADING_CORNER)
          key = UPPER_RIGHT_CORNER;
        else if (key == UPPER_TRAILING_CORNER)
          key = UPPER_LEFT_CORNER;
      }

    if (key == LOWER_RIGHT_CORNER)
      return lowerRight;
    else if (key == UPPER_RIGHT_CORNER)
      return upperRight;
    else if (key == LOWER_LEFT_CORNER)
      return lowerLeft;
    else if (key == UPPER_LEFT_CORNER)
      return upperLeft;
    return null;
  }

  public JScrollBar getHorizontalScrollBar()
  {
    return horizontalScrollBar;
  }

  public int getHorizontalScrollBarPolicy()
  {
    return horizontalScrollBarPolicy;
  }

  public JViewport getRowHeader()
  {
    return rowHeader;
  }

  public JScrollBar getVerticalScrollBar()
  {
    return verticalScrollBar;
  }

  public int getVerticalScrollBarPolicy()
  {
    return verticalScrollBarPolicy;
  }

  public JViewport getViewport()
  {
    return viewport;
  }

  public Border getViewportBorder()
  {
    return viewportBorder;
  }

  public Rectangle getViewportBorderBounds()
  {
    if (viewportBorder == null)
      {
        if (getViewport() == null)
          return new Rectangle(0,0,0,0);
        else
          return getViewport().getBounds();
      }
    else
      {
        Insets i = viewportBorder.getBorderInsets(getViewport());
        if (getViewport() == null)
          return new Rectangle(0,0,
                               i.left+i.right, i.top+i.bottom);
        else
          {
            Rectangle b = getViewport().getBounds();
            return new Rectangle(b.x - i.left, 
                                 b.y - i.top,
                                 b.width + i.left + i.right, 
                                 b.height + i.top + i.bottom);
          }
      }
  }
  
  public boolean isWheelScrollingEnabled()
  {
    return wheelScrollingEnabled;
  }



  private void sync()
  {
    LayoutManager m = super.getLayout();
    if (m != null && m instanceof ScrollPaneLayout)
      {
        ScrollPaneLayout sl = (ScrollPaneLayout) m;
        sl.syncWithScrollPane(this);
      }
  }

  private void removeNonNull(Component c)
  {
    if (c != null)
      remove(c);
  }

  private void addNonNull(Component c)
  {
    if (c != null)
      add(c);
  }

  public void setComponentOrientation(ComponentOrientation co)
  {
    ComponentOrientation old = super.getComponentOrientation();
    super.setComponentOrientation(co);
    firePropertyChange("componentOrientation", old, co);
    sync();
  }

  public void setColumnHeader(JViewport h)
  {
    if (columnHeader == h)
      return;
    
    JViewport old = columnHeader;
    removeNonNull(old);
    columnHeader = h;
    addNonNull(h);
    firePropertyChange("columnHeader", old, h);
    sync();
  }

  public void setColumnHeaderView(Component c)
  {
    if (columnHeader == null)
      setColumnHeader(createViewport());
    columnHeader.setView(c);
    sync();
  }

  public void setCorner(String key, Component c)
  {
    if (getComponentOrientation() 
        == ComponentOrientation.LEFT_TO_RIGHT)
      {
        if (key == LOWER_LEADING_CORNER)
          key = LOWER_LEFT_CORNER;
        else if (key == LOWER_TRAILING_CORNER)
          key = LOWER_RIGHT_CORNER;
        else if (key == UPPER_LEADING_CORNER)
          key = UPPER_LEFT_CORNER;
        else if (key == UPPER_TRAILING_CORNER)
          key = UPPER_RIGHT_CORNER;
      }
    else if (getComponentOrientation() 
             == ComponentOrientation.RIGHT_TO_LEFT)
      {
        if (key == LOWER_LEADING_CORNER)
          key = LOWER_RIGHT_CORNER;
        else if (key == LOWER_TRAILING_CORNER)
          key = LOWER_LEFT_CORNER;
        else if (key == UPPER_LEADING_CORNER)
          key = UPPER_RIGHT_CORNER;
        else if (key == UPPER_TRAILING_CORNER)
          key = UPPER_LEFT_CORNER;
      }

    if (key == LOWER_RIGHT_CORNER)
      {
        removeNonNull(lowerRight);
        lowerRight = c;
        addNonNull(c);
      }
    else if (key == UPPER_RIGHT_CORNER)
      {
        removeNonNull(upperRight);
        upperRight = c;
        addNonNull(c);
      }
    else if (key == LOWER_LEFT_CORNER)
      {
        removeNonNull(lowerLeft);
        lowerLeft = c;
        addNonNull(c);
      }
    else if (key == UPPER_LEFT_CORNER)
      {
        removeNonNull(upperLeft);
        upperLeft = c;
        addNonNull(c);
      }
    else
      throw new IllegalArgumentException("unknown corner " + key);
    sync();
  }

  public void setHorizontalScrollBar(JScrollBar h)
  {
    if (horizontalScrollBar == h)
      return;

    JScrollBar old = horizontalScrollBar;
    removeNonNull(old);
    horizontalScrollBar = h;
    addNonNull(h);
    firePropertyChange("horizontalScrollBar", old, h);
    sync();

    if (old != null)
      {
        BoundedRangeModel model = old.getModel();
        if (model != null)
          model.removeChangeListener(scrollListener);
      }
    if (h != null)
      {
        BoundedRangeModel model = h.getModel();
        if (model != null)
          model.addChangeListener(scrollListener);
      }
  }

  public void setHorizontalScrollBarPolicy(int h)
  {
    if (horizontalScrollBarPolicy == h)
      return;
    
    if (h != HORIZONTAL_SCROLLBAR_AS_NEEDED
        && h != HORIZONTAL_SCROLLBAR_NEVER
        && h != HORIZONTAL_SCROLLBAR_ALWAYS)
      throw new IllegalArgumentException("unknown horizontal scrollbar policy");    

    int old = horizontalScrollBarPolicy;
    horizontalScrollBarPolicy = h;
    firePropertyChange("horizontalScrollBarPolicy", old, h);
    sync();
  }

  public void setLayout(LayoutManager l)
  {
    LayoutManager old = super.getLayout();
    ScrollPaneLayout tmp = (ScrollPaneLayout) l;
    super.setLayout(l);
    tmp.syncWithScrollPane(this);
    firePropertyChange("layout", old, l);
    sync();
  }

  public void setRowHeader(JViewport v)
  {
    if (rowHeader == v)
      return;
    
    JViewport old = rowHeader;
    removeNonNull(old);
    rowHeader = v;
    addNonNull(v);
    firePropertyChange("rowHeader", old, v);
    sync();
  }

  public void setRowHeaderView(Component c)
  {
    if (rowHeader == null)
      setRowHeader(createViewport());
    rowHeader.setView(c);
    sync();
  }

  public void setVerticalScrollBar(JScrollBar v)
  {
    if (verticalScrollBar == v)
      return;
    
    JScrollBar old = verticalScrollBar;
    removeNonNull(old);
    verticalScrollBar = v;
    addNonNull(v);
    firePropertyChange("verticalScrollBar", old, v);
    sync();

    if (old != null)
      {
        BoundedRangeModel model = old.getModel();
        if (model != null)
          model.removeChangeListener(scrollListener);
      }
    if (v != null)
      {
        BoundedRangeModel model = v.getModel();
        if (model != null)
          model.addChangeListener(scrollListener);
      }
  }

  public void setVerticalScrollBarPolicy(int v)
  {
    if (verticalScrollBarPolicy == v)
      return;
    
    if (v != VERTICAL_SCROLLBAR_AS_NEEDED
        && v != VERTICAL_SCROLLBAR_NEVER
        && v != VERTICAL_SCROLLBAR_ALWAYS)
      throw new IllegalArgumentException("unknown vertical scrollbar policy");    
    
    int old = verticalScrollBarPolicy;
    verticalScrollBarPolicy = v;
    firePropertyChange("verticalScrollBarPolicy", old, v);
    sync();
  }

  public void setWheelScrollingEnabled(boolean b)
  {
    if (wheelScrollingEnabled == b)
      return;
    
    boolean old = wheelScrollingEnabled;
    wheelScrollingEnabled = b;
    firePropertyChange("wheelScrollingEnabled", old, b);
    sync();
  }

  public void setViewport(JViewport v)
  {
    if (viewport == v)
      return;
    
    JViewport old = viewport;
    removeNonNull(old);
    if (old != null)
      old.removeChangeListener(scrollListener);
    viewport = v;
    if (v != null)
      v.addChangeListener(scrollListener);
    addNonNull(v);
    revalidate();
    repaint();
    firePropertyChange("viewport", old, v);
    sync();
  }

  public void setViewportBorder(Border b)
  {
    if (viewportBorder == b)
      return;
    
    Border old = viewportBorder;
    viewportBorder = b;
    firePropertyChange("viewportBorder", old, b);
    sync();
  }
    
  public void setViewportView(Component view)
  {
    if (getViewport() == null)
      {
        setViewport(createViewport());
      }
	
    if (view != null)
      {
        getViewport().setView(view);
      }
    sync();
  }

  public boolean isValidateRoot()
  {
    return true;
  }

  ChangeListener createScrollListener()
  {
    return new ChangeListener() 
      {
        
        public void stateChanged(ChangeEvent event)
        {
          JScrollBar vsb = JScrollPane.this.getVerticalScrollBar();
          JScrollBar hsb = JScrollPane.this.getHorizontalScrollBar();
          JViewport vp = JScrollPane.this.getViewport();

          if (vp != null && event.getSource() == vp)
            {
              // if the viewport changed, we should update the VSB / HSB
              // models according to the new vertical and horizontal sizes

              Rectangle vr = vp.getViewRect();
              Dimension vs = vp.getViewSize();
              if (vsb != null
                  && (vsb.getMinimum() != 0
                      || vsb.getMaximum() != vs.height
                      || vsb.getValue() != vr.y
                      || vsb.getVisibleAmount() != vr.height))
                vsb.setValues(vr.y, vr.height, 0, vs.height);

              if (hsb != null
                  && (hsb.getMinimum() != 0
                      || hsb.getMaximum() != vs.width
                      || hsb.getValue() != vr.width
                      || hsb.getVisibleAmount() != vr.height))
                hsb.setValues(vr.x, vr.width, 0, vs.width);
            }
          else
            {
              // otherwise we got a change update from either the VSB or
              // HSB model, and we need to update the viewport positions of
              // both the main viewport and any row or column headers to
              // match.

              int xpos = 0;
              int ypos = 0;
              
              if (vsb != null)
                ypos = vsb.getValue();
              
              if (hsb != null)
                xpos = hsb.getValue();

              Point pt = new Point(xpos, ypos);

              if (vp != null
                  && vp.getViewPosition() != pt)
                vp.setViewPosition(pt);

              pt.x = 0;

              if (rowHeader != null 
                  && rowHeader.getViewPosition() != pt)
                rowHeader.setViewPosition(pt);
              
              pt.x = xpos;
              pt.y = 0;

              if (columnHeader != null 
                  && columnHeader.getViewPosition() != pt)
                columnHeader.setViewPosition(pt);

            }
        }
      };
  }


  public JScrollPane() 
  {
    this(null);
  }
    
  public JScrollPane(Component view) 
  {
    this(view, 
         VERTICAL_SCROLLBAR_AS_NEEDED, 
         HORIZONTAL_SCROLLBAR_AS_NEEDED);
  }

  public JScrollPane(int vsbPolicy, int hsbPolicy) 
  {
    this(null, vsbPolicy, hsbPolicy);
  }

  public JScrollPane(Component view, int vsbPolicy, int hsbPolicy) 
  {
    scrollListener = createScrollListener();
    setVerticalScrollBarPolicy(vsbPolicy);
    setVerticalScrollBar(createVerticalScrollBar());
    setHorizontalScrollBarPolicy(hsbPolicy);
    setHorizontalScrollBar(createHorizontalScrollBar());
    setViewportView(view);
    setLayout(new ScrollPaneLayout());
    setOpaque(false);
    updateUI();
  }

  
  public JScrollBar createHorizontalScrollBar()
  {
    return new ScrollBar(SwingConstants.HORIZONTAL);
  }

  public JScrollBar createVerticalScrollBar()
  {
    return new ScrollBar(SwingConstants.VERTICAL);
  }
    
  protected JViewport createViewport()
  {
    return new JViewport();
  }

  public String getUIClassID()
  {
    return "ScrollPaneUI";
  }
  
  public void updateUI()
  {
    ScrollPaneUI b = (ScrollPaneUI)UIManager.getUI(this);
    setUI(b);
  }  

  /**
   * This method returns the scrollpane's UI delegate.
   *
   * @return The scrollpane's UI delegate.
   */
  public ScrollPaneUI getUI()
  {
    return (ScrollPaneUI) ui;
  }

  /**
   * This method sets the scrollpane's UI delegate.
   *
   * @param ui The scrollpane's UI delegate.
   */
  public void setUI(ScrollPaneUI ui)
  {
    super.setUI(ui);
  }

  protected class ScrollBar 
    extends JScrollBar
    implements UIResource
  {
    public ScrollBar(int orientation)
    {
      super(orientation);
    }

    public int getBlockIncrement(int direction)
    {
      Component view = JScrollPane.this.getViewport().getView();
      if (view == null || (! (view instanceof Scrollable)))
        return super.getBlockIncrement(direction);
      else
        {
          Scrollable s = (Scrollable) view;
          return s.getScrollableBlockIncrement(JScrollPane.this.getViewport().getViewRect(), 
                                               this.getOrientation(),
                                               direction);
        }
    }

    public int getUnitIncrement(int direction)
    {
      Component view = JScrollPane.this.getViewport().getView();
      if (view == null || (! (view instanceof Scrollable)))
        return super.getUnitIncrement(direction);
      else
        {
          Scrollable s = (Scrollable) view;
          return s.getScrollableUnitIncrement(JScrollPane.this.getViewport().getViewRect(), 
                                              this.getOrientation(),
                                              direction);
        }
    }
  }
}
