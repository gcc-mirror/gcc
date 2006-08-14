/* TableView.java -- A table view for HTML tables
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * A conrete implementation of TableView that renders HTML tables.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
class TableView
    extends javax.swing.text.TableView
{
  /**
   * Represents a single table row.
   */
  public class RowView extends TableRow
  {
    /**
     * Creates a new instance of the <code>RowView</code>.
     *
     * @param el the element for which to create a row view
     */
    public RowView(Element el)
    {
      super(el);
    }
    
  /**
   * Get the associated style sheet from the document.
   * 
   * @return the associated style sheet.
   */
    protected StyleSheet getStyleSheet()
    {
      Document d = getElement().getDocument();
      if (d instanceof HTMLDocument)
        return ((HTMLDocument) d).getStyleSheet();
      else
        return null;
    }    
  }

  /**
   * Creates a new HTML table view for the specified element.
   *
   * @param el the element for the table view
   */
  public TableView(Element el)
  {
    super(el);
  }
  
  /**
   * Get the associated style sheet from the document.
   * 
   * @return the associated style sheet.
   */
  protected StyleSheet getStyleSheet()
  {
    Document d = getElement().getDocument();
    if (d instanceof HTMLDocument)
      return ((HTMLDocument) d).getStyleSheet();
    else
      return null;
  }  
  
  /**
   * Creates a view for a table row.
   * 
   * @param el the element that represents the table row
   * @return a view for rendering the table row 
   * (and instance of {@link RowView}).
   */
  protected TableRow createTableRow(Element el) 
  {
    return new RowView(el);
  }  
  
  /**
   * Loads the children of the Table. This completely bypasses the ViewFactory
   * and creates instances of TableRow instead.
   *
   * @param vf ignored
   */
  protected void loadChildren(ViewFactory vf)
  {
    Element el = getElement();
    int numChildren = el.getElementCount();
    View[] rows = new View[numChildren];
    for (int i = 0; i < numChildren; ++i)
      {
        rows[i] = createTableRow(el.getElement(i));
      }
    replace(0, getViewCount(), rows);
  }
}
