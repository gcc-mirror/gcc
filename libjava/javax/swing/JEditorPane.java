/* JEditorPane.java -- 
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
import java.awt.event.KeyEvent;
import java.io.InputStream;
import java.net.URL;
import javax.accessibility.AccessibleContext;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainEditorKit;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

public class JEditorPane extends JTextComponent
{
    URL page_url;
    EditorKit kit;
    String ctype = "text/plain";
    boolean focus_root;
    boolean manages_focus;


    public JEditorPane()
    {
    }

    public JEditorPane(String url)
    {
	this();
	setPage(url);
    }
    
    public JEditorPane(String type, String text)
    {
	ctype = text;
	setText(text);
    }
    
    public JEditorPane(URL url)
    {
	setPage(url);
    }

    void addHyperlinkListener(HyperlinkListener listener)
    {  }
    
    protected  EditorKit createDefaultEditorKit()
    {	return new PlainEditorKit();    }
    
    static EditorKit createEditorKitForContentType(String type)
    {	return new PlainEditorKit();     }
    
  void fireHyperlinkUpdate(HyperlinkEvent e)
  {
  }

  public AccessibleContext getAccessibleContext()
  {      return null;  }

  String getContentType()
    {  return ctype;   }

  EditorKit getEditorKit()
    {  return kit;    }
    
  static String getEditorKitClassNameForContentType(String type)
    { return "text/plain";  }
  
  EditorKit getEditorKitForContentType(String type)
    { return kit;  }
    
    public Dimension getPreferredSize()
    {
	//Returns the preferred size for the JEditorPane.  
	return super.getPreferredSize();
    }

  public boolean getScrollableTracksViewportHeight()
    {  return false;  }
  public boolean getScrollableTracksViewportWidth()
    {  return false;  }

  URL getPage()
    { return page_url;  }

  protected  InputStream getStream(URL page)
    {	
	try {
	    return page.openStream();    
	} catch (Exception e) {
	    System.out.println("Hhmmm, failed to open stream: " + e);
	}	
	return null;
    }

    public String getText()
    { return super.getText();    }
    
    public String getUIClassID()
    {    return "JEditorPane";  }

    public boolean isFocusCycleRoot()
    { return focus_root;    }

    public boolean isManagingFocus()
    { return manages_focus;  }

  protected  String paramString()
    { return "JEditorPane";  }
    
  protected  void processComponentKeyEvent(KeyEvent e)
    {
	//Overridden to handle processing of tab/shift tab. 
    }
    
  protected void processKeyEvent(KeyEvent e)
    {
	//Make sure that TAB and Shift-TAB events get consumed, so that awt doesn't attempt focus traversal.  
    }
    
    void read(InputStream in, Object desc)
    {
	//This method initializes from a stream. 
    }
    
    static void registerEditorKitForContentType(String type, String classname)
    {
	//Establishes the default bindings of type to classname. 
    }
    
    static void registerEditorKitForContentType(String type, String classname, ClassLoader loader)
    {
	//Establishes the default bindings of type to classname.  
    }
    
    void removeHyperlinkListener(HyperlinkListener listener)
    {
	//Removes a hyperlink listener.  
    }
    
    void replaceSelection(String content)
    {
	//Replaces the currently selected content with new content represented by the given string. 
    }
    
    protected  void scrollToReference(String reference)
    {
	//Scrolls the view to the given reference location (that is, the value returned by the UL.getRef method for the URL being displayed).  
    }
    
    void setContentType(String type)
    {
	ctype = type;
	invalidate();
	repaint();
    }
    
    void setEditorKit(EditorKit kit)
    {
	this.kit = kit;
	invalidate();
	repaint();
    }
    
    void setEditorKitForContentType(String type, EditorKit k)
    {
	ctype = type;
	setEditorKit(k);
    }
  
  void setPage(String url)
    {
	//  Sets the current URL being displayed.  
    }
    
    void setPage(URL page)
    {
	//    Sets the current URL being displayed.  
    }
    
    public void setText(String t)
    {	
	super.setText(t);
    }
} // class JEditorPane
