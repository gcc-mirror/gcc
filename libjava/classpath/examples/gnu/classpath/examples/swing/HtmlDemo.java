/* HtmlDemo.java -- HTML viewer demo
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


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.LinkedList;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.html.FormSubmitEvent;

/**
 * Parses and displays HTML content.
 *
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class HtmlDemo extends JPanel
{

  private class LoadActionListener
    implements ActionListener
  {

    public void actionPerformed(ActionEvent event)
    {
      String urlStr = url.getText();
      try
        {
          setPage(new URL(url.getText()));
        }
      catch (MalformedURLException ex)
        {
          // Do something more useful here.
          ex.printStackTrace();
        }
    }
  }

  /**
   * Setting this to true causes the parsed element structure to be dumped.
   */
  private static final boolean DEBUG = true;

  /**
   * The URL entry field.
   */
  JTextField url = new JTextField();

  JTextPane html = new JTextPane();

  int n;

  /**
   * The browsing history.
   *
   * Package private to avoid accessor method.
   */
  LinkedList history;

  public HtmlDemo()
  {
    super();
    history = new LinkedList();
    createContent();
  }

  /**
   * Returns a panel with the demo content. The panel uses a BorderLayout(), and
   * the BorderLayout.SOUTH area is empty, to allow callers to add controls to
   * the bottom of the panel if they want to (a close button is added if this
   * demo is being run as a standalone demo).
   */
  private void createContent()
  {
    setLayout(new BorderLayout());

    JEditorPane.registerEditorKitForContentType("text/html",
                                             BrowserEditorKit.class.getName());
    html.setEditable(false);
    html.addHyperlinkListener(new HyperlinkListener()
    {

      public void hyperlinkUpdate(HyperlinkEvent event)
      {
        if (event instanceof FormSubmitEvent)
          {
            submitForm((FormSubmitEvent) event);
          }
        else
          {
            URL u = event.getURL();
            if (u != null)
              {
                setPage(u);
              }
          }
      }

    });

    JScrollPane scroller = new JScrollPane(html);
    JPanel urlPanel = new JPanel();
    urlPanel.setLayout(new BoxLayout(urlPanel, BoxLayout.X_AXIS));
    url.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
    LoadActionListener action = new LoadActionListener();
    url.addActionListener(action);
    urlPanel.add(url);
    JButton loadButton = new JButton("go");
    urlPanel.add(loadButton);
    loadButton.addActionListener(action);

    // Setup control panel.
    JToolBar controlPanel = createToolBar();
    JPanel browserPanel = new JPanel();
    browserPanel.setLayout(new BorderLayout());
    browserPanel.add(urlPanel, BorderLayout.NORTH);
    browserPanel.add(scroller, BorderLayout.CENTER);
    add(controlPanel, BorderLayout.NORTH);
    add(browserPanel, BorderLayout.CENTER);

    // Load start page.
    try
      {
        URL startpage = getClass().getResource("welcome.html");
        html.setPage(startpage);
        url.setText(startpage.toString());
        history.addLast(startpage);
      }
    catch (Exception ex)
      {
        System.err.println("couldn't load page: "/* + startpage*/);
        ex.printStackTrace();
      }
    setPreferredSize(new Dimension(800, 600));
  }


  /**
   * Creates the toolbar with the control buttons.
   *
   * @return the toolbar with the control buttons
   */
  JToolBar createToolBar()
  {
    JToolBar tb = new JToolBar();
    Icon backIcon = Demo.getIcon("/gnu/classpath/examples/icons/back.png",
                                 "back");
    JButton back = new JButton(backIcon);
    back.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        if (history.size() > 1)
          {
            URL last = (URL) history.removeLast();
            last = (URL) history.getLast();
            url.setText(last.toString());
            try
              {
                html.setPage(last);
              }
            catch (IOException ex)
              {
                // Do something more useful.
                ex.printStackTrace();
              }
          }
      }
    });
    tb.add(back);
    Icon reloadIcon = Demo.getIcon("/gnu/classpath/examples/icons/reload.png",
                                   "reload");
    JButton reload = new JButton(reloadIcon);
    reload.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        if (history.size() > 0)
          {
            URL last = (URL) history.getLast();
            url.setText(last.toString());
            try
              {
                html.setPage(last);
              }
            catch (IOException ex)
              {
                // Do something more useful.
                ex.printStackTrace();
              }
          }
      }
    });
    tb.add(reload);
    return tb;
  }

  /**
   * The executable method to display the editable table.
   *
   * @param args
   *          unused.
   */
  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         HtmlDemo demo = new HtmlDemo();
         JFrame frame = new JFrame();
         frame.getContentPane().add(demo);
         frame.setSize(new Dimension(750, 480));
         frame.setVisible(true);
       }
     });
  }

  /**
   * Helper method to navigate to a new URL.
   *
   * @param u the new URL to navigate to
   */
  void setPage(URL u)
  {
    try
      {
        url.setText(u.toString());
        html.setPage(u);
        history.addLast(u);
      }
    catch (IOException ex)
      {
        // Do something more useful here.
        ex.printStackTrace();
      }
  }

  /**
   * Submits a form when a FormSubmitEvent is received. The HTML API
   * provides automatic form submit but when this is enabled we don't
   * receive any notification and can't update our location field.
   *
   * @param ev the form submit event
   */
  void submitForm(FormSubmitEvent ev)
  {
    URL url = ev.getURL();
    String data = ev.getData();
    FormSubmitEvent.MethodType method = ev.getMethod();
    if (method == FormSubmitEvent.MethodType.POST)
      {
        try
          {
            URLConnection conn = url.openConnection();
            postData(conn, data);
          }
        catch (IOException ex)
          {
            // Deal with this.
            ex.printStackTrace();
          }
      }
    else
      {
        try
          {
            url = new URL(url.toString() + "?" + data);
          }
        catch (MalformedURLException ex)
          {
            ex.printStackTrace();
          }
      }
    setPage(url);
  }

  /**
   * Posts the form data for forms with HTTP POST method.
   *
   * @param conn the connection
   * @param data the form data
   */
  private void postData(URLConnection conn, String data)
  {
    conn.setDoOutput(true);
    PrintWriter out = null;
    try
      {
        out = new PrintWriter(new OutputStreamWriter(conn.getOutputStream()));
        out.print(data);
        out.flush();
      }
    catch (IOException ex)
      {
        // Deal with this!
        ex.printStackTrace();
      }
    finally
      {
        if (out != null)
          out.close();
      }
  }

  /**
   * Returns a DemoFactory that creates a HtmlDemo.
   *
   * @return a DemoFactory that creates a HtmlDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new HtmlDemo();
      }
    };
  }
}
