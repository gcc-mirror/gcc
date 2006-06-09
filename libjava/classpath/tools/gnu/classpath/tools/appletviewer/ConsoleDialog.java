/* ConsoleDialog -- a console dialog for applets
   Copyright (C) 2003, 2004, 2006  Free Software Foundation, Inc.

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

package gnu.classpath.tools.appletviewer;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Panel;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;


/**
 * This class is a little dialog showing standard output and standard error output.
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public class ConsoleDialog extends Frame
  implements ActionListener
{
  static class InternalOutputStream extends OutputStream
  {
    private ConsoleDialog console;

    public InternalOutputStream(ConsoleDialog console)
    {
      super();
      this.console = console;
    }

    public void write(int data) throws IOException
    {
      console.print(String.valueOf((char) data));
    }
  }

  private TextArea textArea;
  private Button buttonClear;
  private Button buttonHide;
  private PrintStream printStream;

  /**
   * Creates a console dialog object.
   */
  public ConsoleDialog()
  {
    super(Main.messages.getString("gcjwebplugin.console_title"));

    setSize(400, 200);
    setLayout(new BorderLayout());
    addWindowListener(new WindowAdapter()
        {
	  public void windowClosing(WindowEvent event)
	  {
	    hide();
	  }
        });

    textArea = new TextArea();
    textArea.setEditable(false);
    add(textArea);

    Panel panel = new Panel();
    panel.setLayout(new FlowLayout());
    add(panel, BorderLayout.SOUTH);

    buttonClear = new Button(Main.messages.getString("gcjwebplugin.console_clear"));
    buttonClear.addActionListener(this);
    panel.add(buttonClear);

    buttonHide = new Button(Main.messages.getString("gcjwebplugin.console_hide"));
    buttonHide.addActionListener(this);
    panel.add(buttonHide);

    printStream = new PrintStream(new InternalOutputStream(this));
    clearTextArea();
  }

  /**
   * Clears the content of the textarea and inserts the initial text.
   */
  public void clearTextArea()
  {
    textArea.setText("");
    
    println("java.vm.version: " + System.getProperty("java.vm.version"));
    println("java.vm.vendor: " + System.getProperty("java.vm.vendor"));
  }

  /**
   * Print a message into the console dialog.
   *
   * @param message the message to print.
   */
  public void print(String message)
  {
    textArea.append(message);
  }

  /**
   * Print a line into the console dialog.
   *
   * @param message the line to print.
   */
  public void println(String message)
  {
    print(message + "\n");
  }

  /**
   * Perform actions on button clicks inside the console dialog.
   *
   * @param event the event.
   */
  public void actionPerformed(ActionEvent event)
  {
    if (event.getSource() == buttonHide)
      hide(); // Hide console window.
    else if (event.getSource() == buttonClear)
      clearTextArea(); // Clear text area and insert standard messages.
  }

  /**
   * Returns a <code>PrintStream</code> object that prints into the
   * console dialog.
   *
   * @return the <code>PrintStream</code> object.
   */
  public PrintStream getPrintStream()
  {
    return printStream;
  }
}
