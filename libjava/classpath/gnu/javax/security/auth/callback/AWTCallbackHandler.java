/* AWTCallbackHandler.java --
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.security.auth.callback;

import gnu.java.lang.CPStringBuilder;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Label;
import java.awt.List;
import java.awt.Panel;
import java.awt.TextArea;
import java.awt.TextField;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import java.util.Locale;

import javax.security.auth.callback.ChoiceCallback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.LanguageCallback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.TextOutputCallback;

public class AWTCallbackHandler extends AbstractCallbackHandler
  implements ActionListener, WindowListener
{

  // Fields.
  // -------------------------------------------------------------------------

  protected String actionCommand;

  private static final String ACTION_CANCEL  = "CANCEL";
  private static final String ACTION_NO      = "NO";
  private static final String ACTION_NONE    = "NONE";
  private static final String ACTION_OK      = "OK";
  private static final String ACTION_YES     = "YES";

  // Constructor.
  // -------------------------------------------------------------------------

  public AWTCallbackHandler()
  {
    super ("AWT");
    actionCommand = ACTION_NONE;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  protected synchronized void handleChoice(ChoiceCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    String[] choices = c.getChoices();
    dialog.setTitle(c.getPrompt());
    Label label = new Label(c.getPrompt());
    List list = new List(Math.min(5, choices.length),
                         c.allowMultipleSelections());
    Panel buttons = new Panel();
    Button ok = new Button(messages.getString("callback.ok"));
    ok.setActionCommand(ACTION_OK);
    ok.addActionListener(this);
    Button cancel = new Button(messages.getString("callback.cancel"));
    cancel.setActionCommand(ACTION_CANCEL);
    cancel.addActionListener(this);
    for (int i = 0; i < choices.length; i++)
      {
        list.add(choices[i]);
      }
    if (c.getDefaultChoice() >= 0 && c.getDefaultChoice() < choices.length)
      {
        list.select(c.getDefaultChoice());
      }
    dialog.setLayout(new BorderLayout());
    dialog.add(label, BorderLayout.NORTH);
    dialog.add(list, BorderLayout.CENTER);
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(cancel);
    buttons.add(ok);
    dialog.add(buttons, BorderLayout.SOUTH);
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    if (actionCommand.equals(ACTION_OK))
      {
        if (c.allowMultipleSelections())
          {
            c.setSelectedIndexes(list.getSelectedIndexes());
          }
        else
          {
            c.setSelectedIndex(list.getSelectedIndex());
          }
      }
    dialog.dispose();
    ownerFrame.dispose();
  }

  protected synchronized void handleConfirmation(ConfirmationCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    switch (c.getMessageType())
      {
      case ConfirmationCallback.ERROR:
        dialog.setTitle(messages.getString("callback.error"));
        break;
      case ConfirmationCallback.INFORMATION:
        dialog.setTitle(messages.getString("callback.information"));
        break;
      case ConfirmationCallback.WARNING:
        dialog.setTitle(messages.getString("callback.warning"));
        break;
      default:
        dialog.setTitle("");
      }
    dialog.setLayout(new GridLayout(2, 1));
    dialog.add(new Label(c.getPrompt()));
    Panel buttons = new Panel();
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    dialog.add(buttons);
    String[] choices = null;
    int[] values = null;
    switch (c.getOptionType())
      {
      case ConfirmationCallback.OK_CANCEL_OPTION:
        choices = new String[] {
          messages.getString("callback.cancel"),
          messages.getString("callback.ok")
        };
        values = new int[] {
          ConfirmationCallback.CANCEL, ConfirmationCallback.OK
        };
        break;
      case ConfirmationCallback.YES_NO_CANCEL_OPTION:
        choices = new String[] {
          messages.getString("callback.cancel"),
          messages.getString("callback.no"),
          messages.getString("callback.yes")
        };
        values = new int[] {
          ConfirmationCallback.CANCEL, ConfirmationCallback.NO,
          ConfirmationCallback.YES
        };
        break;
      case ConfirmationCallback.YES_NO_OPTION:
        choices = new String[] {
          messages.getString("callback.no"),
          messages.getString("callback.yes")
        };
        values = new int[] {
          ConfirmationCallback.NO, ConfirmationCallback.YES
        };
        break;
      case ConfirmationCallback.UNSPECIFIED_OPTION:
        choices = c.getOptions();
        values = new int[choices.length];
        for (int i = 0; i < values.length; i++)
          values[i] = i;
        break;
      default:
        throw new IllegalArgumentException();
      }
    for (int i = 0; i < choices.length; i++)
      {
        Button b = new Button(choices[i]);
        b.setActionCommand(choices[i]);
        b.addActionListener(this);
        buttons.add(b);
      }
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    for (int i = 0; i < choices.length; i++)
      {
        if (actionCommand.equals(choices[i]))
          {
            c.setSelectedIndex(values[i]);
            break;
          }
      }
    dialog.dispose();
    ownerFrame.dispose();
  }

  protected synchronized void handleLanguage(LanguageCallback c)
  {
    Locale[] locales = Locale.getAvailableLocales();
    String[] languages = new String[locales.length];
    Locale def = Locale.getDefault();
    int defind = 0;
    for (int i = 0; i < locales.length; i++)
      {
        CPStringBuilder lang =
          new CPStringBuilder(locales[i].getDisplayLanguage(locales[i]));
        String country = locales[i].getDisplayCountry(locales[i]);
        String variant = locales[i].getDisplayVariant(locales[i]);
        if (country.length() > 0 && variant.length() > 0)
          {
            lang.append(" (");
            lang.append(country);
            lang.append(", ");
            lang.append(variant);
            lang.append(")");
          }
        else if (country.length() > 0)
          {
            lang.append(" (");
            lang.append(country);
            lang.append(")");
          }
        else if (variant.length() > 0)
          {
            lang.append(" (");
            lang.append(variant);
            lang.append(")");
          }
        languages[i] = lang.toString();
        if (locales[i].equals(def))
          defind = i;
      }
    ChoiceCallback c2 =
      new ChoiceCallback(messages.getString("callback.language"), languages,
                         defind, false);
    handleChoice(c2);
    c.setLocale(def);
    if (c2.getSelectedIndexes() != null && c2.getSelectedIndexes().length > 0)
      {
        int index = c2.getSelectedIndexes()[0];
        if (index >= 0 && index < locales.length)
          c.setLocale(locales[index]);
      }
  }

  protected synchronized void handleName(NameCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    dialog.setTitle(c.getPrompt());
    dialog.setLayout(new GridLayout(3, 1));
    Label label = new Label(c.getPrompt());
    TextField input = new TextField();
    if (c.getDefaultName() != null)
      {
        input.setText(c.getDefaultName());
      }
    Panel buttons = new Panel();
    Button ok = new Button(messages.getString("callback.ok"));
    ok.setActionCommand(ACTION_OK);
    ok.addActionListener(this);
    Button cancel = new Button(messages.getString("callback.cancel"));
    cancel.setActionCommand(ACTION_CANCEL);
    cancel.addActionListener(this);
    dialog.add(label);
    dialog.add(input);
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(ok);
    buttons.add(cancel);
    dialog.add(buttons);
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    if (actionCommand.equals(ACTION_OK))
      {
        c.setName(input.getText());
      }
    dialog.dispose();
    ownerFrame.dispose();
  }

  protected synchronized void handlePassword(PasswordCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    dialog.setTitle(c.getPrompt());
    dialog.setLayout(new GridLayout(3, 1));
    Label label = new Label(c.getPrompt());
    TextField input = new TextField();
    if (!c.isEchoOn())
      {
        input.setEchoChar('*');
      }
    Panel buttons = new Panel();
    Button ok = new Button(messages.getString("callback.ok"));
    ok.setActionCommand(ACTION_OK);
    ok.addActionListener(this);
    Button cancel = new Button(messages.getString("callback.cancel"));
    cancel.setActionCommand(ACTION_CANCEL);
    cancel.addActionListener(this);
    dialog.add(label);
    dialog.add(input);
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(ok);
    buttons.add(cancel);
    dialog.add(buttons);
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    if (actionCommand.equals(ACTION_OK))
      {
        c.setPassword(input.getText().toCharArray());
      }
    dialog.dispose();
    ownerFrame.dispose();
  }

  protected synchronized void handleTextInput(TextInputCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    dialog.setTitle(c.getPrompt());
    dialog.setLayout(new BorderLayout());
    Label label = new Label(c.getPrompt());
    TextArea text = new TextArea(10, 40);
    if (c.getDefaultText() != null)
      {
        text.setText(c.getDefaultText());
      }
    Panel buttons = new Panel();
    Button ok = new Button(messages.getString("callback.ok"));
    ok.setActionCommand(ACTION_OK);
    ok.addActionListener(this);
    Button cancel = new Button(messages.getString("callback.cancel"));
    cancel.setActionCommand(ACTION_CANCEL);
    cancel.addActionListener(this);
    dialog.add(label, BorderLayout.NORTH);
    dialog.add(text, BorderLayout.CENTER);
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(ok);
    buttons.add(cancel);
    dialog.add(buttons, BorderLayout.SOUTH);
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    if (actionCommand.equals(ACTION_OK))
      {
        c.setText(text.getText());
      }
    dialog.dispose();
    ownerFrame.dispose();
  }

  protected synchronized void handleTextOutput(TextOutputCallback c)
  {
    Frame ownerFrame = new Frame();
    Dialog dialog = new Dialog(ownerFrame);
    dialog.setLayout(new GridLayout(2, 1));
    switch (c.getMessageType() /*c.getStyle()*/)
      {
      case ConfirmationCallback.ERROR:
        dialog.setTitle(messages.getString("callback.error"));
        break;
      case ConfirmationCallback.INFORMATION:
        dialog.setTitle(messages.getString("callback.information"));
        break;
      case ConfirmationCallback.WARNING:
        dialog.setTitle(messages.getString("callback.warning"));
        break;
      default:
        dialog.setTitle("");
      }
    Label label = new Label(c.getMessage());
    Panel buttons = new Panel();
    Button ok = new Button(messages.getString("callback.ok"));
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(ok);
    ok.addActionListener(this);
    dialog.add(label);
    dialog.add(buttons);
    dialog.pack();
    dialog.show();
    try { wait(); }
    catch (InterruptedException ie) { }
    dialog.dispose();
    ownerFrame.dispose();
  }

  // ActionListener interface implementation.
  // -------------------------------------------------------------------------

  public synchronized void actionPerformed(ActionEvent ae)
  {
    actionCommand = ae.getActionCommand();
    notifyAll();
  }

  // WindowListener interface implementation.
  // -------------------------------------------------------------------------

  public synchronized void windowClosing(WindowEvent we)
  {
    actionCommand = ACTION_NONE;
    notifyAll();
  }

  public void windowOpened(WindowEvent we) { }
  public void windowClosed(WindowEvent we) { }
  public void windowIconified(WindowEvent we) { }
  public void windowDeiconified(WindowEvent we) { }
  public void windowActivated(WindowEvent we) { }
  public void windowDeactivated(WindowEvent we) { }
}
