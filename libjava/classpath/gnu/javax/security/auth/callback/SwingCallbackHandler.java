 /* SwingCallbackHandler.java -- 
    Copyright (C) 2006  Free Software Foundation, Inc.

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

import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.IOException;

import java.util.Locale;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.ChoiceCallback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.LanguageCallback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.TextOutputCallback;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;

public class SwingCallbackHandler extends AbstractCallbackHandler
{
  public SwingCallbackHandler ()
  {
    super ("SWING");
  }

  protected void handleChoice (final ChoiceCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    dialog.setResizable (false);
    Container content = dialog.getContentPane ();
    GridBagLayout layout = new GridBagLayout ();
    content.setLayout (layout);
    JLabel prompt = new JLabel (callback.getPrompt (), JLabel.LEFT);
    content.add (prompt, new GridBagConstraints (0, 0, 1, 1, 0, 0,
                                                 GridBagConstraints.WEST,
                                                 GridBagConstraints.NONE,
                                                 new Insets (5, 5, 5, 5), 5, 5));

    String[] choices = callback.getChoices ();
    final JList choicesList = new JList (choices);
    JScrollPane choicesPane = new JScrollPane (choicesList,
                                               JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                               JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    final int defaultChoice = callback.getDefaultChoice ();
    choicesList.setSelectedIndex (defaultChoice);
    choicesList.setSelectionMode (callback.allowMultipleSelections ()
                                  ? ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
                                  : ListSelectionModel.SINGLE_SELECTION);
    content.add (choicesPane, new GridBagConstraints (0, 1, 1, 1, 1.0, 1.0,
                                                      GridBagConstraints.CENTER,
                                                      GridBagConstraints.BOTH,
                                                      new Insets (0, 10, 0, 10), 5, 5));

    JPanel confirmButtons = new JPanel ();
    confirmButtons.setLayout (new FlowLayout (FlowLayout.RIGHT));
    JButton cancel = new JButton (messages.getString ("callback.cancel"));
    JButton ok = new JButton (messages.getString ("callback.ok"));
    confirmButtons.add (cancel);
    confirmButtons.add (ok);
    content.add (confirmButtons, new GridBagConstraints (0, 2, 1, 1, 0, 0,
                                                         GridBagConstraints.EAST,
                                                         GridBagConstraints.NONE,
                                                         new Insets (5, 5, 5, 5),
                                                         0, 0));
    dialog.getRootPane ().setDefaultButton (ok);

    cancel.addActionListener (new ActionListener ()
      {
        public void actionPerformed (final ActionEvent ae)
        {
          callback.setSelectedIndex (defaultChoice);
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      });
    ok.addActionListener (new ActionListener ()
      {
        public void actionPerformed (final ActionEvent ae)
        {
          if (callback.allowMultipleSelections ())
            {
              int[] indices = choicesList.getSelectedIndices ();
              if (indices != null && indices.length > 0)
                callback.setSelectedIndexes (indices);
              else
                callback.setSelectedIndex (defaultChoice);
            }
          else
            {
              int selected = choicesList.getSelectedIndex ();
              if (selected != -1)
                callback.setSelectedIndex (selected);
              else
                callback.setSelectedIndex (defaultChoice);
            }
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      });

    dialog.pack ();
    dialog.setSize (new Dimension (400, 400));
    dialog.setVisible (true);
    waitForInput (dialog, callback);
  }

  protected void handleConfirmation (final ConfirmationCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    switch (callback.getMessageType ())
      {
      case ConfirmationCallback.ERROR:
        dialog.setTitle (messages.getString ("callback.error"));
        break;
      case ConfirmationCallback.WARNING:
        dialog.setTitle (messages.getString ("callback.warning"));
        break;
      case ConfirmationCallback.INFORMATION:
        dialog.setTitle (messages.getString ("callback.information"));
        break;
      }
    Container content = dialog.getContentPane ();
    content.setLayout (new GridBagLayout ());

    String prompt = callback.getPrompt ();
    if (prompt != null)
      {
        content.add (new JLabel (prompt),
                     new GridBagConstraints (0, 0, 1, 1, 0, 0,
                                             GridBagConstraints.WEST,
                                             GridBagConstraints.NONE,
                                             new Insets (5, 5, 5, 25), 0, 0));
      }

    final String[] options = callback.getOptions ();
    ActionListener listener = new ActionListener ()
      {
        public void actionPerformed (ActionEvent ae)
        {
          String cmd = ae.getActionCommand ();
          if (options != null)
            {
              for (int i = 0; i < options.length; i++)
                {
                  if (cmd.equals (options[i]))
                    {
                      callback.setSelectedIndex (i);
                      break;
                    }
                }
            }
          else
            {
              if (cmd.equals ("cancel"))
                callback.setSelectedIndex (ConfirmationCallback.CANCEL);
              else if (cmd.equals ("okay"))
                callback.setSelectedIndex (ConfirmationCallback.OK);
              else if (cmd.equals ("yes"))
                callback.setSelectedIndex (ConfirmationCallback.YES);
              else if (cmd.equals ("no"))
                callback.setSelectedIndex (ConfirmationCallback.NO);
            }
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      };

    JPanel buttons = new JPanel ();
    buttons.setLayout (new FlowLayout (FlowLayout.RIGHT));
    switch (callback.getOptionType ())
      {
      case ConfirmationCallback.YES_NO_CANCEL_OPTION:
        {
          JButton cancel = new JButton (messages.getString ("callback.cancel"));
          buttons.add (cancel);
          cancel.setActionCommand ("cancel");
          cancel.addActionListener (listener);
        }
        /* Fall-through. */
      case ConfirmationCallback.YES_NO_OPTION:
        {
          JButton yes = new JButton (messages.getString ("callback.yes"));
          JButton no = new JButton (messages.getString ("callback.no"));
          buttons.add (no);
          buttons.add (yes);
          yes.setActionCommand ("yes");
          yes.addActionListener (listener);
          no.setActionCommand ("no");
          no.addActionListener (listener);
          dialog.getRootPane ().setDefaultButton (yes);
        }
        break;
      case ConfirmationCallback.OK_CANCEL_OPTION:
        {
          JButton okay = new JButton (messages.getString ("callback.ok"));
          JButton cancel = new JButton (messages.getString ("callback.cancel"));
          buttons.add (cancel);
          buttons.add (okay);
          okay.setActionCommand ("okay");
          okay.addActionListener (listener);
          cancel.setActionCommand ("cancel");
          cancel.addActionListener (listener);
          dialog.getRootPane ().setDefaultButton (okay);
        }
        break;
      case ConfirmationCallback.UNSPECIFIED_OPTION:
        for (int i = 0; i < options.length; i++)
          {
            JButton button = new JButton (options[i]);
            buttons.add (button);
            button.setActionCommand (options[i]);
            button.addActionListener (listener);
            if (i == options.length - 1)
              dialog.getRootPane ().setDefaultButton (button);
          }
      }
    content.add (buttons,
                 new GridBagConstraints (0, GridBagConstraints.RELATIVE,
                                         1, 1, 1, 1,
                                         GridBagConstraints.SOUTHEAST,
                                         GridBagConstraints.BOTH,
                                         new Insets (5, 5, 5, 5), 0, 0));
    dialog.setResizable (false);
    dialog.pack ();
    dialog.setVisible (true);
    waitForInput (dialog, callback);
  }

  protected void handleLanguage (final LanguageCallback callback)
    throws IOException
  {
    Locale locale = Locale.getDefault ();
    Locale[] locales = Locale.getAvailableLocales ();
    String[] localeNames = new String[locales.length+1];
    int defaultIndex = 0;
    for (int i = 0; i < locales.length; i++)
      {
        localeNames[i+1] = locales[i].getDisplayLanguage (locales[i]);
        String country = locales[i].getDisplayCountry (locales[i]);
        if (country.length () > 0)
          localeNames[i+1] += " (" + country + ")";
        if (locales[i].equals (locale))
          defaultIndex = i;
      }
    locales[0] = locale;
    localeNames[0] = locale.getDisplayLanguage (locale);
    String country = locale.getDisplayCountry (locale);
    if (country.length () > 0)
      localeNames[0] += " (" + country + ")";
    ChoiceCallback cb = new ChoiceCallback (messages.getString ("callback.language"),
                                                                localeNames, 0,
                                                                false);
    handleChoice (cb);
    int selected = cb.getSelectedIndexes ()[0];
    if (selected > 0)
      callback.setLocale (locales[selected - 1]);
    else
      callback.setLocale (locale);
  }

  protected void handleName (final NameCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    Container content = dialog.getContentPane ();
    content.setLayout (new GridBagLayout ());

    content.add (new JLabel (callback.getPrompt ()),
                             new GridBagConstraints (0, 0, 1, 1, 0, 0,
                                                     GridBagConstraints.NORTHEAST,
                                                     GridBagConstraints.VERTICAL,
                                                     new Insets (10, 10, 15, 5), 0, 0));

    final JTextField name = new JTextField ();
    name.setColumns (20);
    String _name;
    if ((_name = callback.getDefaultName ()) != null)
      name.setText (_name);
    content.add (name, new GridBagConstraints (1, 0, 1, 1, 1, 1,
                                               GridBagConstraints.NORTHWEST,
                                               GridBagConstraints.BOTH,
                                               new Insets (10, 5, 15, 10), 0, 0));

    ActionListener listener = new ActionListener ()
      {
        public void actionPerformed (ActionEvent ae)
        {
          String cmd = ae.getActionCommand ();
          if (cmd.equals ("okay"))
            callback.setName (name.getText ());
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      };

    JPanel buttons = new JPanel ();
    buttons.setLayout (new FlowLayout (FlowLayout.RIGHT));
    JButton cancel = new JButton (messages.getString ("callback.cancel"));
    JButton okay = new JButton (messages.getString ("callback.ok"));
    cancel.setActionCommand ("cancel");
    cancel.addActionListener (listener);
    buttons.add (cancel);
    okay.setActionCommand ("okay");
    okay.addActionListener (listener);
    buttons.add (okay);
    content.add (buttons, new GridBagConstraints (0, 1, 2, 1, 0, 0,
                                                  GridBagConstraints.SOUTHEAST,
                                                  GridBagConstraints.NONE,
                                                  new Insets (0, 10, 10, 10), 0, 0));

    dialog.setResizable (false);
    dialog.pack ();
    dialog.setVisible (true);
    dialog.getRootPane ().setDefaultButton (okay);
    waitForInput (dialog, callback);
  }

  protected void handlePassword (final PasswordCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    Container content = dialog.getContentPane ();
    content.setLayout (new GridBagLayout ());

    content.add (new JLabel (callback.getPrompt ()),
                             new GridBagConstraints (0, 0, 1, 1, 0, 0,
                                                     GridBagConstraints.NORTHEAST,
                                                     GridBagConstraints.VERTICAL,
                                                     new Insets (10, 10, 15, 5), 0, 0));

    final JPasswordField password = new JPasswordField ();
    password.setColumns (20);
    password.setEchoChar (callback.isEchoOn () ? '\u0000' : '\u2022');
    content.add (password, new GridBagConstraints (1, 0, 1, 1, 1, 1,
                                                   GridBagConstraints.NORTHWEST,
                                                   GridBagConstraints.BOTH,
                                                   new Insets (10, 5, 15, 10), 0, 0));

    ActionListener listener = new ActionListener ()
      {
        public void actionPerformed (ActionEvent ae)
        {
          String cmd = ae.getActionCommand ();
          if (cmd.equals ("okay"))
            callback.setPassword (password.getPassword ());
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      };

    JPanel buttons = new JPanel ();
    buttons.setLayout (new FlowLayout (FlowLayout.RIGHT));
    JButton cancel = new JButton (messages.getString ("callback.cancel"));
    JButton okay = new JButton (messages.getString ("callback.ok"));
    cancel.setActionCommand ("cancel");
    cancel.addActionListener (listener);
    buttons.add (cancel);
    okay.setActionCommand ("okay");
    okay.addActionListener (listener);
    buttons.add (okay);
    content.add (buttons, new GridBagConstraints (0, 1, 2, 1, 0, 0,
                                                  GridBagConstraints.SOUTHEAST,
                                                  GridBagConstraints.NONE,
                                                  new Insets (0, 10, 10, 10), 0, 0));

    dialog.setResizable (false);
    dialog.pack ();
    dialog.setVisible (true);
    dialog.getRootPane ().setDefaultButton (okay);
    waitForInput (dialog, callback);
  }

  protected void handleTextInput (final TextInputCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    Container content = dialog.getContentPane ();
    content.setLayout (new GridBagLayout ());

    content.add (new JLabel (callback.getPrompt ()),
                             new GridBagConstraints (0, 0, 1, 1, 0, 0,
                                                     GridBagConstraints.NORTHWEST,
                                                     GridBagConstraints.NONE,
                                                     new Insets (10, 10, 15, 5), 0, 0));
    
    final JTextArea text = new JTextArea (24, 80);
    text.setEditable (true);
    String _text;
    if ((_text = callback.getDefaultText ()) != null)
      text.setText (_text);
    text.setFont (new Font ("Monospaced", Font.PLAIN, 12));
    JScrollPane textPane = new JScrollPane (text,
                                            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    content.add (textPane,
                 new GridBagConstraints (0, 1, 1, 1, 1, 1,
                                         GridBagConstraints.CENTER,
                                         GridBagConstraints.BOTH,
                                         new Insets (5, 10, 5, 10), 0, 0));

    ActionListener listener = new ActionListener ()
      {
        public void actionPerformed (ActionEvent ae)
        {
          String cmd = ae.getActionCommand ();
          if (cmd.equals ("okay"))
            callback.setText (text.getText ());
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      };

    JPanel buttons = new JPanel ();
    buttons.setLayout (new FlowLayout (FlowLayout.RIGHT));
    JButton cancel = new JButton (messages.getString ("callback.cancel"));
    JButton okay = new JButton (messages.getString ("callback.ok"));
    cancel.setActionCommand ("cancel");
    cancel.addActionListener (listener);
    buttons.add (cancel);
    okay.setActionCommand ("okay");
    okay.addActionListener (listener);
    buttons.add (okay);
    content.add (buttons, new GridBagConstraints (0, 2, 1, 1, 0, 0,
                                                  GridBagConstraints.SOUTHEAST,
                                                  GridBagConstraints.NONE,
                                                  new Insets (0, 10, 10, 10), 0, 0));

    dialog.setResizable (true);
    dialog.pack ();
    dialog.setVisible (true);
    dialog.getRootPane ().setDefaultButton (okay);
    waitForInput (dialog, callback);
  }

  protected void handleTextOutput (final TextOutputCallback callback)
    throws IOException
  {
    final JDialog dialog = new JDialog ();
    switch (callback.getMessageType ())
      {
      case TextOutputCallback.ERROR:
        dialog.setTitle (messages.getString ("callback.error"));
        break;
      case TextOutputCallback.WARNING:
        dialog.setTitle (messages.getString ("callback.warning"));
        break;
      case TextOutputCallback.INFORMATION:
        dialog.setTitle (messages.getString ("callback.information"));
        break;
      }
    Container content = dialog.getContentPane ();
    content.setLayout (new GridBagLayout ());

    final JTextArea text = new JTextArea (24, 80);
    text.setEditable (false);
    text.setText (callback.getMessage ());
    text.setFont (new Font ("Monospaced", Font.PLAIN, 12));
    JScrollPane textPane = new JScrollPane (text,
                                            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    content.add (textPane,
                 new GridBagConstraints (0, 0, 1, 1, 1, 1,
                                         GridBagConstraints.CENTER,
                                         GridBagConstraints.BOTH,
                                         new Insets (10, 10, 5, 10), 0, 0));

    ActionListener listener = new ActionListener ()
      {
        public void actionPerformed (ActionEvent ae)
        {
          dialog.setVisible (false);
          synchronized (callback)
            {
              callback.notify ();
            }
        }
      };

    JButton okay = new JButton (messages.getString ("callback.ok"));
    okay.setActionCommand ("okay");
    okay.addActionListener (listener);
    content.add (okay, new GridBagConstraints (0, 1, 1, 1, 0, 0,
                                               GridBagConstraints.SOUTHEAST,
                                               GridBagConstraints.NONE,
                                               new Insets (0, 10, 10, 10), 0, 0));

    dialog.setResizable (true);
    dialog.pack ();
    dialog.setVisible (true);
    dialog.getRootPane ().setDefaultButton (okay);
    waitForInput (dialog, callback);
  }

  private void waitForInput (JDialog dialog, Callback callback)
  {
    synchronized (callback)
      {
        while (dialog.isVisible ())
          {
            try
              {
                callback.wait (1000);
              }
            catch (InterruptedException ignored)
              {
              }
          }
      }
    dialog.dispose ();
  }
}