/* ConsoleCallbackHandler.java -- 
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintStream;

import java.util.Iterator;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.TreeSet;

import javax.security.auth.callback.ChoiceCallback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.LanguageCallback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.TextOutputCallback;

/**
 * An implementation of {@link CallbackHandler} that reads and writes
 * information to and from <code>System.in</code> and <code>System.out</code>.
 */
public class ConsoleCallbackHandler extends AbstractCallbackHandler
{

  // Fields.
  // -------------------------------------------------------------------------

  private final PrintStream out;

  // Constructors.
  // -------------------------------------------------------------------------

  public ConsoleCallbackHandler()
  {
    this (System.out);
  }

  public ConsoleCallbackHandler (final PrintStream out)
  {
    super ("CONSOLE");
    this.out = out;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  protected void handleChoice(ChoiceCallback c) throws IOException
  {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    out.println(c.getPrompt());
    out.print('(');
    String[] choices = c.getChoices();
    for (int i = 0; i < choices.length; i++)
      {
        out.print(choices[i]);
        if (i != choices.length - 1)
          out.print(", ");
      }
    out.print(") ");
    if (c.getDefaultChoice() >= 0 && c.getDefaultChoice() < choices.length)
      {
        out.print('[');
        out.print(choices[c.getDefaultChoice()]);
        out.print("] ");
      }
    String reply = in.readLine();
    if (reply == null || reply.length() == 0)
      {
        c.setSelectedIndex(c.getDefaultChoice());
        return;
      }
    if (!c.allowMultipleSelections())
      {
        for (int i = 0; i < choices.length; i++)
          {
            if (reply.trim().equals(choices[i]))
              {
                c.setSelectedIndex(i);
                return;
              }
          }
        c.setSelectedIndex(c.getDefaultChoice());
      }
    else
      {
        TreeSet indices = new TreeSet();
        StringTokenizer tok = new StringTokenizer(reply, ",");
        String[] replies = new String[tok.countTokens()];
        int idx = 0;
        while (tok.hasMoreTokens())
          {
            replies[idx++] = tok.nextToken().trim();
          }
        for (int i = 0; i < choices.length; i++)
          for (int j = 0; j < replies.length; i++)
            {
              if (choices[i].equals(replies[j]))
                {
                  indices.add(Integer.valueOf(i));
                }
            }
        if (indices.size() == 0)
          c.setSelectedIndex(c.getDefaultChoice());
        else
          {
            int[] ii = new int[indices.size()];
            int i = 0;
            for (Iterator it = indices.iterator(); it.hasNext(); )
              ii[i++] = ((Integer) it.next()).intValue();
            c.setSelectedIndexes(ii);
          }
      }
  }

  protected void handleConfirmation(ConfirmationCallback c) throws IOException
  {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    if (c.getPrompt() != null)
      out.print(c.getPrompt());

    String[] choices = null;
    int[] values = null;
    switch (c.getOptionType())
      {
      case ConfirmationCallback.OK_CANCEL_OPTION:
        out.print(messages.getString("callback.okCancel"));
        choices = new String[] {
          messages.getString("callback.ok"),
          messages.getString("callback.cancel"),
          messages.getString("callback.shortOk"),
          messages.getString("callback.shortCancel")
        };
        values = new int[] {
          ConfirmationCallback.OK, ConfirmationCallback.CANCEL,
          ConfirmationCallback.OK, ConfirmationCallback.CANCEL
        };
        break;

      case ConfirmationCallback.YES_NO_CANCEL_OPTION:
        out.print(messages.getString("callback.yesNoCancel"));
        choices = new String[] {
          messages.getString("callback.yes"),
          messages.getString("callback.no"),
          messages.getString("callback.cancel"),
          messages.getString("callback.shortYes"),
          messages.getString("callback.shortNo"),
          messages.getString("callback.shortCancel")
        };
        values = new int[] {
          ConfirmationCallback.YES, ConfirmationCallback.NO,
          ConfirmationCallback.CANCEL, ConfirmationCallback.YES,
          ConfirmationCallback.NO, ConfirmationCallback.CANCEL
        };
        break;

      case ConfirmationCallback.YES_NO_OPTION:
        out.print(messages.getString("callback.yesNo"));
        choices = new String[] { messages.getString("callback.yes"),
                                 messages.getString("callback.no"),
                                 messages.getString("callback.shortYes"),
                                 messages.getString("callback.shortNo") };
        values = new int[] { ConfirmationCallback.YES,
                             ConfirmationCallback.NO,
                             ConfirmationCallback.YES,
                             ConfirmationCallback.NO };
        int defaultOption = c.getDefaultOption();
        if (defaultOption > -1 && defaultOption < choices.length)
          {
            out.print("[");
            out.print(choices[defaultOption]);
            out.print("] ");
          }
        break;

      case ConfirmationCallback.UNSPECIFIED_OPTION:
        choices = c.getOptions();
        values = new int[choices.length];
        for (int i = 0; i < values.length; i++)
          values[i] = i;
        out.print('(');
        for (int i = 0; i < choices.length; i++)
          {
            out.print(choices[i]);
            if (i != choices.length - 1)
              out.print(", ");
          }
        out.print(") [");
        out.print(choices[c.getDefaultOption()]);
        out.print("] ");
        break;

      default:
        throw new IllegalArgumentException();
      }
    String reply = in.readLine();
    if (reply == null)
      {
        c.setSelectedIndex(c.getDefaultOption());
        return;
      }
    reply = reply.trim();
    for (int i = 0; i < choices.length; i++)
      if (reply.equalsIgnoreCase(choices[i]))
        {
          c.setSelectedIndex(values[i]);
          return;
        }
    c.setSelectedIndex(c.getDefaultOption());
  }

  protected void handleLanguage(LanguageCallback c) throws IOException
  {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    out.print(messages.getString("callback.language"));
    String reply = null;
    reply = in.readLine();
    if (reply == null)
      {
        c.setLocale(Locale.getDefault());
      }
    else
      {
        c.setLocale(new Locale(reply.trim()));
      }
  }

  protected void handleName(NameCallback c) throws IOException
  {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    out.print(c.getPrompt());
    String name = in.readLine();
    if (name != null)
      c.setName(name.trim());
  }

  protected void handlePassword(PasswordCallback c) throws IOException
  {
    out.print(c.getPrompt());
    BufferedReader in =
      new BufferedReader(new InputStreamReader(System.in));
    String pass = in.readLine();
    c.setPassword(pass.toCharArray());
  }

  protected void handleTextInput(TextInputCallback c) throws IOException
  {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    out.print(c.getPrompt());
    String text = in.readLine();
    if (text != null)
      c.setText(text);
  }

  protected void handleTextOutput(TextOutputCallback c)
  {
    out.print(c.getMessage());
  }
}
