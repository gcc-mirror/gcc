/* echo-area.c -- How to read a line in the echo area.
   $Id: echo-area.c,v 1.1.1.2 1998/03/24 18:20:08 law Exp $

   Copyright (C) 1993, 97, 98 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

#include "info.h"

#if defined (FD_SET)
#  if defined (hpux)
#    define fd_set_cast(x) (int *)(x)
#  else
#    define fd_set_cast(x) (fd_set *)(x)
#  endif /* !hpux */
#endif /* FD_SET */

/* Non-zero means that C-g was used to quit reading input. */
int info_aborted_echo_area = 0;

/* Non-zero means that the echo area is being used to read input. */
int echo_area_is_active = 0;

/* The address of the last command executed in the echo area. */
VFunction *ea_last_executed_command = (VFunction *)NULL;

/* Non-zero means that the last command executed while reading input
   killed some text. */
int echo_area_last_command_was_kill = 0;

/* Variables which hold on to the current state of the input line. */
static char input_line[1 + EA_MAX_INPUT];
static char *input_line_prompt;
static int input_line_point;
static int input_line_beg;
static int input_line_end;
static NODE input_line_node = {
  (char *)NULL, (char *)NULL, (char *)NULL, input_line, EA_MAX_INPUT, 0
};

static void echo_area_initialize_node ();
static void push_echo_area (), pop_echo_area ();
static int echo_area_stack_contains_completions_p ();

static void ea_kill_text ();

/* Non-zero means we force the user to complete. */
static int echo_area_must_complete_p = 0;
static int completions_window_p ();

/* If non-null, this is a window which was specifically created to display
   possible completions output.  We remember it so we can delete it when
   appropriate. */
static WINDOW *echo_area_completions_window = (WINDOW *)NULL;

/* Variables which keep track of the window which was active prior to
   entering the echo area. */
static WINDOW *calling_window = (WINDOW *)NULL;
static NODE *calling_window_node = (NODE *)NULL;
static long calling_window_point = 0;
static long calling_window_pagetop = 0;

/* Remember the node and pertinent variables of the calling window. */
static void
remember_calling_window (window)
     WINDOW *window;
{
  /* Only do this if the calling window is not the completions window, or,
     if it is the completions window and there is no other window. */
  if (!completions_window_p (window) ||
      ((window == windows) && !(window->next)))
    {
      calling_window = window;
      calling_window_node = window->node;
      calling_window_point = window->point;
      calling_window_pagetop = window->pagetop;
    }
}

/* Restore the caller's window so that it shows the node that it was showing
   on entry to info_read_xxx_echo_area (). */
static void
restore_calling_window ()
{
  register WINDOW *win, *compwin = (WINDOW *)NULL;

  /* If the calling window is still visible, and it is the window that
     we used for completions output, then restore the calling window. */
  for (win = windows; win; win = win->next)
    {
      if (completions_window_p (win))
        compwin = win;

      if (win == calling_window && win == compwin)
        {
          window_set_node_of_window (calling_window, calling_window_node);
          calling_window->point = calling_window_point;
          calling_window->pagetop = calling_window_pagetop;
          compwin = (WINDOW *)NULL;
          break;
        }
    }

  /* Delete the completions window if it is still present, it isn't the
     last window on the screen, and there aren't any prior echo area reads
     pending which created a completions window. */
  if (compwin)
    {
      if ((compwin != windows || windows->next) &&
          !echo_area_stack_contains_completions_p ())
        {
          WINDOW *next;
          int pagetop, start, end, amount;

          next = compwin->next;
          if (next)
            {
              start = next->first_row;
              end = start + next->height;
              amount = - (compwin->height + 1);
              pagetop = next->pagetop;
            }

          info_delete_window_internal (compwin);

          /* This is not necessary because info_delete_window_internal ()
             calls echo_area_inform_of_deleted_window (), which does the
             right thing. */
#if defined (UNNECESSARY)
          echo_area_completions_window = (WINDOW *)NULL;
#endif /* UNNECESSARY */

          if (next)
            {
              display_scroll_display (start, end, amount);
              next->pagetop = pagetop;
              display_update_display (windows);
            }
        }
    }
}

/* Set up a new input line with PROMPT. */
static void
initialize_input_line (prompt)
     char *prompt;
{
  input_line_prompt = prompt;
  if (prompt)
    strcpy (input_line, prompt);
  else
    input_line[0] = '\0';

  input_line_beg = input_line_end = input_line_point = strlen (prompt);
}

static char *
echo_area_after_read ()
{
  char *return_value;

  if (info_aborted_echo_area)
    {
      info_aborted_echo_area = 0;
      return_value = (char *)NULL;
    }
  else
    {
      if (input_line_beg == input_line_end)
        return_value = xstrdup ("");
      else
        {
          int line_len = input_line_end - input_line_beg;
          return_value = (char *) xmalloc (1 + line_len);
          strncpy (return_value, &input_line[input_line_beg], line_len);
          return_value[line_len] = '\0';
        }
    }
  return (return_value);
}

/* Read a line of text in the echo area.  Return a malloc ()'ed string,
   or NULL if the user aborted out of this read.  WINDOW is the currently
   active window, so that we can restore it when we need to.  PROMPT, if
   non-null, is a prompt to print before reading the line. */
char *
info_read_in_echo_area (window, prompt)
     WINDOW *window;
     char *prompt;
{
  char *line;

  /* If the echo area is already active, remember the current state. */
  if (echo_area_is_active)
    push_echo_area ();

  /* Initialize our local variables. */
  initialize_input_line (prompt);

  /* Initialize the echo area for the first (but maybe not the last) time. */
  echo_area_initialize_node ();

  /* Save away the original node of this window, and the window itself,
     so echo area commands can temporarily use this window. */
  remember_calling_window (window);

  /* Let the rest of Info know that the echo area is active. */
  echo_area_is_active++;
  active_window = the_echo_area;

  /* Read characters in the echo area. */
  info_read_and_dispatch ();

  echo_area_is_active--;

  /* Restore the original active window and show point in it. */
  active_window = calling_window;
  restore_calling_window ();
  display_cursor_at_point (active_window);
  fflush (stdout);

  /* Get the value of the line. */
  line = echo_area_after_read ();

  /* If there is a previous loop waiting for us, restore it now. */
  if (echo_area_is_active)
    pop_echo_area ();

  /* Return the results to the caller. */
  return (line);
}

/* (re) Initialize the echo area node. */
static void
echo_area_initialize_node ()
{
  register int i;

  for (i = input_line_end; i < sizeof (input_line); i++)
    input_line[i] = ' ';

  input_line[i - 1] = '\n';
  window_set_node_of_window (the_echo_area, &input_line_node);
  input_line[input_line_end] = '\n';
}

/* Prepare to read characters in the echo area.  This can initialize the
   echo area node, but its primary purpose is to side effect the input
   line buffer contents. */
void
echo_area_prep_read ()
{
  if (the_echo_area->node != &input_line_node)
    echo_area_initialize_node ();

  the_echo_area->point = input_line_point;
  input_line[input_line_end] = '\n';
  display_update_one_window (the_echo_area);
  display_cursor_at_point (active_window);
}


/* **************************************************************** */
/*                                                                  */
/*                   Echo Area Movement Commands                    */
/*                                                                  */
/* **************************************************************** */

DECLARE_INFO_COMMAND (ea_forward, _("Move forward a character"))
{
  if (count < 0)
    ea_backward (window, -count, key);
  else
    {
      input_line_point += count;
      if (input_line_point > input_line_end)
        input_line_point = input_line_end;
    }
}

DECLARE_INFO_COMMAND (ea_backward, _("Move backward a character"))
{
  if (count < 0)
    ea_forward (window, -count, key);
  else
    {
      input_line_point -= count;
      if (input_line_point < input_line_beg)
        input_line_point = input_line_beg;
    }
}

DECLARE_INFO_COMMAND (ea_beg_of_line, _("Move to the start of this line"))
{
  input_line_point = input_line_beg;
}

DECLARE_INFO_COMMAND (ea_end_of_line, _("Move to the end of this line"))
{
  input_line_point = input_line_end;
}

#define alphabetic(c) (islower (c) || isupper (c) || isdigit (c))

/* Move forward a word in the input line. */
DECLARE_INFO_COMMAND (ea_forward_word, _("Move forward a word"))
{
  int c;

  if (count < 0)
    ea_backward_word (window, -count, key);
  else
    {
      while (count--)
        {
          if (input_line_point == input_line_end)
            return;

          /* If we are not in a word, move forward until we are in one.
             Then, move forward until we hit a non-alphabetic character. */
          c = input_line[input_line_point];

          if (!alphabetic (c))
            {
              while (++input_line_point < input_line_end)
                {
                  c = input_line[input_line_point];
                  if (alphabetic (c))
                    break;
                }
            }

          if (input_line_point == input_line_end)
            return;

          while (++input_line_point < input_line_end)
            {
              c = input_line[input_line_point];
              if (!alphabetic (c))
                break;
            }
        }
    }
}

DECLARE_INFO_COMMAND (ea_backward_word, _("Move backward a word"))
{
  int c;

  if (count < 0)
    ea_forward_word (window, -count, key);
  else
    {
      while (count--)
        {
          if (input_line_point == input_line_beg)
            return;

          /* Like ea_forward_word (), except that we look at the
             characters just before point. */

          c = input_line[input_line_point - 1];

          if (!alphabetic (c))
            {
              while ((--input_line_point) != input_line_beg)
                {
                  c = input_line[input_line_point - 1];
                  if (alphabetic (c))
                    break;
                }
            }

          while (input_line_point != input_line_beg)
            {
              c = input_line[input_line_point - 1];
              if (!alphabetic (c))
                break;
              else
                --input_line_point;
            }
        }
    }
}

DECLARE_INFO_COMMAND (ea_delete, _("Delete the character under the cursor"))
{
  register int i;

  if (count < 0)
    ea_rubout (window, -count, key);
  else
    {
      if (input_line_point == input_line_end)
        return;

      if (info_explicit_arg || count > 1)
        {
          int orig_point;

          orig_point = input_line_point;
          ea_forward (window, count, key);
          ea_kill_text (orig_point, input_line_point);
          input_line_point = orig_point;
        }
      else
        {
          for (i = input_line_point; i < input_line_end; i++)
            input_line[i] = input_line[i + 1];

          input_line_end--;
        }
    }
}

DECLARE_INFO_COMMAND (ea_rubout, _("Delete the character behind the cursor"))
{
  if (count < 0)
    ea_delete (window, -count, key);
  else
    {
      int start;

      if (input_line_point == input_line_beg)
        return;

      start = input_line_point;
      ea_backward (window, count, key);

      if (info_explicit_arg || count > 1)
        ea_kill_text (start, input_line_point);
      else
        ea_delete (window, count, key);
    }
}

DECLARE_INFO_COMMAND (ea_abort, _("Cancel or quit operation"))
{
  /* If any text, just discard it, and restore the calling window's node.
     If no text, quit. */
  if (input_line_end != input_line_beg)
    {
      terminal_ring_bell ();
      input_line_end = input_line_point = input_line_beg;
      if (calling_window->node != calling_window_node)
        restore_calling_window ();
    }
  else
    info_aborted_echo_area = 1;
}

DECLARE_INFO_COMMAND (ea_newline, _("Accept (or force completion of) this line"))
{
  /* Stub does nothing.  Simply here to see if it has been executed. */
}

DECLARE_INFO_COMMAND (ea_quoted_insert, _("Insert next character verbatim"))
{
  unsigned char character;

  character = info_get_another_input_char ();
  ea_insert (window, count, character);
}

DECLARE_INFO_COMMAND (ea_insert, _("Insert this character"))
{
  register int i;

  if ((input_line_end + 1) == EA_MAX_INPUT)
    {
      terminal_ring_bell ();
      return;
    }

  for (i = input_line_end + 1; i != input_line_point; i--)
    input_line[i] = input_line[i - 1];

  input_line[input_line_point] = key;
  input_line_point++;
  input_line_end++;
}

DECLARE_INFO_COMMAND (ea_tab_insert, _("Insert a TAB character"))
{
  ea_insert (window, count, '\t');
}

/* Transpose the characters at point.  If point is at the end of the line,
   then transpose the characters before point. */
DECLARE_INFO_COMMAND (ea_transpose_chars, _("Transpose characters at point"))
{
  /* Handle conditions that would make it impossible to transpose
     characters. */
  if (!count || !input_line_point || (input_line_end - input_line_beg) < 2)
    return;

  while (count)
    {
      int t;
      if (input_line_point == input_line_end)
        {
          t = input_line[input_line_point - 1];

          input_line[input_line_point - 1] = input_line[input_line_point - 2];
          input_line[input_line_point - 2] = t;
        }
      else
        {
          t = input_line[input_line_point];

          input_line[input_line_point] = input_line[input_line_point - 1];
          input_line[input_line_point - 1] = t;

          if (count < 0 && input_line_point != input_line_beg)
            input_line_point--;
          else
            input_line_point++;
        }

      if (count < 0)
        count++;
      else
        count--;
    }
}

/* **************************************************************** */
/*                                                                  */
/*                   Echo Area Killing and Yanking                  */
/*                                                                  */
/* **************************************************************** */

static char **kill_ring = (char **)NULL;
static int kill_ring_index = 0; /* Number of kills appearing in KILL_RING. */
static int kill_ring_slots = 0; /* Number of slots allocated to KILL_RING. */
static int kill_ring_loc = 0;   /* Location of current yank pointer. */

/* The largest number of kills that we remember at one time. */
static int max_retained_kills = 15;

DECLARE_INFO_COMMAND (ea_yank, _("Yank back the contents of the last kill"))
{
  register int i;
  register char *text;

  if (!kill_ring_index)
    {
      inform_in_echo_area (_("Kill ring is empty"));
      return;
    }

  text = kill_ring[kill_ring_loc];

  for (i = 0; text[i]; i++)
    ea_insert (window, 1, text[i]);
}

/* If the last command was yank, or yank_pop, and the text just before
   point is identical to the current kill item, then delete that text
   from the line, rotate the index down, and yank back some other text. */
DECLARE_INFO_COMMAND (ea_yank_pop, _("Yank back a previous kill"))
{
  register int len;

  if (((ea_last_executed_command != ea_yank) &&
       (ea_last_executed_command != ea_yank_pop)) ||
      (kill_ring_index == 0))
    return;

  len = strlen (kill_ring[kill_ring_loc]);

  /* Delete the last yanked item from the line. */
  {
    register int i, counter;

    counter = input_line_end - input_line_point;
    
    for (i = input_line_point - len; counter; i++, counter--)
      input_line[i] = input_line[i + len];

    input_line_end -= len;
    input_line_point -= len;
  }

  /* Get a previous kill, and yank that. */
  kill_ring_loc--;
  if (kill_ring_loc < 0)
    kill_ring_loc = kill_ring_index - 1;

  ea_yank (window, count, key);
}

/* Delete the text from point to end of line. */
DECLARE_INFO_COMMAND (ea_kill_line, _("Kill to the end of the line"))
{
  if (count < 0)
    {
      ea_kill_text (input_line_point, input_line_beg);
      input_line_point = input_line_beg;
    }
  else
    ea_kill_text (input_line_point, input_line_end);
}

/* Delete the text from point to beg of line. */
DECLARE_INFO_COMMAND (ea_backward_kill_line,
                      _("Kill to the beginning of the line"))
{
  if (count < 0)
    ea_kill_text (input_line_point, input_line_end);
  else
    {
      ea_kill_text (input_line_point, input_line_beg);
      input_line_point = input_line_beg;
    }
}

/* Delete from point to the end of the current word. */
DECLARE_INFO_COMMAND (ea_kill_word, _("Kill the word following the cursor"))
{
  int orig_point = input_line_point;

  if (count < 0)
    ea_backward_kill_word (window, -count, key);
  else
    {
      ea_forward_word (window, count, key);

      if (input_line_point != orig_point)
        ea_kill_text (orig_point, input_line_point);

      input_line_point = orig_point;
    }
}

/* Delete from point to the start of the current word. */
DECLARE_INFO_COMMAND (ea_backward_kill_word,
                      _("Kill the word preceding the cursor"))
{
  int orig_point = input_line_point;

  if (count < 0)
    ea_kill_word (window, -count, key);
  else
    {
      ea_backward_word (window, count, key);

      if (input_line_point != orig_point)
        ea_kill_text (orig_point, input_line_point);
    }
}

/* The way to kill something.  This appends or prepends to the last
   kill, if the last command was a kill command.  If FROM is less
   than TO, then the killed text is appended to the most recent kill,
   otherwise it is prepended.  If the last command was not a kill command,
   then a new slot is made for this kill. */
static void
ea_kill_text (from, to)
     int from, to;
{
  register int i, counter, distance;
  int killing_backwards, slot;
  char *killed_text;

  killing_backwards = (from > to);

  /* If killing backwards, reverse the values of FROM and TO. */
  if (killing_backwards)
    {
      int temp = from;
      from = to;
      to = temp;
    }

  /* Remember the text that we are about to delete. */
  distance = to - from;
  killed_text = (char *)xmalloc (1 + distance);
  strncpy (killed_text, &input_line[from], distance);
  killed_text[distance] = '\0';

  /* Actually delete the text from the line. */
  counter = input_line_end - to;

  for (i = from; counter; i++, counter--)
    input_line[i] = input_line[i + distance];

  input_line_end -= distance;

  /* If the last command was a kill, append or prepend the killed text to
     the last command's killed text. */
  if (echo_area_last_command_was_kill)
    {
      char *old, *new;

      slot = kill_ring_loc;
      old = kill_ring[slot];
      new = (char *)xmalloc (1 + strlen (old) + strlen (killed_text));

      if (killing_backwards)
        {
          /* Prepend TEXT to current kill. */
          strcpy (new, killed_text);
          strcat (new, old);
        }
      else
        {
          /* Append TEXT to current kill. */
          strcpy (new, old);
          strcat (new, killed_text);
        }

      free (old);
      free (killed_text);
      kill_ring[slot] = new;
    }
  else
    {
      /* Try to store the kill in a new slot, unless that would cause there
         to be too many remembered kills. */
      slot = kill_ring_index;

      if (slot == max_retained_kills)
        slot = 0;

      if (slot + 1 > kill_ring_slots)
        kill_ring = (char **) xrealloc
          (kill_ring,
           (kill_ring_slots += max_retained_kills) * sizeof (char *));

      if (slot != kill_ring_index)
        free (kill_ring[slot]);
      else
        kill_ring_index++;

      kill_ring[slot] = killed_text;

      kill_ring_loc = slot;
    }

  /* Notice that the last command was a kill. */
  echo_area_last_command_was_kill++;
}

/* **************************************************************** */
/*                                                                  */
/*                      Echo Area Completion                        */
/*                                                                  */
/* **************************************************************** */

/* Pointer to an array of REFERENCE to complete over. */
static REFERENCE **echo_area_completion_items = (REFERENCE **)NULL;

/* Sorted array of REFERENCE * which is the possible completions found in
   the variable echo_area_completion_items.  If there is only one element,
   it is the only possible completion. */
static REFERENCE **completions_found = (REFERENCE **)NULL;
static int completions_found_index = 0;
static int completions_found_slots = 0;

/* The lowest common denominator found while completing. */
static REFERENCE *LCD_completion;

/* Internal functions used by the user calls. */
static void build_completions (), completions_must_be_rebuilt ();

/* Variable which holds the output of completions. */
static NODE *possible_completions_output_node = (NODE *)NULL;

static char *compwin_name = "*Completions*";

/* Return non-zero if WINDOW is a window used for completions output. */
static int
completions_window_p (window)
     WINDOW *window;
{
  int result = 0;

  if (internal_info_node_p (window->node) &&
      (strcmp (window->node->nodename, compwin_name) == 0))
    result = 1;

  return (result);
}

/* Workhorse for completion readers.  If FORCE is non-zero, the user cannot
   exit unless the line read completes, or is empty. */
char *
info_read_completing_internal (window, prompt, completions, force)
     WINDOW *window;
     char *prompt;
     REFERENCE **completions;
     int force;
{
  char *line;

  /* If the echo area is already active, remember the current state. */
  if (echo_area_is_active)
    push_echo_area ();

  echo_area_must_complete_p = force;

  /* Initialize our local variables. */
  initialize_input_line (prompt);

  /* Initialize the echo area for the first (but maybe not the last) time. */
  echo_area_initialize_node ();

  /* Save away the original node of this window, and the window itself,
     so echo area commands can temporarily use this window. */
  remember_calling_window (window);

  /* Save away the list of items to complete over. */
  echo_area_completion_items = completions;
  completions_must_be_rebuilt ();

  active_window = the_echo_area;
  echo_area_is_active++;

  /* Read characters in the echo area. */
  while (1)
    {
      info_read_and_dispatch ();

      line = echo_area_after_read ();

      /* Force the completion to take place if the user hasn't accepted
         a default or aborted, and if FORCE is active. */
      if (force && line && *line && completions)
        {
          register int i;

          build_completions ();

          /* If there is only one completion, then make the line be that
             completion. */
          if (completions_found_index == 1)
            {
              free (line);
              line = xstrdup (completions_found[0]->label);
              break;
            }

          /* If one of the completions matches exactly, then that is okay, so
             return the current line. */
          for (i = 0; i < completions_found_index; i++)
            if (strcasecmp (completions_found[i]->label, line) == 0)
              {
                free (line);
                line = xstrdup (completions_found[i]->label);
                break;
              }

          /* If no match, go back and try again. */
          if (i == completions_found_index)
            {
              inform_in_echo_area (_("Not complete"));
              continue;
            }
        }
      break;
    }
  echo_area_is_active--;

  /* Restore the original active window and show point in it. */
  active_window = calling_window;
  restore_calling_window ();
  display_cursor_at_point (active_window);
  fflush (stdout);

  echo_area_completion_items = (REFERENCE **)NULL;
  completions_must_be_rebuilt ();

  /* If there is a previous loop waiting for us, restore it now. */
  if (echo_area_is_active)
    pop_echo_area ();

  return (line);
}
  
/* Read a line in the echo area with completion over COMPLETIONS. */
char *
info_read_completing_in_echo_area (window, prompt, completions)
     WINDOW *window;
     char *prompt;
     REFERENCE **completions;
{
  return (info_read_completing_internal (window, prompt, completions, 1));
}

/* Read a line in the echo area allowing completion over COMPLETIONS, but
   not requiring it. */
char *
info_read_maybe_completing (window, prompt, completions)
     WINDOW *window;
     char *prompt;
     REFERENCE **completions;
{
  return (info_read_completing_internal (window, prompt, completions, 0));
}

DECLARE_INFO_COMMAND (ea_possible_completions, _("List possible completions"))
{
  if (!echo_area_completion_items)
    {
      ea_insert (window, count, key);
      return;
    }

  build_completions ();

  if (!completions_found_index)
    {
      terminal_ring_bell ();
      inform_in_echo_area (_("No completions"));
    }
  else if ((completions_found_index == 1) && (key != '?'))
    {
      inform_in_echo_area (_("Sole completion"));
    }
  else
    {
      register int i, l;
      int limit, count, max_label = 0;

      initialize_message_buffer ();
      printf_to_message_buffer (completions_found_index == 1
                                ? _("One completion:\n")
                                : _("%d completions:\n"));

      /* Find the maximum length of a label. */
      for (i = 0; i < completions_found_index; i++)
        {
          int len = strlen (completions_found[i]->label);
          if (len > max_label)
            max_label = len;
        }

      max_label += 4;

      /* Find out how many columns we should print in. */
      limit = calling_window->width / max_label;
      if (limit != 1 && (limit * max_label == calling_window->width))
        limit--;

      /* Avoid a possible floating exception.  If max_label > width then
         the limit will be 0 and a divide-by-zero fault will result. */
      if (limit == 0)
        limit = 1;

      /* How many iterations of the printing loop? */
      count = (completions_found_index + (limit - 1)) / limit;

      /* Watch out for special case.  If the number of completions is less
         than LIMIT, then just do the inner printing loop. */
      if (completions_found_index < limit)
        count = 1;

      /* Print the sorted items, up-and-down alphabetically. */
      for (i = 0; i < count; i++)
        {
          register int j;

          for (j = 0, l = i; j < limit; j++)
            {
              if (l >= completions_found_index)
                break;
              else
                {
                  char *label;
                  int printed_length, k;

                  label = completions_found[l]->label;
                  printed_length = strlen (label);
                  printf_to_message_buffer ("%s", label);

                  if (j + 1 < limit)
                    {
                      for (k = 0; k < max_label - printed_length; k++)
                        printf_to_message_buffer (" ");
                    }
                }
              l += count;
            }
          printf_to_message_buffer ("\n");
        }

      /* Make a new node to hold onto possible completions.  Don't destroy
         dangling pointers. */
      {
        NODE *temp;

        temp = message_buffer_to_node ();
        add_gcable_pointer (temp->contents);
        name_internal_node (temp, compwin_name);
        possible_completions_output_node = temp;
      }

      /* Find a suitable window for displaying the completions output.
         First choice is an existing window showing completions output.
         If there is only one window, and it is large, make another
         (smaller) window, and use that one.  Otherwise, use the caller's
         window. */
      {
        WINDOW *compwin;

        compwin = get_internal_info_window (compwin_name);

        if (!compwin)
          {
            /* If we can split the window to display most of the completion
               items, then do so. */
            if (calling_window->height > (count * 2)
		&& calling_window->height / 2 >= WINDOW_MIN_SIZE)
              {
                int start, pagetop;
#ifdef SPLIT_BEFORE_ACTIVE
                int end;
#endif

                active_window = calling_window;

                /* Perhaps we can scroll this window on redisplay. */
                start = calling_window->first_row;
                pagetop = calling_window->pagetop;

                compwin =
                  window_make_window (possible_completions_output_node);
                active_window = the_echo_area;
                window_change_window_height
                  (compwin, -(compwin->height - (count + 2)));

                window_adjust_pagetop (calling_window);
                remember_calling_window (calling_window);

#if defined (SPLIT_BEFORE_ACTIVE)
                /* If the pagetop hasn't changed, scrolling the calling
                   window is a reasonable thing to do. */
                if (pagetop == calling_window->pagetop)
                  {
                    end = start + calling_window->height;
                    display_scroll_display
                      (start, end, calling_window->prev->height + 1);
                  }
#else /* !SPLIT_BEFORE_ACTIVE */
                /* If the pagetop has changed, set the new pagetop here. */
                if (pagetop != calling_window->pagetop)
                  {
                    int newtop = calling_window->pagetop;
                    calling_window->pagetop = pagetop;
                    set_window_pagetop (calling_window, newtop);
                  }
#endif /* !SPLIT_BEFORE_ACTIVE */

                echo_area_completions_window = compwin;
                remember_window_and_node (compwin, compwin->node);
              }
            else
              compwin = calling_window;
          }

        if (compwin->node != possible_completions_output_node)
          {
            window_set_node_of_window
              (compwin, possible_completions_output_node);
            remember_window_and_node (compwin, compwin->node);
          }

        display_update_display (windows);
      }
    }
}

DECLARE_INFO_COMMAND (ea_complete, _("Insert completion"))
{
  if (!echo_area_completion_items)
    {
      ea_insert (window, count, key);
      return;
    }

  /* If KEY is SPC, and we are not forcing completion to take place, simply
     insert the key. */
  if (!echo_area_must_complete_p && key == SPC)
    {
      ea_insert (window, count, key);
      return;
    }

  if (ea_last_executed_command == ea_complete)
    {
      /* If the keypress is a SPC character, and we have already tried
         completing once, and there are several completions, then check
         the batch of completions to see if any continue with a space.
         If there are some, insert the space character and continue. */
      if (key == SPC && completions_found_index > 1)
        {
          register int i, offset;

          offset = input_line_end - input_line_beg;

          for (i = 0; i < completions_found_index; i++)
            if (completions_found[i]->label[offset] == ' ')
              break;

          if (completions_found[i])
            ea_insert (window, 1, ' ');
          else
            {
              ea_possible_completions (window, count, key);
              return;
            }
        }
      else
        {
          ea_possible_completions (window, count, key);
          return;
        }
    }

  input_line_point = input_line_end;
  build_completions ();

  if (!completions_found_index)
    terminal_ring_bell ();
  else if (LCD_completion->label[0] == '\0')
    ea_possible_completions (window, count, key);
  else
    {
      register int i;
      input_line_point = input_line_end = input_line_beg;
      for (i = 0; LCD_completion->label[i]; i++)
        ea_insert (window, 1, LCD_completion->label[i]);
    }
}

/* Utility REFERENCE used to store possible LCD. */
static REFERENCE LCD_reference = { (char *)NULL, (char *)NULL, (char *)NULL };

static void remove_completion_duplicates ();

/* Variables which remember the state of the most recent call
   to build_completions (). */
static char *last_completion_request = (char *)NULL;
static REFERENCE **last_completion_items = (REFERENCE **)NULL;

/* How to tell the completion builder to reset internal state. */
static void
completions_must_be_rebuilt ()
{
  maybe_free (last_completion_request);
  last_completion_request = (char *)NULL;
  last_completion_items = (REFERENCE **)NULL;
}

/* Build a list of possible completions from echo_area_completion_items,
   and the contents of input_line. */
static void
build_completions ()
{
  register int i, len;
  register REFERENCE *entry;
  char *request;
  int informed_of_lengthy_job = 0;

  /* If there are no items to complete over, exit immediately. */
  if (!echo_area_completion_items)
    {
      completions_found_index = 0;
      LCD_completion = (REFERENCE *)NULL;
      return;
    }

  /* Check to see if this call to build completions is the same as the last
     call to build completions. */
  len = input_line_end - input_line_beg;
  request = (char *)xmalloc (1 + len);
  strncpy (request, &input_line[input_line_beg], len);
  request[len] = '\0';

  if (last_completion_request && last_completion_items &&
      last_completion_items == echo_area_completion_items &&
      (strcmp (last_completion_request, request) == 0))
    {
      free (request);
      return;
    }

  maybe_free (last_completion_request);
  last_completion_request = request;
  last_completion_items = echo_area_completion_items;

  /* Always start at the beginning of the list. */
  completions_found_index = 0;
  LCD_completion = (REFERENCE *)NULL;

  for (i = 0; (entry = echo_area_completion_items[i]); i++)
    {
      if (strncasecmp (request, entry->label, len) == 0)
        add_pointer_to_array (entry, completions_found_index,
                              completions_found, completions_found_slots,
                              20, REFERENCE *);

      if (!informed_of_lengthy_job && completions_found_index > 100)
        {
          informed_of_lengthy_job = 1;
          window_message_in_echo_area (_("Building completions..."));
        }
    }

  if (!completions_found_index)
    return;

  /* Sort and prune duplicate entries from the completions array. */
  remove_completion_duplicates ();

  /* If there is only one completion, just return that. */
  if (completions_found_index == 1)
    {
      LCD_completion = completions_found[0];
      return;
    }

  /* Find the least common denominator. */
  {
    long shortest = 100000;

    for (i = 1; i < completions_found_index; i++)
      {
        register int j;
        int c1, c2;

        for (j = 0;
             (c1 = info_tolower (completions_found[i - 1]->label[j])) &&
             (c2 = info_tolower (completions_found[i]->label[j]));
             j++)
          if (c1 != c2)
            break;

        if (shortest > j)
          shortest = j;
      }

    maybe_free (LCD_reference.label);
    LCD_reference.label = (char *)xmalloc (1 + shortest);
    strncpy (LCD_reference.label, completions_found[0]->label, shortest);
    LCD_reference.label[shortest] = '\0';
    LCD_completion = &LCD_reference;
  }

  if (informed_of_lengthy_job)
    echo_area_initialize_node ();
}

/* Function called by qsort. */
static int
compare_references (entry1, entry2)
     REFERENCE **entry1, **entry2;
{
  return (strcasecmp ((*entry1)->label, (*entry2)->label));
}

/* Prune duplicate entries from COMPLETIONS_FOUND. */
static void
remove_completion_duplicates ()
{
  register int i, j;
  REFERENCE **temp;
  int newlen;

  if (!completions_found_index)
    return;

  /* Sort the items. */
  qsort (completions_found, completions_found_index, sizeof (REFERENCE *),
         compare_references);

  for (i = 0, newlen = 1; i < completions_found_index - 1; i++)
    {
      if (strcmp (completions_found[i]->label,
                  completions_found[i + 1]->label) == 0)
        completions_found[i] = (REFERENCE *)NULL;
      else
        newlen++;
    }

  /* We have marked all the dead slots.  It is faster to copy the live slots
     twice than to prune the dead slots one by one. */
  temp = (REFERENCE **)xmalloc ((1 + newlen) * sizeof (REFERENCE *));
  for (i = 0, j = 0; i < completions_found_index; i++)
    if (completions_found[i])
      temp[j++] = completions_found[i];

  for (i = 0; i < newlen; i++)
    completions_found[i] = temp[i];

  completions_found[i] = (REFERENCE *)NULL;
  completions_found_index = newlen;
  free (temp);
}

/* Scroll the "other" window.  If there is a window showing completions, scroll
   that one, otherwise scroll the window which was active on entering the read
   function. */
DECLARE_INFO_COMMAND (ea_scroll_completions_window, _("Scroll the completions window"))
{
  WINDOW *compwin;
  int old_pagetop;

  compwin = get_internal_info_window (compwin_name);

  if (!compwin)
    compwin = calling_window;

  old_pagetop = compwin->pagetop;

  /* Let info_scroll_forward () do the work, and print any messages that
     need to be displayed. */
  info_scroll_forward (compwin, count, key);
}

/* Function which gets called when an Info window is deleted while the
   echo area is active.  WINDOW is the window which has just been deleted. */
void
echo_area_inform_of_deleted_window (window)
     WINDOW *window;
{
  /* If this is the calling_window, forget what we remembered about it. */
  if (window == calling_window)
    {
      if (active_window != the_echo_area)
        remember_calling_window (active_window);
      else
        remember_calling_window (windows);
    }

  /* If this window was the echo_area_completions_window, then notice that
     the window has been deleted. */
  if (window == echo_area_completions_window)
    echo_area_completions_window = (WINDOW *)NULL;
}

/* **************************************************************** */
/*                                                                  */
/*                 Pushing and Popping the Echo Area                */
/*                                                                  */
/* **************************************************************** */

/* Push and Pop the echo area. */
typedef struct {
  char *line;
  char *prompt;
  REFERENCE **comp_items;
  int point, beg, end;
  int must_complete;
  NODE node;
  WINDOW *compwin;
} PUSHED_EA;

static PUSHED_EA **pushed_echo_areas = (PUSHED_EA **)NULL;
static int pushed_echo_areas_index = 0;
static int pushed_echo_areas_slots = 0;

/* Pushing the echo_area has a side effect of zeroing the completion_items. */
static void
push_echo_area ()
{
  PUSHED_EA *pushed;

  pushed = (PUSHED_EA *)xmalloc (sizeof (PUSHED_EA));
  pushed->line = xstrdup (input_line);
  pushed->prompt = input_line_prompt;
  pushed->point = input_line_point;
  pushed->beg = input_line_beg;
  pushed->end = input_line_end;
  pushed->node = input_line_node;
  pushed->comp_items = echo_area_completion_items;
  pushed->must_complete = echo_area_must_complete_p;
  pushed->compwin = echo_area_completions_window;

  add_pointer_to_array (pushed, pushed_echo_areas_index, pushed_echo_areas,
                        pushed_echo_areas_slots, 4, PUSHED_EA *);

  echo_area_completion_items = (REFERENCE **)NULL;
}

static void
pop_echo_area ()
{
  PUSHED_EA *popped;

  popped = pushed_echo_areas[--pushed_echo_areas_index];

  strcpy (input_line, popped->line);
  free (popped->line);
  input_line_prompt = popped->prompt;
  input_line_point = popped->point;
  input_line_beg = popped->beg;
  input_line_end = popped->end;
  input_line_node = popped->node;
  echo_area_completion_items = popped->comp_items;
  echo_area_must_complete_p = popped->must_complete;
  echo_area_completions_window = popped->compwin;
  completions_must_be_rebuilt ();

  /* If the completion window no longer exists, forget about it. */
  if (echo_area_completions_window)
    {
      register WINDOW *win;

      for (win = windows; win; win = win->next)
        if (echo_area_completions_window == win)
          break;

      /* If the window wasn't found, then it has already been deleted. */
      if (!win)
        echo_area_completions_window = (WINDOW *)NULL;
    }

  free (popped);
}

/* Returns non-zero if any of the prior stacked calls to read in the echo
   area produced a completions window. */
static int
echo_area_stack_contains_completions_p ()
{
  register int i;

  for (i = 0; i < pushed_echo_areas_index; i++)
    if (pushed_echo_areas[i]->compwin)
      return (1);

  return (0);
}

/* **************************************************************** */
/*                                                                  */
/*             Error Messages While Reading in Echo Area            */
/*                                                                  */
/* **************************************************************** */

#if defined (HAVE_SYS_TIME_H)
#  include <sys/time.h>
#  define HAVE_STRUCT_TIMEVAL
#endif /* HAVE_SYS_TIME_H */

static void
pause_or_input ()
{
#if defined (FD_SET)
  struct timeval timer;
  fd_set readfds;
  int ready;

  FD_ZERO (&readfds);
  FD_SET (fileno (stdin), &readfds);
  timer.tv_sec = 2;
  timer.tv_usec = 750;
  ready = select (fileno (stdin) + 1, &readfds, (fd_set *) NULL,
                  (fd_set *) NULL, &timer);
#endif /* FD_SET */
}

/* Print MESSAGE right after the end of the current line, and wait
   for input or 2.75 seconds, whichever comes first.  Then flush the
   informational message that was printed. */
void
inform_in_echo_area (message)
     char *message;
{
  register int i;
  char *text;

  text = xstrdup (message);
  for (i = 0; text[i] && text[i] != '\n'; i++);
  text[i] = '\0';

  echo_area_initialize_node ();
  sprintf (&input_line[input_line_end], "%s[%s]\n",
           echo_area_is_active ? " ": "", text);
  free (text);
  the_echo_area->point = input_line_point;
  display_update_one_window (the_echo_area);
  display_cursor_at_point (active_window);
  fflush (stdout);
  pause_or_input ();
  echo_area_initialize_node ();
}
