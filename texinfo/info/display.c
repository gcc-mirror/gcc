/* display.c -- How to display Info windows.
   $Id: display.c,v 1.6 1997/07/24 21:13:27 karl Exp $

   Copyright (C) 1993, 97 Free Software Foundation, Inc.

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
#include "display.h"

extern int info_any_buffered_input_p (); /* Found in session.c. */

static void free_display ();
static DISPLAY_LINE **make_display ();

/* An array of display lines which tell us what is currently visible on
   the display.  */
DISPLAY_LINE **the_display = (DISPLAY_LINE **)NULL;

/* Non-zero means do no output. */
int display_inhibited = 0;

/* Initialize THE_DISPLAY to WIDTH and HEIGHT, with nothing in it. */
void
display_initialize_display (width, height)
     int width, height;
{
  free_display (the_display);
  the_display = make_display (width, height);
  display_clear_display (the_display);
}

/* Clear all of the lines in DISPLAY making the screen blank. */
void
display_clear_display (display)
     DISPLAY_LINE **display;
{
  register int i;
  register DISPLAY_LINE *display_line;

  for (i = 0; (display_line = display[i]); i++)
    {
      display[i]->text[0] = '\0';
      display[i]->textlen = 0;
      display[i]->inverse = 0;
    }
}

/* Non-zero if we didn't completely redisplay a window. */
int display_was_interrupted_p = 0;

/* Update the windows pointed to by WINDOW in the_display.  This actually
   writes the text on the screen. */
void
display_update_display (window)
     WINDOW *window;
{
  register WINDOW *win;

  display_was_interrupted_p = 0;

  /* For every window in the list, check contents against the display. */
  for (win = window; win; win = win->next)
    {
      /* Only re-display visible windows which need updating. */
      if (((win->flags & W_WindowVisible) == 0) ||
          ((win->flags & W_UpdateWindow) == 0) ||
          (win->height == 0))
        continue;

      display_update_one_window (win);
      if (display_was_interrupted_p)
        break;
    }

  /* Always update the echo area. */
  display_update_one_window (the_echo_area);
}

/* Display WIN on the_display.  Unlike display_update_display (), this
   function only does one window. */
void
display_update_one_window (win)
     WINDOW *win;
{
  register char *nodetext;      /* Current character to display. */
  register char *last_node_char; /* Position of the last character in node. */
  register int i;               /* General use index. */
  char *printed_line;           /* Buffer for a printed line. */
  int pl_index = 0;             /* Index into PRINTED_LINE. */
  int line_index = 0;           /* Number of lines done so far. */
  DISPLAY_LINE **display = the_display;

  /* If display is inhibited, that counts as an interrupted display. */
  if (display_inhibited)
    display_was_interrupted_p = 1;

  /* If the window has no height, or display is inhibited, quit now. */
  if (!win->height || display_inhibited)
    return;

  /* If the window's first row doesn't appear in the_screen, then it
     cannot be displayed.  This can happen when the_echo_area is the
     window to be displayed, and the screen has shrunk to less than one
     line. */
  if ((win->first_row < 0) || (win->first_row > the_screen->height))
    return;

  /* Print each line in the window into our local buffer, and then
     check the contents of that buffer against the display.  If they
     differ, update the display. */
  printed_line = (char *)xmalloc (1 + win->width);

  if (!win->node || !win->line_starts)
    goto done_with_node_display;

  nodetext = win->line_starts[win->pagetop];
  last_node_char = win->node->contents + win->node->nodelen;

  for (; nodetext < last_node_char; nodetext++)
    {
      char *rep, *rep_carried_over, rep_temp[2];
      int replen;

      if (isprint (*nodetext))
        {
          rep_temp[0] = *nodetext;
          replen = 1;
          rep_temp[1] = '\0';
          rep = rep_temp;
        }
      else
        {
          if (*nodetext == '\r' || *nodetext == '\n')
            {
              replen = win->width - pl_index;
            }
          else
            {
              rep = printed_representation (*nodetext, pl_index);
              replen = strlen (rep);
            }
        }

      /* If this character can be printed without passing the width of
         the line, then stuff it into the line. */
      if (replen + pl_index < win->width)
        {
          /* Optimize if possible. */
          if (replen == 1)
            {
              printed_line[pl_index++] = *rep;
            }
          else
            {
              for (i = 0; i < replen; i++)
                printed_line[pl_index++] = rep[i];
            }
        }
      else
        {
          DISPLAY_LINE *entry;

          /* If this character cannot be printed in this line, we have
             found the end of this line as it would appear on the screen.
             Carefully print the end of the line, and then compare. */
          if (*nodetext == '\n' || *nodetext == '\r' || *nodetext == '\t')
            {
              printed_line[pl_index] = '\0';
              rep_carried_over = (char *)NULL;
            }
          else
            {
              /* The printed representation of this character extends into
                 the next line.  Remember the offset of the last character
                 printed out of REP so that we can carry the character over
                 to the next line. */
              for (i = 0; pl_index < (win->width - 1);)
                printed_line[pl_index++] = rep[i++];
              
              rep_carried_over = rep + i;

              /* If printing the last character in this window couldn't
                 possibly cause the screen to scroll, place a backslash
                 in the rightmost column. */
              if (1 + line_index + win->first_row < the_screen->height)
                {
                  if (win->flags & W_NoWrap)
                    printed_line[pl_index++] = '$';
                  else
                    printed_line[pl_index++] = '\\';
                }
              printed_line[pl_index] = '\0';
            }

          /* We have the exact line as it should appear on the screen.
             Check to see if this line matches the one already appearing
             on the screen. */
          entry = display[line_index + win->first_row];

          /* If the screen line is inversed, then we have to clear
             the line from the screen first.  Why, I don't know. */
          if (entry->inverse)
            {
              terminal_goto_xy (0, line_index + win->first_row);
              terminal_clear_to_eol ();
              entry->inverse = 0;
              entry->text[0] = '\0';
              entry->textlen = 0;
            }

          /* Find the offset where these lines differ. */
          for (i = 0; i < pl_index; i++)
            if (printed_line[i] != entry->text[i])
              break;

          /* If the lines are not the same length, or if they differed
             at all, we must do some redrawing. */
          if ((i != pl_index) || (pl_index != entry->textlen))
            {
              /* Move to the proper point on the terminal. */
              terminal_goto_xy (i, line_index + win->first_row);

              /* If there is any text to print, print it. */
              if (i != pl_index)
                terminal_put_text (printed_line + i);

              /* If the printed text didn't extend all the way to the edge
                 of the window, and text was appearing between here and the
                 edge of the window, clear from here to the end of the line. */
              if ((pl_index < win->width && pl_index < entry->textlen) ||
                  (entry->inverse))
                terminal_clear_to_eol ();

              fflush (stdout);

              /* Update the display text buffer. */
              strcpy (entry->text + i, printed_line + i);
              entry->textlen = pl_index;

              /* Lines showing node text are not in inverse.  Only modelines
                 have that distinction. */
              entry->inverse = 0;
            }

          /* We have done at least one line.  Increment our screen line
             index, and check against the bottom of the window. */
          if (++line_index == win->height)
            break;

          /* A line has been displayed, and the screen reflects that state.
             If there is typeahead pending, then let that typeahead be read
             now, instead of continuing with the display. */
          if (info_any_buffered_input_p ())
            {
              free (printed_line);
              display_was_interrupted_p = 1;
              return;
            }

          /* Reset PL_INDEX to the start of the line. */
          pl_index = 0;

          /* If there are characters from REP left to print, stuff them
             into the buffer now. */
          if (rep_carried_over)
            for (; rep[pl_index]; pl_index++)
              printed_line[pl_index] = rep[pl_index];

          /* If this window has chosen not to wrap lines, skip to the end
             of the physical line in the buffer, and start a new line here. */
          if (pl_index && (win->flags & W_NoWrap))
            {
              char *begin;

              pl_index = 0;
              printed_line[0] = '\0';

              begin = nodetext;
              
              while ((nodetext < last_node_char) && (*nodetext != '\n'))
                nodetext++;
            }
        }
    }

 done_with_node_display:
  /* We have reached the end of the node or the end of the window.  If it
     is the end of the node, then clear the lines of the window from here
     to the end of the window. */
  for (; line_index < win->height; line_index++)
    {
      DISPLAY_LINE *entry = display[line_index + win->first_row];

      /* If this line has text on it then make it go away. */
      if (entry && entry->textlen)
        {
          entry->textlen = 0;
          entry->text[0] = '\0';

          terminal_goto_xy (0, line_index + win->first_row);
          terminal_clear_to_eol ();
        }
    }

  /* Finally, if this window has a modeline it might need to be redisplayed.
     Check the window's modeline against the one in the display, and update
     if necessary. */
  if ((win->flags & W_InhibitMode) == 0)
    {
      window_make_modeline (win);
      line_index = win->first_row + win->height;

      /* This display line must both be in inverse, and have the same
         contents. */
      if ((!display[line_index]->inverse) ||
          (strcmp (display[line_index]->text, win->modeline) != 0))
        {
          terminal_goto_xy (0, line_index);
          terminal_begin_inverse ();
          terminal_put_text (win->modeline);
          terminal_end_inverse ();
          strcpy (display[line_index]->text, win->modeline);
          display[line_index]->inverse = 1;
          display[line_index]->textlen = strlen (win->modeline);
          fflush (stdout);
        }
    }

  /* Okay, this window doesn't need updating anymore. */
  win->flags &= ~W_UpdateWindow;
  free (printed_line);
  fflush (stdout);
}

/* Scroll the region of the_display starting at START, ending at END, and
   moving the lines AMOUNT lines.  If AMOUNT is less than zero, the lines
   are moved up in the screen, otherwise down.  Actually, it is possible
   for no scrolling to take place in the case that the terminal doesn't
   support it.  This doesn't matter to us. */
void
display_scroll_display (start, end, amount)
     int start, end, amount;
{
  register int i, last;
  DISPLAY_LINE *temp;

  /* If this terminal cannot do scrolling, give up now. */
  if (!terminal_can_scroll)
    return;

  /* If there isn't anything displayed on the screen because it is too
     small, quit now. */
  if (!the_display[0])
    return;

  /* If there is typeahead pending, then don't actually do any scrolling. */
  if (info_any_buffered_input_p ())
    return;

  /* Do it on the screen. */
  terminal_scroll_terminal (start, end, amount);

  /* Now do it in the display buffer so our contents match the screen. */
  if (amount > 0)
    {
      last = end + amount;

      /* Shift the lines to scroll right into place. */
      for (i = 0; i < (end - start); i++)
        {
          temp = the_display[last - i];
          the_display[last - i] = the_display[end - i];
          the_display[end - i] = temp;
        }

      /* The lines have been shifted down in the buffer.  Clear all of the
         lines that were vacated. */
      for (i = start; i != (start + amount); i++)
        {
          the_display[i]->text[0] = '\0';
          the_display[i]->textlen = 0;
          the_display[i]->inverse = 0;
        }
    }

  if (amount < 0)
    {
      last = start + amount;
      for (i = 0; i < (end - start); i++)
        {
          temp = the_display[last + i];
          the_display[last + i] = the_display[start + i];
          the_display[start + i] = temp;
        }

      /* The lines have been shifted up in the buffer.  Clear all of the
         lines that are left over. */
      for (i = end + amount; i != end; i++)
        {
          the_display[i]->text[0] = '\0';
          the_display[i]->textlen = 0;
          the_display[i]->inverse = 0;
        }
    }
}

/* Try to scroll lines in WINDOW.  OLD_PAGETOP is the pagetop of WINDOW before
   having had its line starts recalculated.  OLD_STARTS is the list of line
   starts that used to appear in this window.  OLD_COUNT is the number of lines
   that appear in the OLD_STARTS array. */
void
display_scroll_line_starts (window, old_pagetop, old_starts, old_count)
     WINDOW *window;
     int old_pagetop, old_count;
     char **old_starts;
{
  register int i, old, new;     /* Indices into the line starts arrays. */
  int last_new, last_old;       /* Index of the last visible line. */
  int old_first, new_first;     /* Index of the first changed line. */
  int unchanged_at_top = 0;
  int already_scrolled = 0;

  /* Locate the first line which was displayed on the old window. */
  old_first = old_pagetop;
  new_first = window->pagetop;

  /* Find the last line currently visible in this window. */
  last_new = window->pagetop + (window->height - 1);
  if (last_new > window->line_count)
    last_new = window->line_count - 1;

  /* Find the last line which used to be currently visible in this window. */
  last_old = old_pagetop + (window->height - 1);
  if (last_old > old_count)
    last_old = old_count - 1;

  for (old = old_first, new = new_first;
       old < last_old && new < last_new;
       old++, new++)
    if (old_starts[old] != window->line_starts[new])
      break;
    else
      unchanged_at_top++;

  /* Loop through the old lines looking for a match in the new lines. */
  for (old = old_first + unchanged_at_top; old < last_old; old++)
    {
      for (new = new_first; new < last_new; new++)
        if (old_starts[old] == window->line_starts[new])
          {
            /* Find the extent of the matching lines. */
            for (i = 0; (old + i) < last_old; i++)
              if (old_starts[old + i] != window->line_starts[new + i])
                break;

            /* Scroll these lines if there are enough of them. */
            {
              int start, end, amount;

              start = (window->first_row
                       + ((old + already_scrolled) - old_pagetop));
              amount = new - (old + already_scrolled);
              end = window->first_row + window->height;

              /* If we are shifting the block of lines down, then the last
                 AMOUNT lines will become invisible.  Thus, don't bother
                 scrolling them. */
              if (amount > 0)
                end -= amount;

              if ((end - start) > 0)
                {
                  display_scroll_display (start, end, amount);

                  /* Some lines have been scrolled.  Simulate the scrolling
                     by offsetting the value of the old index. */
                  old += i;
                  already_scrolled += amount;
                }
            }
          }
    }
}

/* Move the screen cursor to directly over the current character in WINDOW. */
void
display_cursor_at_point (window)
     WINDOW *window;
{
  int vpos, hpos;

  vpos = window_line_of_point (window) - window->pagetop + window->first_row;
  hpos = window_get_cursor_column (window);
  terminal_goto_xy (hpos, vpos);
  fflush (stdout);
}

/* **************************************************************** */
/*                                                                  */
/*                   Functions Static to this File                  */
/*                                                                  */
/* **************************************************************** */

/* Make a DISPLAY_LINE ** with width and height. */
static DISPLAY_LINE **
make_display (width, height)
     int width, height;
{
  register int i;
  DISPLAY_LINE **display;

  display = (DISPLAY_LINE **)xmalloc ((1 + height) * sizeof (DISPLAY_LINE *));

  for (i = 0; i < height; i++)
    {
      display[i] = (DISPLAY_LINE *)xmalloc (sizeof (DISPLAY_LINE));
      display[i]->text = (char *)xmalloc (1 + width);
      display[i]->textlen = 0;
      display[i]->inverse = 0;
    }
  display[i] = (DISPLAY_LINE *)NULL;
  return (display);
}

/* Free the storage allocated to DISPLAY. */
static void
free_display (display)
     DISPLAY_LINE **display;
{
  register int i;
  register DISPLAY_LINE *display_line;

  if (!display)
    return;

  for (i = 0; (display_line = display[i]); i++)
    {
      free (display_line->text);
      free (display_line);
    }
  free (display);
}
