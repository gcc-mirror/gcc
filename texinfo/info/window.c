/* window.c -- Windows in Info.
   $Id: window.c,v 1.1.1.3 1998/03/24 18:20:20 law Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

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
#include "nodes.h"
#include "window.h"
#include "display.h"
#include "info-utils.h"
#include "infomap.h"

/* The window which describes the screen. */
WINDOW *the_screen = (WINDOW *)NULL;

/* The window which describes the echo area. */
WINDOW *the_echo_area = (WINDOW *)NULL;

/* The list of windows in Info. */
WINDOW *windows = (WINDOW *)NULL;

/* Pointer to the active window in WINDOW_LIST. */
WINDOW *active_window = (WINDOW *)NULL;

/* The size of the echo area in Info.  It never changes, irregardless of the
   size of the screen. */
#define ECHO_AREA_HEIGHT 1

/* Macro returns the amount of space that the echo area truly requires relative
   to the entire screen. */
#define echo_area_required (1 + the_echo_area->height)

/* Initalize the window system by creating THE_SCREEN and THE_ECHO_AREA.
   Create the first window ever.
   You pass the dimensions of the total screen size. */
void
window_initialize_windows (width, height)
     int width, height;
{
  the_screen = (WINDOW *)xmalloc (sizeof (WINDOW));
  the_echo_area = (WINDOW *)xmalloc (sizeof (WINDOW));
  windows = (WINDOW *)xmalloc (sizeof (WINDOW));
  active_window = windows;

  zero_mem (the_screen, sizeof (WINDOW));
  zero_mem (the_echo_area, sizeof (WINDOW));
  zero_mem (active_window, sizeof (WINDOW));

  /* None of these windows has a goal column yet. */
  the_echo_area->goal_column = -1;
  active_window->goal_column = -1;
  the_screen->goal_column = -1;

  /* The active and echo_area windows are visible.
     The echo_area is permanent.
     The screen is permanent. */
  active_window->flags = W_WindowVisible;
  the_echo_area->flags = W_WindowIsPerm | W_InhibitMode | W_WindowVisible;
  the_screen->flags    = W_WindowIsPerm;

  /* The height of the echo area never changes.  It is statically set right
     here, and it must be at least 1 line for display.  The size of the
     initial window cannot be the same size as the screen, since the screen
     includes the echo area.  So, we make the height of the initial window
     equal to the screen's displayable region minus the height of the echo
     area. */
  the_echo_area->height = ECHO_AREA_HEIGHT;
  active_window->height = the_screen->height - 1 - the_echo_area->height;
  window_new_screen_size (width, height, (VFunction *)NULL);

  /* The echo area uses a different keymap than normal info windows. */
  the_echo_area->keymap = echo_area_keymap;
  active_window->keymap = info_keymap;
}

/* Given that the size of the screen has changed to WIDTH and HEIGHT
   from whatever it was before (found in the_screen->height, ->width),
   change the size (and possibly location) of each window in the screen.
   If a window would become too small, call the function DELETER on it,
   after deleting the window from our chain of windows.  If DELETER is NULL,
   nothing extra is done.  The last window can never be deleted, but it can
   become invisible. */

/* If non-null, a function to call with WINDOW as argument when the function
   window_new_screen_size () has deleted WINDOW. */
VFunction *window_deletion_notifier = (VFunction *)NULL;

void
window_new_screen_size (width, height)
     int width, height;
{
  register WINDOW *win;
  int delta_height, delta_each, delta_leftover;
  int numwins;

  /* If no change, do nothing. */
  if (width == the_screen->width && height == the_screen->height)
    return;

  /* If the new window height is too small, make it be zero. */
  if (height < (WINDOW_MIN_SIZE + the_echo_area->height))
    height = 0;
  if (width < 0)
    width = 0;

  /* Find out how many windows will change. */
  for (numwins = 0, win = windows; win; win = win->next, numwins++);

  /* See if some windows will need to be deleted.  This is the case if
     the screen is getting smaller, and the available space divided by
     the number of windows is less than WINDOW_MIN_SIZE.  In that case,
     delete some windows and try again until there is either enough
     space to divy up among the windows, or until there is only one
     window left. */
  while ((height - echo_area_required) / numwins <= WINDOW_MIN_SIZE)
    {
      /* If only one window, make the size of it be zero, and return
         immediately. */
      if (!windows->next)
        {
          windows->height = 0;
          maybe_free (windows->line_starts);
          windows->line_starts = (char **)NULL;
          windows->line_count = 0;
          break;
        }

      /* If we have some temporary windows, delete one of them. */
      for (win = windows; win; win = win->next)
        if (win->flags & W_TempWindow)
          break;

      /* Otherwise, delete the first window, and try again. */
      if (!win)
        win = windows;

      if (window_deletion_notifier)
        (*window_deletion_notifier) (win);

      window_delete_window (win);
      numwins--;
    }

  /* The screen has changed height and width. */
  delta_height = height - the_screen->height;   /* This is how much. */
  the_screen->height = height;                  /* This is the new height. */
  the_screen->width = width;                    /* This is the new width. */

  /* Set the start of the echo area. */
  the_echo_area->first_row = height - the_echo_area->height;
  the_echo_area->width = width;

  /* Check to see if the screen can really be changed this way. */
  if ((!windows->next) && ((windows->height == 0) && (delta_height < 0)))
    return;

  /* Divide the change in height among the available windows. */
  delta_each = delta_height / numwins;
  delta_leftover = delta_height - (delta_each * numwins);

  /* Change the height of each window in the chain by delta_each.  Change
     the height of the last window in the chain by delta_each and by the
     leftover amount of change.  Change the width of each window to be
     WIDTH. */
  for (win = windows; win; win = win->next)
    {
      if ((win->width != width) && ((win->flags & W_InhibitMode) == 0))
        {
          win->width = width;
          maybe_free (win->modeline);
          win->modeline = (char *)xmalloc (1 + width);
        }

      win->height += delta_each;

      /* If the previous height of this window was zero, it was the only
         window, and it was not visible.  Thus we need to compensate for
         the echo_area. */
      if (win->height == delta_each)
        win->height -= (1 + the_echo_area->height);

      /* If this is not the first window in the chain, then change the
         first row of it.  We cannot just add delta_each to the first row,
         since this window's first row is the sum of the collective increases
         that have gone before it.  So we just add one to the location of the
         previous window's modeline. */
      if (win->prev)
        win->first_row = (win->prev->first_row + win->prev->height) + 1;

      /* The last window in the chain gets the extra space (or shrinkage). */
      if (!win->next)
        win->height += delta_leftover;

      if (win->node)
        recalculate_line_starts (win);

      win->flags |= W_UpdateWindow;
    }

  /* If the screen got smaller, check over the windows just shrunk to
     keep them within bounds.  Some of the windows may have gotten smaller
     than WINDOW_MIN_HEIGHT in which case some of the other windows are
     larger than the available display space in the screen.  Because of our
     intial test above, we know that there is enough space for all of the
     windows. */
  if ((delta_each < 0) && ((windows->height != 0) && windows->next))
    {
      int avail;

      avail = the_screen->height - (numwins + the_echo_area->height);
      win = windows;

      while (win)
        {
          if ((win->height < WINDOW_MIN_HEIGHT) ||
              (win->height > avail))
            {
              WINDOW *lastwin;

              /* Split the space among the available windows. */
              delta_each = avail / numwins;
              delta_leftover = avail - (delta_each * numwins);

              for (win = windows; win; win = win->next)
                {
                  lastwin = win;
                  if (win->prev)
                    win->first_row =
                      (win->prev->first_row + win->prev->height) + 1;
                  win->height = delta_each;
                }

              /* Give the leftover space (if any) to the last window. */
              lastwin->height += delta_leftover;
              break;
            }
          else
            win= win->next;
        }
    }
}

/* Make a new window showing NODE, and return that window structure.
   If NODE is passed as NULL, then show the node showing in the active
   window.  If the window could not be made return a NULL pointer.  The
   active window is not changed.*/
WINDOW *
window_make_window (node)
     NODE *node;
{
  WINDOW *window;

  if (!node)
    node = active_window->node;

  /* If there isn't enough room to make another window, return now. */
  if ((active_window->height / 2) < WINDOW_MIN_SIZE)
    return ((WINDOW *)NULL);

  /* Make and initialize the new window.
     The fudging about with -1 and +1 is because the following window in the
     chain cannot start at window->height, since that is where the modeline
     for the previous window is displayed.  The inverse adjustment is made
     in window_delete_window (). */
  window = (WINDOW *)xmalloc (sizeof (WINDOW));
  window->width = the_screen->width;
  window->height = (active_window->height / 2) - 1;
#if defined (SPLIT_BEFORE_ACTIVE)
  window->first_row = active_window->first_row;
#else
  window->first_row = active_window->first_row +
    (active_window->height - window->height);
#endif
  window->keymap = info_keymap;
  window->goal_column = -1;
  window->modeline = (char *)xmalloc (1 + window->width);
  window->line_starts = (char **)NULL;
  window->flags = W_UpdateWindow | W_WindowVisible;
  window_set_node_of_window (window, node);

  /* Adjust the height of the old active window. */
  active_window->height -= (window->height + 1);
#if defined (SPLIT_BEFORE_ACTIVE)
  active_window->first_row += (window->height + 1);
#endif
  active_window->flags |= W_UpdateWindow;

  /* Readjust the new and old windows so that their modelines and contents
     will be displayed correctly. */
#if defined (NOTDEF)
  /* We don't have to do this for WINDOW since window_set_node_of_window ()
     already did. */
  window_adjust_pagetop (window);
  window_make_modeline (window);
#endif /* NOTDEF */

  /* We do have to readjust the existing active window. */
  window_adjust_pagetop (active_window);
  window_make_modeline (active_window);

#if defined (SPLIT_BEFORE_ACTIVE)
  /* This window is just before the active one.  The active window gets
     bumped down one.  The active window is not changed. */
  window->next = active_window;

  window->prev = active_window->prev;
  active_window->prev = window;

  if (window->prev)
    window->prev->next = window;
  else
    windows = window;
#else
  /* This window is just after the active one.  Which window is active is
     not changed. */
  window->prev = active_window;
  window->next = active_window->next;
  active_window->next = window;
  if (window->next)
    window->next->prev = window;
#endif /* !SPLIT_BEFORE_ACTIVE */
  return (window);
}

/* These useful macros make it possible to read the code in
   window_change_window_height (). */
#define grow_me_shrinking_next(me, next, diff) \
  do { \
    me->height += diff; \
    next->height -= diff; \
    next->first_row += diff; \
    window_adjust_pagetop (next); \
  } while (0)

#define grow_me_shrinking_prev(me, prev, diff) \
  do { \
    me->height += diff; \
    prev->height -= diff; \
    me->first_row -=diff; \
    window_adjust_pagetop (prev); \
  } while (0)

#define shrink_me_growing_next(me, next, diff) \
  do { \
    me->height -= diff; \
    next->height += diff; \
    next->first_row -= diff; \
    window_adjust_pagetop (next); \
  } while (0)

#define shrink_me_growing_prev(me, prev, diff) \
  do { \
    me->height -= diff; \
    prev->height += diff; \
    me->first_row += diff; \
    window_adjust_pagetop (prev); \
  } while (0)

/* Change the height of WINDOW by AMOUNT.  This also automagically adjusts
   the previous and next windows in the chain.  If there is only one user
   window, then no change takes place. */
void
window_change_window_height (window, amount)
     WINDOW *window;
     int amount;
{
  register WINDOW *win, *prev, *next;

  /* If there is only one window, or if the amount of change is zero,
     return immediately. */
  if (!windows->next || amount == 0)
    return;

  /* Find this window in our chain. */
  for (win = windows; win; win = win->next)
    if (win == window)
      break;

  /* If the window is isolated (i.e., doesn't appear in our window list,
     then quit now. */
  if (!win)
    return;

  /* Change the height of this window by AMOUNT, if that is possible.
     It can be impossible if there isn't enough available room on the
     screen, or if the resultant window would be too small. */

    prev = window->prev;
    next = window->next;

  /* WINDOW decreasing in size? */
  if (amount < 0)
    {
      int abs_amount = -amount; /* It is easier to deal with this way. */

      /* If the resultant window would be too small, stop here. */
      if ((window->height - abs_amount) < WINDOW_MIN_HEIGHT)
        return;

      /* If we have two neighboring windows, choose the smaller one to get
         larger. */
      if (next && prev)
        {
          if (prev->height < next->height)
            shrink_me_growing_prev (window, prev, abs_amount);
          else
            shrink_me_growing_next (window, next, abs_amount);
        }
      else if (next)
        shrink_me_growing_next (window, next, abs_amount);
      else
        shrink_me_growing_prev (window, prev, abs_amount);
    }

  /* WINDOW increasing in size? */
  if (amount > 0)
    {
      int total_avail, next_avail = 0, prev_avail = 0;

      if (next)
        next_avail = next->height - WINDOW_MIN_SIZE;

      if (prev)
        prev_avail = prev->height - WINDOW_MIN_SIZE;

      total_avail = next_avail + prev_avail;

      /* If there isn't enough space available to grow this window, give up. */
      if (amount > total_avail)
        return;

      /* If there aren't two neighboring windows, or if one of the neighbors
         is larger than the other one by at least AMOUNT, grow that one. */
      if ((next && !prev) || ((next_avail - amount) >= prev_avail))
        grow_me_shrinking_next (window, next, amount);
      else if ((prev && !next) || ((prev_avail - amount) >= next_avail))
        grow_me_shrinking_prev (window, prev, amount);
      else
        {
          int change;

          /* This window has two neighbors.  They both must be shrunk in to
             make enough space for WINDOW to grow.  Make them both the same
             size. */
          if (prev_avail > next_avail)
            {
              change = prev_avail - next_avail;
              grow_me_shrinking_prev (window, prev, change);
              amount -= change;
            }
          else
            {
              change = next_avail - prev_avail;
              grow_me_shrinking_next (window, next, change);
              amount -= change;
            }

          /* Both neighbors are the same size.  Split the difference in
             AMOUNT between them. */
          while (amount)
            {
              window->height++;
              amount--;

              /* Odd numbers grow next, even grow prev. */
              if (amount & 1)
                {
                  prev->height--;
                  window->first_row--;
                }
              else
                {
                  next->height--;
                  next->first_row++;
                }
            }
          window_adjust_pagetop (prev);
          window_adjust_pagetop (next);
        }
    }
  if (prev)
    prev->flags |= W_UpdateWindow;

  if (next)
    next->flags |= W_UpdateWindow;

  window->flags |= W_UpdateWindow;
  window_adjust_pagetop (window);
}

/* Tile all of the windows currently displayed in the global variable
   WINDOWS.  If argument STYLE is TILE_INTERNALS, tile windows displaying
   internal nodes as well, otherwise do not change the height of such
   windows. */
void
window_tile_windows (style)
     int style;
{
  WINDOW *win, *last_adjusted;
  int numwins, avail, per_win_height, leftover;
  int do_internals;

  numwins = avail = 0;
  do_internals = (style == TILE_INTERNALS);

  for (win = windows; win; win = win->next)
    if (do_internals || !win->node ||
        (win->node->flags & N_IsInternal) == 0)
      {
        avail += win->height;
        numwins++;
      }

  if (numwins <= 1 || !the_screen->height)
    return;

  /* Find the size for each window.  Divide the size of the usable portion
     of the screen by the number of windows. */
  per_win_height = avail / numwins;
  leftover = avail - (per_win_height * numwins);

  last_adjusted = (WINDOW *)NULL;
  for (win = windows; win; win = win->next)
    {
      if (do_internals || !win->node ||
          (win->node->flags & N_IsInternal) == 0)
        {
          last_adjusted = win;
          win->height = per_win_height;
        }
    }

  if (last_adjusted)
    last_adjusted->height += leftover;

  /* Readjust the first_row of every window in the chain. */
  for (win = windows; win; win = win->next)
    {
      if (win->prev)
        win->first_row = win->prev->first_row + win->prev->height + 1;

      window_adjust_pagetop (win);
      win->flags |= W_UpdateWindow;
    }
}

/* Toggle the state of line wrapping in WINDOW.  This can do a bit of fancy
   redisplay. */
void
window_toggle_wrap (window)
     WINDOW *window;
{
  if (window->flags & W_NoWrap)
    window->flags &= ~W_NoWrap;
  else
    window->flags |= W_NoWrap;

  if (window != the_echo_area)
    {
      char **old_starts;
      int old_lines, old_pagetop;

      old_starts = window->line_starts;
      old_lines = window->line_count;
      old_pagetop = window->pagetop;

      calculate_line_starts (window);

      /* Make sure that point appears within this window. */
      window_adjust_pagetop (window);

      /* If the pagetop hasn't changed maybe we can do some scrolling now
         to speed up the display.  Many of the line starts will be the same,
         so scrolling here is a very good optimization.*/
      if (old_pagetop == window->pagetop)
        display_scroll_line_starts
          (window, old_pagetop, old_starts, old_lines);
      maybe_free (old_starts);
    }
  window->flags |= W_UpdateWindow;
}

/* Set WINDOW to display NODE. */
void
window_set_node_of_window (window, node)
     WINDOW *window;
     NODE *node;
{
  window->node = node;
  window->pagetop = 0;
  window->point = 0;
  recalculate_line_starts (window);
  window->flags |= W_UpdateWindow;
  window_adjust_pagetop (window);
  window_make_modeline (window);
}

/* Delete WINDOW from the list of known windows.  If this window was the
   active window, make the next window in the chain be the active window.
   If the active window is the next or previous window, choose that window
   as the recipient of the extra space.  Otherwise, prefer the next window. */
void
window_delete_window (window)
     WINDOW *window;
{
  WINDOW *next, *prev, *window_to_fix;

  next = window->next;
  prev = window->prev;

  /* You cannot delete the only window or a permanent window. */
  if ((!next && !prev) || (window->flags & W_WindowIsPerm))
    return;

  if (next)
    next->prev = prev;

  if (!prev)
    windows = next;
  else
    prev->next = next;

  if (window->line_starts)
    free (window->line_starts);

  if (window->modeline)
    free (window->modeline);

  if (window == active_window)
    {
      /* If there isn't a next window, then there must be a previous one,
         since we cannot delete the last window.  If there is a next window,
         prefer to use that as the active window. */
      if (next)
        active_window = next;
      else
        active_window = prev;
    }

  if (next && active_window == next)
    window_to_fix = next;
  else if (prev && active_window == prev)
    window_to_fix = prev;
  else if (next)
    window_to_fix = next;
  else if (prev)
    window_to_fix = prev;
  else
    window_to_fix = windows;
    
  if (window_to_fix->first_row > window->first_row)
    {
      int diff;

      /* Try to adjust the visible part of the node so that as little
         text as possible has to move. */
      diff = window_to_fix->first_row - window->first_row;
      window_to_fix->first_row = window->first_row;

      window_to_fix->pagetop -= diff;
      if (window_to_fix->pagetop < 0)
        window_to_fix->pagetop = 0;
    }

  /* The `+ 1' is to offset the difference between the first_row locations.
     See the code in window_make_window (). */
  window_to_fix->height += window->height + 1;
  window_to_fix->flags |= W_UpdateWindow;

  free (window);
}

/* For every window in CHAIN, set the flags member to have FLAG set. */
void
window_mark_chain (chain, flag)
     WINDOW *chain;
     int flag;
{
  register WINDOW *win;

  for (win = chain; win; win = win->next)
    win->flags |= flag;
}

/* For every window in CHAIN, clear the flags member of FLAG. */
void
window_unmark_chain (chain, flag)
     WINDOW *chain;
     int flag;
{
  register WINDOW *win;

  for (win = chain; win; win = win->next)
    win->flags &= ~flag;
}

/* Return the number of characters it takes to display CHARACTER on the
   screen at HPOS. */
int
character_width (character, hpos)
     int character, hpos;
{
  int printable_limit = 127;
  int width = 1;

  if (ISO_Latin_p)
    printable_limit = 255;

  if (character > printable_limit)
    width = 3;
  else if (iscntrl (character))
    {
      switch (character)
        {
        case '\r':
        case '\n':
          width = the_screen->width - hpos;
          break;
        case '\t':
          width = ((hpos + 8) & 0xf8) - hpos;
          break;
        default:
          width = 2;
        }
    }
  else if (character == DEL)
    width = 2;

  return (width);
}

/* Return the number of characters it takes to display STRING on the screen
   at HPOS. */
int
string_width (string, hpos)
     char *string;
     int hpos;
{
  register int i, width, this_char_width;

  for (width = 0, i = 0; string[i]; i++)
    {
      this_char_width = character_width (string[i], hpos);
      width += this_char_width;
      hpos += this_char_width;
    }
  return (width);
}

/* Quickly guess the approximate number of lines to that NODE would
   take to display.  This really only counts carriage returns. */
int
window_physical_lines (node)
     NODE *node;
{
  register int i, lines;
  char *contents;

  if (!node)
    return (0);

  contents = node->contents;
  for (i = 0, lines = 1; i < node->nodelen; i++)
    if (contents[i] == '\n')
      lines++;

  return (lines);
}

/* Calculate a list of line starts for the node belonging to WINDOW.  The line
   starts are pointers to the actual text within WINDOW->NODE. */
void
calculate_line_starts (window)
     WINDOW *window;
{
  register int i, hpos;
  char **line_starts = (char **)NULL;
  int line_starts_index = 0, line_starts_slots = 0;
  int bump_index;
  NODE *node;

  window->line_starts = (char **)NULL;
  window->line_count = 0;
  node = window->node;

  if (!node)
    return;

  /* Grovel the node starting at the top, and for each line calculate the
     width of the characters appearing in that line.  Add each line start
     to our array. */
  i = 0;
  hpos = 0;
  bump_index = 0;

  while (i < node->nodelen)
    {
      char *line = node->contents + i;
      unsigned int cwidth, c;

      add_pointer_to_array (line, line_starts_index, line_starts,
                            line_starts_slots, 100, char *);
      if (bump_index)
        {
          i++;
          bump_index = 0;
        }

      while (1)
        {
          c = node->contents[i];
          cwidth = character_width (c, hpos);

          /* If this character fits within this line, just do the next one. */
          if ((hpos + cwidth) < window->width)
            {
              i++;
              hpos += cwidth;
              continue;
            }
          else
            {
              /* If this character would position the cursor at the start of
                 the next printed screen line, then do the next line. */
              if (c == '\n' || c == '\r' || c == '\t')
                {
                  i++;
                  hpos = 0;
                  break;
                }
              else
                {
                  /* This character passes the window width border.  Postion
                     the cursor after the printed character, but remember this
                     line start as where this character is.  A bit tricky. */

                  /* If this window doesn't wrap lines, proceed to the next
                     physical line here. */
                  if (window->flags & W_NoWrap)
                    {
                      hpos = 0;
                      while (i < node->nodelen && node->contents[i] != '\n')
                        i++;

                      if (node->contents[i] == '\n')
                        i++;
                    }
                  else
                    {
                      hpos = the_screen->width - hpos;
                      bump_index++;
                    }
                  break;
                }
            }
        }
    }
  window->line_starts = line_starts;
  window->line_count = line_starts_index;
}

/* Given WINDOW, recalculate the line starts for the node it displays. */
void
recalculate_line_starts (window)
     WINDOW *window;
{
  maybe_free (window->line_starts);
  calculate_line_starts (window);
}

/* Global variable control redisplay of scrolled windows.  If non-zero, it
   is the desired number of lines to scroll the window in order to make
   point visible.  A user might set this to 1 for smooth scrolling.  If
   set to zero, the line containing point is centered within the window. */
int window_scroll_step = 0;

/* Adjust the pagetop of WINDOW such that the cursor point will be visible. */
void
window_adjust_pagetop (window)
     WINDOW *window;
{
  register int line = 0;
  char *contents;

  if (!window->node)
    return;

  contents = window->node->contents;

  /* Find the first printed line start which is after WINDOW->point. */
  for (line = 0; line < window->line_count; line++)
    {
      char *line_start;

      line_start = window->line_starts[line];

      if ((line_start - contents) > window->point)
        break;
    }

  /* The line index preceding the line start which is past point is the
     one containing point. */
  line--;

  /* If this line appears in the current displayable page, do nothing.
     Otherwise, adjust the top of the page to make this line visible. */
  if ((line < window->pagetop) ||
      (line - window->pagetop > (window->height - 1)))
    {
      /* The user-settable variable "scroll-step" is used to attempt
         to make point visible, iff it is non-zero.  If that variable
         is zero, then the line containing point is centered within
         the window. */
      if (window_scroll_step < window->height)
        {
          if ((line < window->pagetop) &&
              ((window->pagetop - window_scroll_step) <= line))
            window->pagetop -= window_scroll_step;
          else if ((line - window->pagetop > (window->height - 1)) &&
                   ((line - (window->pagetop + window_scroll_step)
                     < window->height)))
            window->pagetop += window_scroll_step;
          else
            window->pagetop = line - ((window->height - 1) / 2);
        }
      else
        window->pagetop = line - ((window->height - 1) / 2);

      if (window->pagetop < 0)
        window->pagetop = 0;
      window->flags |= W_UpdateWindow;
    }
}

/* Return the index of the line containing point. */
int
window_line_of_point (window)
     WINDOW *window;
{
  register int i, start = 0;

  /* Try to optimize.  Check to see if point is past the pagetop for
     this window, and if so, start searching forward from there. */
  if ((window->pagetop > -1 && window->pagetop < window->line_count) &&
      (window->line_starts[window->pagetop] - window->node->contents)
      <= window->point)
    start = window->pagetop;

  for (i = start; i < window->line_count; i++)
    {
      if ((window->line_starts[i] - window->node->contents) > window->point)
        break;
    }

  return (i - 1);
}

/* Get and return the goal column for this window. */
int
window_get_goal_column (window)
     WINDOW *window;
{
  if (!window->node)
    return (-1);

  if (window->goal_column != -1)
    return (window->goal_column);

  /* Okay, do the work.  Find the printed offset of the cursor
     in this window. */
  return (window_get_cursor_column (window));
}

/* Get and return the printed column offset of the cursor in this window. */
int
window_get_cursor_column (window)
     WINDOW *window;
{
  int i, hpos, end;
  char *line;

  i = window_line_of_point (window);

  if (i < 0)
    return (-1);

  line = window->line_starts[i];
  end = window->point - (line - window->node->contents);

  for (hpos = 0, i = 0; i < end; i++)
    hpos += character_width (line[i], hpos);

  return (hpos);
}

/* Count the number of characters in LINE that precede the printed column
   offset of GOAL. */
int
window_chars_to_goal (line, goal)
     char *line;
     int goal;
{
  register int i, check, hpos;

  for (hpos = 0, i = 0; line[i] != '\n'; i++)
    {

      check = hpos + character_width (line[i], hpos);

      if (check > goal)
        break;

      hpos = check;
    }
  return (i);
}

/* Create a modeline for WINDOW, and store it in window->modeline. */
void
window_make_modeline (window)
     WINDOW *window;
{
  register int i;
  char *modeline;
  char location_indicator[4];
  int lines_remaining;

  /* Only make modelines for those windows which have one. */
  if (window->flags & W_InhibitMode)
    return;

  /* Find the number of lines actually displayed in this window. */
  lines_remaining = window->line_count - window->pagetop;

  if (window->pagetop == 0)
    {
      if (lines_remaining <= window->height)
        strcpy (location_indicator, "All");
      else
        strcpy (location_indicator, "Top");
    }
  else
    {
      if (lines_remaining <= window->height)
        strcpy (location_indicator, "Bot");
      else
        {
          float pt, lc;
          int percentage;

          pt = (float)window->pagetop;
          lc = (float)window->line_count;

          percentage = 100 * (pt / lc);

          sprintf (location_indicator, "%2d%%", percentage);
        }
    }

  /* Calculate the maximum size of the information to stick in MODELINE. */
  {
    int modeline_len = 0;
    char *parent = (char *)NULL, *filename = "*no file*";
    char *nodename = "*no node*";
    char *update_message = (char *)NULL;
    NODE *node = window->node;

    if (node)
      {
        if (node->nodename)
          nodename = node->nodename;

        if (node->parent)
          {
            parent = filename_non_directory (node->parent);
            modeline_len += strlen ("Subfile: ") + strlen (node->filename);
          }

        if (node->filename)
          filename = filename_non_directory (node->filename);

        if (node->flags & N_UpdateTags)
          update_message = _("--*** Tags out of Date ***");
      }

    if (update_message)
      modeline_len += strlen (update_message);
    modeline_len += strlen (filename);
    modeline_len += strlen (nodename);
    modeline_len += 4;          /* strlen (location_indicator). */

    /* 10 for the decimal representation of the number of lines in this
       node, and the remainder of the text that can appear in the line. */
    modeline_len += 10 + strlen (_("-----Info: (), lines ----, "));
    modeline_len += window->width;

    modeline = (char *)xmalloc (1 + modeline_len);

    /* Special internal windows have no filename. */
    if (!parent && !*filename)
      sprintf (modeline, _("-%s---Info: %s, %d lines --%s--"),
               (window->flags & W_NoWrap) ? "$" : "-",
               nodename, window->line_count, location_indicator);
    else
      sprintf (modeline, _("-%s%s-Info: (%s)%s, %d lines --%s--"),
               (window->flags & W_NoWrap) ? "$" : "-",
               (node && (node->flags & N_IsCompressed)) ? "zz" : "--",
               parent ? parent : filename,
               nodename, window->line_count, location_indicator);

    if (parent)
      sprintf (modeline + strlen (modeline), _(" Subfile: %s"), filename);

    if (update_message)
      sprintf (modeline + strlen (modeline), "%s", update_message);

    i = strlen (modeline);

    if (i >= window->width)
      modeline[window->width] = '\0';
    else
      {
        while (i < window->width)
          modeline[i++] = '-';
        modeline[i] = '\0';
      }

    strcpy (window->modeline, modeline);
    free (modeline);
  }
}

/* Make WINDOW start displaying at PERCENT percentage of its node. */
void
window_goto_percentage (window, percent)
     WINDOW *window;
     int percent;
{
  int desired_line;

  if (!percent)
    desired_line = 0;
  else
    desired_line =
      (int) ((float)window->line_count * ((float)percent / 100.0));

  window->pagetop = desired_line;
  window->point =
    window->line_starts[window->pagetop] - window->node->contents;
  window->flags |= W_UpdateWindow;
  window_make_modeline (window);
}

/* Get the state of WINDOW, and save it in STATE. */
void
window_get_state (window, state)
     WINDOW *window;
     WINDOW_STATE *state;
{
  state->node = window->node;
  state->pagetop = window->pagetop;
  state->point = window->point;
}

/* Set the node, pagetop, and point of WINDOW. */
void
window_set_state (window, state)
     WINDOW *window;
     WINDOW_STATE *state;
{
  if (window->node != state->node)
    window_set_node_of_window (window, state->node);
  window->pagetop = state->pagetop;
  window->point = state->point;
}


/* **************************************************************** */
/*                                                                  */
/*                 Manipulating Home-Made Nodes                     */
/*                                                                  */
/* **************************************************************** */

/* A place to buffer echo area messages. */
static NODE *echo_area_node = (NODE *)NULL;

/* Make the node of the_echo_area be an empty one. */
static void
free_echo_area ()
{
  if (echo_area_node)
    {
      maybe_free (echo_area_node->contents);
      free (echo_area_node);
    }

  echo_area_node = (NODE *)NULL;
  window_set_node_of_window (the_echo_area, echo_area_node);
}
  
/* Clear the echo area, removing any message that is already present.
   The echo area is cleared immediately. */
void
window_clear_echo_area ()
{
  free_echo_area ();
  display_update_one_window (the_echo_area);
}

/* Make a message appear in the echo area, built from FORMAT, ARG1 and ARG2.
   The arguments are treated similar to printf () arguments, but not all of
   printf () hair is present.  The message appears immediately.  If there was
   already a message appearing in the echo area, it is removed. */
void
window_message_in_echo_area (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  free_echo_area ();
  echo_area_node = build_message_node (format, arg1, arg2);
  window_set_node_of_window (the_echo_area, echo_area_node);
  display_update_one_window (the_echo_area);
}

/* Place a temporary message in the echo area built from FORMAT, ARG1
   and ARG2.  The message appears immediately, but does not destroy
   any existing message.  A future call to unmessage_in_echo_area ()
   restores the old contents. */
static NODE **old_echo_area_nodes = (NODE **)NULL;
static int old_echo_area_nodes_index = 0;
static int old_echo_area_nodes_slots = 0;

void
message_in_echo_area (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  if (echo_area_node)
    {
      add_pointer_to_array (echo_area_node, old_echo_area_nodes_index,
                            old_echo_area_nodes, old_echo_area_nodes_slots,
                            4, NODE *);
    }
  echo_area_node = (NODE *)NULL;
  window_message_in_echo_area (format, arg1, arg2);
}

void
unmessage_in_echo_area ()
{
  free_echo_area ();

  if (old_echo_area_nodes_index)
    echo_area_node = old_echo_area_nodes[--old_echo_area_nodes_index];

  window_set_node_of_window (the_echo_area, echo_area_node);
  display_update_one_window (the_echo_area);
}

/* A place to build a message. */
static char *message_buffer = (char *)NULL;
static int message_buffer_index = 0;
static int message_buffer_size = 0;

/* Ensure that there is enough space to stuff LENGTH characters into
   MESSAGE_BUFFER. */
static void
message_buffer_resize (length)
     int length;
{
  if (!message_buffer)
    {
      message_buffer_size = length + 1;
      message_buffer = (char *)xmalloc (message_buffer_size);
      message_buffer_index = 0;
    }

  while (message_buffer_size <= message_buffer_index + length)
    message_buffer = (char *)
      xrealloc (message_buffer,
                message_buffer_size += 100 + (2 * length));
}

/* Format MESSAGE_BUFFER with the results of printing FORMAT with ARG1 and
   ARG2. */
static void
build_message_buffer (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  register int i, len;
  void *args[2];
  int arg_index = 0;

  args[0] = arg1;
  args[1] = arg2;

  len = strlen (format);

  message_buffer_resize (len);

  for (i = 0; format[i]; i++)
    {
      if (format[i] != '%')
        {
          message_buffer[message_buffer_index++] = format[i];
          len--;
        }
      else
        {
          char c;

          c = format[++i];

          switch (c)
            {
            case '%':           /* Insert a percent sign. */
              message_buffer_resize (len + 1);
              message_buffer[message_buffer_index++] = '%';
              break;

            case 's':           /* Insert the current arg as a string. */
              {
                char *string;
                int string_len;

                string = (char *)args[arg_index++];
                string_len = strlen (string);

                message_buffer_resize (len + string_len);
                sprintf
                  (message_buffer + message_buffer_index, "%s", string);
                message_buffer_index += string_len;
              }
              break;

            case 'd':           /* Insert the current arg as an integer. */
              {
                long long_val;
                int integer;

                long_val = (long)args[arg_index++];
                integer = (int)long_val;

                message_buffer_resize (len + 32);
                sprintf
                  (message_buffer + message_buffer_index, "%d", integer);
                message_buffer_index = strlen (message_buffer);
              }
              break;

            case 'c':           /* Insert the current arg as a character. */
              {
                long long_val;
                int character;

                long_val = (long)args[arg_index++];
                character = (int)long_val;

                message_buffer_resize (len + 1);
                message_buffer[message_buffer_index++] = character;
              }
              break;

            default:
              abort ();
            }
        }
    }
  message_buffer[message_buffer_index] = '\0';
}

/* Build a new node which has FORMAT printed with ARG1 and ARG2 as the
   contents. */
NODE *
build_message_node (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  NODE *node;

  message_buffer_index = 0;
  build_message_buffer (format, arg1, arg2);

  node = message_buffer_to_node ();
  return (node);
}

/* Convert the contents of the message buffer to a node. */
NODE *
message_buffer_to_node ()
{
  NODE *node;

  node = (NODE *)xmalloc (sizeof (NODE));
  node->filename = (char *)NULL;
  node->parent = (char *)NULL;
  node->nodename = (char *)NULL;
  node->flags = 0;

  /* Make sure that this buffer ends with a newline. */
  node->nodelen = 1 + strlen (message_buffer);
  node->contents = (char *)xmalloc (1 + node->nodelen);
  strcpy (node->contents, message_buffer);
  node->contents[node->nodelen - 1] = '\n';
  node->contents[node->nodelen] = '\0';
  return (node);
}

/* Useful functions can be called from outside of window.c. */
void
initialize_message_buffer ()
{
  message_buffer_index = 0;
}

/* Print FORMAT with ARG1,2 to the end of the current message buffer. */
void
printf_to_message_buffer (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  build_message_buffer (format, arg1, arg2);
}

/* Return the current horizontal position of the "cursor" on the most
   recently output message buffer line. */
int
message_buffer_length_this_line ()
{
  register int i;

  if (!message_buffer_index)
    return (0);

  for (i = message_buffer_index; i && message_buffer[i - 1] != '\n'; i--);

  return (string_width (message_buffer + i, 0));
}

/* Pad STRING to COUNT characters by inserting blanks. */
int
pad_to (count, string)
     int count;
     char *string;
{
  register int i;

  i = strlen (string);

  if (i >= count)
    string[i++] = ' ';
  else
    {
      while (i < count)
        string[i++] = ' ';
    }
  string[i] = '\0';

  return (i);
}
