/* terminal.c -- How to handle the physical terminal for Info.
   $Id: terminal.c,v 1.1.1.3 1998/03/24 18:20:18 law Exp $

   Copyright (C) 1988, 89, 90, 91, 92, 93, 96, 97, 98
   Free Software Foundation, Inc.

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
#include "terminal.h"
#include "termdep.h"

#include <sys/types.h>
#include <signal.h>

/* The Unix termcap interface code. */
#ifdef HAVE_NCURSES_TERMCAP_H
#include <ncurses/termcap.h>
#else
#ifdef HAVE_TERMCAP_H
#include <termcap.h>
#else
/* On Solaris2, sys/types.h #includes sys/reg.h, which #defines PC.
   Unfortunately, PC is a global variable used by the termcap library. */
#undef PC

/* Termcap requires these variables, whether we access them or not. */
char *BC, *UP;
char PC;      /* Pad character */
short ospeed; /* Terminal output baud rate */
extern int tgetnum (), tgetflag (), tgetent ();
extern char *tgetstr (), *tgoto ();
extern void tputs ();
#endif /* not HAVE_TERMCAP_H */
#endif /* not HAVE_NCURSES_TERMCAP_H */

/* Function "hooks".  If you make one of these point to a function, that
   function is called when appropriate instead of its namesake.  Your
   function is called with exactly the same arguments that were passed
   to the namesake function. */
VFunction *terminal_begin_inverse_hook = (VFunction *)NULL;
VFunction *terminal_end_inverse_hook = (VFunction *)NULL;
VFunction *terminal_prep_terminal_hook = (VFunction *)NULL;
VFunction *terminal_unprep_terminal_hook = (VFunction *)NULL;
VFunction *terminal_up_line_hook = (VFunction *)NULL;
VFunction *terminal_down_line_hook = (VFunction *)NULL;
VFunction *terminal_clear_screen_hook = (VFunction *)NULL;
VFunction *terminal_clear_to_eol_hook = (VFunction *)NULL;
VFunction *terminal_get_screen_size_hook = (VFunction *)NULL;
VFunction *terminal_goto_xy_hook = (VFunction *)NULL;
VFunction *terminal_initialize_terminal_hook = (VFunction *)NULL;
VFunction *terminal_new_terminal_hook = (VFunction *)NULL;
VFunction *terminal_put_text_hook = (VFunction *)NULL;
VFunction *terminal_ring_bell_hook = (VFunction *)NULL;
VFunction *terminal_write_chars_hook = (VFunction *)NULL;
VFunction *terminal_scroll_terminal_hook = (VFunction *)NULL;

/* **************************************************************** */
/*                                                                  */
/*                      Terminal and Termcap                        */
/*                                                                  */
/* **************************************************************** */

/* A buffer which holds onto the current terminal description, and a pointer
   used to float within it. */
static char *term_buffer = (char *)NULL;
static char *term_string_buffer = (char *)NULL;

/* Some strings to control terminal actions.  These are output by tputs (). */
static char *term_goto, *term_clreol, *term_cr, *term_clrpag;
static char *term_begin_use, *term_end_use;
static char *term_AL, *term_DL, *term_al, *term_dl;

static char *term_keypad_on, *term_keypad_off;

/* How to go up a line. */
static char *term_up;

/* How to go down a line. */
static char *term_dn;

/* An audible bell, if the terminal can be made to make noise. */
static char *audible_bell;

/* A visible bell, if the terminal can be made to flash the screen. */
static char *visible_bell;

/* The string to write to turn on the meta key, if this term has one. */
static char *term_mm;

/* The string to write to turn off the meta key, if this term has one. */
static char *term_mo;

/* The string to turn on inverse mode, if this term has one. */
static char *term_invbeg;

/* The string to turn off inverse mode, if this term has one. */
static char *term_invend;

/* Although I can't find any documentation that says this is supposed to
   return its argument, all the code I've looked at (termutils, less)
   does so, so fine.  */
static int
output_character_function (c)
     int c;
{
  putc (c, stdout);
  return c;
}

/* Macro to send STRING to the terminal. */
#define send_to_terminal(string) \
  do { \
    if (string) \
      tputs (string, 1, output_character_function); \
     } while (0)

/* Tell the terminal that we will be doing cursor addressable motion.  */
static void
terminal_begin_using_terminal ()
{
  RETSIGTYPE (*sigsave) ();

  if (term_keypad_on)
      send_to_terminal (term_keypad_on);
  
  if (!term_begin_use || !*term_begin_use)
    return;

#ifdef SIGWINCH
  sigsave = signal (SIGWINCH, SIG_IGN); 
#endif

  send_to_terminal (term_begin_use);
  /* Without this fflush and sleep, running info in a shelltool or
     cmdtool (TERM=sun-cmd) with scrollbars loses -- the scrollbars are
     not restored properly.
     From: strube@physik3.gwdg.de (Hans Werner Strube).  */
  fflush (stdout);
  sleep (1);

#ifdef SIGWINCH
  signal (SIGWINCH, sigsave);
#endif
}

/* Tell the terminal that we will not be doing any more cursor
   addressable motion. */
static void
terminal_end_using_terminal ()
{
  RETSIGTYPE (*sigsave) ();

  if (term_keypad_off)
      send_to_terminal (term_keypad_off);
  
  if (!term_end_use || !*term_end_use)
    return;

#ifdef SIGWINCH
  sigsave = signal (SIGWINCH, SIG_IGN);
#endif

  send_to_terminal (term_end_use);
  fflush (stdout);
  sleep (1);

#ifdef SIGWINCH
  signal (SIGWINCH, sigsave);
#endif
}

/* **************************************************************** */
/*                                                                  */
/*                   Necessary Terminal Functions                   */
/*                                                                  */
/* **************************************************************** */

/* The functions and variables on this page implement the user visible
   portion of the terminal interface. */

/* The width and height of the terminal. */
int screenwidth, screenheight;

/* Non-zero means this terminal can't really do anything. */
int terminal_is_dumb_p = 0;

/* Non-zero means that this terminal has a meta key. */
int terminal_has_meta_p = 0;

/* Non-zero means that this terminal can produce a visible bell. */
int terminal_has_visible_bell_p = 0;

/* Non-zero means to use that visible bell if at all possible. */
int terminal_use_visible_bell_p = 0;

/* Non-zero means that the terminal can do scrolling. */
int terminal_can_scroll = 0;

/* The key sequences output by the arrow keys, if this terminal has any. */
char *term_ku = (char *)NULL;
char *term_kd = (char *)NULL;
char *term_kr = (char *)NULL;
char *term_kl = (char *)NULL;
char *term_kP = (char *)NULL;   /* page-up */
char *term_kN = (char *)NULL;   /* page-down */

/* Move the cursor to the terminal location of X and Y. */
void
terminal_goto_xy (x, y)
     int x, y;
{
  if (terminal_goto_xy_hook)
    (*terminal_goto_xy_hook) (x, y);
  else
    {
      if (term_goto)
        tputs (tgoto (term_goto, x, y), 1, output_character_function);
    }
}

/* Print STRING to the terminal at the current position. */
void
terminal_put_text (string)
     char *string;
{
  if (terminal_put_text_hook)
    (*terminal_put_text_hook) (string);
  else
    {
      printf ("%s", string);
    }
}

/* Print NCHARS from STRING to the terminal at the current position. */
void
terminal_write_chars (string, nchars)
     char *string;
     int nchars;
{
  if (terminal_write_chars_hook)
    (*terminal_write_chars_hook) (string, nchars);
  else
    {
      if (nchars)
        fwrite (string, 1, nchars, stdout);
    }
}

/* Clear from the current position of the cursor to the end of the line. */
void
terminal_clear_to_eol ()
{
  if (terminal_clear_to_eol_hook)
    (*terminal_clear_to_eol_hook) ();
  else
    {
      send_to_terminal (term_clreol);
    }
}

/* Clear the entire terminal screen. */
void
terminal_clear_screen ()
{
  if (terminal_clear_screen_hook)
    (*terminal_clear_screen_hook) ();
  else
    {
      send_to_terminal (term_clrpag);
    }
}

/* Move the cursor up one line. */
void
terminal_up_line ()
{
  if (terminal_up_line_hook)
    (*terminal_up_line_hook) ();
  else
    {
      send_to_terminal (term_up);
    }
}

/* Move the cursor down one line. */
void
terminal_down_line ()
{
  if (terminal_down_line_hook)
    (*terminal_down_line_hook) ();
  else
    {
      send_to_terminal (term_dn);
    }
}

/* Turn on reverse video if possible. */
void
terminal_begin_inverse ()
{
  if (terminal_begin_inverse_hook)
    (*terminal_begin_inverse_hook) ();
  else
    {
      send_to_terminal (term_invbeg);
    }
}

/* Turn off reverse video if possible. */
void
terminal_end_inverse ()
{
  if (terminal_end_inverse_hook)
    (*terminal_end_inverse_hook) ();
  else
    {
      send_to_terminal (term_invend);
    }
}

/* Ring the terminal bell.  The bell is run visibly if it both has one and
   terminal_use_visible_bell_p is non-zero. */
void
terminal_ring_bell ()
{
  if (terminal_ring_bell_hook)
    (*terminal_ring_bell_hook) ();
  else
    {
      if (terminal_has_visible_bell_p && terminal_use_visible_bell_p)
        send_to_terminal (visible_bell);
      else
        send_to_terminal (audible_bell);
    }
}

/* At the line START, delete COUNT lines from the terminal display. */
static void
terminal_delete_lines (start, count)
     int start, count;
{
  int lines;

  /* Normalize arguments. */
  if (start < 0)
    start = 0;

  lines = screenheight - start;
  terminal_goto_xy (0, start);
  if (term_DL)
    tputs (tgoto (term_DL, 0, count), lines, output_character_function);
  else
    {
      while (count--)
        tputs (term_dl, lines, output_character_function);
    }

  fflush (stdout);
}

/* At the line START, insert COUNT lines in the terminal display. */
static void
terminal_insert_lines (start, count)
     int start, count;
{
  int lines;

  /* Normalize arguments. */
  if (start < 0)
    start = 0;

  lines = screenheight - start;
  terminal_goto_xy (0, start);

  if (term_AL)
    tputs (tgoto (term_AL, 0, count), lines, output_character_function);
  else
    {
      while (count--)
        tputs (term_al, lines, output_character_function);
    }

  fflush (stdout);
}

/* Scroll an area of the terminal, starting with the region from START
   to END, AMOUNT lines.  If AMOUNT is negative, the lines are scrolled
   towards the top of the screen, else they are scrolled towards the
   bottom of the screen. */
void
terminal_scroll_terminal (start, end, amount)
     int start, end, amount;
{
  if (!terminal_can_scroll)
    return;

  /* Any scrolling at all? */
  if (amount == 0)
    return;

  if (terminal_scroll_terminal_hook)
    (*terminal_scroll_terminal_hook) (start, end, amount);
  else
    {
      /* If we are scrolling down, delete AMOUNT lines at END.  Then insert
         AMOUNT lines at START. */
      if (amount > 0)
        {
          terminal_delete_lines (end, amount);
          terminal_insert_lines (start, amount);
        }

      /* If we are scrolling up, delete AMOUNT lines before START.  This
         actually does the upwards scroll.  Then, insert AMOUNT lines
         after the already scrolled region (i.e., END - AMOUNT). */
      if (amount < 0)
        {
          int abs_amount = -amount;
          terminal_delete_lines (start - abs_amount, abs_amount);
          terminal_insert_lines (end - abs_amount, abs_amount);
        }
    }
}

/* Re-initialize the terminal considering that the TERM/TERMCAP variable
   has changed. */
void
terminal_new_terminal (terminal_name)
     char *terminal_name;
{
  if (terminal_new_terminal_hook)
    (*terminal_new_terminal_hook) (terminal_name);
  else
    {
      terminal_initialize_terminal (terminal_name);
    }
}

/* Set the global variables SCREENWIDTH and SCREENHEIGHT. */
void
terminal_get_screen_size ()
{
  if (terminal_get_screen_size_hook)
    (*terminal_get_screen_size_hook) ();
  else
    {
      screenwidth = screenheight = 0;

#if defined (TIOCGWINSZ)
      {
        struct winsize window_size;

        if (ioctl (fileno (stdout), TIOCGWINSZ, &window_size) == 0)
          {
            screenwidth = (int) window_size.ws_col;
            screenheight = (int) window_size.ws_row;
          }
      }
#endif                          /* TIOCGWINSZ */

      /* Environment variable COLUMNS overrides setting of "co". */
      if (screenwidth <= 0)
        {
          char *sw = getenv ("COLUMNS");

          if (sw)
            screenwidth = atoi (sw);

          if (screenwidth <= 0)
            screenwidth = tgetnum ("co");
        }

      /* Environment variable LINES overrides setting of "li". */
      if (screenheight <= 0)
        {
          char *sh = getenv ("LINES");

          if (sh)
            screenheight = atoi (sh);

          if (screenheight <= 0)
            screenheight = tgetnum ("li");
        }

      /* If all else fails, default to 80x24 terminal. */
      if (screenwidth <= 0)
        screenwidth = 80;

      if (screenheight <= 0)
        screenheight = 24;
    }
}

/* Initialize the terminal which is known as TERMINAL_NAME.  If this
   terminal doesn't have cursor addressability, `terminal_is_dumb_p'
   becomes nonzero.  The variables SCREENHEIGHT and SCREENWIDTH are set
   to the dimensions that this terminal actually has.  The variable
   TERMINAL_HAS_META_P becomes nonzero if this terminal supports a Meta
   key.  Finally, the terminal screen is cleared. */
void
terminal_initialize_terminal (terminal_name)
     char *terminal_name;
{
  char *term, *buffer;

  terminal_is_dumb_p = 0;

  if (terminal_initialize_terminal_hook)
    {
      (*terminal_initialize_terminal_hook) (terminal_name);
      return;
    }

  term = terminal_name ? terminal_name : getenv ("TERM");

  if (!term_string_buffer)
    term_string_buffer = (char *)xmalloc (2048);

  if (!term_buffer)
    term_buffer = (char *)xmalloc (2048);

  buffer = term_string_buffer;

  term_clrpag = term_cr = term_clreol = (char *)NULL;

  if (!term)
    term = "dumb";

  if (tgetent (term_buffer, term) <= 0)
    {
      terminal_is_dumb_p = 1;
      screenwidth = 80;
      screenheight = 24;
      term_cr = "\r";
      term_up = term_dn = audible_bell = visible_bell = (char *)NULL;
      term_ku = term_kd = term_kl = term_kr = (char *)NULL;
      term_kP = term_kN = (char *)NULL;
      return;
    }

  BC = tgetstr ("pc", &buffer);
  PC = BC ? *BC : 0;

#if defined (TIOCGETP)
  {
    struct sgttyb sg;

    if (ioctl (fileno (stdout), TIOCGETP, &sg) != -1)
      ospeed = sg.sg_ospeed;
    else
      ospeed = B9600;
  }
#else
  ospeed = B9600;
#endif                          /* !TIOCGETP */

  term_cr = tgetstr ("cr", &buffer);
  term_clreol = tgetstr ("ce", &buffer);
  term_clrpag = tgetstr ("cl", &buffer);
  term_goto = tgetstr ("cm", &buffer);

  /* Find out about this terminals scrolling capability. */
  term_AL = tgetstr ("AL", &buffer);
  term_DL = tgetstr ("DL", &buffer);
  term_al = tgetstr ("al", &buffer);
  term_dl = tgetstr ("dl", &buffer);

  terminal_can_scroll = ((term_AL || term_al) && (term_DL || term_dl));

  term_invbeg = tgetstr ("mr", &buffer);
  if (term_invbeg)
    term_invend = tgetstr ("me", &buffer);
  else
    term_invend = (char *)NULL;

  if (!term_cr)
    term_cr =  "\r";

  terminal_get_screen_size ();

  term_up = tgetstr ("up", &buffer);
  term_dn = tgetstr ("dn", &buffer);
  visible_bell = tgetstr ("vb", &buffer);
  terminal_has_visible_bell_p = (visible_bell != (char *)NULL);
  audible_bell = tgetstr ("bl", &buffer);
  if (!audible_bell)
    audible_bell = "\007";
  term_begin_use = tgetstr ("ti", &buffer);
  term_end_use = tgetstr ("te", &buffer);

  term_keypad_on = tgetstr ("ks", &buffer);
  term_keypad_off = tgetstr ("ke", &buffer);

  /* Check to see if this terminal has a meta key. */
  terminal_has_meta_p = (tgetflag ("km") || tgetflag ("MT"));
  if (terminal_has_meta_p)
    {
      term_mm = tgetstr ("mm", &buffer);
      term_mo = tgetstr ("mo", &buffer);
    }
  else
    {
      term_mm = (char *)NULL;
      term_mo = (char *)NULL;
    }

  /* Attempt to find the arrow keys.  */
  term_ku = tgetstr ("ku", &buffer);
  term_kd = tgetstr ("kd", &buffer);
  term_kr = tgetstr ("kr", &buffer);
  term_kl = tgetstr ("kl", &buffer);

  term_kP = tgetstr ("kP", &buffer);
  term_kN = tgetstr ("kN", &buffer);

  /* If this terminal is not cursor addressable, then it is really dumb. */
  if (!term_goto)
    terminal_is_dumb_p = 1;
}

/* **************************************************************** */
/*                                                                  */
/*               How to Read Characters From the Terminal           */
/*                                                                  */
/* **************************************************************** */

#if defined (TIOCGETC)
/* A buffer containing the terminal interrupt characters upon entry
   to Info. */
struct tchars original_tchars;
#endif

#if defined (TIOCGLTC)
/* A buffer containing the local terminal mode characters upon entry
   to Info. */
struct ltchars original_ltchars;
#endif

#if defined (HAVE_TERMIOS_H)
struct termios original_termios, ttybuff;
#else
#  if defined (HAVE_TERMIO_H)
/* A buffer containing the terminal mode flags upon entry to info. */
struct termio original_termio, ttybuff;
#  else /* !HAVE_TERMIO_H */
/* Buffers containing the terminal mode flags upon entry to info. */
int original_tty_flags = 0;
int original_lmode;
struct sgttyb ttybuff;
#  endif /* !HAVE_TERMIO_H */
#endif /* !HAVE_TERMIOS_H */

/* Prepare to start using the terminal to read characters singly. */
void
terminal_prep_terminal ()
{
  int tty;

  if (terminal_prep_terminal_hook)
    {
      (*terminal_prep_terminal_hook) ();
      return;
    }

  terminal_begin_using_terminal ();

  tty = fileno (stdin);

#if defined (HAVE_TERMIOS_H)
  tcgetattr (tty, &original_termios);
  tcgetattr (tty, &ttybuff);
#else
#  if defined (HAVE_TERMIO_H)
  ioctl (tty, TCGETA, &original_termio);
  ioctl (tty, TCGETA, &ttybuff);
#  endif
#endif

#if defined (HAVE_TERMIOS_H) || defined (HAVE_TERMIO_H)
  ttybuff.c_iflag &= (~ISTRIP & ~INLCR & ~IGNCR & ~ICRNL & ~IXON);
/* These output flags are not part of POSIX, so only use them if they
   are defined.  */
#ifdef ONLCR
  ttybuff.c_oflag &= ~ONLCR ;
#endif
#ifdef OCRNL
  ttybuff.c_oflag &= ~OCRNL;
#endif
  ttybuff.c_lflag &= (~ICANON & ~ECHO);

  ttybuff.c_cc[VMIN] = 1;
  ttybuff.c_cc[VTIME] = 0;

  if (ttybuff.c_cc[VINTR] == '\177')
    ttybuff.c_cc[VINTR] = -1;

  if (ttybuff.c_cc[VQUIT] == '\177')
    ttybuff.c_cc[VQUIT] = -1;

#ifdef VLNEXT
  if (ttybuff.c_cc[VLNEXT] == '\026')
    ttybuff.c_cc[VLNEXT] = -1;
#endif /* VLNEXT */
#endif /* TERMIOS or TERMIO */

#if defined (HAVE_TERMIOS_H)
  tcsetattr (tty, TCSANOW, &ttybuff);
#else
#  if defined (HAVE_TERMIO_H)
  ioctl (tty, TCSETA, &ttybuff);
#  endif
#endif

#if !defined (HAVE_TERMIOS_H) && !defined (HAVE_TERMIO_H)
  ioctl (tty, TIOCGETP, &ttybuff);

  if (!original_tty_flags)
    original_tty_flags = ttybuff.sg_flags;

  /* Make this terminal pass 8 bits around while we are using it. */
#  if defined (PASS8)
  ttybuff.sg_flags |= PASS8;
#  endif /* PASS8 */

#  if defined (TIOCLGET) && defined (LPASS8)
  {
    int flags;
    ioctl (tty, TIOCLGET, &flags);
    original_lmode = flags;
    flags |= LPASS8;
    ioctl (tty, TIOCLSET, &flags);
  }
#  endif /* TIOCLGET && LPASS8 */

#  if defined (TIOCGETC)
  {
    struct tchars temp;

    ioctl (tty, TIOCGETC, &original_tchars);
    temp = original_tchars;

    /* C-s and C-q. */
    temp.t_startc = temp.t_stopc = -1;

    /* Often set to C-d. */
    temp.t_eofc = -1;

    /* If the a quit or interrupt character conflicts with one of our
       commands, then make it go away. */
    if (temp.t_intrc == '\177')
      temp.t_intrc = -1;

    if (temp.t_quitc == '\177')
      temp.t_quitc = -1;

    ioctl (tty, TIOCSETC, &temp);
  }
#  endif /* TIOCGETC */

#  if defined (TIOCGLTC)
  {
    struct ltchars temp;

    ioctl (tty, TIOCGLTC, &original_ltchars);
    temp = original_ltchars;

    /* Make the interrupt keys go away.  Just enough to make people happy. */
    temp.t_lnextc = -1;         /* C-v. */
    temp.t_dsuspc = -1;         /* C-y. */
    temp.t_flushc = -1;         /* C-o. */
    ioctl (tty, TIOCSLTC, &temp);
  }
#  endif /* TIOCGLTC */

  ttybuff.sg_flags &= ~ECHO;
  ttybuff.sg_flags |= CBREAK;
  ioctl (tty, TIOCSETN, &ttybuff);
#endif /* !HAVE_TERMIOS_H && !HAVE_TERMIO_H */
}

/* Restore the tty settings back to what they were before we started using
   this terminal. */
void
terminal_unprep_terminal ()
{
  int tty;

  if (terminal_unprep_terminal_hook)
    {
      (*terminal_unprep_terminal_hook) ();
      return;
    }

  tty = fileno (stdin);

#if defined (HAVE_TERMIOS_H)
  tcsetattr (tty, TCSANOW, &original_termios);
#else
#  if defined (HAVE_TERMIO_H)
  ioctl (tty, TCSETA, &original_termio);
#  else /* !HAVE_TERMIO_H */
  ioctl (tty, TIOCGETP, &ttybuff);
  ttybuff.sg_flags = original_tty_flags;
  ioctl (tty, TIOCSETN, &ttybuff);

#  if defined (TIOCGETC)
  ioctl (tty, TIOCSETC, &original_tchars);
#  endif /* TIOCGETC */

#  if defined (TIOCGLTC)
  ioctl (tty, TIOCSLTC, &original_ltchars);
#  endif /* TIOCGLTC */

#  if defined (TIOCLGET) && defined (LPASS8)
  ioctl (tty, TIOCLSET, &original_lmode);
#  endif /* TIOCLGET && LPASS8 */

#  endif /* !HAVE_TERMIO_H */
#endif /* !HAVE_TERMIOS_H */
  terminal_end_using_terminal ();
}

