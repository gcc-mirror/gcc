/* Copyright (c) 2008, 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include "upc_config.h"

#define GUM_MAX_BUF 1024
#define GUM_CONDATA_FMT "Host %s PID %d MYTHREAD %d THREADS %d"
#define GUM_GDBSDATA_FMT "Host %s port %d"

void
__upc_gum_init (int nthreads, int thread_id)
{
  const char *gum_host = GUM_HOST_DEFAULT;
  int gum_port = GUM_PORT_DEFAULT;
  const char *gdbserver = "gdbserver";
  const char *gum_port_env = getenv (GUM_PORT_ENV);
  const char *gum_gdbserverpath_env = getenv (GUM_GDBSERVERPATH_ENV);
  const char *gum_attach_delay_env = getenv (GUM_ATTACH_DELAY_ENV);
  int attach_delay = GUM_ATTACH_DELAY_DEFAULT;
  int mypid = getpid();
  char myhost[GUM_MAX_BUF];
  char hostname[GUM_MAX_BUF];
  struct hostent *hostent;
  struct sockaddr_in sockaddr;
  int gum_sock_fd;
  FILE *gum_in, *gum_out;
  char gum_reply[GUM_MAX_BUF];
  int gum_reply_len;
  char gum_mux_host[GUM_MAX_BUF];
  int gum_mux_port;
  char gum_mux_connect[GUM_MAX_BUF];
  int gdbserver_pid;
  if (gethostname (myhost, sizeof (myhost)) != 0)
    {
       perror ("gethostname");
       abort ();
    }
  if (gum_port_env)
    {
      const char *p = gum_port_env;
      size_t hostlen;
      while (*p && *p != ':')
	++p;
      if (!*p)
	{
	  fprintf (stderr,
		   "Missing separator in %s environment variable: %s\n",
		   GUM_PORT_ENV, gum_port_env);
	  exit (2);
	}
      hostlen = (p - gum_port_env);
      if (!hostlen)
	{
	  fprintf (stderr,
		   "empty host name in `%s' environment variable: `%s'\n",
		   GUM_PORT_ENV, gum_port_env);
	  exit (2);
	}
      if (hostlen > (sizeof (hostname) - 1))
	{
	  fprintf (stderr,
		   "host name in `%s' environment variable is too long: `%s'\n",
		   GUM_PORT_ENV, gum_port_env);
	  exit (2);
	}
      strncpy (hostname, gum_port_env, hostlen);
      hostname[hostlen] = '\0';
      gum_host = (const char *) hostname;
      p = p + 1;
      gum_port = atoi (p);
      if (!gum_port)
	{
	  fprintf (stderr,
		   "Invalid port number in %s environment variable: %s\n",
		   GUM_PORT_ENV, gum_port_env);
	  exit (2);
	}
    }
  if (gum_gdbserverpath_env)
    {
      struct stat statbuf;
      if (stat (gum_gdbserverpath_env, &statbuf) != 0)
	{
	  fprintf (stderr,
		   "Cannot locate gdbserver via enviroment variable %s: %s\n",
		   GUM_GDBSERVERPATH_ENV, gum_gdbserverpath_env);
	  exit (2);
	}
      gdbserver = gum_gdbserverpath_env;
    }
  if (gum_attach_delay_env)
    {
      attach_delay = atoi (gum_attach_delay_env);
    }
  hostent = gethostbyname (gum_host);
  if (!hostent)
    {
      fprintf (stderr, "%s: unknown GUM host\n", gum_host);
      exit (2);
    }
  gum_sock_fd = socket (PF_INET, SOCK_STREAM, 0);
  if (!gum_sock_fd)
    {
      perror ("Can't create GUM socket");
      abort ();
    }
  sockaddr.sin_family = PF_INET;
  sockaddr.sin_port = htons (gum_port);
  memcpy (&sockaddr.sin_addr.s_addr, hostent->h_addr,
	  sizeof (struct in_addr));
  if (connect (gum_sock_fd, (struct sockaddr *) &sockaddr, sizeof (sockaddr))
      < 0)
    {
      perror ("Can't connect to GUM host");
      abort ();
    }
  gum_in = fdopen (gum_sock_fd, "r");
  if (!gum_in)
    {
      perror ("fdopen of gum_in failed");
      abort ();
    }
  setlinebuf (gum_in);
  gum_out = fdopen (gum_sock_fd, "w");
  if (!gum_out)
    {
      perror ("fdopen of gum_out failed");
      abort ();
    }
  setlinebuf (gum_out);
  fprintf (gum_out, GUM_CONDATA_FMT, myhost, mypid, thread_id, nthreads);
  fflush (gum_out);
  if (!fgets (gum_reply, sizeof (gum_reply), gum_in))
    {
      fprintf (stderr, "Can't read GUM reply\n");
      exit (2);
    }
  fclose (gum_in);
  fclose (gum_out);
  close (gum_sock_fd);
  gum_reply_len = strlen (gum_reply);
  if (gum_reply_len && gum_reply[gum_reply_len - 1] == '\n')
    gum_reply[--gum_reply_len] = '\0';
  if (sscanf (gum_reply, GUM_GDBSDATA_FMT, gum_mux_host, &gum_mux_port) != 2)
    {
      fprintf (stderr, "%d: invalid GUM reply: %s\n", mypid, gum_reply);
      exit (2);
    }
  if (snprintf (gum_mux_connect, sizeof (gum_mux_connect), "%s:%d",
		gum_mux_host, gum_mux_port) >= (int) sizeof (gum_mux_connect))
    {
      fprintf (stderr, "%d: GUM mux connect buffer exceeds size\n", mypid);
      exit (2);
    }
  if ((gdbserver_pid = fork ()) > 0)
    {
      /* Give gdbserver a chance to connect to us.  */
      sleep (attach_delay);
    }
  else if (!gdbserver_pid)
    {
      char mypidstr[12];
      sprintf (mypidstr, "%d", mypid);
      execl (gdbserver, gdbserver, gum_mux_connect, "--attach", mypidstr, NULL);
      perror ("gdbserver exec failed");
      abort ();
    }
  else
    {
      perror ("fork of gdbserver failed");
      abort ();
    }
}
