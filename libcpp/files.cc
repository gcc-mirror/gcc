/* Part of CPP library.  File handling.
   Copyright (C) 1986-2025 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Split out of cpplib.c, Zack Weinberg, Oct 1998
   Reimplemented, Neil Booth, Jul 2003

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"
#include "mkdeps.h"
#include "obstack.h"
#include "hashtab.h"
#include "md5.h"
#include <dirent.h>

/* Variable length record files on VMS will have a stat size that includes
   record control characters that won't be included in the read size.  */
#ifdef VMS
# define FAB_C_VAR 2 /* variable length records (see Starlet fabdef.h) */
# define STAT_SIZE_RELIABLE(ST) ((ST).st_fab_rfm != FAB_C_VAR)
#else
# define STAT_SIZE_RELIABLE(ST) true
#endif

#ifdef __DJGPP__
#include <io.h>
  /* For DJGPP redirected input is opened in text mode.  */
#  define set_stdin_to_binary_mode() \
     if (! isatty (0)) setmode (0, O_BINARY)
#else
#  define set_stdin_to_binary_mode() /* Nothing */
#endif

/* This structure represents a file searched for by CPP, whether it
   exists or not.  An instance may be pointed to by more than one
   cpp_file_hash_entry; at present no reference count is kept.  */
struct _cpp_file
{
  /* Filename as given to #include or command line switch.  */
  const char *name;

  /* The full path used to find the file.  */
  const char *path;

  /* The full path of the pch file.  */
  const char *pchname;

  /* The file's path with the basename stripped.  NULL if it hasn't
     been calculated yet.  */
  const char *dir_name;

  /* Chain through all files.  */
  struct _cpp_file *next_file;

  /* The contents of NAME after calling read_file().  */
  const uchar *buffer;

  /* Pointer to the real start of BUFFER.  read_file() might increment
     BUFFER; when freeing, this this pointer must be used instead.  */
  const uchar *buffer_start;

  /* The macro, if any, preventing re-inclusion.  */
  const cpp_hashnode *cmacro;

  /* The directory in the search path where FILE was found.  Used for
     #include_next and determining whether a header is a system
     header.  */
  cpp_dir *dir;

  /* As filled in by stat(2) for the file.  */
  struct stat st;

  /* Size for #embed, perhaps smaller than st.st_size.  */
  size_t limit;

  /* Offset for #embed.  */
  off_t offset;

  /* File descriptor.  Invalid if -1, otherwise open.  */
  int fd;

  /* Zero if this file was successfully opened and stat()-ed,
     otherwise errno obtained from failure.  */
  int err_no;

  /* Number of times the file has been stacked for preprocessing.  */
  unsigned short stack_count;

  /* If opened with #import or contains #pragma once.  */
  bool once_only : 1;

  /* If read() failed before.  */
  bool dont_read : 1;

  /* If BUFFER above contains the true contents of the file.  */
  bool buffer_valid : 1;

  /* If this file is implicitly preincluded.  */
  bool implicit_preinclude : 1;

  /* Set if a header wasn't found with __has_include or __has_include_next
     and error should be emitted if it is included normally.  */
  bool deferred_error : 1;

  /* File loaded from #embed.  */
  bool embed : 1;

  /* > 0: Known C++ Module header unit, <0: known not.  ==0, unknown  */
  int header_unit : 2;
};

/* A singly-linked list for all searches for a given file name, with
   its head pointed to by a slot in FILE_HASH.  The file name is what
   appeared between the quotes in a #include directive; it can be
   determined implicitly from the hash table location or explicitly
   from FILE->name.

   FILE is a structure containing details about the file that was
   found with that search, or details of how the search failed.

   START_DIR is the starting location of the search in the include
   chain.  The current directories for "" includes are also hashed in
   the hash table and therefore unique.  Files that are looked up
   without using a search path, such as absolute filenames and file
   names from the command line share a special starting directory so
   they don't cause cache hits with normal include-chain lookups.

   If START_DIR is NULL then the entry is for a directory, not a file,
   and the directory is in DIR.  Since the starting point in a file
   lookup chain is never NULL, this means that simple pointer
   comparisons against START_DIR can be made to determine cache hits
   in file lookups.

   If a cache lookup fails because of e.g. an extra "./" in the path,
   then nothing will break.  It is just less efficient as CPP will
   have to do more work re-preprocessing the file, and/or comparing
   its contents against earlier once-only files.
*/
struct cpp_file_hash_entry
{
  struct cpp_file_hash_entry *next;
  cpp_dir *start_dir;
  location_t location;
  union
  {
    _cpp_file *file;
    cpp_dir *dir;
  } u;
};

/* Number of entries to put in a cpp_file_hash_entry pool.  */
#define FILE_HASH_POOL_SIZE 127

/* A file hash entry pool.  We allocate cpp_file_hash_entry object from
   one of these.  */
struct file_hash_entry_pool
{
  /* Number of entries used from this pool.  */
  unsigned int file_hash_entries_used;
  /* Next pool in the chain; used when freeing.  */
  struct file_hash_entry_pool *next;
  /* The memory pool.  */
  struct cpp_file_hash_entry pool[FILE_HASH_POOL_SIZE];
};

static bool open_file (_cpp_file *file);
static bool pch_open_file (cpp_reader *pfile, _cpp_file *file,
			   bool *invalid_pch);
static bool find_file_in_dir (cpp_reader *pfile, _cpp_file *file,
			      bool *invalid_pch, location_t loc);
static bool read_file_guts (cpp_reader *pfile, _cpp_file *file,
			    location_t loc, const char *input_charset);
static bool read_file (cpp_reader *pfile, _cpp_file *file,
		       location_t loc);
static const char *dir_name_of_file (_cpp_file *file);
static void open_file_failed (cpp_reader *pfile, _cpp_file *file, int,
			      location_t);
static struct cpp_file_hash_entry *search_cache (struct cpp_file_hash_entry *head,
						 const cpp_dir *start_dir,
						 bool is_embed);
static _cpp_file *make_cpp_file (cpp_dir *, const char *fname);
static void destroy_cpp_file (_cpp_file *);
static cpp_dir *make_cpp_dir (cpp_reader *, const char *dir_name, int sysp);
static void allocate_file_hash_entries (cpp_reader *pfile);
static struct cpp_file_hash_entry *new_file_hash_entry (cpp_reader *pfile);
static int report_missing_guard (void **slot, void *b);
static hashval_t file_hash_hash (const void *p);
static int file_hash_eq (const void *p, const void *q);
static char *read_filename_string (int ch, FILE *f);
static void read_name_map (cpp_dir *dir);
static char *remap_filename (cpp_reader *pfile, _cpp_file *file);
static char *append_file_to_dir (const char *fname, cpp_dir *dir);
static bool validate_pch (cpp_reader *, _cpp_file *file, const char *pchname);
static int pchf_save_compare (const void *e1, const void *e2);
static int pchf_compare (const void *d_p, const void *e_p);
static bool check_file_against_entries (cpp_reader *, _cpp_file *, bool);

/* Given a filename in FILE->PATH, with the empty string interpreted
   as <stdin>, open it.

   On success FILE contains an open file descriptor and stat
   information for the file.  On failure the file descriptor is -1 and
   the appropriate errno is also stored in FILE.  Returns TRUE iff
   successful.

   We used to open files in nonblocking mode, but that caused more
   problems than it solved.  Do take care not to acquire a controlling
   terminal by mistake (this can't happen on sane systems, but
   paranoia is a virtue).

   Use the three-argument form of open even though we aren't
   specifying O_CREAT, to defend against broken system headers.

   O_BINARY tells some runtime libraries (notably DJGPP) not to do
   newline translation; we can handle DOS line breaks just fine
   ourselves.  */
static bool
open_file (_cpp_file *file)
{
  if (file->path[0] == '\0')
    {
      file->fd = 0;
      set_stdin_to_binary_mode ();
    }
  else
    file->fd = open (file->path, O_RDONLY | O_NOCTTY | O_BINARY, 0666);

  if (file->fd != -1)
    {
      if (fstat (file->fd, &file->st) == 0)
	{
	  if (!S_ISDIR (file->st.st_mode))
	    {
	      file->err_no = 0;
	      return true;
	    }

	  /* Ignore a directory and continue the search.  The file we're
	     looking for may be elsewhere in the search path.  */
	  errno = ENOENT;
	}

      close (file->fd);
      file->fd = -1;
    }
#if defined(_WIN32) && !defined(__CYGWIN__)
  else if (errno == EACCES)
    {
      /* On most UNIX systems, open succeeds on a directory.  Above,
         we check if we have opened a directory and if so, set errno
         to ENOENT.  However, on Windows, opening a directory
         fails with EACCES.  We want to return ENOENT in that
         case too.  */
      if (stat (file->path, &file->st) == 0
          && S_ISDIR (file->st.st_mode))
        errno = ENOENT;
      else
	/* The call to stat may have reset errno.  */
	errno = EACCES;
    }
#endif
  else if (errno == ENOTDIR)
    errno = ENOENT;

  file->err_no = errno;

  return false;
}

/* Temporary PCH intercept of opening a file.  Try to find a PCH file
   based on FILE->name and FILE->dir, and test those found for
   validity using PFILE->cb.valid_pch.  Return true iff a valid file is
   found.  Set *INVALID_PCH if a PCH file is found but wasn't valid.  */

static bool
pch_open_file (cpp_reader *pfile, _cpp_file *file, bool *invalid_pch)
{
  static const char extension[] = ".gch";
  const char *path = file->path;
  size_t len, flen;
  char *pchname;
  struct stat st;
  bool valid = false;

  /* No PCH on <stdin> or if not requested.  */
  if (file->name[0] == '\0' || !pfile->cb.valid_pch)
    return false;

  /* If the file is not included as first include from either the toplevel
     file or the command-line it is not a valid use of PCH.  */
  for (_cpp_file *f = pfile->all_files; f; f = f->next_file)
    if (f->implicit_preinclude)
      continue;
    else if (pfile->main_file == f)
      break;
    else
      return false;

  flen = strlen (path);
  len = flen + sizeof (extension);
  pchname = XNEWVEC (char, len);
  memcpy (pchname, path, flen);
  memcpy (pchname + flen, extension, sizeof (extension));

  if (stat (pchname, &st) == 0)
    {
      DIR *pchdir;
      struct dirent *d;
      size_t dlen, plen = len;

      if (!S_ISDIR (st.st_mode))
	valid = validate_pch (pfile, file, pchname);
      else if ((pchdir = opendir (pchname)) != NULL)
	{
	  pchname[plen - 1] = '/';
	  while ((d = readdir (pchdir)) != NULL)
	    {
	      dlen = strlen (d->d_name) + 1;
	      if ((strcmp (d->d_name, ".") == 0)
		  || (strcmp (d->d_name, "..") == 0))
		continue;
	      if (dlen + plen > len)
		{
		  len += dlen + 64;
		  pchname = XRESIZEVEC (char, pchname, len);
		}
	      memcpy (pchname + plen, d->d_name, dlen);
	      valid = validate_pch (pfile, file, pchname);
	      if (valid)
		break;
	    }
	  closedir (pchdir);
	}
      if (!valid)
	*invalid_pch = true;
    }

  if (valid)
    file->pchname = pchname;
  else
    free (pchname);

  return valid;
}

/* Canonicalize the path to FILE.  Return the canonical form if it is
   shorter, otherwise return NULL.  This function does NOT free the
   memory pointed by FILE.  */

static char *
maybe_shorter_path (const char * file)
{
  char * file2 = lrealpath (file);
  if (file2 && strlen (file2) < strlen (file))
    {
      return file2;
    }
  else
    {
      free (file2);
      return NULL;
    }
}

/* Try to open the path FILE->name appended to FILE->dir.  This is
   where remap and PCH intercept the file lookup process.  Return true
   if the file was found, whether or not the open was successful.
   Set *INVALID_PCH to true if a PCH file is found but wasn't valid.
   Use LOC when emitting any diagnostics.  */

static bool
find_file_in_dir (cpp_reader *pfile, _cpp_file *file, bool *invalid_pch,
		  location_t loc)
{
  char *path;

  if (CPP_OPTION (pfile, remap) && (path = remap_filename (pfile, file)))
    ;
  else
    if (file->dir->construct)
      path = file->dir->construct (file->name, file->dir);
    else
      path = append_file_to_dir (file->name, file->dir);

  if (path)
    {
      hashval_t hv;
      char *copy;
      void **pp;

      /* We try to canonicalize system headers.  For DOS based file
       * system, we always try to shorten non-system headers, as DOS
       * has a tighter constraint on max path length.  */
      if ((CPP_OPTION (pfile, canonical_system_headers) && file->dir->sysp)
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
	  || !file->dir->sysp
#endif
	 )
	{
	  char * canonical_path = maybe_shorter_path (path);
	  if (canonical_path)
	    {
	      /* The canonical path was newly allocated.  Let's free the
		 non-canonical one.  */
	      free (path);
	      path = canonical_path;
	    }
	}

      hv = htab_hash_string (path);
      if (htab_find_with_hash (pfile->nonexistent_file_hash, path, hv) != NULL)
	{
	  file->err_no = ENOENT;
	  return false;
	}

      file->path = path;
      if (!file->embed && pch_open_file (pfile, file, invalid_pch))
	return true;

      if (open_file (file))
	return true;

      if (file->err_no != ENOENT)
	{
	  open_file_failed (pfile, file, 0, loc);
	  return true;
	}

      /* We copy the path name onto an obstack partly so that we don't
	 leak the memory, but mostly so that we don't fragment the
	 heap.  */
      copy = (char *) obstack_copy0 (&pfile->nonexistent_file_ob, path,
				     strlen (path));
      free (path);
      pp = htab_find_slot_with_hash (pfile->nonexistent_file_hash,
				     copy, hv, INSERT);
      *pp = copy;

      file->path = file->name;
    }
  else
    {
      file->err_no = ENOENT;
      file->path = NULL;
    }

  return false;
}

/* Return true iff the missing_header callback found the given HEADER.  */
static bool
search_path_exhausted (cpp_reader *pfile, const char *header, _cpp_file *file)
{
  missing_header_cb func = pfile->cb.missing_header;

  /* When the regular search path doesn't work, try context dependent
     headers search paths.  */
  if (func
      && file->dir == NULL)
    {
      if ((file->path = func (pfile, header, &file->dir)) != NULL)
	{
	  if (open_file (file))
	    return true;
	  free ((void *)file->path);
	}
      file->path = file->name;
    }

  return false;
}

bool
_cpp_find_failed (_cpp_file *file)
{
  return file->err_no != 0;
}

/* Given a filename FNAME search for such a file in the include path
   starting from START_DIR.  If FNAME is the empty string it is
   interpreted as STDIN if START_DIR is PFILE->no_search_path.

   If the file is not found in the file cache fall back to the O/S and
   add the result to our cache.

   If the file was not found in the filesystem, or there was an error
   opening it, then ERR_NO is nonzero and FD is -1.  If the file was
   found, then ERR_NO is zero and FD could be -1 or an open file
   descriptor.  FD can be -1 if the file was found in the cache and
   had previously been closed.  To open it again pass the return value
   to open_file().

   If KIND is _cpp_FFK_PRE_INCLUDE then it is OK for the file to be
   missing.  If present, it is OK for a precompiled header to be
   included after it.

   Use LOC as the location for any errors.  */

_cpp_file *
_cpp_find_file (cpp_reader *pfile, const char *fname, cpp_dir *start_dir,
		int angle_brackets, _cpp_find_file_kind kind, location_t loc)
{
  bool invalid_pch = false;
  bool saw_bracket_include = false;
  bool saw_quote_include = false;
  bool saw_embed_include = false;
  struct cpp_dir *found_in_cache = NULL;
  bool is_embed = kind == _cpp_FFK_EMBED || kind == _cpp_FFK_HAS_EMBED;

  /* Ensure we get no confusion between cached files and directories.  */
  if (start_dir == NULL)
    cpp_error_at (pfile, CPP_DL_ICE, loc, "NULL directory in %<find_file%>");

  void **hash_slot
    = htab_find_slot_with_hash (pfile->file_hash, fname,
				htab_hash_string (fname), INSERT);

  /* First check the cache before we resort to memory allocation.  */
  cpp_file_hash_entry *entry
    = search_cache ((struct cpp_file_hash_entry *) *hash_slot, start_dir,
		    is_embed);
  if (entry)
    {
      if (entry->u.file->deferred_error
	  && (kind == _cpp_FFK_NORMAL || kind == _cpp_FFK_EMBED))
	{
	  open_file_failed (pfile, entry->u.file, angle_brackets, loc);
	  entry->u.file->deferred_error = false;
	}
      return entry->u.file;
    }

  _cpp_file *file = make_cpp_file (start_dir, fname);
  file->implicit_preinclude
    = (kind == _cpp_FFK_PRE_INCLUDE
       || (pfile->buffer && pfile->buffer->file->implicit_preinclude));
  file->embed = is_embed;

  if (kind == _cpp_FFK_FAKE)
    file->dont_read = true;
  else
    /* Try each path in the include chain.  */
    for (;;)
      {
	if (find_file_in_dir (pfile, file, &invalid_pch, loc))
	  break;

	if (is_embed
	    && file->dir == start_dir
	    && start_dir != pfile->embed_include
	    && start_dir != &pfile->no_search_path)
	  file->dir = pfile->embed_include;
	else
	  file->dir = file->dir->next;
	if (file->dir == NULL)
	  {
	    if (!is_embed
		&& search_path_exhausted (pfile, fname, file))
	      {
		/* Although this file must not go in the cache,
		   because the file found might depend on things (like
		   the current file) that aren't represented in the
		   cache, it still has to go in the list of all files
		   so that #import works.  */
		file->next_file = pfile->all_files;
		pfile->all_files = file;
		if (*hash_slot == NULL)
		  {
		    /* If *hash_slot is NULL, the above
		       htab_find_slot_with_hash call just created the
		       slot, but we aren't going to store there anything
		       of use, so need to remove the newly created entry.
		       htab_clear_slot requires that it is non-NULL, so
		       store some non-NULL but valid pointer there,
		       htab_clear_slot will immediately overwrite it.  */
		    *hash_slot = file;
		    htab_clear_slot (pfile->file_hash, hash_slot);
		  }
		return file;
	      }

	    if (invalid_pch)
	      {
		cpp_error (pfile, CPP_DL_ERROR,
			   "one or more PCH files were found,"
			   " but they were invalid");
		if (!cpp_get_options (pfile)->warn_invalid_pch)
		  cpp_error (pfile, CPP_DL_NOTE,
			     "use %<-Winvalid-pch%> for more information");
	      }

	    if (kind == _cpp_FFK_PRE_INCLUDE)
	      {
		free ((char *) file->name);
		free (file);
		if (*hash_slot == NULL)
		  {
		    /* See comment on the above htab_clear_slot call.  */
		    *hash_slot = &hash_slot;
		    htab_clear_slot (pfile->file_hash, hash_slot);
		  }
		return NULL;
	      }

	    if (kind != _cpp_FFK_HAS_INCLUDE && kind != _cpp_FFK_HAS_EMBED)
	      open_file_failed (pfile, file, angle_brackets, loc);
	    else
	      file->deferred_error = true;
	    break;
	  }

	/* Only check the cache for the starting location (done above)
	   and the quote and bracket chain heads because there are no
	   other possible starting points for searches.  */
	if (file->dir == pfile->bracket_include)
	  saw_bracket_include = true;
	else if (file->dir == pfile->quote_include)
	  saw_quote_include = true;
	else if (file->dir == pfile->embed_include)
	  saw_embed_include = true;
	else
	  continue;

	entry
	  = search_cache ((struct cpp_file_hash_entry *) *hash_slot,
			  file->dir, is_embed);
	if (entry)
	  {
	    found_in_cache = file->dir;
	    break;
	  }
      }

  if (entry)
    {
      /* Cache for START_DIR too, sharing the _cpp_file structure.  */
      free ((char *) file->name);
      free (file);
      file = entry->u.file;
    }
  else
    {
      /* This is a new file; put it in the list.  */
      file->next_file = pfile->all_files;
      pfile->all_files = file;
    }

  /* Store this new result in the hash table.  */
  entry = new_file_hash_entry (pfile);
  entry->next = (struct cpp_file_hash_entry *) *hash_slot;
  entry->start_dir = start_dir;
  entry->location = loc;
  entry->u.file = file;
  *hash_slot = (void *) entry;

  /* If we passed the quote or bracket chain heads, cache them also.
     This speeds up processing if there are lots of -I options.  */
  if (saw_bracket_include
      && pfile->bracket_include != start_dir
      && found_in_cache != pfile->bracket_include)
    {
      entry = new_file_hash_entry (pfile);
      entry->next = (struct cpp_file_hash_entry *) *hash_slot;
      entry->start_dir = pfile->bracket_include;
      entry->location = loc;
      entry->u.file = file;
      *hash_slot = (void *) entry;
    }
  if (saw_quote_include
      && pfile->quote_include != start_dir
      && found_in_cache != pfile->quote_include)
    {
      entry = new_file_hash_entry (pfile);
      entry->next = (struct cpp_file_hash_entry *) *hash_slot;
      entry->start_dir = pfile->quote_include;
      entry->location = loc;
      entry->u.file = file;
      *hash_slot = (void *) entry;
    }
  if (saw_embed_include
      && pfile->embed_include != start_dir
      && found_in_cache != pfile->embed_include)
    {
      entry = new_file_hash_entry (pfile);
      entry->next = (struct cpp_file_hash_entry *) *hash_slot;
      entry->start_dir = pfile->embed_include;
      entry->location = loc;
      entry->u.file = file;
      *hash_slot = (void *) entry;
    }

  return file;
}

/* Read a file into FILE->buffer, returning true on success.

   If FILE->fd is something weird, like a block device, we don't want
   to read it at all.  Don't even try to figure out what something is,
   except for plain files and block devices, since there is no
   reliable portable way of doing this.

   Use LOC for any diagnostics.

   PFILE may be NULL.  In this case, no diagnostics are issued.

   FIXME: Flush file cache and try again if we run out of memory.  */
static bool
read_file_guts (cpp_reader *pfile, _cpp_file *file, location_t loc,
		const char *input_charset)
{
  ssize_t size, pad, total, count;
  uchar *buf;
  bool regular;

  if (S_ISBLK (file->st.st_mode))
    {
      if (pfile)
	cpp_error_at (pfile, CPP_DL_ERROR, loc,
		      "%s is a block device", file->path);
      return false;
    }

  regular = S_ISREG (file->st.st_mode) != 0;
  if (regular)
    {
      /* off_t might have a wider range than ssize_t - in other words,
	 the max size of a file might be bigger than the address
	 space.  We can't handle a file that large.  (Anyone with
	 a single source file bigger than 2GB needs to rethink
	 their coding style.)  Some systems (e.g. AIX 4.1) define
	 SSIZE_MAX to be much smaller than the actual range of the
	 type.  Use INTTYPE_MAXIMUM unconditionally to ensure this
	 does not bite us.  */
      if (file->st.st_size > INTTYPE_MAXIMUM (ssize_t))
	{
	  if (pfile)
	    cpp_error_at (pfile, CPP_DL_ERROR, loc,
			  "%s is too large", file->path);
	  return false;
	}

      size = file->st.st_size;
    }
  else
    /* 8 kilobytes is a sensible starting size.  It ought to be bigger
       than the kernel pipe buffer, and it's definitely bigger than
       the majority of C source files.  */
    size = 8 * 1024;

  pad = CPP_BUFFER_PADDING;
  /* The '+ PAD' here is space for the final '\n' and PAD-1 bytes of padding,
     allowing search_line_fast to use (possibly misaligned) vector loads.  */
  buf = XNEWVEC (uchar, size + pad);
  total = 0;
  while ((count = read (file->fd, buf + total, size - total)) > 0)
    {
      total += count;

      if (total == size)
	{
	  if (regular)
	    break;
	  size *= 2;
	  buf = XRESIZEVEC (uchar, buf, size + pad);
	}
    }

  if (count < 0)
    {
      if (pfile)
	cpp_errno_filename (pfile, CPP_DL_ERROR, file->path, loc);
      free (buf);
      return false;
    }

  if (pfile && regular && total != size && STAT_SIZE_RELIABLE (file->st))
    cpp_error_at (pfile, CPP_DL_WARNING, loc,
		  "%s is shorter than expected", file->path);

  file->buffer = _cpp_convert_input (pfile,
				     input_charset,
				     buf, size + pad, total,
				     &file->buffer_start,
				     &file->st.st_size);
  file->buffer_valid = file->buffer;
  return file->buffer_valid;
}

/* Convenience wrapper around read_file_guts that opens the file if
   necessary and closes the file descriptor after reading.  FILE must
   have been passed through find_file() at some stage.  Use LOC for
   any diagnostics.  Unlike read_file_guts(), PFILE may not be NULL.  */
static bool
read_file (cpp_reader *pfile, _cpp_file *file, location_t loc)
{
  /* If we already have its contents in memory, succeed immediately.  */
  if (file->buffer_valid)
    return true;

  /* If an earlier read failed for some reason don't try again.  */
  if (file->dont_read || file->err_no)
    return false;

  if (file->fd == -1 && !open_file (file))
    {
      open_file_failed (pfile, file, 0, loc);
      return false;
    }

  file->dont_read = !read_file_guts (pfile, file, loc,
				     CPP_OPTION (pfile, input_charset));
  close (file->fd);
  file->fd = -1;

  return !file->dont_read;
}

/* Returns TRUE if FILE is already known to be idempotent, and should
   therefore not be read again.  */
static bool
is_known_idempotent_file (cpp_reader *pfile, _cpp_file *file, bool import)
{
  /* Skip once-only files.  */
  if (file->once_only)
    return true;

  /* We must mark the file once-only if #import now, before header
     guard checks.  Otherwise, undefining the header guard might
     cause the file to be re-stacked.  */
  if (import)
    {
      _cpp_mark_file_once_only (pfile, file);

      /* Don't stack files that have been stacked before.  */
      if (file->stack_count)
	return true;
    }

  /* Skip if the file had a header guard and the macro is defined.
     PCH relies on this appearing before the PCH handler below.  */
  if (file->cmacro && cpp_macro_p (file->cmacro))
    return true;

  /* Handle PCH files immediately; don't stack them.  */
  if (file->pchname)
    {
      pfile->cb.read_pch (pfile, file->pchname, file->fd, file->path);
      file->fd = -1;
      free ((void *) file->pchname);
      file->pchname = NULL;
      return true;
    }

  return false;
}

/* Return TRUE if file has unique contents, so we should read process
   it.  The file's contents must already have been read.  */

static bool
has_unique_contents (cpp_reader *pfile, _cpp_file *file, bool import,
		     location_t loc)
{
  /* Check the file against the PCH file.  This is done before
     checking against files we've already seen, since it may save on
     I/O.  */
  if (check_file_against_entries (pfile, file, import))
    {
      /* If this isn't a #import, but yet we can't include the file,
	 that means that it was #import-ed in the PCH file,
	 so we can never include it again.  */
      if (! import)
	_cpp_mark_file_once_only (pfile, file);
      return false;
    }

  /* Now we've read the file's contents, we can stack it if there
     are no once-only files.  */
  if (!pfile->seen_once_only)
    return true;

  /* We may have read the file under a different name.  Look
     for likely candidates and compare file contents to be sure.  */
  for (_cpp_file *f = pfile->all_files; f; f = f->next_file)
    {
      if (f == file)
	continue; /* It'sa me!  */

      if (f->embed)
	continue;

      if ((import || f->once_only)
	  && f->err_no == 0
	  && f->st.st_mtime == file->st.st_mtime
	  && f->st.st_size == file->st.st_size)
	{
	  _cpp_file *ref_file;

	  if (f->buffer && !f->buffer_valid)
	    {
	      /* We already have a buffer but it is not valid, because
		 the file is still stacked.  Make a new one.  */
	      ref_file = make_cpp_file (f->dir, f->name);
	      ref_file->path = f->path;
	    }
	  else
	    /* The file is not stacked anymore.  We can reuse it.  */
	    ref_file = f;

	  bool same_file_p = (read_file (pfile, ref_file, loc)
			      /* Size might have changed in read_file().  */
			      && ref_file->st.st_size == file->st.st_size
			      && !memcmp (ref_file->buffer, file->buffer,
					  file->st.st_size));

	  if (f->buffer && !f->buffer_valid)
	    {
	      ref_file->path = 0;
	      destroy_cpp_file (ref_file);
	    }

	  if (same_file_p)
	    /* Already seen under a different name.  */
	    return false;
	}
    }

  return true;
}

/* Place the file referenced by FILE into a new buffer on the buffer
   stack if possible.  Returns true if a buffer is stacked.  Use LOC
   for any diagnostics.  */

bool
_cpp_stack_file (cpp_reader *pfile, _cpp_file *file, include_type type,
		 location_t loc)
{
  if (is_known_idempotent_file (pfile, file, type == IT_IMPORT))
    return false;

  int sysp = 0;
  char *buf = nullptr;

  /* Check C++ module include translation.  */
  if (!file->header_unit && type < IT_HEADER_HWM
      /* Do not include translate include-next.  */
      && type != IT_INCLUDE_NEXT
      && pfile->cb.translate_include)
    buf = (pfile->cb.translate_include
	   (pfile, pfile->line_table, loc, file->path));

  if (buf)
    {
      /* We don't increment the line number at the end of a buffer,
	 because we don't usually need that location (we're popping an
	 include file).  However in this case we do want to do the
	 increment.  So push a writable buffer of two newlines to acheive
	 that.  (We also need an extra newline, so this looks like a regular
	 file, which we do that to to make sure we don't fall off the end in the
	 middle of a line.  */
      if (type != IT_CMDLINE)
	{
	  static uchar newlines[] = "\n\n\n";
	  cpp_push_buffer (pfile, newlines, 2, true);
	}

      size_t len = strlen (buf);
      buf[len] = '\n'; /* See above  */
      cpp_buffer *buffer
	= cpp_push_buffer (pfile, reinterpret_cast<unsigned char *> (buf),
			   len, true);
      buffer->to_free = buffer->buf;
      if (type == IT_CMDLINE)
	/* Tell _cpp_pop_buffer to change files.  */
	buffer->file = file;

      file->header_unit = +1;
      _cpp_mark_file_once_only (pfile, file);
    }
  else
    {
      /* Not a header unit, and we know it.  */
      file->header_unit = -1;

      if (!read_file (pfile, file, loc))
	return false;

      if (!has_unique_contents (pfile, file, type == IT_IMPORT, loc))
	return false;

      if (pfile->buffer && file->dir)
	sysp = MAX (pfile->buffer->sysp, file->dir->sysp);

      /* Add the file to the dependencies on its first inclusion.  */
      if (CPP_OPTION (pfile, deps.style) > (sysp != 0)
	  && !file->stack_count
	  && file->path[0]
	  && !(pfile->main_file == file
	       && CPP_OPTION (pfile, deps.ignore_main_file)))
	deps_add_dep (pfile->deps, file->path);

      /* Clear buffer_valid since _cpp_clean_line messes it up.  */
      file->buffer_valid = false;
      file->stack_count++;

      /* Stack the buffer.  */
      cpp_buffer *buffer
	= cpp_push_buffer (pfile, file->buffer, file->st.st_size,
			   CPP_OPTION (pfile, preprocessed)
			   && !CPP_OPTION (pfile, directives_only));
      buffer->file = file;
      buffer->sysp = sysp;
      buffer->to_free = file->buffer_start;

      /* Initialize controlling macro state.  */
      pfile->mi_valid = true;
      pfile->mi_cmacro = 0;
    }

  /* In the case of a normal #include, we're now at the start of the
     line *following* the #include.  A separate location_t for this
     location makes no sense, until we do the LC_LEAVE.

     This does not apply if we found a PCH file, we're not a regular
     include, or we ran out of locations.  */
  bool decrement = (file->pchname == NULL
		    && type < IT_DIRECTIVE_HWM
		    && (pfile->line_table->highest_location
			!= LINE_MAP_MAX_LOCATION - 1));

  if (decrement && LINEMAPS_ORDINARY_USED (pfile->line_table))
    {
      const line_map_ordinary *map
	= LINEMAPS_LAST_ORDINARY_MAP (pfile->line_table);
      if (map && map->start_location == pfile->line_table->highest_location)
	decrement = false;
    }

  if (decrement)
    pfile->line_table->highest_location--;

  /* Normally a header unit becomes an __import directive in the current file,
     but with -include we need something to LC_LEAVE to trigger the file_change
     hook and continue to the next -include or the main source file.  */
  if (file->header_unit <= 0 || type == IT_CMDLINE)
    /* Add line map and do callbacks.  */
    _cpp_do_file_change (pfile, LC_ENTER, file->path,
		       /* With preamble injection, start on line zero,
			  so the preamble doesn't appear to have been
			  included from line 1.  Likewise when
			  starting preprocessed, we expect an initial
			  locating line.  */
			 type == IT_PRE_MAIN ? 0 : 1, sysp);
  else if (decrement)
    {
      /* Adjust the line back one so we appear on the #include line itself.  */
      const line_map_ordinary *map
	= LINEMAPS_LAST_ORDINARY_MAP (pfile->line_table);
      linenum_type line = SOURCE_LINE (map, pfile->line_table->highest_line);
      linemap_line_start (pfile->line_table, line - 1, 0);
    }

  return true;
}

/* Mark FILE to be included once only.  */
void
_cpp_mark_file_once_only (cpp_reader *pfile, _cpp_file *file)
{
  pfile->seen_once_only = true;
  file->once_only = true;
}

/* Return the directory from which searching for FNAME should start,
   considering the directive TYPE and ANGLE_BRACKETS.  If there is
   nothing left in the path, returns NULL.  */
struct cpp_dir *
search_path_head (cpp_reader *pfile, const char *fname, int angle_brackets,
		  enum include_type type, bool suppress_diagnostic)
{
  cpp_dir *dir;
  _cpp_file *file;

  if (IS_ABSOLUTE_PATH (fname))
    return &pfile->no_search_path;

  /* pfile->buffer is NULL when processing an -include command-line flag.  */
  file = pfile->buffer == NULL ? pfile->main_file : pfile->buffer->file;

  /* For #include_next, skip in the search path past the dir in which
     the current file was found, but if it was found via an absolute
     path use the normal search logic.  */
  if (type == IT_INCLUDE_NEXT && file->dir
      && file->dir != &pfile->no_search_path)
    dir = file->dir->next;
  else if (angle_brackets)
    dir = type == IT_EMBED ? pfile->embed_include : pfile->bracket_include;
  else if (type == IT_CMDLINE)
    /* -include and -imacros use the #include "" chain with the
       preprocessor's cwd prepended.  */
    return make_cpp_dir (pfile, "./", false);
  else if (pfile->quote_ignores_source_dir && type != IT_EMBED)
    dir = pfile->quote_include;
  else
    return make_cpp_dir (pfile, dir_name_of_file (file),
			 pfile->buffer ? pfile->buffer->sysp : 0);

  if (dir == NULL && !suppress_diagnostic)
    cpp_error (pfile, CPP_DL_ERROR,
	       "no include path in which to search for %s", fname);

  return dir;
}

/* Strip the basename from the file's path.  It ends with a slash if
   of nonzero length.  Note that this procedure also works for
   <stdin>, which is represented by the empty string.  */
static const char *
dir_name_of_file (_cpp_file *file)
{
  if (!file->dir_name)
    {
      size_t len = lbasename (file->path) - file->path;
      char *dir_name = XNEWVEC (char, len + 1);

      memcpy (dir_name, file->path, len);
      dir_name[len] = '\0';
      file->dir_name = dir_name;
    }

  return file->dir_name;
}

/* Handles #include-family directives (distinguished by TYPE),
   including HEADER, and the command line -imacros and -include.
   Returns true if a buffer was stacked.  */
bool
_cpp_stack_include (cpp_reader *pfile, const char *fname, int angle_brackets,
		    enum include_type type, location_t loc)
{
  /* For -include command-line flags we have type == IT_CMDLINE.
     When the first -include file is processed we have the case, where
     pfile->cur_token == pfile->cur_run->base, we are directly called up
     by the front end.  However in the case of the second -include file,
     we are called from _cpp_lex_token -> _cpp_get_fresh_line ->
     cpp_push_include, with pfile->cur_token != pfile->cur_run->base,
     and pfile->cur_token[-1].src_loc not (yet) initialized.
     However, when the include file cannot be found, we need src_loc to
     be initialized to some safe value: 0 means UNKNOWN_LOCATION.  */
  if (type == IT_CMDLINE && pfile->cur_token != pfile->cur_run->base)
    pfile->cur_token[-1].src_loc = 0;

  cpp_dir *dir = search_path_head (pfile, fname, angle_brackets, type);
  if (!dir)
    return false;

  _cpp_file *file = _cpp_find_file (pfile, fname, dir, angle_brackets,
				    type == IT_DEFAULT ? _cpp_FFK_PRE_INCLUDE
				    : _cpp_FFK_NORMAL, loc);
  if (type == IT_DEFAULT && file == NULL)
    return false;

  return _cpp_stack_file (pfile, file, type, loc);
}

/* NAME is a header file name, find the _cpp_file, if any.  */

static _cpp_file *
test_header_unit (cpp_reader *pfile, const char *name, bool angle,
		  location_t loc)
{
  if (cpp_dir *dir = search_path_head (pfile, name, angle, IT_INCLUDE))
    return _cpp_find_file (pfile, name, dir, angle, _cpp_FFK_NORMAL, loc);

  return nullptr;
}

/* NAME is a header file name, find the path we'll use to open it and infer that
   it is a header-unit.  */

const char *
_cpp_find_header_unit (cpp_reader *pfile, const char *name, bool angle,
		       location_t loc)
{
  if (_cpp_file *file = test_header_unit (pfile, name, angle, loc))
    {
      if (file->fd > 0)
	{
	  /* Don't leave it open.  */
	  close (file->fd);
	  file->fd = 0;
	}

      file->header_unit = +1;
      _cpp_mark_file_once_only (pfile, file);

      return file->path;
    }

  return nullptr;
}

/* NAME is a header file name, find the path we'll use to open it.  But do not
   infer it is a header unit.  */

const char *
cpp_probe_header_unit (cpp_reader *pfile, const char *name, bool angle,
		       location_t loc)
{
  if (_cpp_file *file = test_header_unit (pfile, name, angle, loc))
    return file->path;

  return nullptr;
}

/* Helper function for _cpp_stack_embed.  Finish #embed/__has_embed processing
   after a file is found and data loaded into buffer.  */

static int
finish_embed (cpp_reader *pfile, _cpp_file *file,
	      struct cpp_embed_params *params)
{
  const uchar *buffer = file->buffer;
  size_t limit = file->limit;
  if (params->offset - file->offset > limit)
    limit = 0;
  else
    {
      buffer += params->offset - file->offset;
      limit -= params->offset - file->offset;
    }
  if (params->limit < limit)
    limit = params->limit;

  size_t embed_tokens = 0;
  if (CPP_OPTION (pfile, lang) != CLK_ASM
      && limit >= 64)
    embed_tokens = ((limit - 2) / INT_MAX) + (((limit - 2) % INT_MAX) != 0);

  size_t max = INTTYPE_MAXIMUM (size_t) / sizeof (cpp_token);
  if ((embed_tokens ? (embed_tokens > (max - 3) / 2) : (limit > max / 2))
      || (limit
	  ? (params->prefix.count > max
	     || params->suffix.count > max
	     || ((embed_tokens ? embed_tokens * 2 + 3 : limit * 2 - 1)
		 + params->prefix.count
		 + params->suffix.count > max))
	  : params->if_empty.count > max))
    {
      cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
		    "%s is too large", file->path);
      return 0;
    }

  size_t len = 0;
  for (size_t i = 0; i < limit; ++i)
    {
      if (buffer[i] < 10)
	len += 2;
      else if (buffer[i] < 100)
	len += 3;
#if UCHAR_MAX == 255
      else
	len += 4;
#else
      else if (buffer[i] < 1000)
	len += 4;
      else
	{
	  char buf[64];
	  len += sprintf (buf, "%d", buffer[i]) + 1;
	}
#endif
      if (len > INTTYPE_MAXIMUM (ssize_t))
	{
	  cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
			"%s is too large", file->path);
	  return 0;
	}
      if (embed_tokens && i == 0)
	i = limit - 2;
    }
  uchar *s = len ? _cpp_unaligned_alloc (pfile, len) : NULL;
  _cpp_buff *tok_buff = NULL;
  cpp_token *tok = &pfile->directive_result, *toks = tok;
  size_t count = 0;
  if (limit)
    count = (params->prefix.count
	     + (embed_tokens ? embed_tokens * 2 + 3 : limit * 2 - 1)
	     + params->suffix.count) - 1;
  else if (params->if_empty.count)
    count = params->if_empty.count - 1;
  if (count)
    {
      tok_buff = _cpp_get_buff (pfile, count * sizeof (cpp_token));
      toks = (cpp_token *) tok_buff->base;
    }
  cpp_embed_params_tokens *prefix
    = limit ? &params->prefix : &params->if_empty;
  if (prefix->count)
    {
      *tok = *prefix->base_run.base;
      tok = toks;
      tokenrun *cur_run = &prefix->base_run;
      while (cur_run)
	{
	  size_t cnt = (cur_run->next ? cur_run->limit
			: prefix->cur_token) - cur_run->base;
	  cpp_token *t = cur_run->base;
	  if (cur_run == &prefix->base_run)
	    {
	      t++;
	      cnt--;
	    }
	  memcpy (tok, t, cnt * sizeof (cpp_token));
	  tok += cnt;
	  cur_run = cur_run->next;
	}
    }
  for (size_t i = 0; i < limit; ++i)
    {
      tok->src_loc = params->loc;
      tok->type = CPP_NUMBER;
      tok->flags = NO_EXPAND;
      if (i == 0)
	tok->flags |= PREV_WHITE;
      tok->val.str.text = s;
      tok->val.str.len = sprintf ((char *) s, "%d", buffer[i]);
      s += tok->val.str.len + 1;
      if (tok == &pfile->directive_result)
	tok = toks;
      else
	tok++;
      if (i < limit - 1)
	{
	  tok->src_loc = params->loc;
	  tok->type = CPP_COMMA;
	  tok->flags = NO_EXPAND;
	  tok++;
	}
      if (i == 0 && embed_tokens)
	{
	  ++i;
	  for (size_t j = 0; j < embed_tokens; ++j)
	    {
	      tok->src_loc = params->loc;
	      tok->type = CPP_EMBED;
	      tok->flags = NO_EXPAND;
	      tok->val.str.text = &buffer[i];
	      tok->val.str.len
		= limit - 1 - i > INT_MAX ? INT_MAX : limit - 1 - i;
	      i += tok->val.str.len;
	      if (tok->val.str.len < 32 && j)
		{
		  /* Avoid CPP_EMBED with a fewer than 32 bytes, shrink the
		     previous CPP_EMBED by 64 and grow this one by 64.  */
		  tok[-2].val.str.len -= 64;
		  tok->val.str.text -= 64;
		  tok->val.str.len += 64;
		}
	      tok++;
	      tok->src_loc = params->loc;
	      tok->type = CPP_COMMA;
	      tok->flags = NO_EXPAND;
	      tok++;
	    }
	  --i;
	}
    }
  if (limit && params->suffix.count)
    {
      tokenrun *cur_run = &params->suffix.base_run;
      cpp_token *orig_tok = tok;
      while (cur_run)
	{
	  size_t cnt = (cur_run->next ? cur_run->limit
			: params->suffix.cur_token) - cur_run->base;
	  cpp_token *t = cur_run->base;
	  memcpy (tok, t, cnt * sizeof (cpp_token));
	  tok += cnt;
	  cur_run = cur_run->next;
	}
      orig_tok->flags |= PREV_WHITE;
    }
  pfile->directive_result.flags |= PREV_WHITE;
  if (count)
    {
      _cpp_push_token_context (pfile, NULL, toks, count);
      pfile->context->buff = tok_buff;
    }
  return limit ? 1 : 2;
}

/* Helper function for initialization of base64_dec table.
   Can't rely on ASCII compatibility, so check each letter
   separately.  */

constexpr signed char
base64_dec_fn (unsigned char c)
{
  return (c == 'A' ? 0 : c == 'B' ? 1 : c == 'C' ? 2 : c == 'D' ? 3
	  : c == 'E' ? 4 : c == 'F' ? 5 : c == 'G' ? 6 : c == 'H' ? 7
	  : c == 'I' ? 8 : c == 'J' ? 9 : c == 'K' ? 10 : c == 'L' ? 11
	  : c == 'M' ? 12 : c == 'N' ? 13 : c == 'O' ? 14 : c == 'P' ? 15
	  : c == 'Q' ? 16 : c == 'R' ? 17 : c == 'S' ? 18 : c == 'T' ? 19
	  : c == 'U' ? 20 : c == 'V' ? 21 : c == 'W' ? 22 : c == 'X' ? 23
	  : c == 'Y' ? 24 : c == 'Z' ? 25
	  : c == 'a' ? 26 : c == 'b' ? 27 : c == 'c' ? 28 : c == 'd' ? 29
	  : c == 'e' ? 30 : c == 'f' ? 31 : c == 'g' ? 32 : c == 'h' ? 33
	  : c == 'i' ? 34 : c == 'j' ? 35 : c == 'k' ? 36 : c == 'l' ? 37
	  : c == 'm' ? 38 : c == 'n' ? 39 : c == 'o' ? 40 : c == 'p' ? 41
	  : c == 'q' ? 42 : c == 'r' ? 43 : c == 's' ? 44 : c == 't' ? 45
	  : c == 'u' ? 46 : c == 'v' ? 47 : c == 'w' ? 48 : c == 'x' ? 49
	  : c == 'y' ? 50 : c == 'z' ? 51
	  : c == '0' ? 52 : c == '1' ? 53 : c == '2' ? 54 : c == '3' ? 55
	  : c == '4' ? 56 : c == '5' ? 57 : c == '6' ? 58 : c == '7' ? 59
	  : c == '8' ? 60 : c == '9' ? 61 : c == '+' ? 62 : c == '/' ? 63
	  : -1);
}

/* base64 decoding table.  */

static constexpr signed char base64_dec[] = {
#define B64D0(x) base64_dec_fn (x)
#define B64D1(x) B64D0 (x), B64D0 (x + 1), B64D0 (x + 2), B64D0 (x + 3)
#define B64D2(x) B64D1 (x), B64D1 (x + 4), B64D1 (x + 8), B64D1 (x + 12)
#define B64D3(x) B64D2 (x), B64D2 (x + 16), B64D2 (x + 32), B64D2 (x + 48)
  B64D3 (0), B64D3 (64), B64D3 (128), B64D3 (192)
};

/* Helper function for _cpp_stack_embed.  Handle #embed/__has_embed with
   gnu::base64 parameter.  */

static int
finish_base64_embed (cpp_reader *pfile, const char *fname, bool angle,
		     struct cpp_embed_params *params)
{
  size_t len, end, i, j, base64_len = 0, cnt;
  uchar *buf = NULL, *q, pbuf[4], qbuf[3];
  const uchar *base64_str;
  if (angle || strcmp (fname, "."))
    {
      if (!params->has_embed)
	cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
		      "%<gnu::base64%> parameter can be only used with "
		      "%<\".\"%>");
      return 0;
    }
  tokenrun *cur_run = &params->base64.base_run;
  cpp_token *tend, *tok;
  while (cur_run)
    {
      tend = cur_run->next ? cur_run->limit : params->base64.cur_token;
      for (tok = cur_run->base; tok < tend; ++tok)
	{
	  if (tok->val.str.len < 2
	      || tok->val.str.text[0] != '"'
	      || tok->val.str.text[tok->val.str.len - 1] != '"')
	    {
	    fail:
	      cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
			    "%<gnu::base64%> argument not valid base64 "
			    "encoded string");
	      free (buf);
	      return 0;
	    }
	  if (tok->val.str.len - 2 > (~(size_t) 0) - base64_len)
	    goto fail;
	  base64_len += tok->val.str.len - 2;
	}
      cur_run = cur_run->next;
    }
  if ((base64_len & 3) != 0)
    goto fail;
  len = base64_len / 4 * 3;
  end = len;

  if (params->has_embed)
    q = qbuf;
  else
    {
      buf = XNEWVEC (uchar, len ? len : 1);
      q = buf;
    }
  cur_run = &params->base64.base_run;
  tend = cur_run->next ? cur_run->limit : params->base64.cur_token;
  tok = cur_run->base;
  base64_str = tok->val.str.text + 1;
  cnt = tok->val.str.len - 2;
  ++tok;
  for (i = 0; i < end; i += 3)
    {
      for (j = 0; j < 4; ++j)
	{
	  while (cnt == 0)
	    {
	      if (tok == tend)
		{
		  cur_run = cur_run->next;
		  tend = (cur_run->next ? cur_run->limit
			  : params->base64.cur_token);
		  tok = cur_run->base;
		}
	      base64_str = tok->val.str.text + 1;
	      cnt = tok->val.str.len - 2;
	      ++tok;
	    }
	  pbuf[j] = *base64_str;
	  base64_str++;
	  --cnt;
	}
      if (pbuf[3] == '=' && i + 3 >= end)
	{
	  end = len - 3;
	  --len;
	  if (pbuf[2] == '=')
	    --len;
	  break;
	}
      int a = base64_dec[pbuf[0]];
      int b = base64_dec[pbuf[1]];
      int c = base64_dec[pbuf[2]];
      int d = base64_dec[pbuf[3]];
      if (a == -1 || b == -1 || c == -1 || d == -1)
	goto fail;
      q[0] = (a << 2) | (b >> 4);
      q[1] = (b << 4) | (c >> 2);
      q[2] = (c << 6) | d;
      if (!params->has_embed)
	q += 3;
    }
  if (len != end)
    {
      int a = base64_dec[pbuf[0]];
      int b = base64_dec[pbuf[1]];
      if (a == -1 || b == -1)
	goto fail;
      q[0] = (a << 2) | (b >> 4);
      if (len - end == 2)
	{
	  int c = base64_dec[pbuf[2]];
	  if (c == -1)
	    goto fail;
	  q[1] = (b << 4) | (c >> 2);
	  if ((c & 3) != 0)
	    goto fail;
	}
      else if ((b & 15) != 0)
	goto fail;
    }
  if (params->has_embed)
    return len ? 1 : 2;
  _cpp_file *file = make_cpp_file (NULL, "");
  file->embed = 1;
  file->next_file = pfile->all_files;
  pfile->all_files = file;
  params->limit = -1;
  params->offset = 0;
  file->limit = len;
  file->buffer = buf;
  file->path = xstrdup ("<base64>");
  return finish_embed (pfile, file, params);
}

/* Try to load FNAME with #embed/__has_embed parameters PARAMS.
   If !PARAMS->has_embed, return new token in pfile->directive_result
   (first token) and rest in a pushed non-macro context.
   Returns 0 for not found/errors, 1 for non-empty resource and 2
   for empty resource.  */

int
_cpp_stack_embed (cpp_reader *pfile, const char *fname, bool angle,
		  struct cpp_embed_params *params)
{
  if (params->base64.count)
    return finish_base64_embed (pfile, fname, angle, params);
  cpp_dir *dir = search_path_head (pfile, fname, angle, IT_EMBED,
				   params->has_embed);
  if (!dir)
    return 0;
  _cpp_file *file = _cpp_find_file (pfile, fname, dir, angle,
				    params->has_embed
				    ? _cpp_FFK_HAS_EMBED : _cpp_FFK_EMBED,
				    params->loc);
  if (!file)
    return 0;
  if (file->dont_read || file->err_no)
    return 0;
  _cpp_file *orig_file = file;
  if (file->buffer_valid
      && (!S_ISREG (file->st.st_mode)
	  || file->offset + (cpp_num_part) 0 > params->offset
	  || (file->limit < file->st.st_size - file->offset + (size_t) 0
	      && (params->offset - file->offset > (cpp_num_part) file->limit
		  || file->limit - (params->offset
				    - file->offset) < params->limit))))
    {
      bool found = false;
      if (S_ISREG (file->st.st_mode))
	{
	  while (file->next_file
		 && file->next_file->embed
		 && file->next_file->buffer_valid
		 && file->next_file->dir == file->dir
		 && strcmp (file->name, file->next_file->name) == 0
		 && strcmp (file->path, file->next_file->path) == 0)
	    {
	      file = file->next_file;
	      if (file->offset + (cpp_num_part) 0 <= params->offset
		  && (file->limit >= (file->st.st_size - file->offset
				      + (size_t) 0)
		      || (params->offset
			  - file->offset <= (cpp_num_part) file->limit
			  && file->limit - (params->offset
					    - file->offset) >= params->limit)))
		{
		  found = true;
		  break;
		}
	    }
	}
      if (!found)
	{
	  _cpp_file *file2 = make_cpp_file (file->dir, file->name);
	  file2->path = xstrdup (file->path);
	  file2->next_file = file->next_file;
	  file2->embed = true;
	  file->next_file = file2;
	  file = file2;
	}
    }
  if (!file->buffer_valid)
    {
      if (file->fd == -1 && !open_file (file))
	{
	  if (params->has_embed)
	    file->deferred_error = true;
	  else
	    open_file_failed (pfile, file, 0, params->loc);
	  return 0;
	}
      if (S_ISBLK (file->st.st_mode))
	{
	  if (params->has_embed)
	    {
	      close (file->fd);
	      file->fd = -1;
	      return 0;
	    }
	  cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
			"%s is a block device", file->path);
	fail:
	  close (file->fd);
	  file->fd = -1;
	  file->dont_read = true;
	  return 0;
	}

      if (CPP_OPTION (pfile, deps.style)
	  && !params->has_embed
	  && file == orig_file
	  && file->path[0])
	deps_add_dep (pfile->deps, file->path);

      bool regular = S_ISREG (file->st.st_mode) != 0;
      ssize_t size, total, count;
      uchar *buf;
      if (regular)
	{
	  cpp_num_part limit;
	  if (file->st.st_size + (cpp_num_part) 0 < params->offset)
	    limit = 0;
	  else if (file->st.st_size - params->offset < params->limit)
	    limit = file->st.st_size - params->offset;
	  else
	    limit = params->limit;
	  if (params->has_embed)
	    return limit != 0 ? 1 : 2;
	  if (limit > INTTYPE_MAXIMUM (ssize_t))
	    {
	      cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
			    "%s is too large", file->path);
	      goto fail;
	    }
	  if (lseek (file->fd, params->offset, SEEK_CUR)
	      != (off_t) params->offset)
	    {
	      cpp_errno_filename (pfile, CPP_DL_ERROR, file->path,
				  params->loc);
	      goto fail;
	    }
	  file->offset = params->offset;
	  file->limit = limit;
	  size = limit;
	}
      else if (params->has_embed)
	return 2;
      else if (params->limit > 8 * 1024)
	size = 8 * 1024;
      else
	size = params->limit;
      buf = XNEWVEC (uchar, size ? size : 1);
      total = 0;

      if (!regular && params->offset)
	{
	  uchar *buf2 = buf;
	  ssize_t size2 = size;
	  cpp_num_part total2 = params->offset;

	  if (params->offset > 8 * 1024 && size < 8 * 1024)
	    {
	      size2 = 32 * 1024;
	      buf2 = XNEWVEC (uchar, size2);
	    }
	  do
	    {
	      if ((cpp_num_part) size2 > total2)
		size2 = total2;
	      count = read (file->fd, buf2, size2);
	      if (count < 0)
		{
		  cpp_errno_filename (pfile, CPP_DL_ERROR, file->path,
				      params->loc);
		  if (buf2 != buf)
		    free (buf2);
		  free (buf);
		  goto fail;
		}
	      total2 -= count;
	    }
	  while (total2);
	  if (buf2 != buf)
	    free (buf2);
	}

      while ((count = read (file->fd, buf + total, size - total)) > 0)
	{
	  total += count;
	  if (total == size)
	    {
	      if (regular || size + (cpp_num_part) 0 == params->limit)
		break;
	      size = (size_t) size * 2;
	      if (size < 0)
		{
		  if (params->limit <= INTTYPE_MAXIMUM (ssize_t))
		    size = params->limit;
		  else
		    {
		      cpp_error_at (pfile, CPP_DL_ERROR, params->loc,
				    "%s is too large", file->path);
		      free (buf);
		      goto fail;
		    }
		}
	      else if (size + (cpp_num_part) 0 > params->limit)
		size = params->limit;
	      buf = XRESIZEVEC (uchar, buf, size);
	    }
	}

      if (count < 0)
	{
	  cpp_errno_filename (pfile, CPP_DL_ERROR, file->path, params->loc);
	  free (buf);
	  goto fail;
	}

      if (regular && total != size && STAT_SIZE_RELIABLE (file->st))
	{
	  cpp_error_at (pfile, CPP_DL_WARNING, params->loc,
			"%s is shorter than expected", file->path);
	  file->limit = total;
	}
      else if (!regular)
	{
	  file->offset = params->offset;
	  file->limit = total;
	}

      file->buffer_start = buf;
      file->buffer = buf;
      file->buffer_valid = 1;
      close (file->fd);
      file->fd = -1;
    }
  else if (params->has_embed)
    {
      if (params->offset - file->offset > file->limit)
	return 2;
      size_t limit = file->limit - (params->offset - file->offset);
      return limit && params->limit ? 1 : 2;
    }

  return finish_embed (pfile, file, params);
}

/* Retrofit the just-entered main file asif it was an include.  This
   will permit correct include_next use, and mark it as a system
   header if that's where it resides.  We use filesystem-appropriate
   prefix matching of the include path to locate the main file.  */
void
cpp_retrofit_as_include (cpp_reader *pfile)
{
  /* We should be the outermost.  */
  gcc_assert (!pfile->buffer->prev);

  if (const char *name = pfile->main_file->name)
    {
      /* Locate name on the include dir path, using a prefix match.  */
      size_t name_len = strlen (name);
      for (cpp_dir *dir = pfile->quote_include; dir; dir = dir->next)
	if (dir->len < name_len
	    && IS_DIR_SEPARATOR (name[dir->len])
	    && !filename_ncmp (name, dir->name, dir->len))
	  {
	    pfile->main_file->dir = dir;
	    if (dir->sysp)
	      cpp_make_system_header (pfile, 1, 0);
	    break;
	  }
    }

  /* Initialize controlling macro state.  */
  pfile->mi_valid = true;
  pfile->mi_cmacro = 0;
}

/* Could not open FILE.  The complication is dependency output.  */
static void
open_file_failed (cpp_reader *pfile, _cpp_file *file, int angle_brackets,
		  location_t loc)
{
  int sysp = pfile->line_table->highest_line > 1 && pfile->buffer ? pfile->buffer->sysp : 0;
  bool print_dep = CPP_OPTION (pfile, deps.style) > (angle_brackets || !!sysp);

  errno = file->err_no;
  if (print_dep && CPP_OPTION (pfile, deps.missing_files) && errno == ENOENT)
    {
      deps_add_dep (pfile->deps, file->name);
      /* If the preprocessor output (other than dependency information) is
         being used, we must also flag an error.  */
      if (CPP_OPTION (pfile, deps.need_preprocessor_output))
	cpp_errno_filename (pfile, CPP_DL_FATAL,
			    file->path ? file->path : file->name,
			    loc);
    }
  else
    {
      /* If we are not outputting dependencies, or if we are and dependencies
         were requested for this file, or if preprocessor output is needed
         in addition to dependency information, this is an error.

         Otherwise (outputting dependencies but not for this file, and not
         using the preprocessor output), we can still produce correct output
         so it's only a warning.  */
      if (CPP_OPTION (pfile, deps.style) == DEPS_NONE
          || print_dep
          || CPP_OPTION (pfile, deps.need_preprocessor_output))
	cpp_errno_filename (pfile, CPP_DL_FATAL,
			    file->path ? file->path : file->name,
			    loc);
      else
	cpp_errno_filename (pfile, CPP_DL_WARNING,
			    file->path ? file->path : file->name,
			    loc);
    }
}

/* Search in the chain beginning at HEAD for a file whose search path
   started at START_DIR != NULL.  */
static struct cpp_file_hash_entry *
search_cache (struct cpp_file_hash_entry *head, const cpp_dir *start_dir,
	      bool is_embed)
{
  while (head && (head->start_dir != start_dir
		  || head->u.file->embed != is_embed))
    head = head->next;

  return head;
}

/* Allocate a new _cpp_file structure.  */
static _cpp_file *
make_cpp_file (cpp_dir *dir, const char *fname)
{
  _cpp_file *file = XCNEW (_cpp_file);
  file->fd = -1;
  file->dir = dir;
  file->name = xstrdup (fname);

  return file;
}

/* Release a _cpp_file structure.  */
static void
destroy_cpp_file (_cpp_file *file)
{
  free ((void *) file->buffer_start);
  free ((void *) file->name);
  free ((void *) file->path);
  free (file);
}

/* Release all the files allocated by this reader.  */
static void
destroy_all_cpp_files (cpp_reader *pfile)
{
  _cpp_file *iter = pfile->all_files;
  while (iter)
    {
      _cpp_file *next = iter->next_file;
      destroy_cpp_file (iter);
      iter = next;
    }
}

/* A hash of directory names.  The directory names are the path names
   of files which contain a #include "", the included file name is
   appended to this directories.

   To avoid duplicate entries we follow the convention that all
   non-empty directory names should end in a '/'.  DIR_NAME must be
   stored in permanently allocated memory.  */
static cpp_dir *
make_cpp_dir (cpp_reader *pfile, const char *dir_name, int sysp)
{
  struct cpp_file_hash_entry *entry, **hash_slot;
  cpp_dir *dir;

  hash_slot = (struct cpp_file_hash_entry **)
    htab_find_slot_with_hash (pfile->dir_hash, dir_name,
			      htab_hash_string (dir_name),
			      INSERT);

  /* Have we already hashed this directory?  */
  for (entry = *hash_slot; entry; entry = entry->next)
    if (entry->start_dir == NULL)
      return entry->u.dir;

  dir = XCNEW (cpp_dir);
  dir->next = pfile->quote_include;
  dir->name = (char *) dir_name;
  dir->len = strlen (dir_name);
  dir->sysp = sysp;
  dir->construct = 0;

  /* Store this new result in the hash table.  */
  entry = new_file_hash_entry (pfile);
  entry->next = *hash_slot;
  entry->start_dir = NULL;
  entry->location = pfile->line_table->highest_location;
  entry->u.dir = dir;
  *hash_slot = entry;

  return dir;
}

/* Create a new block of memory for file hash entries.  */
static void
allocate_file_hash_entries (cpp_reader *pfile)
{
  struct file_hash_entry_pool *pool = XNEW (struct file_hash_entry_pool);
  pool->file_hash_entries_used = 0;
  pool->next = pfile->file_hash_entries;
  pfile->file_hash_entries = pool;
}

/* Return a new file hash entry.  */
static struct cpp_file_hash_entry *
new_file_hash_entry (cpp_reader *pfile)
{
  unsigned int idx;
  if (pfile->file_hash_entries->file_hash_entries_used == FILE_HASH_POOL_SIZE)
    allocate_file_hash_entries (pfile);

  idx = pfile->file_hash_entries->file_hash_entries_used++;
  return &pfile->file_hash_entries->pool[idx];
}

/* Free the file hash entry pools.  */
static void
free_file_hash_entries (cpp_reader *pfile)
{
  struct file_hash_entry_pool *iter = pfile->file_hash_entries;
  while (iter)
    {
      struct file_hash_entry_pool *next = iter->next;
      free (iter);
      iter = next;
    }
}

/* Returns TRUE if a file FNAME has ever been successfully opened.
   This routine is not intended to correctly handle filenames aliased
   by links or redundant . or .. traversals etc.  */
bool
cpp_included (cpp_reader *pfile, const char *fname)
{
  struct cpp_file_hash_entry *entry;

  entry = (struct cpp_file_hash_entry *)
     htab_find_with_hash (pfile->file_hash, fname, htab_hash_string (fname));

  while (entry && (entry->start_dir == NULL || entry->u.file->err_no))
    entry = entry->next;

  return entry != NULL;
}

/* Returns TRUE if a file FNAME has ever been successfully opened
   before LOCATION.  This routine is not intended to correctly handle
   filenames aliased by links or redundant . or .. traversals etc.  */
bool
cpp_included_before (cpp_reader *pfile, const char *fname,
		     location_t location)
{
  struct cpp_file_hash_entry *entry
    = (struct cpp_file_hash_entry *)
      htab_find_with_hash (pfile->file_hash, fname, htab_hash_string (fname));

  if (IS_ADHOC_LOC (location))
    location = get_location_from_adhoc_loc (pfile->line_table, location);

  while (entry && (entry->start_dir == NULL || entry->u.file->err_no
		   || entry->location > location))
    entry = entry->next;

  return entry != NULL;
}

/* Calculate the hash value of a file hash entry P.  */

static hashval_t
file_hash_hash (const void *p)
{
  struct cpp_file_hash_entry *entry = (struct cpp_file_hash_entry *) p;
  const char *hname;
  if (entry->start_dir)
    hname = entry->u.file->name;
  else
    hname = entry->u.dir->name;

  return htab_hash_string (hname);
}

/* Compare a string Q against a file hash entry P.  */
static int
file_hash_eq (const void *p, const void *q)
{
  struct cpp_file_hash_entry *entry = (struct cpp_file_hash_entry *) p;
  const char *fname = (const char *) q;
  const char *hname;

  if (entry->start_dir)
    hname = entry->u.file->name;
  else
    hname = entry->u.dir->name;

  return filename_cmp (hname, fname) == 0;
}

/* Compare entries in the nonexistent file hash table.  These are just
   strings.  */
static int
nonexistent_file_hash_eq (const void *p, const void *q)
{
  return filename_cmp ((const char *) p, (const char *) q) == 0;
}

/* Initialize everything in this source file.  */
void
_cpp_init_files (cpp_reader *pfile)
{
  pfile->file_hash = htab_create_alloc (127, file_hash_hash, file_hash_eq,
					NULL, xcalloc, free);
  pfile->dir_hash = htab_create_alloc (127, file_hash_hash, file_hash_eq,
					NULL, xcalloc, free);
  allocate_file_hash_entries (pfile);
  pfile->nonexistent_file_hash = htab_create_alloc (127, htab_hash_string,
						    nonexistent_file_hash_eq,
						    NULL, xcalloc, free);
  obstack_specify_allocation (&pfile->nonexistent_file_ob, 0, 0,
			      xmalloc, free);
}

/* Finalize everything in this source file.  */
void
_cpp_cleanup_files (cpp_reader *pfile)
{
  htab_delete (pfile->file_hash);
  htab_delete (pfile->dir_hash);
  htab_delete (pfile->nonexistent_file_hash);
  obstack_free (&pfile->nonexistent_file_ob, 0);
  free_file_hash_entries (pfile);
  destroy_all_cpp_files (pfile);
}

/* Make the parser forget about files it has seen.  This can be useful
   for resetting the parser to start another run.  */
void
cpp_clear_file_cache (cpp_reader *pfile)
{
  _cpp_cleanup_files (pfile);
  pfile->file_hash_entries = NULL;
  pfile->all_files = NULL;
  _cpp_init_files (pfile);
}

/* Enter a file name in the hash for the sake of cpp_included.  */
void
_cpp_fake_include (cpp_reader *pfile, const char *fname)
{
  /* It does not matter what are the contents of fake_source_dir, it will never
     be inspected; we just use its address to uniquely signify that this file
     was added as a fake include, so a later call to _cpp_find_file (to include
     the file for real) won't find the fake one in the hash table.  */
  static cpp_dir fake_source_dir;
  _cpp_find_file (pfile, fname, &fake_source_dir, 0, _cpp_FFK_FAKE, 0);
}

/* Not everyone who wants to set system-header-ness on a buffer can
   see the details of a buffer.  This is an exported interface because
   fix-header needs it.  */
void
cpp_make_system_header (cpp_reader *pfile, int syshdr, int externc)
{
  int flags = 0;
  const class line_maps *line_table = pfile->line_table;
  const line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  /* 1 = system header, 2 = system header to be treated as C.  */
  if (syshdr)
    flags = 1 + (externc != 0);
  pfile->buffer->sysp = flags;
  _cpp_do_file_change (pfile, LC_RENAME, ORDINARY_MAP_FILE_NAME (map),
		       SOURCE_LINE (map, pfile->line_table->highest_line),
		       flags);
}

/* Allow the client to change the current file.  Used by the front end
   to achieve pseudo-file names like <built-in>.
   If REASON is LC_LEAVE, then NEW_NAME must be NULL.  */
void
cpp_change_file (cpp_reader *pfile, enum lc_reason reason,
		 const char *new_name)
{
  _cpp_do_file_change (pfile, reason, new_name, 1, 0);
}

struct report_missing_guard_data
{
  cpp_reader *pfile;
  const char **paths;
  size_t count;
};

/* Callback function for htab_traverse.  */
static int
report_missing_guard (void **slot, void *d)
{
  struct cpp_file_hash_entry *entry = (struct cpp_file_hash_entry *) *slot;
  struct report_missing_guard_data *data
    = (struct report_missing_guard_data *) d;

  /* Skip directories.  */
  if (entry->start_dir != NULL)
    {
      _cpp_file *file = entry->u.file;

      /* We don't want MI guard advice for the main file.  */
      if (!file->once_only
	  && file->cmacro == NULL
	  && file->stack_count == 1
	  && data->pfile->main_file != file)
	{
	  if (data->paths == NULL)
	    {
	      data->paths = XCNEWVEC (const char *, data->count);
	      data->count = 0;
	    }

	  data->paths[data->count++] = file->path;
	}
    }

  /* Keep traversing the hash table.  */
  return 1;
}

/* Comparison function for qsort.  */
static int
report_missing_guard_cmp (const void *p1, const void *p2)
{
  return strcmp (*(const char *const *) p1, *(const char *const *) p2);
}

/* Report on all files that might benefit from a multiple include guard.
   Triggered by -H.  */
void
_cpp_report_missing_guards (cpp_reader *pfile)
{
  struct report_missing_guard_data data;

  data.pfile = pfile;
  data.paths = NULL;
  data.count = htab_elements (pfile->file_hash);
  htab_traverse (pfile->file_hash, report_missing_guard, &data);

  if (data.paths != NULL)
    {
      size_t i;

      /* Sort the paths to avoid outputting them in hash table
	 order.  */
      qsort (data.paths, data.count, sizeof (const char *),
	     report_missing_guard_cmp);
      fputs (_("Multiple include guards may be useful for:\n"),
	     stderr);
      for (i = 0; i < data.count; i++)
	{
	  fputs (data.paths[i], stderr);
	  putc ('\n', stderr);
	}
      free (data.paths);
    }
}

/* Locate HEADER, and determine whether it is newer than the current
   file.  If it cannot be located or dated, return -1, if it is
   newer, return 1, otherwise 0.  */
int
_cpp_compare_file_date (cpp_reader *pfile, const char *fname,
			int angle_brackets)
{
  _cpp_file *file;
  struct cpp_dir *dir;

  dir = search_path_head (pfile, fname, angle_brackets, IT_INCLUDE);
  if (!dir)
    return -1;

  file = _cpp_find_file (pfile, fname, dir, angle_brackets, _cpp_FFK_NORMAL, 0);
  if (file->err_no)
    return -1;

  if (file->fd != -1)
    {
      close (file->fd);
      file->fd = -1;
    }

  return file->st.st_mtime > pfile->buffer->file->st.st_mtime;
}

/* Pushes the given file onto the buffer stack.  Returns nonzero if
   successful.  */
bool
cpp_push_include (cpp_reader *pfile, const char *fname)
{
  return _cpp_stack_include (pfile, fname, false, IT_CMDLINE,
			     pfile->line_table->highest_line);
}

/* Pushes the given file, implicitly included at the start of a
   compilation, onto the buffer stack but without any errors if the
   file is not found.  Returns nonzero if successful.  */
bool
cpp_push_default_include (cpp_reader *pfile, const char *fname)
{
  return _cpp_stack_include (pfile, fname, true, IT_DEFAULT,
			     pfile->line_table->highest_line);
}

/* Do appropriate cleanup when a file INC's buffer is popped off the
   input stack.  */
void
_cpp_pop_file_buffer (cpp_reader *pfile, _cpp_file *file,
		      const unsigned char *to_free)
{
  /* Record the inclusion-preventing macro, which could be NULL
     meaning no controlling macro.  */
  if (pfile->mi_valid && file->cmacro == NULL)
    {
      file->cmacro = pfile->mi_cmacro;
      if (pfile->mi_cmacro
	  && pfile->mi_def_cmacro
	  && pfile->cb.get_suggestion)
	{
	  auto mi_cmacro = (const char *) NODE_NAME (pfile->mi_cmacro);
	  auto mi_def_cmacro = (const char *) NODE_NAME (pfile->mi_def_cmacro);
	  const char *names[] = { mi_def_cmacro, NULL };
	  if (pfile->cb.get_suggestion (pfile, mi_cmacro, names)
	      && cpp_warning_with_line (pfile, CPP_W_HEADER_GUARD,
					pfile->mi_loc, 0,
					"header guard %qs followed by "
					"%<#define%> of a different macro",
					mi_cmacro))
	    cpp_error_at (pfile, CPP_DL_NOTE, pfile->mi_def_loc,
			  "%qs is defined here; did you mean %qs?",
			  mi_def_cmacro, mi_cmacro);
	}
    }

  /* Invalidate control macros in the #including file.  */
  pfile->mi_valid = false;

  if (to_free)
    {
      if (to_free == file->buffer_start)
	{
	  file->buffer_start = NULL;
	  file->buffer = NULL;
	  file->buffer_valid = false;
	}
      free ((void *) to_free);
    }
}

/* Return the file name associated with FILE.  */
const char *
_cpp_get_file_name (_cpp_file *file)
{
  return file->name;
}

/* Inteface to file statistics record in _cpp_file structure. */
struct stat *
_cpp_get_file_stat (_cpp_file *file)
{
  return &file->st;
}

/* Return the directory where FILE was found.  */
struct cpp_dir *
_cpp_get_file_dir (_cpp_file *file)
{
  return file->dir;
}

/* Set the include chain for "" to QUOTE, for <> to BRACKET.  If
   QUOTE_IGNORES_SOURCE_DIR, then "" includes do not look in the
   directory of the including file.

   If BRACKET does not lie in the QUOTE chain, it is set to QUOTE.

   EMBED is include chain for #embed <>.  */
void
cpp_set_include_chains (cpp_reader *pfile, cpp_dir *quote, cpp_dir *bracket,
			cpp_dir *embed, int quote_ignores_source_dir)
{
  pfile->quote_include = quote;
  pfile->bracket_include = quote;
  pfile->quote_ignores_source_dir = quote_ignores_source_dir;
  pfile->embed_include = embed;

  for (; quote; quote = quote->next)
    {
      quote->name_map = NULL;
      quote->len = strlen (quote->name);
      if (quote == bracket)
	pfile->bracket_include = bracket;
    }
  for (; embed; embed = embed->next)
    {
      embed->name_map = NULL;
      embed->len = strlen (embed->name);
    }
}

/* Append the file name to the directory to create the path, but don't
   turn / into // or // into ///; // may be a namespace escape.  */
static char *
append_file_to_dir (const char *fname, cpp_dir *dir)
{
  size_t dlen, flen;
  char *path;

  dlen = dir->len;
  flen = strlen (fname);
  path = XNEWVEC (char, dlen + 1 + flen + 1);
  memcpy (path, dir->name, dlen);
  if (dlen && !IS_DIR_SEPARATOR (path[dlen - 1]))
    path[dlen++] = '/';
  memcpy (&path[dlen], fname, flen + 1);

  return path;
}

/* Read a space delimited string of unlimited length from a stdio
   file F.  */
static char *
read_filename_string (int ch, FILE *f)
{
  char *alloc, *set;
  int len;

  len = 20;
  set = alloc = XNEWVEC (char, len + 1);
  if (! is_space (ch))
    {
      *set++ = ch;
      while ((ch = getc (f)) != EOF && ! is_space (ch))
	{
	  if (set - alloc == len)
	    {
	      len *= 2;
	      alloc = XRESIZEVEC (char, alloc, len + 1);
	      set = alloc + len / 2;
	    }
	  *set++ = ch;
	}
    }
  *set = '\0';
  ungetc (ch, f);
  return alloc;
}

/* Read the file name map file for DIR.  */
static void
read_name_map (cpp_dir *dir)
{
  static const char FILE_NAME_MAP_FILE[] = "header.gcc";
  char *name;
  FILE *f;
  size_t len, count = 0, room = 9;

  len = dir->len;
  name = (char *) alloca (len + sizeof (FILE_NAME_MAP_FILE) + 1);
  memcpy (name, dir->name, len);
  if (len && !IS_DIR_SEPARATOR (name[len - 1]))
    name[len++] = '/';
  strcpy (name + len, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");

  dir->name_map = XNEWVEC (const char *, room);

  /* Silently return NULL if we cannot open.  */
  if (f)
    {
      int ch;

      while ((ch = getc (f)) != EOF)
	{
	  char *to;

	  if (is_space (ch))
	    continue;

	  if (count + 2 > room)
	    {
	      room += 8;
	      dir->name_map = XRESIZEVEC (const char *, dir->name_map, room);
	    }

	  dir->name_map[count] = read_filename_string (ch, f);
	  while ((ch = getc (f)) != EOF && is_hspace (ch))
	    ;

	  to = read_filename_string (ch, f);
	  if (IS_ABSOLUTE_PATH (to))
	    dir->name_map[count + 1] = to;
	  else
	    {
	      dir->name_map[count + 1] = append_file_to_dir (to, dir);
	      free (to);
	    }

	  count += 2;
	  while ((ch = getc (f)) != '\n')
	    if (ch == EOF)
	      break;
	}

      fclose (f);
    }

  /* Terminate the list of maps.  */
  dir->name_map[count] = NULL;
}

/* Remap a FILE's name based on the file_name_map, if any, for
   FILE->dir.  If the file name has any directory separators,
   recursively check those directories too.  */
static char *
remap_filename (cpp_reader *pfile, _cpp_file *file)
{
  const char *fname, *p;
  char *new_dir, *p3;
  cpp_dir *dir;
  size_t index, len;

  dir = file->dir;
  fname = file->name;

  for (;;)
    {
      if (!dir->name_map)
	read_name_map (dir);

      for (index = 0; dir->name_map[index]; index += 2)
	if (!filename_cmp (dir->name_map[index], fname))
	    return xstrdup (dir->name_map[index + 1]);
      if (IS_ABSOLUTE_PATH (fname))
	return NULL;
      p = strchr (fname, '/');
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
      {
	const char *p2 = strchr (fname, '\\');
	if (!p || (p2 && p > p2))
	  p = p2;
      }
#endif
      if (!p || p == fname)
	return NULL;

      len = dir->len + (p - fname + 1);
      new_dir = XNEWVEC (char, len + 2);
      p3 = new_dir + dir->len;
      memcpy (new_dir, dir->name, dir->len);
      if (dir->len && !IS_DIR_SEPARATOR (dir->name[dir->len - 1]))
	{
	  *p3++ = '/';
	  len++;
	}
      memcpy (p3, fname, p - fname + 1);
      new_dir[len] = '\0';

      dir = make_cpp_dir (pfile, new_dir, dir->sysp);
      fname = p + 1;
    }
}

/* Returns true if PCHNAME is a valid PCH file for FILE.  */
static bool
validate_pch (cpp_reader *pfile, _cpp_file *file, const char *pchname)
{
  const char *saved_path = file->path;
  bool valid = false;

  file->path = pchname;
  if (open_file (file))
    {
      valid = 1 & pfile->cb.valid_pch (pfile, pchname, file->fd);

      if (!valid)
	{
	  close (file->fd);
	  file->fd = -1;
	}

      if (CPP_OPTION (pfile, print_include_names))
	{
	  unsigned int i;
	  for (i = 1; i < pfile->line_table->depth; i++)
	    putc ('.', stderr);
	  fprintf (stderr, "%c %s\n",
		   valid ? '!' : 'x', pchname);
	}
    }

  file->path = saved_path;
  return valid;
}

/* Get the path associated with the _cpp_file F.  The path includes
   the base name from the include directive and the directory it was
   found in via the search path.  */

const char *
cpp_get_path (struct _cpp_file *f)
{
  return f->path;
}

/* Get the directory associated with the _cpp_file F.  */

cpp_dir *
cpp_get_dir (struct _cpp_file *f)
{
  return f->dir;
}

/* Get the cpp_buffer currently associated with the cpp_reader
   PFILE.  */

cpp_buffer *
cpp_get_buffer (cpp_reader *pfile)
{
  return pfile->buffer;
}

/* Get the _cpp_file associated with the cpp_buffer B.  */

_cpp_file *
cpp_get_file (cpp_buffer *b)
{
  return b->file;
}

/* Get the previous cpp_buffer given a cpp_buffer B.  The previous
   buffer is the buffer that included the given buffer.  */

cpp_buffer *
cpp_get_prev (cpp_buffer *b)
{
  return b->prev;
}

/* This data structure holds the list of header files that were seen
   while the PCH was being built.  The 'entries' field is kept sorted
   in memcmp() order; yes, this means that on little-endian systems,
   it's sorted initially by the least-significant byte of 'size', but
   that's OK.  The code does rely on having entries with the same size
   next to each other.  */

struct pchf_entry {
  /* The size of this file.  This is used to save running a MD5 checksum
     if the sizes don't match.  */
  off_t size;
  /* The MD5 checksum of this file.  */
  unsigned char sum[16];
  /* Is this file to be included only once?  */
  bool once_only;
};

struct pchf_data {
  /* Number of pchf_entry structures.  */
  size_t count;

  /* Are there any values with once_only set?
     This is used as an optimisation, it means we don't have to search
     the structure if we're processing a regular #include.  */
  bool have_once_only;

  struct pchf_entry entries[1];
};

static struct pchf_data *pchf;

/* A qsort ordering function for pchf_entry structures.  */

static int
pchf_save_compare (const void *e1, const void *e2)
{
  return memcmp (e1, e2, sizeof (struct pchf_entry));
}

/* Create and write to F a pchf_data structure.  */

bool
_cpp_save_file_entries (cpp_reader *pfile, FILE *fp)
{
  size_t count = 0;
  struct pchf_data *result;
  size_t result_size;
  _cpp_file *f;
  bool ret;

  for (f = pfile->all_files; f; f = f->next_file)
    ++count;

  result_size = (sizeof (struct pchf_data)
		 + sizeof (struct pchf_entry) * (count - 1));
  result = XCNEWVAR (struct pchf_data, result_size);

  result->count = 0;
  result->have_once_only = false;

  for (f = pfile->all_files; f; f = f->next_file)
    {
      size_t count;

      /* This should probably never happen, since if a read error occurred
	 the PCH file shouldn't be written...  */
      if (f->dont_read || f->err_no)
	continue;

      if (f->stack_count == 0)
	continue;

      count = result->count++;

      result->entries[count].once_only = f->once_only;
      /* |= is avoided in the next line because of an HP C compiler bug */
      result->have_once_only = result->have_once_only | f->once_only;
      if (f->buffer_valid)
	md5_buffer ((const char *)f->buffer,
		    f->st.st_size, result->entries[count].sum);
      else
	{
	  FILE *ff;
	  int oldfd = f->fd;

	  if (!open_file (f))
	    {
	      open_file_failed (pfile, f, 0, 0);
	      free (result);
	      return false;
	    }
	  ff = fdopen (f->fd, "rb");
	  md5_stream (ff, result->entries[count].sum);
	  fclose (ff);
	  f->fd = oldfd;
	}
      result->entries[count].size = f->st.st_size;
    }

  result_size = (sizeof (struct pchf_data)
                 + sizeof (struct pchf_entry) * (result->count - 1));

  qsort (result->entries, result->count, sizeof (struct pchf_entry),
	 pchf_save_compare);

  ret = fwrite (result, result_size, 1, fp) == 1;
  free (result);
  return ret;
}

/* Read the pchf_data structure from F.  */

bool
_cpp_read_file_entries (cpp_reader *pfile ATTRIBUTE_UNUSED, FILE *f)
{
  struct pchf_data d;

  if (fread (&d, sizeof (struct pchf_data) - sizeof (struct pchf_entry), 1, f)
       != 1)
    return false;

  pchf = XNEWVAR (struct pchf_data, sizeof (struct pchf_data)
		  + sizeof (struct pchf_entry) * (d.count - 1));
  memcpy (pchf, &d, sizeof (struct pchf_data) - sizeof (struct pchf_entry));
  if (fread (pchf->entries, sizeof (struct pchf_entry), d.count, f)
      != d.count)
    return false;
  return true;
}

/* The parameters for pchf_compare.  */

struct pchf_compare_data
{
  /* The size of the file we're looking for.  */
  off_t size;

  /* The MD5 checksum of the file, if it's been computed.  */
  unsigned char sum[16];

  /* Is SUM valid?  */
  bool sum_computed;

  /* Do we need to worry about entries that don't have ONCE_ONLY set?  */
  bool check_included;

  /* The file that we're searching for.  */
  _cpp_file *f;
};

/* bsearch comparison function; look for D_P in E_P.  */

static int
pchf_compare (const void *d_p, const void *e_p)
{
  const struct pchf_entry *e = (const struct pchf_entry *)e_p;
  struct pchf_compare_data *d = (struct pchf_compare_data *)d_p;
  int result;

  result = memcmp (&d->size, &e->size, sizeof (off_t));
  if (result != 0)
    return result;

  if (! d->sum_computed)
    {
      _cpp_file *const f = d->f;

      md5_buffer ((const char *)f->buffer, f->st.st_size, d->sum);
      d->sum_computed = true;
    }

  result = memcmp (d->sum, e->sum, 16);
  if (result != 0)
    return result;

  if (d->check_included || e->once_only)
    return 0;
  else
    return 1;
}

/* Check that F is not in a list read from a PCH file (if any).
   Assumes that f->buffer_valid is true.  Return TRUE if the file
   should not be read.  */

static bool
check_file_against_entries (cpp_reader *pfile ATTRIBUTE_UNUSED,
			    _cpp_file *f,
			    bool check_included)
{
  struct pchf_compare_data d;

  if (pchf == NULL
      || (! check_included && ! pchf->have_once_only))
    return false;

  d.size = f->st.st_size;
  d.sum_computed = false;
  d.f = f;
  d.check_included = check_included;
  return bsearch (&d, pchf->entries, pchf->count, sizeof (struct pchf_entry),
		  pchf_compare) != NULL;
}

/* Return true if the file FNAME is found in the appropriate include file path
   as indicated by ANGLE_BRACKETS.  */

bool
_cpp_has_header (cpp_reader *pfile, const char *fname, int angle_brackets,
		 enum include_type type)
{
  cpp_dir *start_dir = search_path_head (pfile, fname, angle_brackets, type,
					 /* suppress_diagnostic = */ true);
  if (!start_dir)
    return false;
  _cpp_file *file = _cpp_find_file (pfile, fname, start_dir, angle_brackets,
				    _cpp_FFK_HAS_INCLUDE, 0);
  return file->err_no != ENOENT;
}

/* Read a file and convert to input charset, the same as if it were being read
   by a cpp_reader.  */

cpp_converted_source
cpp_get_converted_source (const char *fname, const char *input_charset)
{
  cpp_converted_source res = {};
  _cpp_file file = {};
  file.fd = -1;
  file.name = lbasename (fname);
  file.path = fname;
  if (!open_file (&file))
    return res;
  const bool ok = read_file_guts (NULL, &file, 0, input_charset);
  close (file.fd);
  if (!ok)
    return res;
  res.to_free = (char *) file.buffer_start;
  res.data = (char *) file.buffer;
  res.len = file.st.st_size;
  return res;
}
