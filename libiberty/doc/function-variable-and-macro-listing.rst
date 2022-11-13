..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _functions:

Function, Variable, and Macro Listing.
--------------------------------------

.. Automatically generated from *.c and others (the comments before
   each entry tell you which file and where in that file).  DO NOT EDIT!
   Edit the *.c files, configure with -enable-maintainer-mode,
   run 'make stamp-functions' and gather-docs will build a new copy.
   alloca.c:26

.. function:: void* alloca (size_t size)

  This function allocates memory which will be automatically reclaimed
  after the procedure exits.  The ``libiberty`` implementation does not free
  the memory immediately but will do so eventually during subsequent
  calls to this function.  Memory is allocated using ``xmalloc`` under
  normal circumstances.

  The header file :samp:`alloca-conf.h` can be used in conjunction with the
  GNU Autoconf test ``AC_FUNC_ALLOCA`` to test for and properly make
  available this function.  The ``AC_FUNC_ALLOCA`` test requires that
  client code use a block of preprocessor code to be safe (see the Autoconf
  manual for more); this header incorporates that logic and more, including
  the possibility of a GCC built-in function.

.. asprintf.c:32

.. function:: int asprintf (char **resptr, const char *format, ...)

  Like ``sprintf``, but instead of passing a pointer to a buffer, you
  pass a pointer to a pointer.  This function will compute the size of
  the buffer needed, allocate memory with ``malloc``, and store a
  pointer to the allocated memory in ``*resptr``.  The value
  returned is the same as ``sprintf`` would return.  If memory could
  not be allocated, minus one is returned and ``NULL`` is stored in
  ``*resptr``.

.. atexit.c:6

.. function:: int atexit (void (*f)())

  Causes function :samp:`{f}` to be called at exit.  Returns 0.

.. basename.c:6

.. function:: char* basename (const char *name)

  Returns a pointer to the last component of pathname :samp:`{name}`.
  Behavior is undefined if the pathname ends in a directory separator.

.. bcmp.c:6

.. function:: int bcmp (char *x, char *y, int count)

  Compares the first :samp:`{count}` bytes of two areas of memory.  Returns
  zero if they are the same, nonzero otherwise.  Returns zero if
  :samp:`{count}` is zero.  A nonzero result only indicates a difference,
  it does not indicate any sorting order (say, by having a positive
  result mean :samp:`{x}` sorts before :samp:`{y}`).

.. bcopy.c:3

.. function:: void bcopy (char *in, char *out, int length)

  Copies :samp:`{length}` bytes from memory region :samp:`{in}` to region
  :samp:`{out}`.  The use of ``bcopy`` is deprecated in new programs.

.. bsearch.c:33

.. function:: void* bsearch (const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *))

  Performs a search over an array of :samp:`{nmemb}` elements pointed to by
  :samp:`{base}` for a member that matches the object pointed to by :samp:`{key}`.
  The size of each member is specified by :samp:`{size}`.  The array contents
  should be sorted in ascending order according to the :samp:`{compar}`
  comparison function.  This routine should take two arguments pointing to
  the :samp:`{key}` and to an array member, in that order, and should return an
  integer less than, equal to, or greater than zero if the :samp:`{key}` object
  is respectively less than, matching, or greater than the array member.

.. bsearch_r.c:33

.. function:: void* bsearch_r (const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *, void *), void *arg)

  Performs a search over an array of :samp:`{nmemb}` elements pointed to by
  :samp:`{base}` for a member that matches the object pointed to by :samp:`{key}`.
  The size of each member is specified by :samp:`{size}`.  The array contents
  should be sorted in ascending order according to the :samp:`{compar}`
  comparison function.  This routine should take three arguments: the first
  two point to the :samp:`{key}` and to an array member, and the last is passed
  down unchanged from ``bsearch_r`` 's last argument.  It should return an
  integer less than, equal to, or greater than zero if the :samp:`{key}` object
  is respectively less than, matching, or greater than the array member.

.. argv.c:138

.. function:: char** buildargv (char *sp)

  Given a pointer to a string, parse the string extracting fields
  separated by whitespace and optionally enclosed within either single
  or double quotes (which are stripped off), and build a vector of
  pointers to copies of the string for each field.  The input string
  remains unchanged.  The last element of the vector is followed by a
  ``NULL`` element.

  All of the memory for the pointer array and copies of the string
  is obtained from ``xmalloc``.  All of the memory can be returned to the
  system with the single function call ``freeargv``, which takes the
  returned result of ``buildargv``, as it's argument.

  Returns a pointer to the argument vector if successful.  Returns
  ``NULL`` if :samp:`{sp}` is ``NULL`` or if there is insufficient
  memory to complete building the argument vector.

  If the input is a null string (as opposed to a ``NULL`` pointer),
  then buildarg returns an argument vector that has one arg, a null
  string.

.. bzero.c:6

.. function:: void bzero (char *mem, int count)

  Zeros :samp:`{count}` bytes starting at :samp:`{mem}`.  Use of this function
  is deprecated in favor of ``memset``.

.. calloc.c:6

.. function:: void* calloc (size_t nelem, size_t elsize)

  Uses ``malloc`` to allocate storage for :samp:`{nelem}` objects of
  :samp:`{elsize}` bytes each, then zeros the memory.

.. filename_cmp.c:201

.. function:: int canonical_filename_eq (const char *a, const char *b)

  Return non-zero if file names :samp:`{a}` and :samp:`{b}` are equivalent.
  This function compares the canonical versions of the filenames as returned by
  ``lrealpath()``, so that so that different file names pointing to the same
  underlying file are treated as being identical.

.. choose-temp.c:45

.. function:: char* choose_temp_base (void)

  Return a prefix for temporary file names or ``NULL`` if unable to
  find one.  The current directory is chosen if all else fails so the
  program is exited if a temporary directory can't be found (``mktemp``
  fails).  The buffer for the result is obtained with ``xmalloc``.

  This function is provided for backwards compatibility only.  Its use is
  not recommended.

.. make-temp-file.c:95

.. function:: const char* choose_tmpdir ()

  Returns a pointer to a directory path suitable for creating temporary
  files in.

.. clock.c:27

.. function:: long clock (void)

  Returns an approximation of the CPU time used by the process as a
  ``clock_t`` ; divide this number by :samp:`CLOCKS_PER_SEC` to get the
  number of seconds used.

.. concat.c:24

.. function:: char* concat (const char *s1, const char *s2, ..., NULL)

  Concatenate zero or more of strings and return the result in freshly
  ``xmalloc`` ed memory.  The argument list is terminated by the first
  ``NULL`` pointer encountered.  Pointers to empty strings are ignored.

.. argv.c:495

.. function:: int countargv (char * const *argv)

  Return the number of elements in :samp:`{argv}`.
  Returns zero if :samp:`{argv}` is NULL.

.. crc32.c:140

.. function:: unsigned int crc32 (const unsigned char *buf, int len, unsigned int init)

  Compute the 32-bit CRC of :samp:`{buf}` which has length :samp:`{len}`.  The
  starting value is :samp:`{init}` ; this may be used to compute the CRC of
  data split across multiple buffers by passing the return value of each
  call as the :samp:`{init}` parameter of the next.

  This is used by the :command:`gdb` remote protocol for the :samp:`qCRC`
  command.  In order to get the same results as gdb for a block of data,
  you must pass the first CRC parameter as ``0xffffffff``.

  This CRC can be specified as:

  Width  : 32
    Poly   : 0x04c11db7
    Init   : parameter, typically 0xffffffff
    RefIn  : false
    RefOut : false
    XorOut : 0

  This differs from the "standard" CRC-32 algorithm in that the values
  are not reflected, and there is no final XOR value.  These differences
  make it easy to compose the values of multiple blocks.

.. argv.c:59

.. function:: char** dupargv (char * const *vector)

  Duplicate an argument vector.  Simply scans through :samp:`{vector}`,
  duplicating each argument until the terminating ``NULL`` is found.
  Returns a pointer to the argument vector if successful.  Returns
  ``NULL`` if there is insufficient memory to complete building the
  argument vector.

.. strerror.c:572

.. function:: int errno_max (void)

  Returns the maximum ``errno`` value for which a corresponding
  symbolic name or message is available.  Note that in the case where we
  use the ``sys_errlist`` supplied by the system, it is possible for
  there to be more symbolic names than messages, or vice versa.  In
  fact, the manual page for ``perror(3C)`` explicitly warns that one
  should check the size of the table (``sys_nerr``) before indexing
  it, since new error codes may be added to the system before they are
  added to the table.  Thus ``sys_nerr`` might be smaller than value
  implied by the largest ``errno`` value defined in ``<errno.h>``.

  We return the maximum value that can be used to obtain a meaningful
  symbolic name or message.

.. argv.c:352

.. function:: void expandargv (int *argcp, char ***argvp)

  The :samp:`{argcp}` and ``argvp`` arguments are pointers to the usual
  ``argc`` and ``argv`` arguments to ``main``.  This function
  looks for arguments that begin with the character :samp:`@`.  Any such
  arguments are interpreted as 'response files'.  The contents of the
  response file are interpreted as additional command line options.  In
  particular, the file is separated into whitespace-separated strings;
  each such string is taken as a command-line option.  The new options
  are inserted in place of the option naming the response file, and
  ``*argcp`` and ``*argvp`` will be updated.  If the value of
  ``*argvp`` is modified by this function, then the new value has
  been dynamically allocated and can be deallocated by the caller with
  ``freeargv``.  However, most callers will simply call
  ``expandargv`` near the beginning of ``main`` and allow the
  operating system to free the memory when the program exits.

.. fdmatch.c:23

.. function:: int fdmatch (int fd1, int fd2)

  Check to see if two open file descriptors refer to the same file.
  This is useful, for example, when we have an open file descriptor for
  an unnamed file, and the name of a file that we believe to correspond
  to that fd.  This can happen when we are exec'd with an already open
  file (``stdout`` for example) or from the SVR4 :samp:`/proc` calls
  that return open file descriptors for mapped address spaces.  All we
  have to do is open the file by name and check the two file descriptors
  for a match, which is done by comparing major and minor device numbers
  and inode numbers.

.. fopen_unlocked.c:49

.. function:: FILE * fdopen_unlocked (int fildes, const char * mode)

  Opens and returns a ``FILE`` pointer via ``fdopen``.  If the
  operating system supports it, ensure that the stream is setup to avoid
  any multi-threaded locking.  Otherwise return the ``FILE`` pointer
  unchanged.

.. ffs.c:3

.. function:: int ffs (int valu)

  Find the first (least significant) bit set in :samp:`{valu}`.  Bits are
  numbered from right to left, starting with bit 1 (corresponding to the
  value 1).  If :samp:`{valu}` is zero, zero is returned.

.. filename_cmp.c:37

.. function:: int filename_cmp (const char *s1, const char *s2)

  Return zero if the two file names :samp:`{s1}` and :samp:`{s2}` are equivalent.
  If not equivalent, the returned value is similar to what ``strcmp``
  would return.  In other words, it returns a negative value if :samp:`{s1}`
  is less than :samp:`{s2}`, or a positive value if :samp:`{s2}` is greater than
  :samp:`{s2}`.

  This function does not normalize file names.  As a result, this function
  will treat filenames that are spelled differently as different even in
  the case when the two filenames point to the same underlying file.
  However, it does handle the fact that on DOS-like file systems, forward
  and backward slashes are equal.

.. filename_cmp.c:183

.. function:: int filename_eq (const void *s1, const void *s2)

  Return non-zero if file names :samp:`{s1}` and :samp:`{s2}` are equivalent.
  This function is for use with hashtab.c hash tables.

.. filename_cmp.c:152

.. function:: hashval_t filename_hash (const void *s)

  Return the hash value for file name :samp:`{s}` that will be compared
  using filename_cmp.
  This function is for use with hashtab.c hash tables.

.. filename_cmp.c:94

.. function:: int filename_ncmp (const char *s1, const char *s2, size_t n)

  Return zero if the two file names :samp:`{s1}` and :samp:`{s2}` are equivalent
  in range :samp:`{n}`.
  If not equivalent, the returned value is similar to what ``strncmp``
  would return.  In other words, it returns a negative value if :samp:`{s1}`
  is less than :samp:`{s2}`, or a positive value if :samp:`{s2}` is greater than
  :samp:`{s2}`.

  This function does not normalize file names.  As a result, this function
  will treat filenames that are spelled differently as different even in
  the case when the two filenames point to the same underlying file.
  However, it does handle the fact that on DOS-like file systems, forward
  and backward slashes are equal.

.. fnmatch.txh:1

.. function:: int fnmatch (const char *pattern, const char *string, int flags)

  Matches :samp:`{string}` against :samp:`{pattern}`, returning zero if it
  matches, ``FNM_NOMATCH`` if not.  :samp:`{pattern}` may contain the
  wildcards ``?`` to match any one character, ``*`` to match any
  zero or more characters, or a set of alternate characters in square
  brackets, like :samp:`[a-gt8]`, which match one character (``a``
  through ``g``, or ``t``, or ``8``, in this example) if that one
  character is in the set.  A set may be inverted (i.e., match anything
  except what's in the set) by giving ``^`` or ``!`` as the first
  character in the set.  To include those characters in the set, list them
  as anything other than the first character of the set.  To include a
  dash in the set, list it last in the set.  A backslash character makes
  the following character not special, so for example you could match
  against a literal asterisk with :samp:`\\*`.  To match a literal
  backslash, use :samp:`\\\\`.

  ``flags`` controls various aspects of the matching process, and is a
  boolean OR of zero or more of the following values (defined in
  ``<fnmatch.h>``):

  .. envvar:: FNM_PATHNAME

    :samp:`{string}` is assumed to be a path name.  No wildcard will ever match
    ``/``.

  .. envvar:: FNM_NOESCAPE

    Do not interpret backslashes as quoting the following special character.

  .. envvar:: FNM_PERIOD

    A leading period (at the beginning of :samp:`{string}`, or if
    ``FNM_PATHNAME`` after a slash) is not matched by ``*`` or
    ``?`` but must be matched explicitly.

  .. envvar:: FNM_LEADING_DIR

    Means that :samp:`{string}` also matches :samp:`{pattern}` if some initial part
    of :samp:`{string}` matches, and is followed by ``/`` and zero or more
    characters.  For example, :samp:`foo*` would match either :samp:`foobar`
    or :samp:`foobar/grill`.

  .. envvar:: FNM_CASEFOLD

    Ignores case when performing the comparison.

.. fopen_unlocked.c:39

.. function:: FILE * fopen_unlocked (const char *path, const char * mode)

  Opens and returns a ``FILE`` pointer via ``fopen``.  If the
  operating system supports it, ensure that the stream is setup to avoid
  any multi-threaded locking.  Otherwise return the ``FILE`` pointer
  unchanged.

.. argv.c:93

.. function:: void freeargv (char **vector)

  Free an argument vector that was built using ``buildargv``.  Simply
  scans through :samp:`{vector}`, freeing the memory for each argument until
  the terminating ``NULL`` is found, and then frees :samp:`{vector}`
  itself.

.. fopen_unlocked.c:59

.. function:: FILE * freopen_unlocked (const char * path, const char * mode, FILE * stream)

  Opens and returns a ``FILE`` pointer via ``freopen``.  If the
  operating system supports it, ensure that the stream is setup to avoid
  any multi-threaded locking.  Otherwise return the ``FILE`` pointer
  unchanged.

.. getruntime.c:86

.. function:: long get_run_time (void)

  Returns the time used so far, in microseconds.  If possible, this is
  the time used by this process, else it is the elapsed time since the
  process started.

.. getcwd.c:6

.. function:: char* getcwd (char *pathname, int len)

  Copy the absolute pathname for the current working directory into
  :samp:`{pathname}`, which is assumed to point to a buffer of at least
  :samp:`{len}` bytes, and return a pointer to the buffer.  If the current
  directory's path doesn't fit in :samp:`{len}` characters, the result is
  ``NULL`` and ``errno`` is set.  If :samp:`{pathname}` is a null pointer,
  ``getcwd`` will obtain :samp:`{len}` bytes of space using
  ``malloc``.

.. getpagesize.c:5

.. function:: int getpagesize (void)

  Returns the number of bytes in a page of memory.  This is the
  granularity of many of the system memory management routines.  No
  guarantee is made as to whether or not it is the same as the basic
  memory management hardware page size.

.. getpwd.c:5

.. function:: char* getpwd (void)

  Returns the current working directory.  This implementation caches the
  result on the assumption that the process will not call ``chdir``
  between calls to ``getpwd``.

.. gettimeofday.c:12

.. function:: int gettimeofday (struct timeval *tp, void *tz)

  Writes the current time to :samp:`{tp}`.  This implementation requires
  that :samp:`{tz}` be NULL.  Returns 0 on success, -1 on failure.

.. hex.c:33

.. function:: void hex_init (void)

  Initializes the array mapping the current character set to
  corresponding hex values.  This function must be called before any
  call to ``hex_p`` or ``hex_value``.  If you fail to call it, a
  default ASCII-based table will normally be used on ASCII systems.

.. hex.c:42

.. function:: int hex_p (int c)

  Evaluates to non-zero if the given character is a valid hex character,
  or zero if it is not.  Note that the value you pass will be cast to
  ``unsigned char`` within the macro.

.. hex.c:50

.. function:: unsigned int hex_value (int c)

  Returns the numeric equivalent of the given character when interpreted
  as a hexadecimal digit.  The result is undefined if you pass an
  invalid hex digit.  Note that the value you pass will be cast to
  ``unsigned char`` within the macro.

  The ``hex_value`` macro returns ``unsigned int``, rather than
  signed ``int``, to make it easier to use in parsing addresses from
  hex dump files: a signed ``int`` would be sign-extended when
  converted to a wider unsigned type --- like ``bfd_vma``, on some
  systems.

.. safe-ctype.c:24

.. index:: HOST_CHARSET

.. c:macro:: HOST_CHARSET

  This macro indicates the basic character set and encoding used by the
  host: more precisely, the encoding used for character constants in
  preprocessor :samp:`#if` statements (the C "execution character set").
  It is defined by :samp:`safe-ctype.h`, and will be an integer constant
  with one of the following values:

.. envvar:: HOST_CHARSET_UNKNOWN

  The host character set is unknown - that is, not one of the next two
  possibilities.

.. envvar:: HOST_CHARSET_ASCII

  The host character set is ASCII.

.. envvar:: HOST_CHARSET_EBCDIC

  The host character set is some variant of EBCDIC.  (Only one of the
  nineteen EBCDIC varying characters is tested; exercise caution.)

.. hashtab.c:327

.. function:: htab_t htab_create_typed_alloc (size_t size, htab_hash hash_f, htab_eq eq_f, htab_del del_f, htab_alloc alloc_tab_f, htab_alloc alloc_f, htab_free free_f)

  This function creates a hash table that uses two different allocators
  :samp:`{alloc_tab_f}` and :samp:`{alloc_f}` to use for allocating the table itself
  and its entries respectively.  This is useful when variables of different
  types need to be allocated with different allocators.

  The created hash table is slightly larger than :samp:`{size}` and it is
  initially empty (all the hash table entries are ``HTAB_EMPTY_ENTRY``).
  The function returns the created hash table, or ``NULL`` if memory
  allocation fails.

.. index.c:5

.. function:: char* index (char *s, int c)

  Returns a pointer to the first occurrence of the character :samp:`{c}` in
  the string :samp:`{s}`, or ``NULL`` if not found.  The use of ``index`` is
  deprecated in new programs in favor of ``strchr``.

.. insque.c:6

.. function:: void insque (struct qelem *elem, struct qelem *pred)
              void remque (struct qelem *elem)

  Routines to manipulate queues built from doubly linked lists.  The
  ``insque`` routine inserts :samp:`{elem}` in the queue immediately
  after :samp:`{pred}`.  The ``remque`` routine removes :samp:`{elem}` from
  its containing queue.  These routines expect to be passed pointers to
  structures which have as their first members a forward pointer and a
  back pointer, like this prototype (although no prototype is provided):

  .. code-block:: c++

    struct qelem {
      struct qelem *q_forw;
      struct qelem *q_back;
      char q_data[];
    };

.. safe-ctype.c:45

.. c:macro:: ISALPHA (c)
             ISALNUM (c)
             ISBLANK (c)
             ISCNTRL (c)
             ISDIGIT (c)
             ISGRAPH (c)
             ISLOWER (c)
             ISPRINT (c)
             ISPUNCT (c)
             ISSPACE (c)
             ISUPPER (c)
             ISXDIGIT (c)

These twelve macros are defined by :samp:`safe-ctype.h`.  Each has the
same meaning as the corresponding macro (with name in lowercase)
defined by the standard header :samp:`ctype.h`.  For example,
``ISALPHA`` returns true for alphabetic characters and false for
others.  However, there are two differences between these macros and
those provided by :samp:`ctype.h`:

* These macros are guaranteed to have well-defined behavior for all
  values representable by ``signed char`` and ``unsigned char``, and
  for ``EOF``.

* These macros ignore the current locale; they are true for these
  fixed sets of characters:

  .. list-table::

     * - ``ALPHA``
       - A-Za-z
     * - ``ALNUM``
       - A-Za-z0-9
     * - ``BLANK``
       - space tab
     * - ``CNTRL``
       - ``!PRINT``
     * - ``DIGIT``
       - 0-9
     * - ``GRAPH``
       - ``ALNUM || PUNCT``
     * - ``LOWER``
       - a-z
     * - ``PRINT``
       - ``GRAPH ||`` space
     * - ``PUNCT``
       - `~!@#$%^&\*()_-=+[{]}\|;:'",<.>/?
     * - ``SPACE``
       - space tab \n \r \f \v
     * - ``UPPER``
       - A-Z
     * - ``XDIGIT``
       - 0-9A-Fa-f

  Note that, if the host character set is ASCII or a superset thereof,
  all these macros will return false for all values of ``char`` outside
  the range of 7-bit ASCII.  In particular, both ISPRINT and ISCNTRL return
  false for characters with numeric values from 128 to 255.

.. safe-ctype.c:94

.. c:macro:: ISIDNUM (c)
             ISIDST (c)
             IS_VSPACE (c)
             IS_NVSPACE (c)
             IS_SPACE_OR_NUL (c)
             IS_ISOBASIC (c)

These six macros are defined by safe-ctype.h and provide
additional character classes which are useful when doing lexical
analysis of C or similar languages.  They are true for the following
sets of characters:

.. list-table::

   * - ``IDNUM``
     - A-Za-z0-9\_
   * - ``IDST``
     - A-Za-z\_
   * - ``VSPACE``
     - \r \n
   * - ``NVSPACE``
     - space tab \f \v \0
   * - ``SPACE_OR_NUL``
     - ``VSPACE || NVSPACE``
   * - ``ISOBASIC``
     - ``VSPACE || NVSPACE || PRINT``

.. lbasename.c:23

.. function:: const char* lbasename (const char *name)

  Given a pointer to a string containing a typical pathname
  (:samp:`/usr/src/cmd/ls/ls.c` for example), returns a pointer to the
  last component of the pathname (:samp:`ls.c` in this case).  The
  returned pointer is guaranteed to lie within the original
  string.  This latter fact is not true of many vendor C
  libraries, which return special strings or modify the passed
  strings for particular input.

  In particular, the empty string returns the same empty string,
  and a path ending in ``/`` returns the empty string after it.

.. lrealpath.c:25

.. function:: const char* lrealpath (const char *name)

  Given a pointer to a string containing a pathname, returns a canonical
  version of the filename.  Symlinks will be resolved, and '.' and '..'
  components will be simplified.  The returned value will be allocated using
  ``malloc``, or ``NULL`` will be returned on a memory allocation error.

.. make-relative-prefix.c:23

.. function:: const char* make_relative_prefix (const char *progname, const char *bin_prefix, const char *prefix)

  Given three paths :samp:`{progname}`, :samp:`{bin_prefix}`, :samp:`{prefix}`,
  return the path that is in the same position relative to
  :samp:`{progname}` 's directory as :samp:`{prefix}` is relative to
  :samp:`{bin_prefix}`.  That is, a string starting with the directory
  portion of :samp:`{progname}`, followed by a relative pathname of the
  difference between :samp:`{bin_prefix}` and :samp:`{prefix}`.

  If :samp:`{progname}` does not contain any directory separators,
  ``make_relative_prefix`` will search :envvar:`PATH` to find a program
  named :samp:`{progname}`.  Also, if :samp:`{progname}` is a symbolic link,
  the symbolic link will be resolved.

  For example, if :samp:`{bin_prefix}` is ``/alpha/beta/gamma/gcc/delta``,
  :samp:`{prefix}` is ``/alpha/beta/gamma/omega/``, and :samp:`{progname}` is
  ``/red/green/blue/gcc``, then this function will return
  ``/red/green/blue/../../omega/``.

  The return value is normally allocated via ``malloc``.  If no
  relative prefix can be found, return ``NULL``.

.. make-temp-file.c:173

.. function:: char* make_temp_file (const char *suffix)

  Return a temporary file name (as a string) or ``NULL`` if unable to
  create one.  :samp:`{suffix}` is a suffix to append to the file name.  The
  string is ``malloc`` ed, and the temporary file has been created.

.. memchr.c:3

.. function:: void* memchr (const void *s, int c, size_t n)

  This function searches memory starting at ``*s`` for the
  character :samp:`{c}`.  The search only ends with the first occurrence of
  :samp:`{c}`, or after :samp:`{length}` characters; in particular, a null
  character does not terminate the search.  If the character :samp:`{c}` is
  found within :samp:`{length}` characters of ``*s``, a pointer
  to the character is returned.  If :samp:`{c}` is not found, then ``NULL`` is
  returned.

.. memcmp.c:6

.. function:: int memcmp (const void *x, const void *y, size_t count)

  Compares the first :samp:`{count}` bytes of two areas of memory.  Returns
  zero if they are the same, a value less than zero if :samp:`{x}` is
  lexically less than :samp:`{y}`, or a value greater than zero if :samp:`{x}`
  is lexically greater than :samp:`{y}`.  Note that lexical order is determined
  as if comparing unsigned char arrays.

.. memcpy.c:6

.. function:: void* memcpy (void *out, const void *in, size_t length)

  Copies :samp:`{length}` bytes from memory region :samp:`{in}` to region
  :samp:`{out}`.  Returns a pointer to :samp:`{out}`.

.. memmem.c:20

.. function:: void* memmem (const void *haystack,size_t haystack_len, const void *needle, size_t needle_len)

  Returns a pointer to the first occurrence of :samp:`{needle}` (length
  :samp:`{needle_len}`) in :samp:`{haystack}` (length :samp:`{haystack_len}`).
  Returns ``NULL`` if not found.

.. memmove.c:6

.. function:: void* memmove (void *from, const void *to, size_t count)

  Copies :samp:`{count}` bytes from memory area :samp:`{from}` to memory area
  :samp:`{to}`, returning a pointer to :samp:`{to}`.

.. mempcpy.c:23

.. function:: void* mempcpy (void *out, const void *in, size_t length)

  Copies :samp:`{length}` bytes from memory region :samp:`{in}` to region
  :samp:`{out}`.  Returns a pointer to :samp:`{out}` + :samp:`{length}`.

.. memset.c:6

.. function:: void* memset (void *s, int c, size_t count)

  Sets the first :samp:`{count}` bytes of :samp:`{s}` to the constant byte
  :samp:`{c}`, returning a pointer to :samp:`{s}`.

.. mkstemps.c:60

.. function:: int mkstemps (char *pattern, int suffix_len)

  Generate a unique temporary file name from :samp:`{pattern}`.
  :samp:`{pattern}` has the form:

  .. code-block:: c++

       path/ccXXXXXXsuffix

  :samp:`{suffix_len}` tells us how long :samp:`{suffix}` is (it can be zero
  length).  The last six characters of :samp:`{pattern}` before :samp:`{suffix}`
  must be :samp:`XXXXXX`; they are replaced with a string that makes the
  filename unique.  Returns a file descriptor open on the file for
  reading and writing.

.. pexecute.txh:278

.. function:: void pex_free (struct pex_obj obj)

  Clean up and free all data associated with :samp:`{obj}`.  If you have not
  yet called ``pex_get_times`` or ``pex_get_status``, this will
  try to kill the subprocesses.

.. pexecute.txh:251

.. function:: int pex_get_status (struct pex_obj *obj, int count, int *vector)

  Returns the exit status of all programs run using :samp:`{obj}`.
  :samp:`{count}` is the number of results expected.  The results will be
  placed into :samp:`{vector}`.  The results are in the order of the calls
  to ``pex_run``.  Returns 0 on error, 1 on success.

.. pexecute.txh:261

.. function:: int pex_get_times (struct pex_obj *obj, int count, struct pex_time *vector)

  Returns the process execution times of all programs run using
  :samp:`{obj}`.  :samp:`{count}` is the number of results expected.  The
  results will be placed into :samp:`{vector}`.  The results are in the
  order of the calls to ``pex_run``.  Returns 0 on error, 1 on
  success.

  ``struct pex_time`` has the following fields of the type
  ``unsigned long`` : ``user_seconds``,
  ``user_microseconds``, ``system_seconds``,
  ``system_microseconds``.  On systems which do not support reporting
  process times, all the fields will be set to ``0``.

.. pexecute.txh:2

.. function:: struct pex_obj * pex_init (int flags, const char *pname, const char *tempbase)

  Prepare to execute one or more programs, with standard output of each
  program fed to standard input of the next.  This is a system
  independent interface to execute a pipeline.

  :samp:`{flags}` is a bitwise combination of the following:

  .. index:: PEX_RECORD_TIMES

  .. envvar:: PEX_RECORD_TIMES

    Record subprocess times if possible.

    .. index:: PEX_USE_PIPES

  .. envvar:: PEX_USE_PIPES

    Use pipes for communication between processes, if possible.

    .. index:: PEX_SAVE_TEMPS

  .. envvar:: PEX_SAVE_TEMPS

    Don't delete temporary files used for communication between
    processes.

  :samp:`{pname}` is the name of program to be executed, used in error
  messages.  :samp:`{tempbase}` is a base name to use for any required
  temporary files; it may be ``NULL`` to use a randomly chosen name.

.. pexecute.txh:161

.. function:: FILE * pex_input_file (struct pex_obj *obj, int flags, const char *in_name)

  Return a stream for a temporary file to pass to the first program in
  the pipeline as input.

  The name of the input file is chosen according to the same rules
  ``pex_run`` uses to choose output file names, based on
  :samp:`{in_name}`, :samp:`{obj}` and the ``PEX_SUFFIX`` bit in :samp:`{flags}`.

  Don't call ``fclose`` on the returned stream; the first call to
  ``pex_run`` closes it automatically.

  If :samp:`{flags}` includes ``PEX_BINARY_OUTPUT``, open the stream in
  binary mode; otherwise, open it in the default mode.  Including
  ``PEX_BINARY_OUTPUT`` in :samp:`{flags}` has no effect on Unix.

.. pexecute.txh:179

.. function:: FILE * pex_input_pipe (struct pex_obj *obj, int binary)

  Return a stream :samp:`{fp}` for a pipe connected to the standard input of
  the first program in the pipeline; :samp:`{fp}` is opened for writing.
  You must have passed ``PEX_USE_PIPES`` to the ``pex_init`` call
  that returned :samp:`{obj}`.

  You must close :samp:`{fp}` using ``fclose`` yourself when you have
  finished writing data to the pipeline.

  The file descriptor underlying :samp:`{fp}` is marked not to be inherited
  by child processes.

  On systems that do not support pipes, this function returns
  ``NULL``, and sets ``errno`` to ``EINVAL``.  If you would
  like to write code that is portable to all systems the ``pex``
  functions support, consider using ``pex_input_file`` instead.

  There are two opportunities for deadlock using
  ``pex_input_pipe`` :

  * Most systems' pipes can buffer only a fixed amount of data; a process
    that writes to a full pipe blocks.  Thus, if you write to :samp:`fp`
    before starting the first process, you run the risk of blocking when
    there is no child process yet to read the data and allow you to
    continue.  ``pex_input_pipe`` makes no promises about the
    size of the pipe's buffer, so if you need to write any data at all
    before starting the first process in the pipeline, consider using
    ``pex_input_file`` instead.

  * Using ``pex_input_pipe`` and ``pex_read_output`` together
    may also cause deadlock.  If the output pipe fills up, so that each
    program in the pipeline is waiting for the next to read more data, and
    you fill the input pipe by writing more data to :samp:`{fp}`, then there
    is no way to make progress: the only process that could read data from
    the output pipe is you, but you are blocked on the input pipe.

.. pexecute.txh:286

.. function:: const char * pex_one (int flags, const char *executable, char * const *argv, const char *pname, const char *outname, const char *errname, int *status, int *err)

  An interface to permit the easy execution of a
  single program.  The return value and most of the parameters are as
  for a call to ``pex_run``.  :samp:`{flags}` is restricted to a
  combination of ``PEX_SEARCH``, ``PEX_STDERR_TO_STDOUT``, and
  ``PEX_BINARY_OUTPUT``.  :samp:`{outname}` is interpreted as if
  ``PEX_LAST`` were set.  On a successful return, ``*status`` will
  be set to the exit status of the program.

.. pexecute.txh:237

.. function:: FILE * pex_read_err (struct pex_obj *obj, int binary)

  Returns a ``FILE`` pointer which may be used to read the standard
  error of the last program in the pipeline.  When this is used,
  ``PEX_LAST`` should not be used in a call to ``pex_run``.  After
  this is called, ``pex_run`` may no longer be called with the same
  :samp:`{obj}`.  :samp:`{binary}` should be non-zero if the file should be
  opened in binary mode.  Don't call ``fclose`` on the returned file;
  it will be closed by ``pex_free``.

.. pexecute.txh:224

.. function:: FILE * pex_read_output (struct pex_obj *obj, int binary)

  Returns a ``FILE`` pointer which may be used to read the standard
  output of the last program in the pipeline.  When this is used,
  ``PEX_LAST`` should not be used in a call to ``pex_run``.  After
  this is called, ``pex_run`` may no longer be called with the same
  :samp:`{obj}`.  :samp:`{binary}` should be non-zero if the file should be
  opened in binary mode.  Don't call ``fclose`` on the returned file;
  it will be closed by ``pex_free``.

.. pexecute.txh:34

.. function:: const char * pex_run (struct pex_obj *obj, int flags, const char *executable, char * const *argv, const char *outname, const char *errname, int *err)

  Execute one program in a pipeline.  On success this returns
  ``NULL``.  On failure it returns an error message, a statically
  allocated string.

  :samp:`{obj}` is returned by a previous call to ``pex_init``.

  :samp:`{flags}` is a bitwise combination of the following:

  .. index:: PEX_LAST

  .. envvar:: PEX_LAST

    This must be set on the last program in the pipeline.  In particular,
    it should be set when executing a single program.  The standard output
    of the program will be sent to :samp:`{outname}`, or, if :samp:`{outname}` is
    ``NULL``, to the standard output of the calling program.  Do *not*
    set this bit if you want to call ``pex_read_output``
    (described below).  After a call to ``pex_run`` with this bit set,
    :samp:`{pex_run}` may no longer be called with the same :samp:`{obj}`.

    .. index:: PEX_SEARCH

  .. envvar:: PEX_SEARCH

    Search for the program using the user's executable search path.

    .. index:: PEX_SUFFIX

  .. envvar:: PEX_SUFFIX

    :samp:`{outname}` is a suffix.  See the description of :samp:`{outname}`,
    below.

    .. index:: PEX_STDERR_TO_STDOUT

  .. envvar:: PEX_STDERR_TO_STDOUT

    Send the program's standard error to standard output, if possible.

    .. index:: PEX_BINARY_INPUT, PEX_BINARY_OUTPUT, PEX_BINARY_ERROR

  .. envvar:: PEX_BINARY_INPUT

    The standard input (output or error) of the program should be read (written) in
    binary mode rather than text mode.  These flags are ignored on systems
    which do not distinguish binary mode and text mode, such as Unix.  For
    proper behavior these flags should match appropriately---a call to
    ``pex_run`` using ``PEX_BINARY_OUTPUT`` should be followed by a
    call using ``PEX_BINARY_INPUT``.

    .. index:: PEX_STDERR_TO_PIPE

  .. envvar:: PEX_STDERR_TO_PIPE

    Send the program's standard error to a pipe, if possible.  This flag
    cannot be specified together with ``PEX_STDERR_TO_STDOUT``.  This
    flag can be specified only on the last program in pipeline.

  :samp:`{executable}` is the program to execute.  :samp:`{argv}` is the set of
  arguments to pass to the program; normally ``argv[0]`` will
  be a copy of :samp:`{executable}`.

  :samp:`{outname}` is used to set the name of the file to use for standard
  output.  There are two cases in which no output file will be used:

  * if ``PEX_LAST`` is not set in :samp:`{flags}`, and ``PEX_USE_PIPES``
    was set in the call to ``pex_init``, and the system supports pipes

  * if ``PEX_LAST`` is set in :samp:`{flags}`, and :samp:`{outname}` is
    ``NULL``

  Otherwise the code will use a file to hold standard
  output.  If ``PEX_LAST`` is not set, this file is considered to be
  a temporary file, and it will be removed when no longer needed, unless
  ``PEX_SAVE_TEMPS`` was set in the call to ``pex_init``.

  There are two cases to consider when setting the name of the file to
  hold standard output.

  * ``PEX_SUFFIX`` is set in :samp:`{flags}`.  In this case
    :samp:`{outname}` may not be ``NULL``.  If the :samp:`{tempbase}` parameter
    to ``pex_init`` was not ``NULL``, then the output file name is
    the concatenation of :samp:`{tempbase}` and :samp:`{outname}`.  If
    :samp:`{tempbase}` was ``NULL``, then the output file name is a random
    file name ending in :samp:`{outname}`.

  * ``PEX_SUFFIX`` was not set in :samp:`{flags}`.  In this
    case, if :samp:`{outname}` is not ``NULL``, it is used as the output
    file name.  If :samp:`{outname}` is ``NULL``, and :samp:`{tempbase}` was
    not NULL, the output file name is randomly chosen using
    :samp:`{tempbase}`.  Otherwise the output file name is chosen completely
    at random.

  :samp:`{errname}` is the file name to use for standard error output.  If
  it is ``NULL``, standard error is the same as the caller's.
  Otherwise, standard error is written to the named file.

  On an error return, the code sets ``*err`` to an ``errno``
  value, or to 0 if there is no relevant ``errno``.

.. pexecute.txh:145

.. function:: const char * pex_run_in_environment (struct pex_obj *obj, int flags, const char *executable, char * const *argv, char * const *env, int env_size, const char *outname, const char *errname, int *err)

  Execute one program in a pipeline, permitting the environment for the
  program to be specified.  Behaviour and parameters not listed below are
  as for ``pex_run``.

  :samp:`{env}` is the environment for the child process, specified as an array of
  character pointers.  Each element of the array should point to a string of the
  form ``VAR=VALUE``, with the exception of the last element that must be
  ``NULL``.

.. pexecute.txh:301

.. function:: int pexecute (const char *program, char * const *argv, const char *this_pname, const char *temp_base, char **errmsg_fmt, char **errmsg_arg, int flags)

  This is the old interface to execute one or more programs.  It is
  still supported for compatibility purposes, but is no longer
  documented.

.. strsignal.c:541

.. function:: void psignal (int signo, char *message)

  Print :samp:`{message}` to the standard error, followed by a colon,
  followed by the description of the signal specified by :samp:`{signo}`,
  followed by a newline.

.. putenv.c:21

.. function:: int putenv (const char *string)

  Uses ``setenv`` or ``unsetenv`` to put :samp:`{string}` into
  the environment or remove it.  If :samp:`{string}` is of the form
  :samp:`name=value` the string is added; if no :samp:`=` is present the
  name is unset/removed.

.. pexecute.txh:312

.. function:: int pwait (int pid, int *status, int flags)

  Another part of the old execution interface.

.. random.c:39

.. function:: long int random (void)
              void srandom (unsigned int seed)
              void* initstate (unsigned int seed, void *arg_state, unsigned long n)
              void* setstate (void *arg_state)

  Random number functions.  ``random`` returns a random number in the
  range 0 to ``LONG_MAX``.  ``srandom`` initializes the random
  number generator to some starting point determined by :samp:`{seed}`
  (else, the values returned by ``random`` are always the same for each
  run of the program).  ``initstate`` and ``setstate`` allow fine-grained
  control over the state of the random number generator.

.. concat.c:160

.. function:: char* reconcat (char *optr, const char *s1, ..., NULL)

  Same as ``concat``, except that if :samp:`{optr}` is not ``NULL`` it
  is freed after the string is created.  This is intended to be useful
  when you're extending an existing string or building up a string in a
  loop:

  .. code-block:: c++

      str = reconcat (str, "pre-", str, NULL);

.. rename.c:6

.. function:: int rename (const char *old, const char *new)

  Renames a file from :samp:`{old}` to :samp:`{new}`.  If :samp:`{new}` already
  exists, it is removed.

.. rindex.c:5

.. function:: char* rindex (const char *s, int c)

  Returns a pointer to the last occurrence of the character :samp:`{c}` in
  the string :samp:`{s}`, or ``NULL`` if not found.  The use of ``rindex`` is
  deprecated in new programs in favor of ``strrchr``.

.. setenv.c:22

.. function:: int setenv (const char *name, const char *value, int overwrite)
              void unsetenv (const char *name)

  ``setenv`` adds :samp:`{name}` to the environment with value
  :samp:`{value}`.  If the name was already present in the environment,
  the new value will be stored only if :samp:`{overwrite}` is nonzero.
  The companion ``unsetenv`` function removes :samp:`{name}` from the
  environment.  This implementation is not safe for multithreaded code.

.. setproctitle.c:31

.. function:: void setproctitle (const char *fmt, ...)

  Set the title of a process to :samp:`{fmt}`. va args not supported for now,
  but defined for compatibility with BSD.

.. strsignal.c:348

.. function:: int signo_max (void)

  Returns the maximum signal value for which a corresponding symbolic
  name or message is available.  Note that in the case where we use the
  ``sys_siglist`` supplied by the system, it is possible for there to
  be more symbolic names than messages, or vice versa.  In fact, the
  manual page for ``psignal(3b)`` explicitly warns that one should
  check the size of the table (``NSIG``) before indexing it, since
  new signal codes may be added to the system before they are added to
  the table.  Thus ``NSIG`` might be smaller than value implied by
  the largest signo value defined in ``<signal.h>``.

  We return the maximum value that can be used to obtain a meaningful
  symbolic name or message.

.. sigsetmask.c:8

.. function:: int sigsetmask (int set)

  Sets the signal mask to the one provided in :samp:`{set}` and returns
  the old mask (which, for libiberty's implementation, will always
  be the value ``1``).

.. simple-object.txh:96

.. function:: const char * simple_object_attributes_compare   (simple_object_attributes *attrs1, simple_object_attributes *attrs2, int *err)

  Compare :samp:`{attrs1}` and :samp:`{attrs2}`.  If they could be linked
  together without error, return ``NULL``.  Otherwise, return an
  error message and set ``*err`` to an errno value or ``0``
  if there is no relevant errno.

.. simple-object.txh:81

.. function:: simple_object_attributes * simple_object_fetch_attributes   (simple_object_read *simple_object, const char **errmsg, int *err)

  Fetch the attributes of :samp:`{simple_object}`.  The attributes are
  internal information such as the format of the object file, or the
  architecture it was compiled for.  This information will persist until
  ``simple_object_attributes_release`` is called, even if
  :samp:`{simple_object}` itself is released.

  On error this returns ``NULL``, sets ``*errmsg`` to an
  error message, and sets ``*err`` to an errno value or
  ``0`` if there is no relevant errno.

.. simple-object.txh:49

.. function:: int simple_object_find_section (simple_object_read *simple_object, off_t *offset, off_t *length, const char **errmsg, int *err)

  Look for the section :samp:`{name}` in :samp:`{simple_object}`.  This returns
  information for the first section with that name.

  If found, return 1 and set ``*offset`` to the offset in the
  file of the section contents and set ``*length`` to the
  length of the section contents.  The value in ``*offset``
  will be relative to the offset passed to
  ``simple_object_open_read``.

  If the section is not found, and no error occurs,
  ``simple_object_find_section`` returns ``0`` and set
  ``*errmsg`` to ``NULL``.

  If an error occurs, ``simple_object_find_section`` returns
  ``0``, sets ``*errmsg`` to an error message, and sets
  ``*err`` to an errno value or ``0`` if there is no
  relevant errno.

.. simple-object.txh:27

.. function:: const char * simple_object_find_sections   (simple_object_read *simple_object, int (*pfn) (void *data, const char *name, off_t offset, off_t length), void *data, int *err)

  This function calls :samp:`{pfn}` for each section in :samp:`{simple_object}`.
  It calls :samp:`{pfn}` with the section name, the offset within the file
  of the section contents, and the length of the section contents.  The
  offset within the file is relative to the offset passed to
  ``simple_object_open_read``.  The :samp:`{data}` argument to this
  function is passed along to :samp:`{pfn}`.

  If :samp:`{pfn}` returns ``0``, the loop over the sections stops and
  ``simple_object_find_sections`` returns.  If :samp:`{pfn}` returns some
  other value, the loop continues.

  On success ``simple_object_find_sections`` returns.  On error it
  returns an error string, and sets ``*err`` to an errno value
  or ``0`` if there is no relevant errno.

.. simple-object.txh:2

.. function:: simple_object_read * simple_object_open_read   (int descriptor, off_t offset, const char *segment_name, const char **errmsg, int *err)

  Opens an object file for reading.  Creates and returns an
  ``simple_object_read`` pointer which may be passed to other
  functions to extract data from the object file.

  :samp:`{descriptor}` holds a file descriptor which permits reading.

  :samp:`{offset}` is the offset into the file; this will be ``0`` in the
  normal case, but may be a different value when reading an object file
  in an archive file.

  :samp:`{segment_name}` is only used with the Mach-O file format used on
  Darwin aka Mac OS X.  It is required on that platform, and means to
  only look at sections within the segment with that name.  The
  parameter is ignored on other systems.

  If an error occurs, this functions returns ``NULL`` and sets
  ``*errmsg`` to an error string and sets ``*err`` to
  an errno value or ``0`` if there is no relevant errno.

.. simple-object.txh:107

.. function:: void simple_object_release_attributes   (simple_object_attributes *attrs)

  Release all resources associated with :samp:`{attrs}`.

.. simple-object.txh:73

.. function:: void simple_object_release_read   (simple_object_read *simple_object)

  Release all resources associated with :samp:`{simple_object}`.  This does
  not close the file descriptor.

.. simple-object.txh:184

.. function:: void simple_object_release_write   (simple_object_write *simple_object)

  Release all resources associated with :samp:`{simple_object}`.

.. simple-object.txh:114

.. function:: simple_object_write * simple_object_start_write   (simple_object_attributes attrs, const char *segment_name, const char **errmsg, int *err)

  Start creating a new object file using the object file format
  described in :samp:`{attrs}`.  You must fetch attribute information from
  an existing object file before you can create a new one.  There is
  currently no support for creating an object file de novo.

  :samp:`{segment_name}` is only used with Mach-O as found on Darwin aka Mac
  OS X.  The parameter is required on that target.  It means that all
  sections are created within the named segment.  It is ignored for
  other object file formats.

  On error ``simple_object_start_write`` returns ``NULL``, sets
  ``*ERRMSG`` to an error message, and sets ``*err``
  to an errno value or ``0`` if there is no relevant errno.

.. simple-object.txh:153

.. function:: const char * simple_object_write_add_data   (simple_object_write *simple_object, simple_object_write_section *section, const void *buffer, size_t size, int copy, int *err)

  Add data :samp:`{buffer}` / :samp:`{size}` to :samp:`{section}` in
  :samp:`{simple_object}`.  If :samp:`{copy}` is non-zero, the data will be
  copied into memory if necessary.  If :samp:`{copy}` is zero, :samp:`{buffer}`
  must persist until ``simple_object_write_to_file`` is called.  is
  released.

  On success this returns ``NULL``.  On error this returns an error
  message, and sets ``*err`` to an errno value or 0 if there is
  no relevant erro.

.. simple-object.txh:134

.. function:: simple_object_write_section * simple_object_write_create_section   (simple_object_write *simple_object, const char *name, unsigned int align, const char **errmsg, int *err)

  Add a section to :samp:`{simple_object}`.  :samp:`{name}` is the name of the
  new section.  :samp:`{align}` is the required alignment expressed as the
  number of required low-order 0 bits (e.g., 2 for alignment to a 32-bit
  boundary).

  The section is created as containing data, readable, not writable, not
  executable, not loaded at runtime.  The section is not written to the
  file until ``simple_object_write_to_file`` is called.

  On error this returns ``NULL``, sets ``*errmsg`` to an
  error message, and sets ``*err`` to an errno value or
  ``0`` if there is no relevant errno.

.. simple-object.txh:170

.. function:: const char * simple_object_write_to_file   (simple_object_write *simple_object, int descriptor, int *err)

  Write the complete object file to :samp:`{descriptor}`, an open file
  descriptor.  This writes out all the data accumulated by calls to
  ``simple_object_write_create_section`` and
  :samp:`{simple_object_write_add_data}`.

  This returns ``NULL`` on success.  On error this returns an error
  message and sets ``*err`` to an errno value or ``0`` if
  there is no relevant errno.

.. snprintf.c:28

.. function:: int snprintf (char *buf, size_t n, const char *format, ...)

  This function is similar to ``sprintf``, but it will write to
  :samp:`{buf}` at most ``n-1`` bytes of text, followed by a
  terminating null byte, for a total of :samp:`{n}` bytes.
  On error the return value is -1, otherwise it returns the number of
  bytes, not including the terminating null byte, that would have been
  written had :samp:`{n}` been sufficiently large, regardless of the actual
  value of :samp:`{n}`.  Note some pre-C99 system libraries do not implement
  this correctly so users cannot generally rely on the return value if
  the system version of this function is used.

.. spaces.c:22

.. function:: char* spaces (int count)

  Returns a pointer to a memory region filled with the specified
  number of spaces and null terminated.  The returned pointer is
  valid until at least the next call.

.. splay-tree.c:305

.. function:: splay_tree splay_tree_new_with_typed_alloc (splay_tree_compare_fn compare_fn, splay_tree_delete_key_fn delete_key_fn, splay_tree_delete_value_fn delete_value_fn, splay_tree_allocate_fn tree_allocate_fn, splay_tree_allocate_fn node_allocate_fn, splay_tree_deallocate_fn deallocate_fn, void * allocate_data)

  This function creates a splay tree that uses two different allocators
  :samp:`{tree_allocate_fn}` and :samp:`{node_allocate_fn}` to use for allocating the
  tree itself and its nodes respectively.  This is useful when variables of
  different types need to be allocated with different allocators.

  The splay tree will use :samp:`{compare_fn}` to compare nodes,
  :samp:`{delete_key_fn}` to deallocate keys, and :samp:`{delete_value_fn}` to
  deallocate values.  Keys and values will be deallocated when the
  tree is deleted using splay_tree_delete or when a node is removed
  using splay_tree_remove.  splay_tree_insert will release the previously
  inserted key and value using :samp:`{delete_key_fn}` and :samp:`{delete_value_fn}`
  if the inserted key is already found in the tree.

.. stack-limit.c:28

.. function:: void stack_limit_increase (unsigned long pref)

  Attempt to increase stack size limit to :samp:`{pref}` bytes if possible.

.. stpcpy.c:23

.. function:: char* stpcpy (char *dst, const char *src)

  Copies the string :samp:`{src}` into :samp:`{dst}`.  Returns a pointer to
  :samp:`{dst}` + strlen(:samp:`{src}`).

.. stpncpy.c:23

.. function:: char* stpncpy (char *dst, const char *src, size_t len)

  Copies the string :samp:`{src}` into :samp:`{dst}`, copying exactly :samp:`{len}`
  and padding with zeros if necessary.  If :samp:`{len}` < strlen(:samp:`{src}`)
  then return :samp:`{dst}` + :samp:`{len}`, otherwise returns :samp:`{dst}` +
  strlen(:samp:`{src}`).

.. strcasecmp.c:15

.. function:: int strcasecmp (const char *s1, const char *s2)

  A case-insensitive ``strcmp``.

.. strchr.c:6

.. function:: char* strchr (const char *s, int c)

  Returns a pointer to the first occurrence of the character :samp:`{c}` in
  the string :samp:`{s}`, or ``NULL`` if not found.  If :samp:`{c}` is itself the
  null character, the results are undefined.

.. strdup.c:3

.. function:: char* strdup (const char *s)

  Returns a pointer to a copy of :samp:`{s}` in memory obtained from
  ``malloc``, or ``NULL`` if insufficient memory was available.

.. strerror.c:675

.. function:: const char* strerrno (int errnum)

  Given an error number returned from a system call (typically returned
  in ``errno``), returns a pointer to a string containing the
  symbolic name of that error number, as found in ``<errno.h>``.

  If the supplied error number is within the valid range of indices for
  symbolic names, but no name is available for the particular error
  number, then returns the string :samp:`Error {num}`, where :samp:`{num}`
  is the error number.

  If the supplied error number is not within the range of valid
  indices, then returns ``NULL``.

  The contents of the location pointed to are only guaranteed to be
  valid until the next call to ``strerrno``.

.. strerror.c:608

.. function:: char* strerror (int errnoval)

  Maps an ``errno`` number to an error message string, the contents
  of which are implementation defined.  On systems which have the
  external variables ``sys_nerr`` and ``sys_errlist``, these
  strings will be the same as the ones used by ``perror``.

  If the supplied error number is within the valid range of indices for
  the ``sys_errlist``, but no message is available for the particular
  error number, then returns the string :samp:`Error {num}`, where
  :samp:`{num}` is the error number.

  If the supplied error number is not a valid index into
  ``sys_errlist``, returns ``NULL``.

  The returned string is only guaranteed to be valid only until the
  next call to ``strerror``.

.. strncasecmp.c:15

.. function:: int strncasecmp (const char *s1, const char *s2)

  A case-insensitive ``strncmp``.

.. strncmp.c:6

.. function:: int strncmp (const char *s1, const char *s2, size_t n)

  Compares the first :samp:`{n}` bytes of two strings, returning a value as
  ``strcmp``.

.. strndup.c:23

.. function:: char* strndup (const char *s, size_t n)

  Returns a pointer to a copy of :samp:`{s}` with at most :samp:`{n}` characters
  in memory obtained from ``malloc``, or ``NULL`` if insufficient
  memory was available.  The result is always NUL terminated.

.. strnlen.c:6

.. function:: size_t strnlen (const char *s, size_t maxlen)

  Returns the length of :samp:`{s}`, as with ``strlen``, but never looks
  past the first :samp:`{maxlen}` characters in the string.  If there is no
  '\0' character in the first :samp:`{maxlen}` characters, returns
  :samp:`{maxlen}`.

.. strrchr.c:6

.. function:: char* strrchr (const char *s, int c)

  Returns a pointer to the last occurrence of the character :samp:`{c}` in
  the string :samp:`{s}`, or ``NULL`` if not found.  If :samp:`{c}` is itself the
  null character, the results are undefined.

.. strsignal.c:383

.. function:: const char * strsignal (int signo)

  Maps an signal number to an signal message string, the contents of
  which are implementation defined.  On systems which have the external
  variable ``sys_siglist``, these strings will be the same as the
  ones used by ``psignal()``.

  If the supplied signal number is within the valid range of indices for
  the ``sys_siglist``, but no message is available for the particular
  signal number, then returns the string :samp:`Signal {num}`, where
  :samp:`{num}` is the signal number.

  If the supplied signal number is not a valid index into
  ``sys_siglist``, returns ``NULL``.

  The returned string is only guaranteed to be valid only until the next
  call to ``strsignal``.

.. strsignal.c:448

.. function:: const char* strsigno (int signo)

  Given an signal number, returns a pointer to a string containing the
  symbolic name of that signal number, as found in ``<signal.h>``.

  If the supplied signal number is within the valid range of indices for
  symbolic names, but no name is available for the particular signal
  number, then returns the string :samp:`Signal {num}`, where
  :samp:`{num}` is the signal number.

  If the supplied signal number is not within the range of valid
  indices, then returns ``NULL``.

  The contents of the location pointed to are only guaranteed to be
  valid until the next call to ``strsigno``.

.. strstr.c:6

.. function:: char* strstr (const char *string, const char *sub)

  This function searches for the substring :samp:`{sub}` in the string
  :samp:`{string}`, not including the terminating null characters.  A pointer
  to the first occurrence of :samp:`{sub}` is returned, or ``NULL`` if the
  substring is absent.  If :samp:`{sub}` points to a string with zero
  length, the function returns :samp:`{string}`.

.. strtod.c:27

.. function:: double strtod (const char *string, char **endptr)

  This ISO C function converts the initial portion of :samp:`{string}` to a
  ``double``.  If :samp:`{endptr}` is not ``NULL``, a pointer to the
  character after the last character used in the conversion is stored in
  the location referenced by :samp:`{endptr}`.  If no conversion is
  performed, zero is returned and the value of :samp:`{string}` is stored in
  the location referenced by :samp:`{endptr}`.

.. strerror.c:734

.. function:: int strtoerrno (const char *name)

  Given the symbolic name of a error number (e.g., ``EACCES``), map it
  to an errno value.  If no translation is found, returns 0.

.. strtol.c:33

.. function:: long int strtol (const char *string, char **endptr, int base)
              unsigned long int strtoul (const char *string, char **endptr, int base)

  The ``strtol`` function converts the string in :samp:`{string}` to a
  long integer value according to the given :samp:`{base}`, which must be
  between 2 and 36 inclusive, or be the special value 0.  If :samp:`{base}`
  is 0, ``strtol`` will look for the prefixes ``0`` and ``0x``
  to indicate bases 8 and 16, respectively, else default to base 10.
  When the base is 16 (either explicitly or implicitly), a prefix of
  ``0x`` is allowed.  The handling of :samp:`{endptr}` is as that of
  ``strtod`` above.  The ``strtoul`` function is the same, except
  that the converted value is unsigned.

.. strtoll.c:33

.. function:: long long int strtoll (const char *string, char **endptr, int base)
              unsigned long long int strtoull (const char *string, char **endptr, int base)

  The ``strtoll`` function converts the string in :samp:`{string}` to a
  long long integer value according to the given :samp:`{base}`, which must be
  between 2 and 36 inclusive, or be the special value 0.  If :samp:`{base}`
  is 0, ``strtoll`` will look for the prefixes ``0`` and ``0x``
  to indicate bases 8 and 16, respectively, else default to base 10.
  When the base is 16 (either explicitly or implicitly), a prefix of
  ``0x`` is allowed.  The handling of :samp:`{endptr}` is as that of
  ``strtod`` above.  The ``strtoull`` function is the same, except
  that the converted value is unsigned.

.. strsignal.c:502

.. function:: int strtosigno (const char *name)

  Given the symbolic name of a signal, map it to a signal number.  If no
  translation is found, returns 0.

.. strverscmp.c:25

.. function:: int strverscmp (const char *s1, const char *s2)

  The ``strverscmp`` function compares the string :samp:`{s1}` against
  :samp:`{s2}`, considering them as holding indices/version numbers.  Return
  value follows the same conventions as found in the ``strverscmp``
  function.  In fact, if :samp:`{s1}` and :samp:`{s2}` contain no digits,
  ``strverscmp`` behaves like ``strcmp``.

  Basically, we compare strings normally (character by character), until
  we find a digit in each string - then we enter a special comparison
  mode, where each sequence of digits is taken as a whole.  If we reach the
  end of these two parts without noticing a difference, we return to the
  standard comparison mode.  There are two types of numeric parts:
  "integral" and "fractional" (those  begin with a '0'). The types
  of the numeric parts affect the way we sort them:

  * integral/integral: we compare values as you would expect.

  * fractional/integral: the fractional part is less than the integral one.
    Again, no surprise.

  * fractional/fractional: the things become a bit more complex.
    If the common prefix contains only leading zeroes, the longest part is less
    than the other one; else the comparison behaves normally.

  .. code-block::

    strverscmp ("no digit", "no digit")
        ⇒ 0    // same behavior as strcmp.
    strverscmp ("item#99", "item#100")
        ⇒ <0   // same prefix, but 99 < 100.
    strverscmp ("alpha1", "alpha001")
        ⇒ >0   // fractional part inferior to integral one.
    strverscmp ("part1_f012", "part1_f01")
        ⇒ >0   // two fractional parts.
    strverscmp ("foo.009", "foo.0")
        ⇒ <0   // idem, but with leading zeroes only.

  This function is especially useful when dealing with filename sorting,
  because filenames frequently hold indices/version numbers.

.. timeval-utils.c:43

.. function:: void timeval_add (struct timeval *a, struct timeval *b, struct timeval *result)

  Adds :samp:`{a}` to :samp:`{b}` and stores the result in :samp:`{result}`.

.. timeval-utils.c:67

.. function:: void timeval_sub (struct timeval *a, struct timeval *b, struct timeval *result)

  Subtracts :samp:`{b}` from :samp:`{a}` and stores the result in :samp:`{result}`.

.. tmpnam.c:3

.. function:: char* tmpnam (char *s)

  This function attempts to create a name for a temporary file, which
  will be a valid file name yet not exist when ``tmpnam`` checks for
  it.  :samp:`{s}` must point to a buffer of at least ``L_tmpnam`` bytes,
  or be ``NULL``.  Use of this function creates a security risk, and it must
  not be used in new projects.  Use ``mkstemp`` instead.

.. unlink-if-ordinary.c:27

.. function:: int unlink_if_ordinary (const char*)

  Unlinks the named file, unless it is special (e.g. a device file).
  Returns 0 when the file was unlinked, a negative value (and errno set) when
  there was an error deleting the file, and a positive value if no attempt
  was made to unlink the file because it is special.

.. fopen_unlocked.c:31

.. function:: void unlock_std_streams (void)

  If the OS supports it, ensure that the standard I/O streams,
  ``stdin``, ``stdout`` and ``stderr`` are setup to avoid any
  multi-threaded locking.  Otherwise do nothing.

.. fopen_unlocked.c:23

.. function:: void unlock_stream (FILE * stream)

  If the OS supports it, ensure that the supplied stream is setup to
  avoid any multi-threaded locking.  Otherwise leave the ``FILE``
  pointer unchanged.  If the :samp:`{stream}` is ``NULL`` do nothing.

.. vasprintf.c:47

.. function:: int vasprintf (char **resptr, const char *format, va_list args)

  Like ``vsprintf``, but instead of passing a pointer to a buffer,
  you pass a pointer to a pointer.  This function will compute the size
  of the buffer needed, allocate memory with ``malloc``, and store a
  pointer to the allocated memory in ``*resptr``.  The value
  returned is the same as ``vsprintf`` would return.  If memory could
  not be allocated, minus one is returned and ``NULL`` is stored in
  ``*resptr``.

.. vfork.c:6

.. function:: int vfork (void)

  Emulates ``vfork`` by calling ``fork`` and returning its value.

.. vprintf.c:3

.. function:: int vprintf (const char *format, va_list ap)
              int vfprintf (FILE *stream, const char *format, va_list ap)
              int vsprintf (char *str, const char *format, va_list ap)

  These functions are the same as ``printf``, ``fprintf``, and
  ``sprintf``, respectively, except that they are called with a
  ``va_list`` instead of a variable number of arguments.  Note that
  they do not call ``va_end`` ; this is the application's
  responsibility.  In ``libiberty`` they are implemented in terms of the
  nonstandard but common function ``_doprnt``.

.. vsnprintf.c:28

.. function:: int vsnprintf (char *buf, size_t n, const char *format, va_list ap)

  This function is similar to ``vsprintf``, but it will write to
  :samp:`{buf}` at most ``n-1`` bytes of text, followed by a
  terminating null byte, for a total of :samp:`{n}` bytes.  On error the
  return value is -1, otherwise it returns the number of characters that
  would have been printed had :samp:`{n}` been sufficiently large,
  regardless of the actual value of :samp:`{n}`.  Note some pre-C99 system
  libraries do not implement this correctly so users cannot generally
  rely on the return value if the system version of this function is
  used.

.. waitpid.c:3

.. function:: int waitpid (int pid, int *status, int)

  This is a wrapper around the ``wait`` function.  Any 'special'
  values of :samp:`{pid}` depend on your implementation of ``wait``, as
  does the return value.  The third argument is unused in ``libiberty``.

.. argv.c:289

.. function:: int writeargv (char * const *argv, FILE *file)

  Write each member of ARGV, handling all necessary quoting, to the file
  named by FILE, separated by whitespace.  Return 0 on success, non-zero
  if an error occurred while writing to FILE.

.. xasprintf.c:31

.. function:: char* xasprintf (const char *format, ...)

  Print to allocated string without fail.  If ``xasprintf`` fails,
  this will print a message to ``stderr`` (using the name set by
  ``xmalloc_set_program_name``, if any) and then call ``xexit``.

.. xatexit.c:11

.. function:: int xatexit (void (*fn) (void))

  Behaves as the standard ``atexit`` function, but with no limit on
  the number of registered functions.  Returns 0 on success, or -1 on
  failure.  If you use ``xatexit`` to register functions, you must use
  ``xexit`` to terminate your program.

.. xmalloc.c:38

.. function:: void* xcalloc (size_t nelem, size_t elsize)

  Allocate memory without fail, and set it to zero.  This routine functions
  like ``calloc``, but will behave the same as ``xmalloc`` if memory
  cannot be found.

.. xexit.c:22

.. function:: void xexit (int code)

  Terminates the program.  If any functions have been registered with
  the ``xatexit`` replacement function, they will be called first.
  Termination is handled via the system's normal ``exit`` call.

.. xmalloc.c:22

.. function:: void* xmalloc (size_t)

  Allocate memory without fail.  If ``malloc`` fails, this will print
  a message to ``stderr`` (using the name set by
  ``xmalloc_set_program_name``,
  if any) and then call ``xexit``.  Note that it is therefore safe for
  a program to contain ``#define malloc xmalloc`` in its source.

.. xmalloc.c:53

.. function:: void xmalloc_failed (size_t)

  This function is not meant to be called by client code, and is listed
  here for completeness only.  If any of the allocation routines fail, this
  function will be called to print an error message and terminate execution.

.. xmalloc.c:46

.. function:: void xmalloc_set_program_name (const char *name)

  You can use this to set the name of the program used by
  ``xmalloc_failed`` when printing a failure message.

.. xmemdup.c:7

.. function:: void* xmemdup (void *input, size_t copy_size, size_t alloc_size)

  Duplicates a region of memory without fail.  First, :samp:`{alloc_size}` bytes
  are allocated, then :samp:`{copy_size}` bytes from :samp:`{input}` are copied into
  it, and the new memory is returned.  If fewer bytes are copied than were
  allocated, the remaining memory is zeroed.

.. xmalloc.c:32

.. function:: void* xrealloc (void *ptr, size_t size)

  Reallocate memory without fail.  This routine functions like ``realloc``,
  but will behave the same as ``xmalloc`` if memory cannot be found.

.. xstrdup.c:7

.. function:: char* xstrdup (const char *s)

  Duplicates a character string without fail, using ``xmalloc`` to
  obtain memory.

.. xstrerror.c:7

.. function:: char* xstrerror (int errnum)

  Behaves exactly like the standard ``strerror`` function, but
  will never return a ``NULL`` pointer.

.. xstrndup.c:23

.. function:: char* xstrndup (const char *s, size_t n)

  Returns a pointer to a copy of :samp:`{s}` with at most :samp:`{n}` characters
  without fail, using ``xmalloc`` to obtain memory.  The result is
  always NUL terminated.

.. xvasprintf.c:38

.. function:: char* xvasprintf (const char *format, va_list args)

  Print to allocated string without fail.  If ``xvasprintf`` fails,
  this will print a message to ``stderr`` (using the name set by
  ``xmalloc_set_program_name``, if any) and then call ``xexit``.