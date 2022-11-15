..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: extensions, functions, extension

.. _extensions:

Extensions
**********

``libiberty`` includes additional functionality above and beyond standard
functions, which has proven generically useful in GNU programs, such as
obstacks and regex.  These functions are often copied from other
projects as they gain popularity, and are included here to provide a
central location from which to use, maintain, and distribute them.

.. toctree::
  :maxdepth: 2


.. This is generated from the glibc manual using contrib/make-obstacks-texi.pl

.. index:: obstacks

.. _obstacks:

Obstacks
^^^^^^^^

An :dfn:`obstack` is a pool of memory containing a stack of objects.  You
can create any number of separate obstacks, and then allocate objects in
specified obstacks.  Within each obstack, the last object allocated must
always be the first one freed, but distinct obstacks are independent of
each other.

Aside from this one constraint of order of freeing, obstacks are totally
general: an obstack can contain any number of objects of any size.  They
are implemented with macros, so allocation is usually very fast as long as
the objects are usually small.  And the only space overhead per object is
the padding needed to start each object on a suitable boundary.

.. toctree::
  :maxdepth: 2


.. _creating-obstacks:

Creating Obstacks
~~~~~~~~~~~~~~~~~

The utilities for manipulating obstacks are declared in the header
file :samp:`obstack.h`.

.. index:: obstack.h, struct obstack

Data Type struct obstackAn obstack is represented by a data structure of type ``struct
obstack``.  This structure has a small fixed size; it records the status
of the obstack and how to find the space in which objects are allocated.
It does not contain any of the objects themselves.  You should not try
to access the contents of the structure directly; use only the macros
described in this chapter.

You can declare variables of type ``struct obstack`` and use them as
obstacks, or you can allocate obstacks dynamically like any other kind
of object.  Dynamic allocation of obstacks allows your program to have a
variable number of different stacks.  (You can even allocate an
obstack structure in another obstack, but this is rarely useful.)

All the macros that work with obstacks require you to specify which
obstack to use.  You do this with a pointer of type ``struct obstack
*``.  In the following, we often say 'an obstack' when strictly
speaking the object at hand is such a pointer.

The objects in the obstack are packed into large blocks called
:dfn:`chunks`.  The ``struct obstack`` structure points to a chain of
the chunks currently in use.

The obstack library obtains a new chunk whenever you allocate an object
that won't fit in the previous chunk.  Since the obstack library manages
chunks automatically, you don't need to pay much attention to them, but
you do need to supply a function which the obstack library should use to
get a chunk.  Usually you supply a function which uses ``malloc``
directly or indirectly.  You must also supply a function to free a chunk.
These matters are described in the following section.

.. _preparing-for-obstacks:

Preparing for Using Obstacks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each source file in which you plan to use obstacks
must include the header file :samp:`obstack.h`, like this:

.. code-block:: c++

  #include <obstack.h>

.. index:: obstack_chunk_alloc, obstack_chunk_free

Also, if the source file uses the macro ``obstack_init``, it must
declare or define two macros that will be called by the
obstack library.  One, ``obstack_chunk_alloc``, is used to allocate
the chunks of memory into which objects are packed.  The other,
``obstack_chunk_free``, is used to return chunks when the objects in
them are freed.  These macros should appear before any use of obstacks
in the source file.

Usually these are defined to use ``malloc`` via the intermediary
``xmalloc`` (see `Unconstrained Allocation <https://www.gnu.org/software/libc/manual/html_node/Unconstrained-Allocation.html>`_
in The GNU C Library Reference Manual).  This is done with
the following pair of macro definitions:

.. code-block:: c++

  #define obstack_chunk_alloc xmalloc
  #define obstack_chunk_free free

Though the memory you get using obstacks really comes from ``malloc``,
using obstacks is faster because ``malloc`` is called less often, for
larger blocks of memory.  See :ref:`obstack-chunks`, for full details.

At run time, before the program can use a ``struct obstack`` object
as an obstack, it must initialize the obstack by calling
``obstack_init`` or one of its variants, ``obstack_begin``,
``obstack_specify_allocation``, or
``obstack_specify_allocation_with_arg``.

.. function:: int obstack_init (struct obstack *obstack_ptr)

  Initialize obstack :samp:`{obstack_ptr}` for allocation of objects.  This
  macro calls the obstack's ``obstack_chunk_alloc`` function.  If
  allocation of memory fails, the function pointed to by
  ``obstack_alloc_failed_handler`` is called.  The ``obstack_init``
  macro always returns 1 (Compatibility notice: Former versions of
  obstack returned 0 if allocation failed).

Here are two examples of how to allocate the space for an obstack and
initialize it.  First, an obstack that is a static variable:

.. code-block:: c++

  static struct obstack myobstack;
  ...
  obstack_init (&myobstack);

Second, an obstack that is itself dynamically allocated:

.. code-block:: c++

  struct obstack *myobstack_ptr
    = (struct obstack *) xmalloc (sizeof (struct obstack));

  obstack_init (myobstack_ptr);

.. function:: int obstack_begin (struct obstack *obstack_ptr, size_t chunk_size)

  Like ``obstack_init``, but specify chunks to be at least
  :samp:`{chunk_size}` bytes in size.

.. function:: int obstack_specify_allocation (struct obstack *obstack_ptr, size_t chunk_size, size_t alignment, void *(*chunkfun) (size_t), void (*freefun) (void *))

  Like ``obstack_init``, specifying chunk size, chunk
  alignment, and memory allocation functions.  A :samp:`{chunk_size}` or
  :samp:`{alignment}` of zero results in the default size or alignment
  respectively being used.

.. function:: int obstack_specify_allocation_with_arg (struct obstack *obstack_ptr, size_t chunk_size, size_t alignment, void *(*chunkfun) (void *, size_t), void (*freefun) (void *, void *), void *arg)

  Like ``obstack_specify_allocation``, but specifying memory
  allocation functions that take an extra first argument, :samp:`{arg}`.

.. index:: obstack_alloc_failed_handler

Variable obstack_alloc_failed_handlerThe value of this variable is a pointer to a function that
``obstack`` uses when ``obstack_chunk_alloc`` fails to allocate
memory.  The default action is to print a message and abort.
You should supply a function that either calls ``exit``
(see `Program Termination <https://www.gnu.org/software/libc/manual/html_node/Program-Termination.html>`_ in The GNU C Library Reference Manual)
or ``longjmp`` and doesn't return.

.. code-block:: c++

  void my_obstack_alloc_failed (void)
  ...
  obstack_alloc_failed_handler = &my_obstack_alloc_failed;

.. index:: allocation (obstacks)

.. _allocation-in-an-obstack:

Allocation in an Obstack
~~~~~~~~~~~~~~~~~~~~~~~~

The most direct way to allocate an object in an obstack is with
``obstack_alloc``, which is invoked almost like ``malloc``.

.. function:: void * obstack_alloc (struct obstack *obstack_ptr, size_t size)

  This allocates an uninitialized block of :samp:`{size}` bytes in an obstack
  and returns its address.  Here :samp:`{obstack_ptr}` specifies which obstack
  to allocate the block in; it is the address of the ``struct obstack``
  object which represents the obstack.  Each obstack macro
  requires you to specify an :samp:`{obstack_ptr}` as the first argument.

  This macro calls the obstack's ``obstack_chunk_alloc`` function if
  it needs to allocate a new chunk of memory; it calls
  ``obstack_alloc_failed_handler`` if allocation of memory by
  ``obstack_chunk_alloc`` failed.

For example, here is a function that allocates a copy of a string :samp:`{str}`
in a specific obstack, which is in the variable ``string_obstack`` :

.. code-block:: c++

  struct obstack string_obstack;

  char *
  copystring (char *string)
  {
    size_t len = strlen (string) + 1;
    char *s = (char *) obstack_alloc (&string_obstack, len);
    memcpy (s, string, len);
    return s;
  }

To allocate a block with specified contents, use the macro ``obstack_copy``.

.. function:: void * obstack_copy (struct obstack *obstack_ptr, void *address, size_t size)

  This allocates a block and initializes it by copying :samp:`{size}`
  bytes of data starting at :samp:`{address}`.  It calls
  ``obstack_alloc_failed_handler`` if allocation of memory by
  ``obstack_chunk_alloc`` failed.

.. function:: void * obstack_copy0 (struct obstack *obstack_ptr, void *address, size_t size)

  Like ``obstack_copy``, but appends an extra byte containing a null
  character.  This extra byte is not counted in the argument :samp:`{size}`.

The ``obstack_copy0`` macro is convenient for copying a sequence
of characters into an obstack as a null-terminated string.  Here is an
example of its use:

.. code-block:: c++

  char *
  obstack_savestring (char *addr, size_t size)
  {
    return obstack_copy0 (&myobstack, addr, size);
  }

Contrast this with the previous example of ``savestring`` using
``malloc`` see (`Basic Allocation <http://www.gnu.org/software/libc/manual/html_node/Basic-Allocation.html#Basic-Allocation>`_).

.. index:: freeing (obstacks)

.. _freeing-obstack-objects:

Freeing Objects in an Obstack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To free an object allocated in an obstack, use the macro
``obstack_free``.  Since the obstack is a stack of objects, freeing
one object automatically frees all other objects allocated more recently
in the same obstack.

.. function:: void obstack_free (struct obstack *obstack_ptr, void *object)

  If :samp:`{object}` is a null pointer, everything allocated in the obstack
  is freed.  Otherwise, :samp:`{object}` must be the address of an object
  allocated in the obstack.  Then :samp:`{object}` is freed, along with
  everything allocated in :samp:`{obstack}` since :samp:`{object}`.

Note that if :samp:`{object}` is a null pointer, the result is an
uninitialized obstack.  To free all memory in an obstack but leave it
valid for further allocation, call ``obstack_free`` with the address
of the first object allocated on the obstack:

.. code-block:: c++

  obstack_free (obstack_ptr, first_object_allocated_ptr);

Recall that the objects in an obstack are grouped into chunks.  When all
the objects in a chunk become free, the obstack library automatically
frees the chunk (see :ref:`preparing-for-obstacks`).  Then other
obstacks, or non-obstack allocation, can reuse the space of the chunk.

.. index:: macros

.. _obstack-functions:

Obstack Functions and Macros
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The interfaces for using obstacks are shown here as functions to
specify the return type and argument types, but they are really
defined as macros.  This means that the arguments don't actually have
types, but they generally behave as if they have the types shown.
You can call these macros like functions, but you cannot use them in
any other way (for example, you cannot take their address).

Calling the macros requires a special precaution: namely, the first
operand (the obstack pointer) may not contain any side effects, because
it may be computed more than once.  For example, if you write this:

.. code-block:: c++

  obstack_alloc (get_obstack (), 4);

you will find that ``get_obstack`` may be called several times.
If you use ``*obstack_list_ptr++`` as the obstack pointer argument,
you will get very strange results since the incrementation may occur
several times.

If you use the GNU C compiler, this precaution is not necessary, because
various language extensions in GNU C permit defining the macros so as to
compute each argument only once.

Note that arguments other than the first will only be evaluated once,
even when not using GNU C.

``obstack.h`` does declare a number of functions,
``_obstack_begin``, ``_obstack_begin_1``,
``_obstack_newchunk``, ``_obstack_free``, and
``_obstack_memory_used``.  You should not call these directly.

.. index:: growing objects (in obstacks), changing the size of a block (obstacks)

.. _growing-objects:

Growing Objects
~~~~~~~~~~~~~~~

Because memory in obstack chunks is used sequentially, it is possible to
build up an object step by step, adding one or more bytes at a time to the
end of the object.  With this technique, you do not need to know how much
data you will put in the object until you come to the end of it.  We call
this the technique of :dfn:`growing objects`.  The special macros
for adding data to the growing object are described in this section.

You don't need to do anything special when you start to grow an object.
Using one of the macros to add data to the object automatically
starts it.  However, it is necessary to say explicitly when the object is
finished.  This is done with ``obstack_finish``.

The actual address of the object thus built up is not known until the
object is finished.  Until then, it always remains possible that you will
add so much data that the object must be copied into a new chunk.

While the obstack is in use for a growing object, you cannot use it for
ordinary allocation of another object.  If you try to do so, the space
already added to the growing object will become part of the other object.

.. function:: void obstack_blank (struct obstack *obstack_ptr, size_t size)

  The most basic macro for adding to a growing object is
  ``obstack_blank``, which adds space without initializing it.

.. function:: void obstack_grow (struct obstack *obstack_ptr, void *data, size_t size)

  To add a block of initialized space, use ``obstack_grow``, which is
  the growing-object analogue of ``obstack_copy``.  It adds :samp:`{size}`
  bytes of data to the growing object, copying the contents from
  :samp:`{data}`.

.. function:: void obstack_grow0 (struct obstack *obstack_ptr, void *data, size_t size)

  This is the growing-object analogue of ``obstack_copy0``.  It adds
  :samp:`{size}` bytes copied from :samp:`{data}`, followed by an additional null
  character.

.. function:: void obstack_1grow (struct obstack *obstack_ptr, char c)

  To add one character at a time, use ``obstack_1grow``.
  It adds a single byte containing :samp:`{c}` to the growing object.

.. function:: void obstack_ptr_grow (struct obstack *obstack_ptr, void *data)

  Adding the value of a pointer one can use
  ``obstack_ptr_grow``.  It adds ``sizeof (void *)`` bytes
  containing the value of :samp:`{data}`.

.. function:: void obstack_int_grow (struct obstack *obstack_ptr, int data)

  A single value of type ``int`` can be added by using
  ``obstack_int_grow``.  It adds ``sizeof (int)`` bytes to
  the growing object and initializes them with the value of :samp:`{data}`.

.. function:: void * obstack_finish (struct obstack *obstack_ptr)

  When you are finished growing the object, use
  ``obstack_finish`` to close it off and return its final address.

  Once you have finished the object, the obstack is available for ordinary
  allocation or for growing another object.

When you build an object by growing it, you will probably need to know
afterward how long it became.  You need not keep track of this as you grow
the object, because you can find out the length from the obstack
with ``obstack_object_size``, before finishing the object.

.. function:: size_t obstack_object_size (struct obstack *obstack_ptr)

  This macro returns the current size of the growing object, in bytes.
  Remember to call ``obstack_object_size`` *before* finishing the object.
  After it is finished, ``obstack_object_size`` will return zero.

If you have started growing an object and wish to cancel it, you should
finish it and then free it, like this:

.. code-block:: c++

  obstack_free (obstack_ptr, obstack_finish (obstack_ptr));

This has no effect if no object was growing.

.. index:: efficiency and obstacks

.. _extra-fast-growing:

Extra Fast Growing Objects
~~~~~~~~~~~~~~~~~~~~~~~~~~

The usual macros for growing objects incur overhead for checking
whether there is room for the new growth in the current chunk.  If you
are frequently constructing objects in small steps of growth, this
overhead can be significant.

You can reduce the overhead by using special 'fast growth'
macros that grow the object without checking.  In order to have a
robust program, you must do the checking yourself.  If you do this checking
in the simplest way each time you are about to add data to the object, you
have not saved anything, because that is what the ordinary growth
macros do.  But if you can arrange to check less often, or check
more efficiently, then you make the program faster.

``obstack_room`` returns the amount of room available
in the current chunk.

.. function:: size_t obstack_room (struct obstack *obstack_ptr)

  This returns the number of bytes that can be added safely to the current
  growing object (or to an object about to be started) in obstack
  :samp:`{obstack}` using the fast growth macros.

While you know there is room, you can use these fast growth macros
for adding data to a growing object:

.. function:: void obstack_1grow_fast (struct obstack *obstack_ptr, char c)

  ``obstack_1grow_fast`` adds one byte containing the
  character :samp:`{c}` to the growing object in obstack :samp:`{obstack_ptr}`.

.. function:: void obstack_ptr_grow_fast (struct obstack *obstack_ptr, void *data)

  ``obstack_ptr_grow_fast`` adds ``sizeof (void *)``
  bytes containing the value of :samp:`{data}` to the growing object in
  obstack :samp:`{obstack_ptr}`.

.. function:: void obstack_int_grow_fast (struct obstack *obstack_ptr, int data)

  ``obstack_int_grow_fast`` adds ``sizeof (int)`` bytes
  containing the value of :samp:`{data}` to the growing object in obstack
  :samp:`{obstack_ptr}`.

.. function:: void obstack_blank_fast (struct obstack *obstack_ptr, size_t size)

  ``obstack_blank_fast`` adds :samp:`{size}` bytes to the
  growing object in obstack :samp:`{obstack_ptr}` without initializing them.

When you check for space using ``obstack_room`` and there is not
enough room for what you want to add, the fast growth macros
are not safe.  In this case, simply use the corresponding ordinary
growth macro instead.  Very soon this will copy the object to a
new chunk; then there will be lots of room available again.

So, each time you use an ordinary growth macro, check afterward for
sufficient space using ``obstack_room``.  Once the object is copied
to a new chunk, there will be plenty of space again, so the program will
start using the fast growth macros again.

Here is an example:

.. code-block:: c++

  void
  add_string (struct obstack *obstack, const char *ptr, size_t len)
  {
    while (len > 0)
      {
        size_t room = obstack_room (obstack);
        if (room == 0)
          {
            /* Not enough room.  Add one character slowly,
               which may copy to a new chunk and make room.  */
            obstack_1grow (obstack, *ptr++);
            len--;
          }
        else
          {
            if (room > len)
              room = len;
            /* Add fast as much as we have room for. */
            len -= room;
            while (room-- > 0)
              obstack_1grow_fast (obstack, *ptr++);
          }
      }
  }

.. index:: shrinking objects

You can use ``obstack_blank_fast`` with a 'negative' size
argument to make the current object smaller.  Just don't try to shrink
it beyond zero length---there's no telling what will happen if you do
that.  Earlier versions of obstacks allowed you to use
``obstack_blank`` to shrink objects.  This will no longer work.

.. index:: obstack status, status of obstack

.. _status-of-an-obstack:

Status of an Obstack
~~~~~~~~~~~~~~~~~~~~

Here are macros that provide information on the current status of
allocation in an obstack.  You can use them to learn about an object while
still growing it.

.. function:: void * obstack_base (struct obstack *obstack_ptr)

  This macro returns the tentative address of the beginning of the
  currently growing object in :samp:`{obstack_ptr}`.  If you finish the object
  immediately, it will have that address.  If you make it larger first, it
  may outgrow the current chunk---then its address will change!

  If no object is growing, this value says where the next object you
  allocate will start (once again assuming it fits in the current
  chunk).

.. function:: void * obstack_next_free (struct obstack *obstack_ptr)

  This macro returns the address of the first free byte in the current
  chunk of obstack :samp:`{obstack_ptr}`.  This is the end of the currently
  growing object.  If no object is growing, ``obstack_next_free``
  returns the same value as ``obstack_base``.

.. function:: size_t obstack_object_size (struct obstack *obstack_ptr)

  This macro returns the size in bytes of the currently growing object.
  This is equivalent to

  .. code-block:: c++

    ((size_t) (obstack_next_free (obstack_ptr) - obstack_base (obstack_ptr)))

.. index:: alignment (in obstacks)

.. _obstacks-data-alignment:

Alignment of Data in Obstacks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each obstack has an :dfn:`alignment boundary`; each object allocated in
the obstack automatically starts on an address that is a multiple of the
specified boundary.  By default, this boundary is aligned so that
the object can hold any type of data.

To access an obstack's alignment boundary, use the macro
``obstack_alignment_mask``.

.. function:: size_t obstack_alignment_mask (struct obstack *obstack_ptr)

  The value is a bit mask; a bit that is 1 indicates that the corresponding
  bit in the address of an object should be 0.  The mask value should be one
  less than a power of 2; the effect is that all object addresses are
  multiples of that power of 2.  The default value of the mask is a value
  that allows aligned objects to hold any type of data: for example, if
  its value is 3, any type of data can be stored at locations whose
  addresses are multiples of 4.  A mask value of 0 means an object can start
  on any multiple of 1 (that is, no alignment is required).

  The expansion of the macro ``obstack_alignment_mask`` is an lvalue,
  so you can alter the mask by assignment.  For example, this statement:

  .. code-block:: c++

    obstack_alignment_mask (obstack_ptr) = 0;

  has the effect of turning off alignment processing in the specified obstack.

Note that a change in alignment mask does not take effect until
*after* the next time an object is allocated or finished in the
obstack.  If you are not growing an object, you can make the new
alignment mask take effect immediately by calling ``obstack_finish``.
This will finish a zero-length object and then do proper alignment for
the next object.

.. index:: efficiency of chunks, chunks

.. _obstack-chunks:

Obstack Chunks
~~~~~~~~~~~~~~

Obstacks work by allocating space for themselves in large chunks, and
then parceling out space in the chunks to satisfy your requests.  Chunks
are normally 4096 bytes long unless you specify a different chunk size.
The chunk size includes 8 bytes of overhead that are not actually used
for storing objects.  Regardless of the specified size, longer chunks
will be allocated when necessary for long objects.

The obstack library allocates chunks by calling the function
``obstack_chunk_alloc``, which you must define.  When a chunk is no
longer needed because you have freed all the objects in it, the obstack
library frees the chunk by calling ``obstack_chunk_free``, which you
must also define.

These two must be defined (as macros) or declared (as functions) in each
source file that uses ``obstack_init`` (see :ref:`creating-obstacks`).
Most often they are defined as macros like this:

.. code-block:: c++

  #define obstack_chunk_alloc malloc
  #define obstack_chunk_free free

Note that these are simple macros (no arguments).  Macro definitions with
arguments will not work!  It is necessary that ``obstack_chunk_alloc``
or ``obstack_chunk_free``, alone, expand into a function name if it is
not itself a function name.

If you allocate chunks with ``malloc``, the chunk size should be a
power of 2.  The default chunk size, 4096, was chosen because it is long
enough to satisfy many typical requests on the obstack yet short enough
not to waste too much memory in the portion of the last chunk not yet used.

.. function:: size_t obstack_chunk_size (struct obstack *obstack_ptr)

  This returns the chunk size of the given obstack.

Since this macro expands to an lvalue, you can specify a new chunk size by
assigning it a new value.  Doing so does not affect the chunks already
allocated, but will change the size of chunks allocated for that particular
obstack in the future.  It is unlikely to be useful to make the chunk size
smaller, but making it larger might improve efficiency if you are
allocating many objects whose size is comparable to the chunk size.  Here
is how to do so cleanly:

.. code-block:: c++

  if (obstack_chunk_size (obstack_ptr) < new-chunk-size)
    obstack_chunk_size (obstack_ptr) = new-chunk-size;

.. _summary-of-obstacks:

Summary of Obstack Macros
~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a summary of all the macros associated with obstacks.  Each
takes the address of an obstack (``struct obstack *``) as its first
argument.

.. function:: int obstack_init (struct obstack *obstack_ptr)

  Initialize use of an obstack.  See :ref:`creating-obstacks`.

.. function:: int obstack_begin (struct obstack *obstack_ptr, size_t chunk_size)

  Initialize use of an obstack, with an initial chunk of
  :samp:`{chunk_size}` bytes.

.. function:: int obstack_specify_allocation (struct obstack *obstack_ptr, size_t chunk_size, size_t alignment, void *(*chunkfun) (size_t), void (*freefun) (void *))

  Initialize use of an obstack, specifying intial chunk size, chunk
  alignment, and memory allocation functions.

.. function:: int obstack_specify_allocation_with_arg (struct obstack *obstack_ptr, size_t chunk_size, size_t alignment, void *(*chunkfun) (void *, size_t), void (*freefun) (void *, void *), void *arg)

  Like ``obstack_specify_allocation``, but specifying memory
  allocation functions that take an extra first argument, :samp:`{arg}`.

.. function:: void *obstack_alloc (struct obstack *obstack_ptr, size_t size)

  Allocate an object of :samp:`{size}` uninitialized bytes.
  See :ref:`allocation-in-an-obstack`.

.. function:: void *obstack_copy (struct obstack *obstack_ptr, void *address, size_t size)

  Allocate an object of :samp:`{size}` bytes, with contents copied from
  :samp:`{address}`.  See :ref:`allocation-in-an-obstack`.

.. function:: void *obstack_copy0 (struct obstack *obstack_ptr, void *address, size_t size)

  Allocate an object of :samp:`{size}` +1 bytes, with :samp:`{size}` of them copied
  from :samp:`{address}`, followed by a null character at the end.
  See :ref:`allocation-in-an-obstack`.

.. function:: void obstack_free (struct obstack *obstack_ptr, void *object)

  Free :samp:`{object}` (and everything allocated in the specified obstack
  more recently than :samp:`{object}`).  See :ref:`freeing-obstack-objects`.

.. function:: void obstack_blank (struct obstack *obstack_ptr, size_t size)

  Add :samp:`{size}` uninitialized bytes to a growing object.
  See :ref:`growing-objects`.

.. function:: void obstack_grow (struct obstack *obstack_ptr, void *address, size_t size)

  Add :samp:`{size}` bytes, copied from :samp:`{address}`, to a growing object.
  See :ref:`growing-objects`.

.. function:: void obstack_grow0 (struct obstack *obstack_ptr, void *address, size_t size)

  Add :samp:`{size}` bytes, copied from :samp:`{address}`, to a growing object,
  and then add another byte containing a null character.  See :ref:`growing-objects`.

.. function:: void obstack_1grow (struct obstack *obstack_ptr, char data_char)

  Add one byte containing :samp:`{data-char}` to a growing object.
  See :ref:`growing-objects`.

.. function:: void *obstack_finish (struct obstack *obstack_ptr)

  Finalize the object that is growing and return its permanent address.
  See :ref:`growing-objects`.

.. function:: size_t obstack_object_size (struct obstack *obstack_ptr)

  Get the current size of the currently growing object.  See :ref:`growing-objects`.

.. function:: void obstack_blank_fast (struct obstack *obstack_ptr, size_t size)

  Add :samp:`{size}` uninitialized bytes to a growing object without checking
  that there is enough room.  See :ref:`extra-fast-growing`.

.. function:: void obstack_1grow_fast (struct obstack *obstack_ptr, char data_char)

  Add one byte containing :samp:`{data-char}` to a growing object without
  checking that there is enough room.  See :ref:`extra-fast-growing`.

.. function:: size_t obstack_room (struct obstack *obstack_ptr)

  Get the amount of room now available for growing the current object.
  See :ref:`extra-fast-growing`.

.. function:: size_t obstack_alignment_mask (struct obstack *obstack_ptr)

  The mask used for aligning the beginning of an object.  This is an
  lvalue.  See :ref:`obstacks-data-alignment`.

.. function:: size_t obstack_chunk_size (struct obstack *obstack_ptr)

  The size for allocating chunks.  This is an lvalue.  See :ref:`obstack-chunks`.

.. function:: void *obstack_base (struct obstack *obstack_ptr)

  Tentative starting address of the currently growing object.
  See :ref:`status-of-an-obstack`.

.. function:: void *obstack_next_free (struct obstack *obstack_ptr)

  Address just after the end of the currently growing object.
  See :ref:`status-of-an-obstack`.
