/* Copyright (C) 2026 Free Software Foundation, Inc.
   This file is part of GCC, distributed under the GNU General Public License,
   version 3 or (at your option) any later version.  See COPYING3.  */

#include "../perform-stack.h"
#include <functional>
#include "../encodings.h"
#undef NDEBUG
#include <cassert>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <new>

extern "C" void __gg__pseudo_return_push(void *, size_t);
extern "C" size_t __gg__pseudo_return_pop();
extern "C" void __gg__pseudo_return_bookmark();
extern "C" size_t __gg__debug_pseudo_return_bookmark();
extern "C" void __gg__pseudo_return_flush();
extern "C" void __gg__init_program_state(cbl_encoding_t, cbl_encoding_t);
extern "C" size_t __gg__unique_prog_id;
extern "C" void *__gg__exit_address;
extern "C" void __gg__module_name_push(const char *);
extern "C" void __gg__push_program_state();


/* Fail the second vector allocation once, after the signature push succeeds.
   The executable's replacement operator also serves allocations in libgcobol. */
static int fail_after = -1;
void *operator new(size_t size)
  {
  if( fail_after == 0 )
    {
    fail_after = -1;
    throw std::bad_alloc();
    }
  if( fail_after > 0 )
    {
    fail_after -= 1;
    }
  if( void *memory = std::malloc(size ? size : 1) )
    {
    return memory;
    }
  throw std::bad_alloc();
  }
void operator delete(void *memory) noexcept { std::free(memory); }
void operator delete(void *memory, size_t) noexcept { std::free(memory); }

static void
check(size_t depth, size_t invocations, uint32_t generation)
  {
  const auto &view = *__gg__debug_perform_stack;
  assert(view.version == 1);
  assert(view.byte_size == sizeof(view));
  assert(view.word_size == sizeof(uintptr_t));
  assert(view.generation == generation && !(generation & 1));
  assert(view.signature_count == depth && view.index_count == depth);
  assert(view.bookmark_count == invocations);
  assert(!depth || (view.signatures && view.indexes));
  assert(!invocations || view.bookmarks);
  }

int
main()
  {
  uint32_t generation = __gg__debug_perform_stack->generation;
  check(0, 0, generation);
  assert(__gg__debug_pseudo_return_bookmark() == 1);
  check(0, 1, generation += 2);
  fail_after = 1;
  bool failed = false;
  try
    {
    __gg__pseudo_return_push(reinterpret_cast<void *>(0x1000), 7);
    }
  catch(const std::bad_alloc &)
    {
    failed = true;
    }
  assert(failed);
  check(0, 1, generation += 2);

  for( size_t i = 0; i < 2048; i += 1 )
    {
    __gg__pseudo_return_push(reinterpret_cast<void *>(0x1000 + i * 16), i);
    check(i + 1, 1, generation += 2);
    auto *signatures = reinterpret_cast<void *const *>(__gg__debug_perform_stack->signatures);
    auto *indexes = reinterpret_cast<const size_t *>(__gg__debug_perform_stack->indexes);
    assert(signatures[i] == reinterpret_cast<void *>(0x1000 + i * 16));
    assert(indexes[i] == i);
    }
  for( size_t i = 0; i < 512; i += 1 )
    {
    assert(__gg__debug_pseudo_return_bookmark() == i + 2);
    check(2048, i + 2, generation += 2);
    auto *bookmarks = reinterpret_cast<const size_t *>(__gg__debug_perform_stack->bookmarks);
    assert(bookmarks[i + 1] == 2048);
    }
  for( size_t i = 512; i > 0; i -= 1 )
    {
    __gg__pseudo_return_flush();
    check(2048, i, generation += 2);
    }
  for( size_t i = 2048; i > 0; i -= 1 )
    {
    assert(__gg__pseudo_return_pop() == i - 1);
    check(i - 1, 1, generation += 2);
    }
  __gg__pseudo_return_push(reinterpret_cast<void *>(0x9000), 99);
  check(1, 1, generation += 2);
  __gg__pseudo_return_bookmark();
  check(1, 2, generation += 2);
  __gg__pseudo_return_push(reinterpret_cast<void *>(0xa000), 100);
  check(2, 2, generation += 2);
  __gg__pseudo_return_flush();
  check(1, 1, generation += 2);
  assert(__gg__pseudo_return_pop() == 99);
  check(0, 1, generation += 2);
  __gg__pseudo_return_flush();
  check(0, 0, generation += 2);

  const_cast<gg_debug_perform_stack_v1 *>(__gg__debug_perform_stack)->generation = UINT32_MAX - 1;
  __gg__pseudo_return_bookmark();
  check(0, 1, 0);
  __gg__pseudo_return_flush();
  check(0, 0, 2);
  __gg__init_program_state(iconv_CP1252_e, iconv_UTF_16LE_e);
  int main_counter = 1;
  int child_counter = 0;
  __gg__unique_prog_id = 1;
  assert(__gg__debug_pseudo_return_bookmark() == 1);
  __gg__module_name_push("Tmain");
  __gg__pseudo_return_push(reinterpret_cast<void *>(0x1000), 1);
  __gg__push_program_state();
  __gg__unique_prog_id += 1;
  assert(__gg__debug_pseudo_return_bookmark() == 2);
  __gg__module_name_push("Tchild");
  __gg__pseudo_return_push(reinterpret_cast<void *>(0x2000), 2);
  generation = __gg__debug_perform_stack->generation;
  assert(__gg__debug_perform_unwind(0, 0, 0) == 1);
  assert(__gg__debug_perform_unwind(3, 0, 0) == 1);
  assert(__gg__debug_perform_unwind(1, 2, 0) == 1);
  assert(__gg__debug_perform_unwind(1, 1, 1) == 1);
  check(2, 2, generation);
  assert(__gg__unique_prog_id == 2 && main_counter == 1);
  assert(__gg__debug_perform_unwind(1, 0, 0, &child_counter) == 0);
  check(0, 1, generation += 2);
  assert(__gg__unique_prog_id == 1 && main_counter == 1);
  assert(__gg__exit_address == nullptr);
  __gg__pseudo_return_push(reinterpret_cast<void *>(0x3000), 3);
  assert(__gg__debug_perform_unwind(1, 0, 1, &main_counter) == 0);
  check(0, 0, generation += 4);
  assert(__gg__unique_prog_id == 0 && main_counter == 0);
  assert(__gg__exit_address == nullptr);

  /* Invocation identities are reusable after forced return.  An unavailable
     compiler counter rejects cleanup without changing published state.  */
  __gg__unique_prog_id = 1;
  __gg__pseudo_return_bookmark();
  __gg__module_name_push("Told");
  generation = __gg__debug_perform_stack->generation;
  assert(__gg__debug_perform_unwind(1, 0, 1, static_cast<int *>(nullptr)) == 2);
  check(0, 1, generation);
  __gg__pseudo_return_flush();
  __gg__unique_prog_id = 0;
  std::puts("PASS: PERFORM stack publication, relocation, rollback, wraparound and forced return");
  }
