// { dg-do run { target c++17 } }

// Bug 128264 pmr::unsynchronized_pool_resource::allocate crashes instead
// of throwing when upstream fails in _Pool::replenish

#include <memory_resource>
#include <new>

// Keep allocating from the memory_resource until it throws bad_alloc.
static void
run_to_failure(std::pmr::memory_resource& upstream)
{
  std::pmr::pool_options opts;
  // Set pool options so that we replenish pools often.
  opts.max_blocks_per_chunk = 4;
  opts.largest_required_pool_block = 128;

  try
  {
    std::pmr::unsynchronized_pool_resource res(opts, &upstream);
    while (true)
      for (int n = 0; n < 152; n += 8)
	(void) res.allocate(n);
  }
  catch (const std::bad_alloc&)
  { }
}

static void
test_pr128264_limit_by_size()
{
  const int bufsz = 10240;
  char* buf = new char[bufsz];

  for (int limit = 0; limit < bufsz; limit += 16)
  {
    // Upstream resource that uses a fixed-size buffer then fails.
    std::pmr::monotonic_buffer_resource res(buf, limit,
					    std::pmr::null_memory_resource());
    run_to_failure(res);
  }
  delete[] buf;
}

// Upstream resource that succeeds for the first N allocations and then throws.
struct failing_resource : std::pmr::memory_resource
{
  explicit failing_resource(int n) : remaining(n) { }

  int remaining;

  void* do_allocate(std::size_t bytes, std::size_t align) override
  {
    if (remaining <= 0)
      throw std::bad_alloc();
    --remaining;
    return std::pmr::new_delete_resource()->allocate(bytes, align);
  }

  void do_deallocate(void* p, std::size_t bytes, std::size_t align) override
  { std::pmr::new_delete_resource()->deallocate(p, bytes, align); }

  bool do_is_equal(const std::pmr::memory_resource& r) const noexcept override
  { return this == &r; }
};

static void
test_pr128264_limit_by_count()
{
  for (int limit = 0; limit < 200; ++limit)
  {
    // Upstream resource that allocates a fixed number of times, then fails.
    failing_resource res(limit);
    run_to_failure(res);
  }
}

int main()
{
  test_pr128264_limit_by_size();
  test_pr128264_limit_by_count();
}
