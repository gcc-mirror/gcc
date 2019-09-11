// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include "../../include/std/pmroptional"

struct pmr_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;
};

struct pmr_type2{};
namespace std{
	template<>
    struct uses_allocator<pmr_type2, std::pmr::polymorphic_allocator<void>>
    : true_type { };
}
struct non_pmr_type
{
  typedef int allocator_type;
};


static_assert(!std::uses_allocator<std::pmr::optional<int>,std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::uses_allocator<std::pmr::optional<pmr_type>,std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::uses_allocator<std::pmr::optional<pmr_type2>,std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(!std::uses_allocator<std::pmr::optional<non_pmr_type>,std::pmr::polymorphic_allocator<void>>{}, "");

