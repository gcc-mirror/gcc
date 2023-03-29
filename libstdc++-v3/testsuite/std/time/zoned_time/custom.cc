// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }

#include <chrono>
#include <set>
#include <stdexcept>
#include <testsuite_hooks.h>

struct local_tz
{
  local_tz() : name(std::chrono::current_zone()->name()) { }

  explicit local_tz(std::string_view name) : name(name) { }

  template<typename Dur>
    std::chrono::sys_time<Dur> to_sys(const std::chrono::local_time<Dur>& d)
    { return std::chrono::locate_zone(name)->to_sys(d); }

  template<typename Dur>
    std::chrono::sys_time<Dur> to_local(const std::chrono::sys_time<Dur>& d)
    { return std::chrono::locate_zone(name)->to_sys(d); }

  template<typename Dur>
    std::chrono::sys_info get_info(const std::chrono::sys_time<Dur>& d)
    { return std::chrono::locate_zone(name)->get_info(d); }

  struct indirect_cmp
  {
    bool operator()(const local_tz* lhs, const local_tz* rhs) const
    { return lhs->name < rhs->name; }
  };

  bool eq(const std::chrono::time_zone* tz) const noexcept
  { return name == tz->name(); }

private:
  std::string_view name;
};

template<> struct std::chrono::zoned_traits<const local_tz*>
{
  static const local_tz* default_zone()
  {
    return locate_zone(std::chrono::current_zone()->name());
  }

  static const local_tz* locate_zone(std::string_view name)
  {
    static std::set<const local_tz*, local_tz::indirect_cmp> zones;
    local_tz tz(name);
    if (auto z = zones.find(&tz); z != zones.end())
      return *z;
    if (std::chrono::locate_zone(name))
      return *zones.insert(new local_tz(tz)).first;
    throw std::runtime_error("zone not found");
  }
};

void
test_custom_tzptr()
{
  using namespace std::chrono;

  zoned_time<seconds, const local_tz*> z;
  VERIFY( z.get_time_zone()->eq(std::chrono::current_zone()) );

  zoned_time<seconds, const local_tz*> z2(std::string_view("Europe/London"));
  VERIFY( z2.get_time_zone()->eq(std::chrono::locate_zone("Europe/London")) );
}

int main()
{
  test_custom_tzptr();
}
