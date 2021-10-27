#include <string>
#include <string_view>
#include <map>
#include <set>
#include <algorithm>
#include <iterator>
#include <iostream>

// This is a slow larval-stage kludge to help massage the generated man
// pages.  It's used like this:
const std::string_view usage = R"(
Takes on stdin, whitespace-separated words of the form

    [bits/]stl_foo.h
    [bits/]std_foo.h

and writes on stdout the nearest matching standard header name.

Takes no command-line arguments.
)";

// List of standard headers
std::set<std::string_view> std_headers;
// Map of partial header filenames to standard headers.
std::map<std::string_view, std::string_view>  headers;

void init_map()
{
    // Enter the glamourous world of data entry!!  Maintain these!
    // Because the map_header function removes common prefixes and suffixes,
    // a header "bits/st[dl]_foo.h" will automatically map to "foo" if that
    // is a standard header, so we don't need to list those cases here.
    headers["atomic_base.h"]            = "atomic";
    headers["atomic_lockfree_defines.h"] = "atomic";
    headers["atomic_timed_wait.h"]      = "atomic";
    headers["atomic_wait.h"]            = "atomic";
    headers["algorithmfwd.h"]           = "algorithm";
    headers["algo.h"]                   = "algorithm";
    headers["algobase.h"]               = "algorithm";
    headers["ranges_algo.h"]            = "algorithm";
    headers["ranges_algobase.h"]        = "algorithm";
    headers["heap.h"]                   = "algorithm";
    headers["exception_ptr.h"]          = "exception";
    headers["nested_exception.h"]       = "exception";
    headers["fs_dir.h"]                 = "filesystem";
    headers["fs_fwd.h"]                 = "filesystem";
    headers["fs_ops.h"]                 = "filesystem";
    headers["fs_path.h"]                = "filesystem";
    headers["binders.h"]                = "functional";
    headers["function.h"]               = "functional";
    headers["functional_hash.h"]        = "functional";
    headers["mofunc_impl.h"]            = "functional";
    headers["move_only_function.h"]     = "functional";
    headers["invoke.h"]                 = "functional";
    headers["refwrap.h"]                = "functional";
    headers["quoted_string.h"]          = "iomanip";
    headers["ios_base.h"]               = "ios";
    headers["basic_ios.h"]              = "ios";
    headers["basic_ios.tcc"]            = "ios";
    headers["iosfwd.h"]                 = "iosfwd";
    headers["iostream.h"]               = "iostream";
    headers["iterator_base_funcs.h"]    = "iterator";
    headers["iterator_base_types.h"]    = "iterator";
    headers["stream_iterator.h"]        = "iterator";
    headers["streambuf_iterator.h"]     = "iterator";
    headers["iterator_concepts.h"]      = "iterator";
    headers["range_access.h"]           = "iterator";
    headers["codecvt.h"]                = "locale";
    headers["c++locale.h"]              = "locale";
    headers["localefwd.h"]              = "locale";
    headers["ctype_base.h"]             = "locale";
    headers["locale_classes.h"]         = "locale";
    headers["locale_classes.tcc"]       = "locale";
    headers["locale_facets.h"]          = "locale";
    headers["locale_facets.tcc"]        = "locale";
    headers["locale_facets_nonio.h"]    = "locale";
    headers["locale_facets_nonio.tcc"]  = "locale";
    headers["locale_conv.h"]            = "locale";
    headers["multimap.h"]               = "map";
    headers["memoryfwd.h"]              = "memory";
    headers["align.h"]                  = "memory";
    headers["alloc_traits.h"]           = "memory";
    headers["auto_ptr.h"]		= "memory";
    headers["construct.h"]              = "memory";
    headers["allocator.h"]              = "memory";
    headers["raw_storage_iter.h"]       = "memory";
    headers["tempbuf.h"]                = "memory";
    headers["uninitialized.h"]          = "memory";
    headers["shared_ptr.h"]             = "memory";
    headers["shared_ptr_base.h"]        = "memory";
    headers["shared_ptr_atomic.h"]      = "memory";
    headers["unique_ptr.h"]             = "memory";
    headers["ranges_uninitialized.h"]   = "memory";
    headers["ptr_traits.h"]             = "memory";
    headers["uses_allocator.h"]         = "memory";
    headers["uses_allocator_args.h"]    = "memory";
    headers["unique_lock.h"]            = "mutex";
    headers["uniform_int_dist.h"]       = "random";
    headers["ranges_base.h"]            = "ranges";
    headers["ranges_util.h"]            = "ranges";
    headers["ranges_cmp.h"]             = "functional";
    headers["regex_automaton.h"]        = "regex";
    headers["regex_automaton.tcc"]      = "regex";
    headers["regex_compiler.h"]         = "regex";
    headers["regex_compiler.tcc"]       = "regex";
    headers["regex_constants.h"]        = "regex";
    headers["regex_error.h"]            = "regex";
    headers["regex_executor.h"]         = "regex";
    headers["regex_executor.tcc"]       = "regex";
    headers["regex_scanner.h"]          = "regex";
    headers["regex_scanner.tcc"]        = "regex";
    headers["semaphore_base.h"]         = "semaphore";
    headers["multiset.h"]               = "set";
    headers["node_handle.h"]            = "set";
    headers["functexcept.h"]            = "stdexcept";
    headers["char_traits.h"]            = "string";
    headers["stringfwd.h"]              = "string";
    headers["postypes.h"]               = "string";
    headers["basic_string.h"]           = "string";
    headers["basic_string.tcc"]         = "string";
    headers["cow_string.h"]             = "string";
    headers["string_view.tcc"]          = "string_view";
    headers["this_thread_sleep.h"]      = "thread";
    headers["tree.h"]                   = "map";
    headers["pair.h"]                   = "utility";
    headers["relops.h"]                 = "utility";
    headers["gslice.h"]                 = "valarray";
    headers["gslice_array.h"]           = "valarray";
    headers["indirect_array.h"]         = "valarray";
    headers["mask_array.h"]             = "valarray";
    headers["slice_array.h"]            = "valarray";
    headers["valarray_after.h"]         = "valarray";
    headers["valarray_before.h"]        = "valarray";
    headers["valarray_array.h"]         = "valarray";
    headers["valarray_array.tcc"]       = "valarray";
    headers["valarray_meta.h"]          = "valarray";
    headers["bvector.h"]                = "vector";

    //headers["concurrence.h"]             who knows
    //headers["atomicity.h"]               who knows

    headers["abs.h"]                    = "cstdlib";
    headers["specfun.h"]                = "cmath";

    // This list is complete as of the October 2021 working draft.
    std_headers = {
	"algorithm", "any", "array", "atomic",
	"barrier", "bit", "bitset",
	"charconv", "chrono", "codecvt", "compare", "complex",
	"concepts", "condition_variable", "coroutine",
	"deque",
	"exception", "execution",
	"filesystem", "format", "forward_list", "fstream",
	"functional", "future",
	"initializer_list", "iomanip", "ios", "iosfwd",
	"iostream", "istream", "iterator",
	"latch", "limits", "list", "locale",
	"map", "memory", "memory_resource", "mutex",
	"new", "numbers", "numeric",
	"optional", "ostream",
	"queue",
	"random", "ranges", "ratio", "regex",
	"scoped_allocator", "semaphore", "set", "shared_mutex",
	"source_location", "span", "spanstream", "sstream",
	"stack", "stacktrace", "stdexcept", "stop_token",
	"streambuf", "string", "string_view", "strstream",
	"syncstream", "system_error",
	"thread", "tuple", "typeindex", "typeinfo", "type_traits",
	"unordered_map", "unordered_set", "utility",
	"valarray", "variant", "vector", "version",

	"cassert", "cctype", "cerrno", "cfenv", "cfloat",
	"cinttypes", "climits", "clocale", "cmath", "csetjmp",
	"csignal", "cstdarg", "cstddef", "cstdint", "cstdio",
	"cstdlib", "cstring", "ctime", "cuchar", "cwchar",
	"cwctype",

	"assert.h", "ctype.h", "errno.h", "fenv.h", "float.h",
	"inttypes.h", "limits.h", "locale.h", "math.h", "setjmp.h",
	"signal.h", "stdarg.h", "stddef.h", "stdint.h", "stdio.h",
	"stdlib.h", "string.h", "time.h", "uchar.h", "wchar.h",
	"wctype.h",
    };

    // In case we missed any:
    for (const auto& h : headers)
	std_headers.insert(h.second);
}


std::string_view map_header (std::string_view header)
{
    // if it doesn't contain a "." then it's already a std header
    if (!header.contains('.'))
    {
	// make sure it's in the set:
	std_headers.insert(header);
	return header;
    }

    for (std::string_view prefix : {"bits/", "stl_", "std_"})
	if (header.starts_with(prefix))
	    header.remove_prefix(prefix.size());

    if (auto it = headers.find(header); it != headers.end())
	return it->second;

    for (std::string_view ext : {".h", ".tcc"})
	if (header.ends_with(ext))
	{
	    header.remove_suffix(ext.size());
	    break;
	}

    if (auto it = std_headers.find(header); it != std_headers.end())
	return *it;

    return {};
}

std::string map_header_or_complain (std::string header)
{
    // For <experimental/xxx.h> and <tr1/xxx.h> try to map <xxx.h>
    // then add the directory back to it.
    if (header.contains('.'))
	for (std::string_view dir : {"experimental/", "tr1/"})
	    if (header.starts_with(dir))
	    {
		auto h = map_header(header.substr(dir.size()));
		if (!h.empty())
		    return std::string(dir) + std::string(h);
		return std::string(header);
	    }

    if (auto mapped = map_header(header); !mapped.empty())
	return std::string(mapped);

    std::cerr << "Could not map <" << header << "> to a standard header\n";
    return std::string(header);
}


int main (int argc, char** argv)
{
    if (argc > 1)
    {
        std::cerr << "Usage: " << argv[0] << '\n' << usage;
	return 1;
    }

    init_map();

    std::transform(std::istream_iterator<std::string>(std::cin), {},
		   std::ostream_iterator<std::string>(std::cout),
		   map_header_or_complain);
}
