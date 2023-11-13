#include "../source/match.h"

namespace cpp2 {

//  Defined out of line here just to avoid bringing <iostream> into the headers,
//  so that we can't accidentally start depending on iostreams in the compiler body
auto cmdline_processor::print(std::string_view s, int width)
    -> void
{
    if (width > 0) {
        std::cout << std::setw(width) << std::left;
    }
    std::cout << s;
}
}

int main() {
    auto g = cpp2::bounded_simulation::graph{
        8, {{1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}}
    };
    auto p = cpp2::bounded_simulation::pattern{
        5, cpp2::bounded_simulation::trivial_predicate{}
    };

    auto rel = cpp2::bounded_simulation::bounded_simulation_match<>(p, g);

    return 0;
}
