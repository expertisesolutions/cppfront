#ifndef __CPP2_MATCH
#define __CPP2_MATCH

#include "cpp2util.h"
#include "lex.h"

#include <assert.h>
#include <ctype.h>
#include <iterator>
#include <map>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

/* Generation rules:
MATCH -> MATCH '->' NODE
NODE -> '(' NODE_DESCRIPTION ')'
NODE_DESCRIPTION -> NODE_LABEL ':' NODE_ATTRIBUTE | ''
NODE_LABEL -> rex [\w_][\w\d_]*
NODE_ATTRIBUTE -> '{' ATTRIBUTE_LIST '}' | '_'
ATTRIBUTE_LIST -> ATTRIBUTE_LIST ',' VALUE | VALUE
VALUE -> LITERAL | CAPTURE
LITERAL -> rex [0-9]*\.?[0-9]+ | rex "[^"]*" | ...
CAPTURE -> '$' (todo...)
*/

namespace cpp2 {

size_t find_closing_paren(std::string_view);

// Expands a match string literal to a full-on lambda object
// Maybe struct with `operator()` ?
struct match_generator {
    constexpr static auto arrow_lexeme = std::string_view{"->"};
    constexpr static auto or_lexeme = std::string_view{"|"};
    constexpr static auto and_lexeme = std::string_view{"&"};
    constexpr static auto node_lexeme = std::string_view{"()"};
    constexpr static auto node_attr_lexeme = std::string_view{"{}"};

    struct node {
        std::optional<std::string> label;
        std::vector<
            std::variant<
                std::monostate,
                token
            >
        > attrs;

        node() = default;

        node(const auto &l)
            : label{l}, attrs{}
        { }

        node(const auto &l, auto &&a)
            : label {l}, attrs{a}
        { }
    };

    std::string literal;
    std::vector<std::string_view> node_views = {};
    std::vector<node> nodes = {};
    /// WARNING: the following must be replaced by a reference
    std::vector<error_entry> errors = {};

    match_generator(const std::string &str)
        : literal{str}
    {
        parse();
    }

private:
    void parse_nodes();

    void get_nodes() {
        // constexpr auto open_parens = std::array{'(', '[', '{'};
        for (size_t i = 0, size = literal.size(); i < size; ++i) {
            const auto ch = literal[i];
            if (ch == '(') {
                const auto *str_data = literal.c_str() + i;
                if (const auto closing_paren_idx = find_closing_paren({str_data, size - i});
                    closing_paren_idx != std::string_view::npos) {
                    node_views.push_back({str_data, closing_paren_idx + 1});
                }
            }
        }

        // for(const auto &node : node_views) {
        //     std::cout << "node: " << node << std::endl;
        // }
    }

    void parse() {
        get_nodes();
        parse_nodes();
    }

};

auto find_closing_paren(std::string_view sv)
    -> size_t
{
    const auto open_close_paren_map = std::map<char, char>{
        {'(', ')'}, {'[', ']'}, {'{', '}'}, {'"', '"'}
    };
    const auto open_paren = sv[0];
    assert (open_close_paren_map.contains(open_paren));
    const auto close_paren = open_close_paren_map.at(open_paren);
    auto count_open_paren = 1;

    for (size_t i = 1, size = sv.size(); i < size; ++i) {
        if (sv[i] == open_paren) {
            ++count_open_paren;
        } else if (sv[i] == close_paren) {
            --count_open_paren;
        }
        if (0 == count_open_paren) {
            return i;
        }
    }

    return std::string_view::npos;
}

auto parse_attr_list(
    std::string_view sv,
    std::vector<error_entry> &errors
)
    -> decltype(match_generator::node::attrs)
{
    using attrs_type = decltype(match_generator::node::attrs);
    auto attrs = attrs_type{};

    std::cout << "parse_attr_list(): sv " << sv << std::endl;

    auto line = std::string{sv}, curr_comment = std::string{};
    auto in_comment = false;
    auto comment_pos = source_position{};
    auto tokens = std::vector<token>{};
    auto comments = std::vector<comment>{};
    // auto errors = std::vector<error_entry>{};
    auto rsm = std::optional<raw_string>{};

    lex_line(line, 1, in_comment, curr_comment, comment_pos, tokens,
             comments, errors, rsm);

    if (line != sv) {
        /// TODO: if string was changed, deal with that
        std::cout << "line " << line << ", sv " << sv << std::endl;
    } else {
        // token string view must point to persistent string view
        for (auto &t : tokens) {
            t = token{
                sv.data() + (t.as_string_view().data() - line.data()),
                t.length(),
                t.position(),
                t.type()
            };
        }   
    }

    // Every token in-between should be alternating literal/identifier
    // and comma. Should we allow for trailing commas?
    auto it = tokens.cbegin();
    const auto it_end = tokens.cend();
    auto next_must_be_comma = false;
    for (; it != it_end; ++it, next_must_be_comma = !next_must_be_comma) {
        const auto type = it->type();
        if (!next_must_be_comma) {
            if (!is_literal(type) || type != lexeme::Identifier) {
                errors.push_back(
                    {   
                        source_position{-1, -1},
                        "Attribute should be wither a literal or identifier"
                    }
                );
            }

            attrs.push_back({*it});
        } else {
            if (type != lexeme::Comma) {
                errors.push_back(
                    {
                        source_position{-1, -1},
                        "There should be a comma after a literal or identifier list"
                    }
                );
            }
        }
    }

    return attrs;
error:
    return {};
}

auto parse_node(
    std::string_view sv_node,
    std::vector<error_entry> &errors
)
    -> match_generator::node
{   
    using attrs_type = decltype(match_generator::node::attrs);
    // const auto str_node = std::string{sv_node};
    const auto node_pat = std::regex{"^\\(([a-zA-Z_]\\w*)(:\\{(.*)\\})?\\)$"};
    constexpr auto label_regex_idx = 1;
    constexpr auto attr_regex_idx = 3;
    auto match = std::cmatch{};

    if (std::regex_search(sv_node.cbegin(), sv_node.cend(), match, node_pat)) {
        for (auto i = 0; i < match.size(); ++i) {
            std::cout << "match " << i << " " << match[i].str() << std::endl;
        }
        if (match.size() >= 4) {
            return {
                match[label_regex_idx].str(),
                parse_attr_list(
                    sv_node.substr(
                        match.position(attr_regex_idx),
                        match.length(attr_regex_idx)
                    ),
                    errors
                )
            };
        } else {
            return {match[1].str()};
        }
    }

    return {};
}

void match_generator::parse_nodes()
{
    for (const auto &nv : node_views) {
        nodes.push_back(parse_node(nv, errors));
    }
}

}

#endif
