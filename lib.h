#include <boost/variant.hpp>

#include <type_traits>
#include <utility>

class Unit {};

template <typename T>
struct LeftTag { T x; };

template <typename T>
struct RightTag { T x; };

template <typename A, typename B>
using Sum = boost::variant<LeftTag<A>, RightTag<B>>;

template <typename A>
class JamVisitor : public boost::static_visitor<A> {
public:
    A operator()(LeftTag<A> const& x) const {
        return x.x;
    }

    A operator()(RightTag<A> const& x) const {
        return x.x;
    }
};

template <typename B>
auto inl(auto x) {
    return Sum<decltype(x), B>{LeftTag<decltype(x)>{x}};
}

template <typename A>
auto inr(auto x) {
    return Sum<A, decltype(x)>{RightTag<decltype(x)>{x}};
}

template <typename A, typename B, typename F, typename G>
class SplitVisitor : public boost::static_visitor<
    Sum<typename std::result_of<F(A)>::type, typename std::result_of<G(B)>::type>> {
public:
    SplitVisitor(F f_, G g_) : f(f_), g(g_) {}

    auto operator()(LeftTag<A> const& x) const {
        return inl<typename std::result_of<G(B)>::type>(f(x.x));
    }

    auto operator()(RightTag<B> const& x) const {
        return inr<typename std::result_of<F(A)>::type>(g(x.x));
    }

private:
    F f;
    G g;
};

auto combine(auto f, auto g, auto xy) {
    return std::make_pair(f(xy.first), g(xy.second));
}

auto dupl(auto x) {
    return std::make_pair(x, x);
}

auto curry(auto f, auto x) {
    return [=] (auto y) {
        return f(std::make_pair(x, y));
    };
}

auto uncurry(auto f, auto xy) {
    return f(xy.first)(xy.second);
}

template <typename A, typename B>
auto split(auto f, auto g, Sum<A, B> xy) {
    return boost::apply_visitor(SplitVisitor<A, B, decltype(f), decltype(g)>{f, g}, xy);
}

template <typename A>
auto jam(Sum<A, A> x) {
    return boost::apply_visitor(JamVisitor<A>{}, x);
}

auto add(auto xy) {
    return xy.first + xy.second;
}
