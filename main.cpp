#include <iostream>
#include <cassert>

///////////////////////////////////////////////////////////////////////////////
// PRIMITIVE_MAP, you may use it for some basic traits

#define T_MAP(t) \
  X(t, char) \
  X(t, unsigned char) \
  X(t, short) \
  X(t, unsigned short) \
  X(t, int) \
  X(t, unsigned int) \
  X(t, long) \
  X(t, unsigned long long) \
  X(t, float) \
  X(t, double) \
  X(t, long double)

#define PRIMITIVE_MAP \
  T_MAP(char) \
  T_MAP(unsigned char) \
  T_MAP(short) \
  T_MAP(unsigned short) \
  T_MAP(int) \
  T_MAP(unsigned int) \
  T_MAP(long) \
  T_MAP(unsigned long) \
  T_MAP(long long) \
  T_MAP(unsigned long long) \
  T_MAP(float) \
  T_MAP(double) \
  T_MAP(long double)

///////////////////////////////////////////////////////////////////////////////
// types

// __null()
struct anull {}; // anull indicates nothing

#define __null() anull


// __int(x)
template <int X>
struct aint {
  enum { value = X };
  using type = aint<X>;
};

#define __int(x) typename aint<(x)>::type


// __bool(x)
template <bool> struct abool;

template <>
struct abool<true> {
  enum { value = true };
  using type = abool<true>;
};

template <>
struct abool<false> {
  enum { value = false };
  using type = abool<false>;
};

#define __bool(v) typename abool<(v)>::type


// __true()
using atrue  = abool<true>;
#define __true() typename atrue::type


// __false()
using afalse = abool<false>;
#define __false() typename afalse::type


// __pair(F, S)
template <typename F, typename S>
struct apair {
  using first  = F;
  using second = S;
};

#define __pair(F, S) apair<F, S>


///////////////////////////////////////////////////////////////////////////////
// functions

// __value(T)
template <typename T>
struct the_value {
  enum { value = 0 };
};

template <int X>
struct the_value<aint<X>> {
  enum { value = X };
};

template <bool B>
struct the_value<abool<B>> {
  enum { value = B };
};

template <>
struct the_value<anull> {
  enum { value = -1 };
};

#define __value(T) the_value<T>::value


// __is_eq(x, y)
template <typename T1, typename T2>
struct is_eq {
  enum { value = false };
  using type = afalse;
};

template <typename T>
struct is_eq<T, T> {
  enum { value = true };
  using type = atrue;
};

#define __is_eq(x, y) typename is_eq<x, y>::type


// __add(x, y)
template <typename X, typename Y> struct f_add;

template <int X, int Y>
struct f_add<aint<X>, aint<Y>> {
  using type = aint<X+Y>;
};

#define __add(x, y) typename f_add<x, y>::type


// __and(x, y)
template <typename X, typename Y> struct f_and;

template <bool X, bool Y>
struct f_and<abool<X>, abool<Y>> {
  using type = abool<X && Y>;
};

#define __and(x, y) typename f_and<x, y>::type


// __or(x, y)
template <typename X, typename Y> struct f_or;

template <bool X, bool Y>
struct f_or<abool<X>, abool<Y>> {
  using type = abool<X || Y>;
};

#define __or(x, y) typename f_or<x, y>::type


// __not(x, y)
template <typename X> struct f_not;

template <bool X>
struct f_not<abool<X>> {
  using type = abool<!X>;
};

#define __not(x) typename f_not<x>::type


// __is_convertible(x, y)
template <typename D, typename B>
struct is_convertible {
private:
  using yes = char;
  using no  = int;

  static yes test(B);   // if D is a B, then this will be invoked
  static no  test(...); // we don't care about the parameters
  static D   a_D();

public:
  using type = abool<sizeof(test(a_D())) == sizeof(yes)>; // we use a_D() instead of D() to avoid the overhead of constructing a new object
};

#define __is_convertible(D, B) typename is_convertible<D, B>::type


// __is_a(x, y)
#define __is_a(D, B) __and(__is_convertible(const D *, const B *), \
                           __and(__not(__is_eq(const B*, const void*)), \
                                 __not(__is_eq(const D, const B))))


// __if(c, t, f)
template <typename C, typename T, typename F> struct s_if;

template <typename T, typename F>
struct s_if<atrue, T, F> {
  using type = T;
};

template <typename T, typename F>
struct s_if<afalse, T, F> {
  using type = F;
};

#define __if(c, t, f) typename s_if<c, t, f>::type


// __reduce(f, x, ...)
template <template <typename, typename> class F, typename X, typename ...N> struct f_reduce; // X is the initval

template <template <typename, typename> class F, typename X, typename N, typename ...R>
struct f_reduce<F, X, N, R...> { // assume N is the first, <4, 3, 2, 1, ...>, then N is 4, the last to be dealed with
  using type = typename f_reduce<F, typename f_reduce<F, X, R...>::type, N>::type;
};

template <template <typename, typename> class F, typename X, typename N>
struct f_reduce<F, X, N> {
  using type = typename F<X, N>::type;
};

template <template <typename, typename> class F, typename X>
struct f_reduce<F, X> {
  using type = X;
};

#define __reduce(f, ...) typename f_reduce<f, __VA_ARGS__>::type


// // __sum(...)
// template <typename ...N>
// struct f_sum {
//   using type = typename f_reduce<f_add, N...>::type;
// };

// template <typename N>
// struct f_sum<N> {
//   using type = N;
// };

// #define __sum(...) typename f_sum<__VA_ARGS__>::type
#define __sum(...) __reduce(f_add, __VA_ARGS__)


// __is_array(x)
template <typename T>
struct is_array {
  using type = afalse;
};

template <typename T>
struct is_array<T[]> {
  using type = atrue;
};

template <typename T>
struct is_array<const T[]> {
  using type = atrue;
};

#define __is_array(x) typename is_array<x>::type


// __is_function
template <typename Ret, typename ...Args>
struct is_function {
  using type = afalse;
};

template <typename Ret, typename ...Args>
struct is_function<Ret(Args...)> {
  using type = atrue;
};


///////////////////////////////////////////////////////////////////////////////
// typelist related functions

// __empty()
struct aempty {};
#define __empty() aempty


// __typelist(...)
template <typename... T>
struct typelist {
  using type = aempty;
};

template <typename H, typename... T>
struct typelist<H, T...> {
  using type = apair<H, typename typelist<T...>::type>;
};

template <typename H>
struct typelist<H> {
  using type = apair<H, aempty>;
};

#define __typelist(...) typename typelist<__VA_ARGS__>::type


// __tl_length(TL)
template <typename TL>
struct tl_length {
  using type = aint<0>;
};

template <typename F, typename S>
struct tl_length<apair<F, S>> {
  using type = typename f_add<aint<1>, typename tl_length<S>::type>::type;
};

#define __tl_length(TL) typename tl_length<TL>::type


// __tl_get(TL, n)
template <typename TL, int N>
struct tl_get {
  using type = anull;
};

template <typename F, typename S, int N>
struct tl_get<apair<F, S>, N> {
  using type = typename tl_get<S, N-1>::type;
};

template <typename F, typename S>
struct tl_get<apair<F, S>, 0> {
  using type = F;
};

#define __tl_get(TL, n) typename tl_get<TL, (n)>::type


// __tl_set(T, n)
template <typename TL, int N, typename S>
struct tl_set {
  using type = TL;
};

template <typename F, typename S, int N, typename X>
struct tl_set<apair<F, S>, N, X> {
  using type = apair<F, typename tl_set<S, N-1, X>::type>;
};

template <typename F, typename S, typename X>
struct tl_set<apair<F, S>, 0, X> {
  using type = apair<X, S>;
};

#define __tl_set(TL, n, S) typename tl_set<TL, (n), S>::type


// __tl_remove
template <typename TL, int N>
struct tl_remove {
  using type = TL;
};

template <typename F, typename S, int N>
struct tl_remove<apair<F, S>, N> {
  using type = apair<F, typename tl_remove<S, N-1>::type>;
};

template <typename F, typename S>
struct tl_remove<apair<F, S>, 0> {
  using type = S;
};

#define __tl_remove(TL, n) typename tl_remove<TL, (n)>::type


// __tl_insert
template <typename TL, int N, typename X>
struct tl_insert {
  using type = TL;
};

template <typename F, typename S, int N, typename X>
struct tl_insert<apair<F, S>, N, X> {
  using type = apair<F, typename tl_insert<S, N-1, X>::type>;
};

template <typename F, typename S, typename X>
struct tl_insert<apair<F, S>, 0, X> {
  using type = apair<X, apair<F, S>>;
};

template <int N, typename X>
struct tl_insert<aempty, N, X> {
  using type = apair<X, aempty>;
};

#define __tl_insert(TL, n, S) typename tl_insert<TL, (n), S>::type
#define __tl_append(TL, S)    __tl_insert(TL, __value(__tl_length(TL)), S)
#define __tl_prepend(TL, S)   __tl_insert(TL, 0, S)


///////////////////////////////////////////////////////////////////////////////
// tests

template <typename T1, typename T2>
struct larger_type {
  using type = __if(__bool(sizeof(T1) > sizeof(T2)), T1, T2);
};

struct LargerOne {
  static const char *s;
  char paddig[2];
};

struct SmallerOne {
  static const char *s;
  char paddig;
};

const char * LargerOne::s = "larger_one";
const char * SmallerOne::s = "smaller_one";

struct Base {};
struct Derived : public Base {};

template <typename X, typename Y>
struct aint_max2 {
  using type = aint<(X::value > Y::value) ? X::value : Y::value>;
};

template <typename X, typename Y>
struct aint_sub2 {
  using type = aint<X::value - Y::value>;
};

#define ASSERT_EQ(a, b) assert((a) == (b))

int main() {
  ASSERT_EQ(1, __value(__is_eq(__int(4), __int(4))));
  ASSERT_EQ(0, __value(__is_eq(__int(4), __bool(true))));
  ASSERT_EQ(1, __value(__is_eq(__true(), __true())));
  ASSERT_EQ(0, __value(__is_eq(__true(), __false())));
  ASSERT_EQ(1, __value(__is_eq(__false(), __false())));
  ASSERT_EQ(3, __value(__add(__int(1), __int(2))));
  ASSERT_EQ(0, __value(__and(__true(), __false())));
  ASSERT_EQ(1, __value(__or(__true(), __false())));
  ASSERT_EQ(0, __value(__not(__true())));
  ASSERT_EQ("larger_one", (larger_type<LargerOne, SmallerOne>::type::s));
  ASSERT_EQ(1, __value(__is_convertible(char, int)));
  ASSERT_EQ(0, __value(__is_convertible(int, void*)));
  ASSERT_EQ(1, __value(__is_convertible(char*, void*)));
  ASSERT_EQ(0, __value(__is_convertible(Base*, Derived*)));
  ASSERT_EQ(1, __value(__is_convertible(Derived*, Base*)));
  ASSERT_EQ(0, __value(__is_a(int, char)));
  ASSERT_EQ(0, __value(__is_a(char, int)));
  ASSERT_EQ(1, __value(__is_a(Derived, Base)));
  ASSERT_EQ(0, __value(__is_a(Base, Derived)));
  ASSERT_EQ(1, __value(__sum(__int(1))));
  ASSERT_EQ(3, __value(__sum(__int(1), __int(2))));
  ASSERT_EQ(6, __value(__sum(__int(1), __int(2), __int(3))));
  ASSERT_EQ(-1, __value(__sum(__int(1), __int(2), __int(-4))));
  ASSERT_EQ(100, __value(__reduce(aint_max2, __int(2), __int(100), __int(2), __int(3))));
  ASSERT_EQ(106, __value(__reduce(f_add, __int(-1), __int(2), __int(100), __int(2), __int(3))));
  ASSERT_EQ(0, __value(__is_array(int)));
  ASSERT_EQ(1, __value(__is_array(int[])));
  ASSERT_EQ(1, __value(__is_array(int[][3])));
  ASSERT_EQ(1, __value(__is_array(const int[][3])));
  ASSERT_EQ(0, __value(__is_array(int&)));
  ASSERT_EQ(0, __value(__is_array(int*)));
  ASSERT_EQ(0, __value(__is_array(const int*)));
  ASSERT_EQ(0, __value(__is_array(int***)));
  ASSERT_EQ(0, __value(__is_array(Base)));
  ASSERT_EQ(0, __value(__is_array(Base*)));
  ASSERT_EQ(1, __value(__is_array(Base[])));
  ASSERT_EQ(0, __value(__is_array(Base&)));

  using L = __typelist(int, float, void *, Base, Derived, const LargerOne &);
  ASSERT_EQ(1, __value(__is_eq(aint<6>, __tl_length(L))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(L, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(L, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(L, 2))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(L, 3))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(L, 4))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(L, 5))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(L, 6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(L, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LS1 = __tl_set(L, 3, float);
  ASSERT_EQ(1, __value(__is_eq(aint<6>, __tl_length(LS1))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LS1, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LS1, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LS1, 2))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LS1, 3))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LS1, 4))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LS1, 5))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LS1, 6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LS1, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LS2 = __tl_set(L, 100, float);
  ASSERT_EQ(1, __value(__is_eq(aint<6>, __tl_length(LS2))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LS2, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LS2, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LS2, 2))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(LS2, 3))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LS2, 4))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LS2, 5))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LS2, 6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LS2, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LD1 = __tl_remove(L, 3);
  ASSERT_EQ(1, __value(__is_eq(aint<5>, __tl_length(LD1))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LD1, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LD1, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LD1, 2))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LD1, 3))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LD1, 4))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LD1, 5))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LD1, 6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LD2 = __tl_remove(L, 100);
  ASSERT_EQ(1, __value(__is_eq(aint<6>, __tl_length(LD2))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LD2, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LD2, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LD2, 2))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(LD2, 3))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LD2, 4))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LD2, 5))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LD2, 6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LD2, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LI1 = __tl_insert(L, 3, double);
  ASSERT_EQ(1, __value(__is_eq(aint<7>, __tl_length(LI1))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LI1, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LI1, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LI1, 2))));
  ASSERT_EQ(1, __value(__is_eq(double, __tl_get(LI1, 3))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(LI1, 4))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LI1, 5))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LI1,6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI1, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI1, 8))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LI2 = __tl_append(L, double);
  ASSERT_EQ(1, __value(__is_eq(aint<7>, __tl_length(LI2))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LI2, 0))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LI2, 1))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LI2, 2))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(LI2, 3))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LI2, 4))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LI2,5))));
  ASSERT_EQ(1, __value(__is_eq(double, __tl_get(LI2,6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI2, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI2, 8))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  using LI3 = __tl_prepend(L, double);
  ASSERT_EQ(1, __value(__is_eq(aint<7>, __tl_length(LI3))));
  ASSERT_EQ(1, __value(__is_eq(double, __tl_get(LI3, 0))));
  ASSERT_EQ(1, __value(__is_eq(int, __tl_get(LI3, 1))));
  ASSERT_EQ(1, __value(__is_eq(float, __tl_get(LI3, 2))));
  ASSERT_EQ(1, __value(__is_eq(void *, __tl_get(LI3, 3))));
  ASSERT_EQ(1, __value(__is_eq(Base, __tl_get(LI3, 4))));
  ASSERT_EQ(1, __value(__is_eq(Derived, __tl_get(LI3, 5))));
  ASSERT_EQ(1, __value(__is_eq(const LargerOne &, __tl_get(LI3,6))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI3, 7))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(LI3, 8))));
  ASSERT_EQ(1, __value(__is_eq(anull, __tl_get(int, 7))));

  return 0;
}