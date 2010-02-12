char jhc_c_compile[] = "avr-gcc -g -Os -I. '-mmcu=attiny25' '-DF_CPU=8000000' -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums -o hs.out hs.out_code.c -DNDEBUG -O3 -fomit-frame-pointer";
char jhc_command[] = "jhc -fffi --cross -mattiny25 RGB.hs";
char jhc_version[] = "jhc 0.7.2 (0.7.2-0)";

/* HsFFI.h for jhc */

#ifndef _JHC_HSFFI_H
#define _JHC_HSFFI_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>


typedef int32_t HsInt;
typedef int8_t  HsInt8;
typedef int16_t HsInt16;
typedef int32_t HsInt32;
typedef int64_t HsInt64;

typedef uint32_t HsWord;
typedef uint8_t  HsWord8;
typedef uint16_t HsWord16;
typedef uint32_t HsWord32;
typedef uint64_t HsWord64;

typedef wchar_t HsChar;
typedef bool HsBool;

typedef double HsDouble;
typedef float HsFloat;

typedef void *HsPtr;
typedef void (*HsFunPtr)(void);
typedef void *HsStablePtr;

#define HS_BOOL_FALSE 0
#define HS_BOOL_TRUE 1

void hs_init (int *argc, char **argv[]);
void hs_exit (void);
void hs_set_argv(int argc, char *argv[]);
void hs_perform_gc(void);
void hs_free_stable_ptr(HsStablePtr sp);
void hs_free_fun_ptr(HsFunPtr fp);

#endif

// jhc_rts_header.h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <assert.h>
#include <float.h>
#ifndef __WIN32__
#include <sys/select.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/utsname.h>
#endif
#include <setjmp.h>


// #define our options

#define _JHC_GC_NONE   0
#define _JHC_JGC       1
#define _JHC_GC_BOEHM  2
#define _JHC_GC_REGION 3


#ifndef _JHC_GC
#define _JHC_GC _JHC_GC_NONE
#endif

#ifndef _JHC_PROFILE
#define _JHC_PROFILE 0
#endif

#ifndef _JHC_DEBUG
#ifdef NDEBUG
#define _JHC_DEBUG 0
#else
#define _JHC_DEBUG 1
#endif
#endif

#ifndef _JHC_STANDALONE
#define _JHC_STANDALONE 1
#endif


// GNU attributes

#ifdef __GNUC__
#define A_ALIGNED  __attribute__ ((aligned))
#define A_CONST    __attribute__ ((const))
#define A_MALLOC   __attribute__ ((malloc))
#define A_MAYALIAS __attribute__ ((__may_alias__))
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE     __attribute__ ((pure))
#define A_UNUSED   __attribute__ ((unused))
#ifdef __i386__
#define A_REGPARM __attribute__ ((fastcall))
#else
#define A_REGPARM
#endif
#define A_STD    A_REGPARM

#else
#define A_ALIGNED
#define A_CONST
#define A_MALLOC
#define A_MAYALIAS
#define A_NORETURN
#define A_PURE
#define A_UNUSED
#define A_STD
#endif

// these should be enabled with newer versions of gcc
#define A_HOT
#define A_COLD
#define A_FALIGNED

#define STR(s) #s
#define XSTR(s) STR(s)
#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))


#ifdef __WIN32__
#define JHC_isWindows   1
#define JHC_isBigEndian 0
#else
#define JHC_isWindows 0
#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)
#endif

#define JHC_isPosix (!JHC_isWindows)


// some default definitions

#define jhc_malloc_whnf jhc_malloc
#define jhc_malloc_suspension jhc_malloc
#define jhc_malloc_atomic jhc_malloc
#define jhc_malloc_atomic_whnf jhc_malloc_atomic
#define jhc_malloc_sanity(p,t) (1)
#define _JHC_GC_CONTEXT 0

extern void _start,_end;

void hs_perform_gc(void) {}
void hs_free_stable_ptr(HsStablePtr sp) {}
void hs_free_fun_ptr(HsFunPtr fp) {}

#if _JHC_PROFILE

#define BUCKETS 7

static unsigned alloced[BUCKETS];
static unsigned alloced_atomic[BUCKETS];

static void
alloc_count(int n,int atomic)
{
        n = n ? ((n - 1)/sizeof(void *)) + 1 : 0;
        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;
        (atomic ? alloced_atomic : alloced)[n]++;
}


static void
print_alloc_size_stats(void) {
        char fmt[] = "%10s %10s %10s %10s %10s\n";
        char fmt2[] = "%10u %10u %10u %10u %10u\n";
        fprintf(stderr,fmt,"Size","Normal","Atomic","Total","Accum");
        fprintf(stderr,fmt,"----","------","------","-----","-----");
        unsigned accum = 0;
        for(int i = 0; i < BUCKETS; i++) {
                accum += alloced[i] + alloced_atomic[i];
                fprintf(stderr,fmt2,i,alloced[i],alloced_atomic[i],alloced_atomic[i] + alloced[i], accum);
        }
}


#else

#define alloc_count(x,y)
#define print_alloc_size_stats()

#endif

#if _JHC_GC == _JHC_GC_BOEHM

#include <gc/gc.h>

#define jhc_malloc GC_malloc
#undef  jhc_malloc_atomic
#define jhc_malloc_atomic GC_malloc_atomic
#define jhc_free GC_free

static inline void jhc_malloc_init(void) { GC_INIT(); }
static inline void jhc_alloc_print_stats(void) { GC_dump(); }

#elif _JHC_GC == _JHC_GC_NONE

// memory allocated in 1MB chunks.
#define JHC_MEM_CHUNK_SIZE (1 << 20)

static char initial_chunk[JHC_MEM_CHUNK_SIZE];

static void *jhc_current_chunk = initial_chunk;
static unsigned mem_chunks,mem_offset;


static inline void
jhc_malloc_init(void) { return; }

static void
jhc_alloc_print_stats(void) {
        fprintf(stderr, "Memory Allocated: %u bytes\n", (JHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);
        print_alloc_size_stats();
}

static void
jhc_malloc_grow(void) {
        void *c = malloc(JHC_MEM_CHUNK_SIZE);
        if(!c) {
                fputs("Out of memory!\n",stderr);
                abort();
        }
        mem_chunks++;
        jhc_current_chunk = c;
        mem_offset = 0;
}

static inline void * A_MALLOC
jhc_malloc_basic(size_t n) {
        n = ALIGN(sizeof(void *),n);
        if (n > (JHC_MEM_CHUNK_SIZE - mem_offset))
                jhc_malloc_grow();
        void *ret = jhc_current_chunk + mem_offset;
        mem_offset += n;
        return ret;
}


#if _JHC_DEBUG

#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__,0)
#undef jhc_malloc_atomic
#define jhc_malloc_atomic(n) jhc_malloc_debug(n,__LINE__,1)

static void * A_MALLOC
jhc_malloc_debug(size_t n,int line,int atomic) {
        alloc_count(n,atomic);
        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

static inline void * A_MALLOC
jhc_malloc(size_t n) {
        alloc_count(n,0);
        return jhc_malloc_basic(n);
}

#undef jhc_malloc_atomic
static inline void * A_MALLOC
jhc_malloc_atomic(size_t n) {
        alloc_count(n,1);
        return jhc_malloc_basic(n);
}


#endif

#elif _JHC_GC == _JHC_GC_JGC

#define GC_STACK_LIMIT 8192
static sptr_t *gc_stack_base;

static inline void
jhc_malloc_init(void) {
        gc_stack_base = malloc(sizeof(sptr_t) * GC_STACK_LIMIT);
}

#elif _JHC_GC == _JHC_GC_REGION

#undef _JHC_GC_CONTEXT
#define _JHC_GC_CONTEXT 1


typedef unsigned jhc_gc_context_t;

#include <sys/queue.h>

static inline void jhc_malloc_init(void) { return; }
static inline void jhc_alloc_print_stats(void) { return; }

// Region chunk size
#define JHC_MEM_CHUNK_SIZE (1 << 12)


struct region {
        SLIST_HEAD(,region_page) pages;
        unsigned offset;
};

struct region_page {
        SLIST_ENTRY(region_page) next;
        uintptr_t data[];
};


#define JHC_REGION_STACK_SIZE  1024
static struct region region_stack[JHC_REGION_STACK_SIZE];

SLIST_HEAD(,region_page) free_pages;


static struct region_page *
new_region_page(unsigned current_region) {
        if(SLIST_EMPTY(&free_pages)) {

                unsigned cr = current_region;
                while(cr < JHC_REGION_STACK_SIZE && !SLIST_EMPTY(&region_stack[cr].pages)) {
                        struct region_page *next = SLIST_FIRST(&region_stack[cr].pages);
                        SLIST_FIRST(&region_stack[cr].pages) = NULL;
                        while(next) {
                                struct region_page *nnext = SLIST_NEXT(next,next);
                                SLIST_INSERT_HEAD(&free_pages, next, next);
                                next = nnext;
                        }
                        cr++;
                }
                if(cr == current_region) {
                        return malloc(JHC_MEM_CHUNK_SIZE);
                }
        }
        struct region_page *r = SLIST_FIRST(&free_pages);
        SLIST_REMOVE_HEAD(&free_pages,next);
        return r;
}

static struct region *
new_region(unsigned current_region) {
        struct region *r = &region_stack[current_region];
        if(SLIST_EMPTY(&r->pages)) {
                r->offset = JHC_MEM_CHUNK_SIZE;
                return r;
        } else {
                // if the region already has some pages allocated, then
                // we reuse one of them and return the rest to the free list.
                struct region_page *next = SLIST_NEXT(SLIST_FIRST(&r->pages),next);
                if(next) {
                        SLIST_NEXT(SLIST_FIRST(&r->pages), next) = NULL;
                        while(next) {
                                struct region_page *nnext = SLIST_NEXT(next,next);
                                SLIST_INSERT_HEAD(&free_pages, next, next);
                                next = nnext;
                        }
                }
                r->offset = sizeof(struct region_page);
                return r;
        }
}



static inline void * A_MALLOC
jhc_malloc_region(unsigned current_region, struct region *r, size_t n) {
        n = ALIGN(sizeof(void *),n);
        struct region_page *rp;
        if (n > (JHC_MEM_CHUNK_SIZE - r->offset)) {
                rp = new_region_page(current_region);
                SLIST_INSERT_HEAD(&r->pages, rp, next);
                r->offset = sizeof(struct region_page);
        } else {
                rp = SLIST_FIRST(&r->pages);
        }


        void *ret = (void *)rp + r->offset;
        r->offset += n;
        return ret;
}





#endif





static void _amain(void);
static void jhc_arch_assert(void);
static int jhc_argc;
static char **jhc_argv;
static char *jhc_progname;
static jmp_buf jhc_uncaught;

static HsInt jhc_stdrnd[2] A_UNUSED = { 1 , 1 };
static HsInt jhc_data_unique A_UNUSED;
#ifdef __WIN32__
static char *jhc_options_os =  "mingw32";
static char *jhc_options_arch = "i386";
#else
struct utsname jhc_utsname;
static char *jhc_options_os = "(unknown os)";
static char *jhc_options_arch = "(unknown arch)";
#endif


#if _JHC_PROFILE

static uintmax_t jhc_prof_function_calls;
static uintmax_t jhc_prof_case_statements;
static uintmax_t jhc_prof_updates;

#define jhc_update_inc()   jhc_prof_updates++
#define jhc_function_inc() jhc_prof_function_calls++
#define jhc_case_inc()     jhc_prof_case_statements++

#else

#define jhc_update_inc()    do { } while(0)
#define jhc_function_inc()  do { } while(0)
#define jhc_case_inc()      do { } while(0)

#endif

static void A_COLD
jhc_print_profile(void) {
#ifndef __WIN32__
        struct tms tm;
        times(&tm);
#endif
        if(!(_JHC_PROFILE || getenv("JHC_RTS_PROFILE"))) return;

        fprintf(stderr, "\n-----------------\n");
        fprintf(stderr, "Profiling: %s\n", jhc_progname);
        fprintf(stderr, "Command: %s\n", jhc_command);
        fprintf(stderr, "Complie: %s\n", jhc_c_compile);
        fprintf(stderr, "Version: %s\n\n", jhc_version);
        jhc_alloc_print_stats();
#ifndef __WIN32__
        float cpt = (float)sysconf(_SC_CLK_TCK);
        fprintf(stderr, "User Time:   %.2fs\n", (float)tm.tms_utime/cpt);
        fprintf(stderr, "System Time: %.2fs\n", (float)tm.tms_stime/cpt);
        fprintf(stderr, "Total Time:  %.2fs\n", (float)(tm.tms_stime + tm.tms_utime)/cpt);
#endif

#if _JHC_PROFILE
        fprintf(stderr, "\nFunction Calls:   %llu\n", (unsigned long long)jhc_prof_function_calls);
        fprintf(stderr, "Case Statements:  %llu\n", (unsigned long long)jhc_prof_case_statements);
        fprintf(stderr, "Updates:          %llu\n", (unsigned long long)jhc_prof_updates);
#endif
        fprintf(stderr, "-----------------\n");
}


static void A_NORETURN A_UNUSED A_COLD
jhc_exit(int n) {
        jhc_print_profile();
        exit(n);
}

static void  A_NORETURN A_UNUSED  A_COLD
jhc_error(char *s) {
        fputs(s,stderr);
        fputs("\n",stderr);
        jhc_print_profile();
        exit(255);
}

static void  A_NORETURN A_UNUSED  A_COLD
jhc_case_fell_off(int n) {
        fflush(stdout);
        fprintf(stderr, "\n%s:%i: case fell off\n", __FILE__, n);
        abort();
}

static HsBool A_UNUSED
jhc_wait_for_input(FILE *f,HsInt timeout) {
#if JHC_isPosix
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fileno(f),&fds);
        struct timeval to = {  0, timeout * 1000 };
        int retval = select(1,&fds,NULL,&fds,&to);
        if(retval)
                return HS_BOOL_TRUE;
        else
                return HS_BOOL_FALSE;
#else
        return HS_BOOL_FALSE;
#endif

}

#ifdef __WIN32__
#define jhc_setjmp(jb) setjmp(*(jmp_buf *)jb)
#define jhc_longjmp(jb) longjmp(*(jmp_buf *)jb,1)
#define getchar_unlocked() getchar()
#define putchar_unlocked(x) putchar(x)
#define getc_unlocked(x) getc(x)
#define putc_unlocked(x,y) putc(x,y)
#else
#define jhc_setjmp(jb) sigsetjmp(*(jmp_buf *)jb,0)
#define jhc_longjmp(jb) siglongjmp(*(jmp_buf *)jb,1)
#endif

struct jhc_continuation {
    void *argument;
    jmp_buf jump_buf;
};

#define prim_umaxbound(t) ((t)~((t)0))
#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*8 - 1))))
#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*8 - 1))))


inline static int A_UNUSED
jhc_utf8_getchar(void)
{
    return getchar_unlocked();
}

inline static int A_UNUSED
jhc_utf8_getc(FILE *f)
{
    return getc_unlocked(f);
}

inline static int A_UNUSED
jhc_utf8_putchar(int ch)
{
    return putchar_unlocked(ch);
}

inline static int A_UNUSED
jhc_utf8_putc(int ch, FILE *f)
{
    return putc_unlocked(ch,f);
}


#if _JHC_STANDALONE
int
main(int argc, char *argv[])
{
        hs_init(&argc,&argv);
        if (jhc_setjmp(jhc_uncaught))
                jhc_error("Uncaught Exception");
        else
                _amain();
        hs_exit();
        return 0;
}
#endif


void hs_set_argv(int argc, char *argv[])
{
        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
}

static int hs_init_count;

void
hs_init(int *argc, char **argv[])
{

        if(!hs_init_count++) {
                /* A few random assertions about the architecture that the compiler
                 * assumes. should be true of any but the oddest of beasts.
                 */

                assert(sizeof(HsPtr) == sizeof(HsFunPtr));
                assert(sizeof(HsPtr) == sizeof(intptr_t));
                assert(sizeof(HsPtr) == sizeof(uintptr_t));
                assert(CHAR_BIT == 8);
                assert(EOF == -1);

                jhc_arch_assert();
                jhc_malloc_init();
                hs_set_argv(*argc,*argv);
#if JHC_isPosix
                if(!uname(&jhc_utsname)) {
                        jhc_options_arch = jhc_utsname.machine;
                        jhc_options_os   = jhc_utsname.sysname;
                }
#endif
                setlocale(LC_ALL,"");
        }
}

void hs_exit (void)
{
        if(!--hs_init_count)
                jhc_print_profile();
}




#define ISLAZY(x)    (((uintptr_t)(x)) & 0x1)
#define DETAG(x)     ((uintptr_t)(x) & ~0x3)
#define GETTAG(x)    ((uintptr_t)(x) & 0x3)

#define GETHEAD(x)   (NODEP(x)->head)
#define NODEP(x)     ((node_t *)(x))
#define DNODEP(x)    ((dnode_t *)(x))
#define EVALTAG(fn)  (assert(((uintptr_t)(fn) & 0x3) == 0),(sptr_t)((uintptr_t)(fn) | P_LAZY))
#define EVALTAGC(fn) ((sptr_t)((void *)(fn) + P_LAZY))
#define EVALFUNC(fn) ((fptr_t)((uintptr_t)(fn) + P_FUNC))
#define VALUE(n)     ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))
#define GETVALUE(n)  ((intptr_t)(n) >> 2)
#define ISVALUE(n)   (assert(!ISLAZY(n)), ((uintptr_t)(n) & 0x2))
#define PROMOTE(n)   ((wptr_t)(n))
#define DEMOTE(n)    ((sptr_t)(n))
#define GETWHAT(x)   (GETTAG(x) == P_VALUE ? ((uintptr_t)(x) >> 16) : DNODEP(x)->what)

#define SETWHAT(x,v)   (DNODEP(x)->what = (v))
#define RAWWHAT(w)     (wptr_t)(((uintptr_t)w << 16) | P_VALUE)


#define P_WHNF  0x0
#define P_LAZY  0x1
#define P_VALUE 0x2
#define P_FUNC  0x3

#define BLACK_HOLE ((fptr_t)0xDEADBEEF)


/*@Internals

# The Run Time System

Jhc is very minimalist in that it does not have a precompiled run time system,
but rather generates what is needed as part of the compilation process.
However, we call whatever conventions and binary layouts used in the generated
executable the run time system. Since jhc generates the code anew each time, it
can build a different 'run time' based on compiler options, trading things like
the garbage collector as needed or changing the closure layout when we know we
have done full program optimization. This describes the 'native' layout upon
which other conventions are layered.

A basic value in jhc is represented by a 'smart pointer' of c type sptr_t. a
smart pointer is the size of a native pointer, but can take on different roles
depending on a pair of tag bits.

smart pointers take on a general form as follows:

    -------------------------
    |    payload        | GL|
    -------------------------

      G - if set, then the garbage collector should not treat value as a pointer to be followed
      L - lazy, this bit being set means the value is not in WHNF

A raw sptr_t on its own in the wild can only take on one of the following values:

    -------------------------
    |    raw value      | 10|
    -------------------------

    -------------------------
    |    whnf location  | 00|
    -------------------------

    -------------------------
    |   lazy location   | 01|
    -------------------------

A raw value can be anything and not necessarily a pointer in general, a WHNF
location is a pointer to some value in WHNF. The system places no restrictions
on what is actually pointed to by a WHNF pointer, however the garbage collector
in use may. In general, the back end is free to choose what to place in the raw
value field or in what a WHNF points to with complete freedom. If an
implementation sees the L bit is clear, it can pass on the smart pointer
without examining it knowing the value is in WHNF.

A lazy location points to a potential closure or an indirection to a WHNF
value. The lazy location is an allocated chunk of memory that is at least
one pointer long. the very first location in a closure must be one of the
following.

    -------------------------
    | raw value or whnf  |X0|
    -------------------------

An evaluated value, interpreted exactly as above. one can always replace any occurance of a
lazy location with an evaluated indirecton.

    -------------------------
    |    code pointer   | 11|
    -------------------------
    |     data ...          |

This is something to evaluate, code pointer is a pointer to a function that takes
the memory location as its only argument, the called function is in charge
of updating the location if needed.

note that it is invalid to have a lazy location point to another lazy
location. there is only ever one level of indirection allowed, and only from
lazy locations

note that a partial application is just like any other value in WHNF as far
as the above is concered. It happens to possibly contain a code pointer.

*/


/*
 * type names
 *
 * sptr_t - a tagged smart pointer, may be a value, may be a pointer to a whnf or lazy location
 * wptr_t - a value guarenteed to be in whnf
 * fptr_t - a pointer to a whnf or a function pointer to something to evaluate, first value in a lazy location.
 * what_t  - the discriminator of a discriminated union
 *
 */

typedef struct node *  sptr_t;
typedef struct dnode * wptr_t;
typedef void *         fptr_t;
typedef uintptr_t      what_t;


typedef struct node {
        fptr_t head;
        sptr_t rest[];
} A_MAYALIAS node_t;

typedef struct dnode {
        what_t what;
        sptr_t rest[];
} A_MAYALIAS dnode_t;

#if _JHC_DEBUG

// these ensure the type synonyms are available to the debugger
uintptr_t _dummy1;
node_t *_dummy2;
dnode_t *_dummy3;
sptr_t *_dummy4;
fptr_t *_dummy5;
wptr_t *_dummy6;


static int A_UNUSED
jhc_valid_whnf(wptr_t s)
{
        return ((GETTAG(s) == P_VALUE) || ((GETTAG(s) == P_WHNF) && jhc_malloc_sanity(s,P_WHNF)));
}

static int A_UNUSED
jhc_valid_lazy(sptr_t s)
{
        if(jhc_valid_whnf((wptr_t)s))
                return 1;
        assert(GETTAG(s) == P_LAZY);
        node_t *ds = (sptr_t)DETAG(s);
        assert(jhc_malloc_sanity(ds,P_LAZY));
        if(ISLAZY(ds->head)) {
                if(ds->head == BLACK_HOLE) return 1;
                assert(GETTAG(ds->head) == P_FUNC);
                fptr_t dhead = (fptr_t)DETAG(ds->head);
                assert(dhead >= &_start && dhead < &_end);
                return 1;
        } else
                return jhc_valid_whnf((wptr_t)ds->head);
}


#else

#define jhc_valid_whnf(x) 1
#define jhc_valid_lazy(x) 1

#endif


typedef wptr_t (*eval_fn)(node_t *node) A_STD;

// both promote and demote evaluate to nothing when debugging is not enabled
// otherwise, they check that their arguments are in the correct form.

static inline wptr_t A_STD A_UNUSED  A_HOT
promote(sptr_t s)
{
        assert(!ISLAZY(s));
        assert(jhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}

static inline sptr_t A_STD A_UNUSED A_HOT
demote(wptr_t s)
{
        assert(!ISLAZY(s));
        assert(jhc_valid_whnf(s));
        return (sptr_t)s;
}

// like eval but you know the target is in WHNF or is a already evaluated indirection
static inline wptr_t A_STD A_UNUSED  A_HOT
follow(sptr_t s)
{
        assert(jhc_valid_lazy(s));
        if(ISLAZY(s)) {
                sptr_t h = (sptr_t)(GETHEAD(DETAG(s)));
                assert(!ISLAZY(h));
                return (wptr_t)h;
        }
        return (wptr_t)s;
}

static inline wptr_t A_STD A_UNUSED  A_HOT
eval(sptr_t s)
{
        assert(jhc_valid_lazy(s));
        if(ISLAZY(s)) {
                assert(GETTAG(s) == P_LAZY);
                void *ds = (void *)DETAG(s);
                sptr_t h = (sptr_t)(GETHEAD(ds));
                assert(h != BLACK_HOLE);
                if(ISLAZY(h)) {
                        eval_fn fn = (eval_fn)DETAG(h);
#if _JHC_DEBUG
                        GETHEAD(ds) = BLACK_HOLE;
#endif
                        wptr_t r = (*fn)(NODEP(ds));
#if _JHC_DEBUG
                        assert(GETHEAD(ds) != BLACK_HOLE);
#endif
                        return r;
                }
                return (wptr_t)h;
        }
        assert(jhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}


static inline void A_STD A_UNUSED A_HOT
update(sptr_t thunk, wptr_t new)
{
        jhc_update_inc();
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!ISLAZY(new));
        GETHEAD(thunk) = (fptr_t)new;
}




static void
jhc_arch_assert(void)
{
}
#include <addresses.h>


void _amain(void) ;
static void b_umain(void) A_STD;
static void ftheMain(void) A_STD;
/* CAFS */

void 
_amain(void)
{
        return (void)b_umain();
}

static void A_STD
b_umain(void)
{
        jhc_function_inc();
        return ftheMain();
}

static void A_STD
ftheMain(void)
{
        jhc_function_inc();
        uintptr_t v10 = ((uintptr_t)DDRB());
        *((uint8_t *)(v10)) = 23;
        uintptr_t v18 = ((uintptr_t)PORTB());
        return *((uint8_t *)(v18)) = 23;
}

