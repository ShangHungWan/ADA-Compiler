/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 260 "javaa.y"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "utils.h"
#include "build.h"
#include "protos.h"
#include "listing.h"

#line 81 "javaa.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_JAVAA_TAB_H_INCLUDED
# define YY_YY_JAVAA_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    LABEL = 258,
    IDENTIFIER = 259,
    INTCONSTANT = 260,
    LONGCONSTANT = 261,
    FLOATCONSTANT = 262,
    DOUBLECONSTANT = 263,
    CHARCONSTANT = 264,
    STRING_LITERAL = 265,
    CLASS = 266,
    EXTENDS = 267,
    ACCESS = 268,
    IMPLEMENTS = 269,
    FIELD = 270,
    METHOD = 271,
    MAX_STACK = 272,
    MAX_LOCALS = 273,
    CODE = 274,
    PUBLIC = 275,
    PRIVATE = 276,
    PROTECTED = 277,
    ABSTRACT = 278,
    FINAL = 279,
    INTERFACE = 280,
    STATIC = 281,
    NATIVE = 282,
    SYNCHRONIZED = 283,
    TRANSIENT = 284,
    VOLATILE = 285,
    BYTE = 286,
    CHAR = 287,
    DOUBLE = 288,
    FLOAT = 289,
    INT = 290,
    LONG = 291,
    SHORT = 292,
    BOOLEAN = 293,
    VOID = 294,
    DEFAULT = 295,
    TO = 296,
    EXCEPTIONS = 297,
    SOURCEFILE = 298,
    THROWS = 299,
    LINENUMBERTABLE = 300,
    LOCALVARIABLETABLE = 301,
    ACC_PUBLIC = 302,
    ACC_PRIVATE = 303,
    ACC_PROTECTED = 304,
    ACC_STATIC = 305,
    ACC_FINAL = 306,
    ACC_SYNCHRONIZED = 307,
    ACC_VOLATILE = 308,
    ACC_TRANSIENT = 309,
    ACC_NATIVE = 310,
    ACC_INTERFACE = 311,
    ACC_ABSTRACT = 312,
    AALOAD = 313,
    AASTORE = 314,
    ACONST_NULL = 315,
    ALOAD_0 = 316,
    ALOAD_1 = 317,
    ALOAD_2 = 318,
    ALOAD_3 = 319,
    ANEWARRAY = 320,
    ARETURN = 321,
    ARRAYLENGTH = 322,
    ASTORE_0 = 323,
    ASTORE_1 = 324,
    ASTORE_2 = 325,
    ASTORE_3 = 326,
    ATHROW = 327,
    BALOAD = 328,
    BASTORE = 329,
    BIPUSH = 330,
    CALOAD = 331,
    CASTORE = 332,
    CHECKCAST = 333,
    D2F = 334,
    D2I = 335,
    D2L = 336,
    DADD = 337,
    DALOAD = 338,
    DASTORE = 339,
    DCMPG = 340,
    DCMPL = 341,
    DCONST_0 = 342,
    DCONST_1 = 343,
    DDIV = 344,
    DLOAD_0 = 345,
    DLOAD_1 = 346,
    DLOAD_2 = 347,
    DLOAD_3 = 348,
    DMUL = 349,
    DNEG = 350,
    DREM = 351,
    DRETURN = 352,
    DSTORE_0 = 353,
    DSTORE_1 = 354,
    DSTORE_2 = 355,
    DSTORE_3 = 356,
    DSUB = 357,
    DUP = 358,
    DUP_X1 = 359,
    DUP_X2 = 360,
    DUP2 = 361,
    DUP2_X1 = 362,
    DUP2_X2 = 363,
    F2D = 364,
    F2I = 365,
    F2L = 366,
    FADD = 367,
    FALOAD = 368,
    FASTORE = 369,
    FCMPG = 370,
    FCMPL = 371,
    FCONST_0 = 372,
    FCONST_1 = 373,
    FCONST_2 = 374,
    FDIV = 375,
    FLOAD_0 = 376,
    FLOAD_1 = 377,
    FLOAD_2 = 378,
    FLOAD_3 = 379,
    FMUL = 380,
    FNEG = 381,
    FREM = 382,
    FRETURN = 383,
    FSTORE_0 = 384,
    FSTORE_1 = 385,
    FSTORE_2 = 386,
    FSTORE_3 = 387,
    FSUB = 388,
    GETFIELD = 389,
    GETSTATIC = 390,
    GOTO = 391,
    GOTO_W = 392,
    I2B = 393,
    I2C = 394,
    I2D = 395,
    I2F = 396,
    I2L = 397,
    I2S = 398,
    IADD = 399,
    IALOAD = 400,
    IAND = 401,
    IASTORE = 402,
    ICONST_0 = 403,
    ICONST_1 = 404,
    ICONST_2 = 405,
    ICONST_3 = 406,
    ICONST_4 = 407,
    ICONST_5 = 408,
    ICONST_M1 = 409,
    IDIV = 410,
    IF_ACMPEQ = 411,
    IF_ACMPNE = 412,
    IF_ICMPEQ = 413,
    IF_ICMPNE = 414,
    IF_ICMPLT = 415,
    IF_ICMPGE = 416,
    IF_ICMPGT = 417,
    IF_ICMPLE = 418,
    IFEQ = 419,
    IFNE = 420,
    IFLT = 421,
    IFGE = 422,
    IFGT = 423,
    IFLE = 424,
    IFNONNULL = 425,
    IFNULL = 426,
    ILOAD_0 = 427,
    ILOAD_1 = 428,
    ILOAD_2 = 429,
    ILOAD_3 = 430,
    IMUL = 431,
    INEG = 432,
    IOR = 433,
    IREM = 434,
    IRETURN = 435,
    ISHL = 436,
    ISHR = 437,
    ISTORE_0 = 438,
    ISTORE_1 = 439,
    ISTORE_2 = 440,
    ISTORE_3 = 441,
    ISUB = 442,
    IUSHR = 443,
    IXOR = 444,
    JSR = 445,
    JSR_W = 446,
    L2D = 447,
    L2F = 448,
    L2I = 449,
    LADD = 450,
    LALOAD = 451,
    LAND = 452,
    LASTORE = 453,
    LCMP = 454,
    LCONST_0 = 455,
    LCONST_1 = 456,
    LDIV = 457,
    LLOAD_0 = 458,
    LLOAD_1 = 459,
    LLOAD_2 = 460,
    LLOAD_3 = 461,
    LMUL = 462,
    LNEG = 463,
    LOR = 464,
    LREM = 465,
    LRETURN = 466,
    LSHL = 467,
    LSHR = 468,
    LSTORE_0 = 469,
    LSTORE_1 = 470,
    LSTORE_2 = 471,
    LSTORE_3 = 472,
    LSUB = 473,
    LUSHR = 474,
    LXOR = 475,
    MONITORENTER = 476,
    MONITOREXIT = 477,
    NOP = 478,
    POP = 479,
    POP2 = 480,
    RETURN = 481,
    SALOAD = 482,
    SASTORE = 483,
    SWAP = 484,
    IINC = 485,
    INSTANCEOF = 486,
    INVOKEINTERFACE = 487,
    INVOKENONVIRTUAL = 488,
    INVOKESTATIC = 489,
    INVOKEVIRTUAL = 490,
    LDC = 491,
    LDC_W = 492,
    LDC2_W = 493,
    MULTIANEWARRAY = 494,
    NEW = 495,
    NEWARRAY = 496,
    PUTFIELD = 497,
    PUTSTATIC = 498,
    SIPUSH = 499,
    ILOAD = 500,
    FLOAD = 501,
    ALOAD = 502,
    LLOAD = 503,
    DLOAD = 504,
    ISTORE = 505,
    FSTORE = 506,
    ASTORE = 507,
    LSTORE = 508,
    DSTORE = 509,
    RET = 510,
    WIDE = 511,
    LOAD = 512,
    STORE = 513,
    LOOKUPSWITCH = 514,
    TABLESWITCH = 515
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 289 "javaa.y"

   Terminal        rk;
   Terminal        NT;
   Terminal        RK;
   Terminal        Rk;

   float           floatval;
   double          doubleval;
   char            charval;
   int             intval;
   long            longval;
   char           *string;

   BaseType        basetype;
   StorageClass    storageclass;
   TypeQualifier   typequalifier;
   ArgType     argtype;

   lookupentry *   lookuplistptr;
   tableentry *    tablelistptr;

   struct {
    char* classname;
    char* fieldmethodname;
   }        classfieldmethodstruct;

   struct _declinfo {
      int   function;
      int   ptrcount;
      int   dimsize[7];
      int   numdims;
      char *name;
   }               declinfo;

   struct {
      TreeNode *formals;
      struct _declinfo D;
   }               funcstuff;

   struct {
      Type          T;
      StorageClass  C;
      TypeQualifier Q;
   } declspecs;

   struct {
      Type          T;
      StorageClass  C;
      TypeQualifier Q;
      TreeNode     *node;
   } declspecsnode;

   struct {
      Type          T;
      StorageClass  C;
      TypeQualifier Q;
      Symbol       *headsym;
      Symbol       *prevsym;
   } structdeclspecs;

   struct {
      Symbol   *headsym;
      Symbol   *prevsym;
   } headprevsym;

   struct {
      int       spec;
      int       val;
      char     *id;
   } idspecval;

   struct {
      Symbol   *sym;
      int       val;
   } symval;

   Type            typetype;

   Symbol         *sym;

   struct {
      TreeNode *decl;
      TreeNode *func;
   } declfunc;

   struct {
      TreeNode *decl;
      TreeNode *func;
      TreeNode *exprs;
      TreeNode *last;
   } declfuncexprs;

   struct {
      TreeNode *exprs;
      TreeNode *last;
   } exprslast;

   TreeNode       *node;

#line 494 "javaa.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_JAVAA_TAB_H_INCLUDED  */

/* Second part of user prologue.  */
#line 389 "javaa.y"

   static void     CheckStructDcl(Type,int);
   static MimeType GenBlankMimeType();
   static Type     GenBlankType();
   static TreeNode *MakeBinOpSubTree(int, TreeNode *, TreeNode *);
   static TreeNode *MakeUnOpSubTree(int, TreeNode *);
   static TreeNode *MakeFuncSibs(TreeNode *, TreeNode *, TreeNode *,
              TreeNode *, TreeNode *);
   static TreeNode *GenEntryArgs(TreeNode *);

#line 521 "javaa.tab.c"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   850

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  271
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  71
/* YYNRULES -- Number of rules.  */
#define YYNRULES  346
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  434

#define YYUNDEFTOK  2
#define YYMAXUTOK   515


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     265,   266,     2,     2,   267,     2,   263,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   270,     2,
       2,   264,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   268,     2,   269,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   261,     2,   262,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   403,   403,   403,   408,   409,   414,   412,   421,   425,
     427,   432,   434,   443,   447,   454,   460,   468,   474,   482,
     484,   488,   490,   494,   496,   500,   502,   506,   508,   512,
     514,   518,   520,   524,   526,   530,   532,   536,   538,   540,
     542,   544,   548,   550,   554,   556,   561,   562,   565,   570,
     574,   578,   582,   587,   592,   597,   598,   602,   607,   602,
     623,   625,   629,   631,   636,   638,   642,   644,   653,   655,
     658,   663,   669,   671,   674,   679,   682,   686,   691,   698,
     700,   702,   704,   706,   708,   710,   712,   714,   722,   724,
     728,   733,   738,   740,   745,   749,   752,   755,   757,   759,
     761,   765,   769,   772,   775,   778,   784,   787,   790,   797,
     800,   803,   806,   809,   813,   816,   821,   823,   825,   827,
     829,   831,   833,   835,   837,   839,   841,   843,   845,   847,
     849,   851,   853,   855,   857,   859,   861,   863,   865,   867,
     869,   871,   873,   875,   877,   879,   881,   883,   885,   887,
     889,   891,   893,   895,   897,   899,   901,   903,   905,   907,
     909,   911,   913,   915,   917,   919,   921,   923,   925,   927,
     929,   931,   933,   935,   937,   939,   941,   943,   945,   947,
     949,   951,   953,   955,   957,   959,   961,   963,   965,   967,
     969,   971,   973,   975,   977,   979,   981,   983,   985,   987,
     989,   991,   993,   995,   997,   999,  1001,  1003,  1005,  1007,
    1009,  1011,  1013,  1015,  1017,  1019,  1021,  1023,  1025,  1027,
    1029,  1031,  1033,  1035,  1037,  1039,  1041,  1043,  1045,  1047,
    1049,  1051,  1053,  1055,  1057,  1059,  1061,  1063,  1065,  1067,
    1069,  1071,  1073,  1075,  1077,  1079,  1081,  1083,  1085,  1087,
    1089,  1091,  1093,  1095,  1097,  1099,  1101,  1103,  1105,  1107,
    1109,  1111,  1113,  1115,  1119,  1121,  1123,  1125,  1127,  1133,
    1135,  1137,  1142,  1144,  1146,  1148,  1153,  1155,  1157,  1161,
    1163,  1165,  1167,  1169,  1171,  1173,  1175,  1177,  1179,  1181,
    1183,  1185,  1187,  1189,  1191,  1193,  1195,  1197,  1199,  1204,
    1206,  1208,  1210,  1212,  1214,  1216,  1218,  1220,  1222,  1224,
    1226,  1228,  1232,  1234,  1238,  1240,  1243,  1245,  1249,  1251,
    1253,  1255,  1257,  1259,  1261,  1263,  1267,  1271,  1275,  1279,
    1283,  1288,  1296,  1298,  1301,  1303,  1308,  1312,  1314,  1317,
    1319,  1323,  1325,  1328,  1331,  1335,  1337
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LABEL", "IDENTIFIER", "INTCONSTANT",
  "LONGCONSTANT", "FLOATCONSTANT", "DOUBLECONSTANT", "CHARCONSTANT",
  "STRING_LITERAL", "CLASS", "EXTENDS", "ACCESS", "IMPLEMENTS", "FIELD",
  "METHOD", "MAX_STACK", "MAX_LOCALS", "CODE", "PUBLIC", "PRIVATE",
  "PROTECTED", "ABSTRACT", "FINAL", "INTERFACE", "STATIC", "NATIVE",
  "SYNCHRONIZED", "TRANSIENT", "VOLATILE", "BYTE", "CHAR", "DOUBLE",
  "FLOAT", "INT", "LONG", "SHORT", "BOOLEAN", "VOID", "DEFAULT", "TO",
  "EXCEPTIONS", "SOURCEFILE", "THROWS", "LINENUMBERTABLE",
  "LOCALVARIABLETABLE", "ACC_PUBLIC", "ACC_PRIVATE", "ACC_PROTECTED",
  "ACC_STATIC", "ACC_FINAL", "ACC_SYNCHRONIZED", "ACC_VOLATILE",
  "ACC_TRANSIENT", "ACC_NATIVE", "ACC_INTERFACE", "ACC_ABSTRACT", "AALOAD",
  "AASTORE", "ACONST_NULL", "ALOAD_0", "ALOAD_1", "ALOAD_2", "ALOAD_3",
  "ANEWARRAY", "ARETURN", "ARRAYLENGTH", "ASTORE_0", "ASTORE_1",
  "ASTORE_2", "ASTORE_3", "ATHROW", "BALOAD", "BASTORE", "BIPUSH",
  "CALOAD", "CASTORE", "CHECKCAST", "D2F", "D2I", "D2L", "DADD", "DALOAD",
  "DASTORE", "DCMPG", "DCMPL", "DCONST_0", "DCONST_1", "DDIV", "DLOAD_0",
  "DLOAD_1", "DLOAD_2", "DLOAD_3", "DMUL", "DNEG", "DREM", "DRETURN",
  "DSTORE_0", "DSTORE_1", "DSTORE_2", "DSTORE_3", "DSUB", "DUP", "DUP_X1",
  "DUP_X2", "DUP2", "DUP2_X1", "DUP2_X2", "F2D", "F2I", "F2L", "FADD",
  "FALOAD", "FASTORE", "FCMPG", "FCMPL", "FCONST_0", "FCONST_1",
  "FCONST_2", "FDIV", "FLOAD_0", "FLOAD_1", "FLOAD_2", "FLOAD_3", "FMUL",
  "FNEG", "FREM", "FRETURN", "FSTORE_0", "FSTORE_1", "FSTORE_2",
  "FSTORE_3", "FSUB", "GETFIELD", "GETSTATIC", "GOTO", "GOTO_W", "I2B",
  "I2C", "I2D", "I2F", "I2L", "I2S", "IADD", "IALOAD", "IAND", "IASTORE",
  "ICONST_0", "ICONST_1", "ICONST_2", "ICONST_3", "ICONST_4", "ICONST_5",
  "ICONST_M1", "IDIV", "IF_ACMPEQ", "IF_ACMPNE", "IF_ICMPEQ", "IF_ICMPNE",
  "IF_ICMPLT", "IF_ICMPGE", "IF_ICMPGT", "IF_ICMPLE", "IFEQ", "IFNE",
  "IFLT", "IFGE", "IFGT", "IFLE", "IFNONNULL", "IFNULL", "ILOAD_0",
  "ILOAD_1", "ILOAD_2", "ILOAD_3", "IMUL", "INEG", "IOR", "IREM",
  "IRETURN", "ISHL", "ISHR", "ISTORE_0", "ISTORE_1", "ISTORE_2",
  "ISTORE_3", "ISUB", "IUSHR", "IXOR", "JSR", "JSR_W", "L2D", "L2F", "L2I",
  "LADD", "LALOAD", "LAND", "LASTORE", "LCMP", "LCONST_0", "LCONST_1",
  "LDIV", "LLOAD_0", "LLOAD_1", "LLOAD_2", "LLOAD_3", "LMUL", "LNEG",
  "LOR", "LREM", "LRETURN", "LSHL", "LSHR", "LSTORE_0", "LSTORE_1",
  "LSTORE_2", "LSTORE_3", "LSUB", "LUSHR", "LXOR", "MONITORENTER",
  "MONITOREXIT", "NOP", "POP", "POP2", "RETURN", "SALOAD", "SASTORE",
  "SWAP", "IINC", "INSTANCEOF", "INVOKEINTERFACE", "INVOKENONVIRTUAL",
  "INVOKESTATIC", "INVOKEVIRTUAL", "LDC", "LDC_W", "LDC2_W",
  "MULTIANEWARRAY", "NEW", "NEWARRAY", "PUTFIELD", "PUTSTATIC", "SIPUSH",
  "ILOAD", "FLOAD", "ALOAD", "LLOAD", "DLOAD", "ISTORE", "FSTORE",
  "ASTORE", "LSTORE", "DSTORE", "RET", "WIDE", "LOAD", "STORE",
  "LOOKUPSWITCH", "TABLESWITCH", "'{'", "'}'", "'.'", "'='", "'('", "')'",
  "','", "'['", "']'", "':'", "$accept", "compilation_unit", "$@1",
  "classlist", "class", "$@2", "class_modifiers", "classword",
  "superclass", "classname", "classfieldmethodname", "endname",
  "abstract_entry", "final_entry", "public_entry", "interface_entry",
  "static_entry", "native_entry", "synchronized_entry", "transient_entry",
  "volatile_entry", "access_specifier", "interfaces", "interfacelist",
  "fieldlist", "field", "fieldconstant", "field_modifiers", "methodlist",
  "method", "$@3", "$@4", "throwslist", "throwsentries", "max_locals_decl",
  "returntype", "arguments", "argumentlist", "methodarguments",
  "methodargumentlist", "methodargument", "type", "basetype", "arrayadder",
  "method_modifiers", "localvar", "arrayorclassname", "code", "op_list",
  "op_line", "label", "op", "no_arg_op", "one_arg_op", "methodref_arg_op",
  "fieldref_arg_op", "class_arg_op", "label_arg_op", "localvar_arg_op",
  "localvar_arg", "lookuplist", "tablelist", "newarraytype", "argument",
  "exceptiontable", "exceptionslist", "linenumbertable", "linenumberlist",
  "localvariabletable", "localvariablelist", "sourcefilename", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   123,   125,    46,    61,    40,    41,    44,    91,    93,
      58
};
# endif

#define YYPACT_NINF (-314)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-88)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -314,    14,     5,  -314,  -314,  -314,    13,    32,     7,  -314,
    -314,    45,  -314,    34,    41,  -314,    30,    62,    53,  -314,
    -314,  -195,  -314,    62,  -192,    62,  -314,    62,  -314,  -314,
    -314,    55,    20,  -314,  -314,  -314,    49,  -314,    57,    -4,
    -314,  -314,     7,    42,    20,    71,  -314,  -178,    56,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,    83,  -180,
      57,  -314,  -314,  -314,    59,  -174,  -177,  -314,     5,  -314,
    -314,  -314,    27,  -314,  -180,     7,    26,  -314,  -314,  -314,
    -314,  -314,    64,  -314,    89,  -314,  -314,    66,  -170,  -314,
    -314,    42,  -169,  -314,  -171,    94,    58,    42,  -314,    62,
      82,  -314,  -314,    62,    95,  -314,    85,    96,  -314,  -314,
    -157,  -314,    63,   333,  -155,    65,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,    42,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,    33,
    -314,    26,  -314,  -314,  -314,  -314,  -314,  -314,    42,  -314,
     -16,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,    68,   104,
     107,  -314,  -314,   590,  -314,  -314,    19,    26,    42,    62,
     108,    33,  -314,  -148,    69,  -154,  -152,  -314,  -314,  -314,
     112,   114,   115,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,   117,    78,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,   114,   114,  -314,  -314,  -314,    -1,
    -314,  -139,  -138,  -146,  -314,  -137,  -140,  -314,  -134,   123,
    -136,  -314,   126,  -314,     0,  -314,  -314,  -180,   127,    42,
     128,    92,    42,   130,   131,  -314,     1,  -314,  -128,  -314,
    -129,  -314,  -127,  -132,  -123,   137,  -124,    43,  -314,   139,
    -314,   127,   140,    42,   142,  -314,  -114,  -314,  -314,  -314,
      42,  -314,  -314,  -314,   128,   144,   145,  -314,   144,  -118,
     146,  -314,  -314,  -314
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,    20,     1,    19,     3,    20,    10,    22,     4,
       9,     0,    21,    24,    12,    23,    26,     0,    43,    25,
       8,    14,    11,     0,     0,     0,    45,    42,    47,    13,
      44,     6,    41,    56,    46,    40,    37,    39,    28,   346,
      38,    27,    22,     0,    41,     0,    55,     0,    34,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,    89,
      28,   345,     7,    33,    36,    53,     0,    78,    20,    57,
      35,    54,     0,    48,    89,    22,     0,    49,    51,    50,
      52,    88,    30,    67,     0,    66,    29,    32,     0,    31,
      90,    73,     0,    72,    75,    76,    61,     0,    77,     0,
       0,    74,    63,    60,     0,    62,    65,     0,    58,    64,
       0,    96,   333,    94,     0,   338,   101,   116,   117,   118,
     119,   120,   121,   122,     0,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   264,   132,   133,   276,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     156,   157,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   272,   273,   279,   280,   189,   190,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,   201,
     202,   203,   204,   205,   206,   281,   282,   283,   284,   285,
     286,   287,   288,   289,   290,   291,   292,   293,   294,   295,
     296,   207,   208,   209,   210,   211,   212,   213,   214,   215,
     216,   217,   218,   219,   220,   221,   222,   223,   224,   297,
     298,   225,   226,   227,   228,   229,   230,   231,   232,   233,
     234,   235,   236,   237,   238,   239,   240,   241,   242,   243,
     244,   245,   246,   247,   248,   249,   250,   251,   252,   253,
     254,   255,   256,   257,   258,   259,   260,   261,   262,     0,
     277,     0,   270,   269,   271,   265,   266,   267,     0,   278,
       0,   274,   275,   268,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   263,   310,   311,     0,     0,
       0,    99,    95,     0,    98,   102,     0,     0,     0,     0,
       0,     0,   336,     0,   342,    92,     0,   107,   313,   312,
       0,     0,     0,   322,   319,   321,   320,   324,   325,   323,
     318,   115,     0,     0,    91,   100,    97,   326,   327,   328,
     330,   331,   329,   103,     0,     0,   106,   109,   110,     0,
     340,     0,     0,     0,   111,    16,     0,   114,     0,     0,
       0,   104,     0,   332,     0,   344,    59,    89,     0,    69,
     315,     0,    69,     0,     0,   337,     0,    93,    18,    15,
       0,    68,    71,     0,     0,     0,     0,     0,   339,     0,
     341,     0,     0,     0,     0,   112,     0,   105,   335,   334,
       0,    17,   108,    70,   315,   317,     0,   314,   317,     0,
       0,   316,   113,   343
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -314,  -314,  -314,   147,  -314,  -314,  -314,  -314,  -314,   -17,
    -313,  -261,    84,   -31,  -314,  -314,    97,  -314,  -314,  -314,
    -314,   110,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314,  -314,  -314,  -314,  -314,  -282,  -237,  -257,  -314,    61,
    -314,   -41,  -117,   -73,  -314,  -164,  -135,  -314,  -314,  -314,
    -314,  -163,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -167,
    -263,  -266,  -314,  -314,  -314,  -314,  -314,  -314,  -314,  -314,
    -314
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,     6,    33,     7,    11,    18,    57,
     376,   399,     8,    13,    16,    20,    42,    87,    90,    64,
      71,    38,    24,    27,    31,    34,    73,    43,    39,    46,
      76,   110,   100,   103,   108,    84,   400,   401,    92,    93,
      94,    85,    59,    67,    69,   321,   337,   112,   113,   322,
     323,   324,   325,   326,   327,   328,   329,   330,   331,   340,
     404,   429,   351,   363,   115,   369,   334,   384,   372,   396,
      47
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      22,    81,    58,   382,   394,   409,    26,   336,    29,   341,
      30,    48,    44,    -5,     3,   343,   344,   345,   346,   347,
     348,   349,   350,   357,   358,   359,   360,   361,     4,   362,
      21,    12,    77,    78,    79,    80,     4,   338,   339,    45,
      35,    36,    37,    10,    82,   364,    21,    21,   418,    14,
      95,   380,   381,    17,    15,    19,    95,    49,    50,    51,
      52,    53,    54,    55,    56,    83,    21,    23,    25,    28,
      32,    40,   320,    49,    50,    51,    52,    53,    54,    55,
      56,    61,   102,    41,    62,    63,   105,    65,    66,    70,
      72,    86,    74,    88,    89,    91,    97,    96,    98,   104,
     106,   109,    99,   107,   111,   114,   332,   335,   352,   353,
     333,   354,   367,   370,   -87,   371,   373,   374,   375,   379,
     377,   378,   385,   387,   386,   389,   388,   390,   391,   392,
     393,   398,   405,   403,   407,   411,   408,   412,   414,   415,
     413,   416,   417,   420,   432,   422,   424,   425,   428,   430,
     421,   433,    75,     9,    60,   406,   423,    68,   101,   355,
     356,   427,   431,   342,   368,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   336,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   383,   395,   410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   335,   320,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,     0,   397,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   116,    21,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   402,     0,
       0,   402,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,    54,
      55,    56,   402,     0,     0,     0,     0,     0,     0,   426,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     156,   157,   158,   159,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,   201,   202,   203,   204,   205,
     206,   207,   208,   209,   210,   211,   212,   213,   214,   215,
     216,   217,   218,   219,   220,   221,   222,   223,   224,   225,
     226,   227,   228,   229,   230,   231,   232,   233,   234,   235,
     236,   237,   238,   239,   240,   241,   242,   243,   244,   245,
     246,   247,   248,   249,   250,   251,   252,   253,   254,   255,
     256,   257,   258,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   270,   271,   272,   273,   274,   275,
     276,   277,   278,   279,   280,   281,   282,   283,   284,   285,
     286,   287,   288,   289,   290,   291,   292,   293,   294,   295,
     296,   297,   298,   299,   300,   301,   302,   303,   304,   305,
     306,   307,   308,   309,   310,   311,   312,   313,   314,   315,
     316,   317,   318,   319,    21,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   214,   215,   216,   217,   218,
     219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   232,   233,   234,   235,   236,   237,   238,
     239,   240,   241,   242,   243,   244,   245,   246,   247,   248,
     249,   250,   251,   252,   253,   254,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319
};

static const yytype_int16 yycheck[] =
{
      17,    74,    43,     4,     4,     4,    23,   124,    25,   291,
      27,    42,    16,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,     4,     5,     6,     7,     8,    23,    10,
       4,    24,     5,     6,     7,     8,    23,     4,     5,    43,
      20,    21,    22,    11,    75,   327,     4,     4,     5,     4,
      91,   364,   365,    12,    20,    25,    97,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     4,    14,   263,   261,
      15,    22,   113,    31,    32,    33,    34,    35,    36,    37,
      38,    10,    99,    26,   262,    29,   103,     4,   268,    30,
     264,    27,   269,     4,    28,   265,   267,   266,     4,    17,
       5,     5,    44,    18,   261,    42,   261,   124,    40,     5,
      45,     4,     4,   261,   268,    46,   268,     5,     4,    41,
       5,     4,   261,   269,   262,   265,   263,   261,     5,   265,
       4,     4,    40,     5,     4,   263,     5,   266,   270,   262,
     267,     4,   266,     4,   262,     5,     4,   261,     4,     4,
     411,     5,    68,     6,    44,   392,   413,    60,    97,   323,
     323,   424,   428,   298,   331,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   262,   262,   262,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   298,   323,    -1,    -1,    -1,    -1,   328,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   329,    -1,   387,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   389,    -1,
      -1,   392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,   413,    -1,    -1,    -1,    -1,    -1,    -1,   420,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     407,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   208,   209,   210,   211,   212,   213,   214,   215,   216,
     217,   218,   219,   220,   221,   222,   223,   224,   225,   226,
     227,   228,   229,   230,   231,   232,   233,   234,   235,   236,
     237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
     247,   248,   249,   250,   251,   252,   253,   254,   255,   256,
     257,   258,   259,   260,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,   155,   156,   157,   158,   159,
     160,   161,   162,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,   201,   202,   203,   204,   205,   206,   207,   208,   209,
     210,   211,   212,   213,   214,   215,   216,   217,   218,   219,
     220,   221,   222,   223,   224,   225,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   236,   237,   238,   239,
     240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
     250,   251,   252,   253,   254,   255,   256,   257,   258,   259,
     260
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   272,   273,     0,    23,   274,   275,   277,   283,   274,
      11,   278,    24,   284,     4,    20,   285,    12,   279,    25,
     286,     4,   280,    14,   293,   263,   280,   294,   261,   280,
     280,   295,    15,   276,   296,    20,    21,    22,   292,   299,
      22,    26,   287,   298,    16,    43,   300,   341,   284,    31,
      32,    33,    34,    35,    36,    37,    38,   280,   312,   313,
     292,    10,   262,    29,   290,     4,   268,   314,   287,   315,
      30,   291,   264,   297,   269,   283,   301,     5,     6,     7,
       8,   314,   284,    39,   306,   312,    27,   288,     4,    28,
     289,   265,   309,   310,   311,   312,   266,   267,     4,    44,
     303,   310,   280,   304,    17,   280,     5,    18,   305,     5,
     302,   261,   318,   319,    42,   335,     3,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   213,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   223,   224,   225,   226,   227,   228,   229,   230,
     231,   232,   233,   234,   235,   236,   237,   238,   239,   240,
     241,   242,   243,   244,   245,   246,   247,   248,   249,   250,
     251,   252,   253,   254,   255,   256,   257,   258,   259,   260,
     312,   316,   320,   321,   322,   323,   324,   325,   326,   327,
     328,   329,   261,    45,   337,   280,   313,   317,     4,     5,
     330,   306,   317,    31,    32,    33,    34,    35,    36,    37,
      38,   333,    40,     5,     4,   316,   322,     4,     5,     6,
       7,     8,    10,   334,   306,   312,   280,     4,   330,   336,
     261,    46,   339,   268,     5,     4,   281,     5,     4,    41,
     281,   281,     4,   262,   338,   261,   262,   269,   263,   265,
     261,     5,   265,     4,     4,   262,   340,   314,     4,   282,
     307,   308,   312,     5,   331,    40,   307,     4,     5,     4,
     262,   263,   266,   267,   270,   262,     4,   266,     5,   280,
       4,   282,     5,   308,     4,   261,   312,   331,     4,   332,
       4,   332,   262,     5
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   271,   273,   272,   274,   274,   276,   275,   277,   278,
     278,   279,   279,   280,   280,   281,   281,   282,   282,   283,
     283,   284,   284,   285,   285,   286,   286,   287,   287,   288,
     288,   289,   289,   290,   290,   291,   291,   292,   292,   292,
     292,   292,   293,   293,   294,   294,   295,   295,   296,   297,
     297,   297,   297,   297,   298,   299,   299,   301,   302,   300,
     303,   303,   304,   304,   305,   305,   306,   306,   307,   307,
     308,   308,   309,   309,   310,   310,   311,   311,   312,   313,
     313,   313,   313,   313,   313,   313,   313,   313,   314,   314,
     315,   316,   317,   317,   318,   319,   319,   320,   320,   320,
     320,   321,   322,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   322,   322,   322,   322,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   324,   324,   324,   324,   324,   325,
     325,   325,   326,   326,   326,   326,   327,   327,   327,   328,
     328,   328,   328,   328,   328,   328,   328,   328,   328,   328,
     328,   328,   328,   328,   328,   328,   328,   328,   328,   329,
     329,   329,   329,   329,   329,   329,   329,   329,   329,   329,
     329,   329,   330,   330,   331,   331,   332,   332,   333,   333,
     333,   333,   333,   333,   333,   333,   334,   334,   334,   334,
     334,   334,   335,   335,   336,   336,   336,   337,   337,   338,
     338,   339,   339,   340,   340,   341,   341
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     0,    11,     4,     1,
       0,     2,     0,     3,     1,     3,     1,     3,     1,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     2,     1,
       1,     0,     2,     0,     2,     1,     2,     0,     6,     2,
       2,     2,     2,     0,     4,     2,     0,     0,     0,    20,
       2,     0,     2,     1,     2,     0,     1,     1,     1,     0,
       3,     1,     1,     0,     3,     1,     1,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     0,
       5,     2,     1,     4,     1,     2,     0,     2,     1,     1,
       2,     1,     1,     2,     3,     6,     2,     2,     7,     2,
       2,     3,     6,     9,     3,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     0,     2,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     0,     5,     5,     0,     4,     0,     3,
       0,     4,     0,     6,     0,     2,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 403 "javaa.y"
      { InitAssembler();}
#line 2193 "javaa.tab.c"
    break;

  case 3:
#line 404 "javaa.y"
            { EndAssembler();}
#line 2199 "javaa.tab.c"
    break;

  case 6:
#line 414 "javaa.y"
        {
        SetThisClass((yyvsp[-6].intval), (yyvsp[-4].string), (yyvsp[-3].string));
        }
#line 2207 "javaa.tab.c"
    break;

  case 8:
#line 422 "javaa.y"
        {(yyval.intval) = (yyvsp[-3].intval) | (yyvsp[-2].intval) | (yyvsp[-1].intval) | (yyvsp[0].intval);}
#line 2213 "javaa.tab.c"
    break;

  case 9:
#line 426 "javaa.y"
        {break;}
#line 2219 "javaa.tab.c"
    break;

  case 10:
#line 427 "javaa.y"
        {break;}
#line 2225 "javaa.tab.c"
    break;

  case 11:
#line 433 "javaa.y"
        { (yyval.string) = (yyvsp[0].string);}
#line 2231 "javaa.tab.c"
    break;

  case 12:
#line 434 "javaa.y"
      {
        char* tempstring;
        int len = strlen("java/lang/Object")+1;
        tempstring = (char *) malloc(sizeof(char)*len);
        strncpy(tempstring,"java/lang/Object", len);
        (yyval.string) = tempstring;
      }
#line 2243 "javaa.tab.c"
    break;

  case 13:
#line 444 "javaa.y"
        {
          (yyval.string) = ConsStrings((yyvsp[-2].string),ConsStrings("/",(yyvsp[0].string)));
        }
#line 2251 "javaa.tab.c"
    break;

  case 14:
#line 448 "javaa.y"
        {
          (yyval.string) = (yyvsp[0].string);
        }
#line 2259 "javaa.tab.c"
    break;

  case 15:
#line 455 "javaa.y"
        {
          if ((yyvsp[0].classfieldmethodstruct).classname == NULL) (yyval.classfieldmethodstruct).classname = (yyvsp[-2].string);
          else (yyval.classfieldmethodstruct).classname = ConsStrings((yyvsp[-2].string),ConsStrings("/",(yyvsp[0].classfieldmethodstruct).classname));
          (yyval.classfieldmethodstruct).fieldmethodname = (yyvsp[0].classfieldmethodstruct).fieldmethodname;
        }
#line 2269 "javaa.tab.c"
    break;

  case 16:
#line 461 "javaa.y"
        {
          (yyval.classfieldmethodstruct).classname = GetThisClass();
          (yyval.classfieldmethodstruct).fieldmethodname = (yyvsp[0].string);
        }
#line 2278 "javaa.tab.c"
    break;

  case 17:
#line 469 "javaa.y"
        {
          if ((yyvsp[0].classfieldmethodstruct).classname == NULL) (yyval.classfieldmethodstruct).classname = (yyvsp[-2].string);
          else (yyval.classfieldmethodstruct).classname = ConsStrings((yyvsp[-2].string),ConsStrings("/",(yyvsp[0].classfieldmethodstruct).classname));
          (yyval.classfieldmethodstruct).fieldmethodname = (yyvsp[0].classfieldmethodstruct).fieldmethodname;
        }
#line 2288 "javaa.tab.c"
    break;

  case 18:
#line 475 "javaa.y"
        {
          (yyval.classfieldmethodstruct).classname = NULL;
          (yyval.classfieldmethodstruct).fieldmethodname = (yyvsp[0].string);
        }
#line 2297 "javaa.tab.c"
    break;

  case 19:
#line 483 "javaa.y"
        {(yyval.intval) = 0x0400;}
#line 2303 "javaa.tab.c"
    break;

  case 20:
#line 484 "javaa.y"
      {(yyval.intval)=0;}
#line 2309 "javaa.tab.c"
    break;

  case 21:
#line 489 "javaa.y"
        {(yyval.intval) = 0x0010;}
#line 2315 "javaa.tab.c"
    break;

  case 22:
#line 490 "javaa.y"
      {(yyval.intval)=0;}
#line 2321 "javaa.tab.c"
    break;

  case 23:
#line 495 "javaa.y"
        {(yyval.intval) = 0x0001;}
#line 2327 "javaa.tab.c"
    break;

  case 24:
#line 496 "javaa.y"
      {(yyval.intval)=0;}
#line 2333 "javaa.tab.c"
    break;

  case 25:
#line 501 "javaa.y"
        {(yyval.intval) = 0x0200;}
#line 2339 "javaa.tab.c"
    break;

  case 26:
#line 502 "javaa.y"
      {(yyval.intval)=0;}
#line 2345 "javaa.tab.c"
    break;

  case 27:
#line 507 "javaa.y"
        {(yyval.intval) = 0x0008;}
#line 2351 "javaa.tab.c"
    break;

  case 28:
#line 508 "javaa.y"
      {(yyval.intval)=0;}
#line 2357 "javaa.tab.c"
    break;

  case 29:
#line 513 "javaa.y"
        {(yyval.intval) = 0x0100;}
#line 2363 "javaa.tab.c"
    break;

  case 30:
#line 514 "javaa.y"
      {(yyval.intval)=0;}
#line 2369 "javaa.tab.c"
    break;

  case 31:
#line 519 "javaa.y"
        {(yyval.intval) = 0x0020;}
#line 2375 "javaa.tab.c"
    break;

  case 32:
#line 520 "javaa.y"
      {(yyval.intval)=0;}
#line 2381 "javaa.tab.c"
    break;

  case 33:
#line 525 "javaa.y"
        {(yyval.intval) = 0x0080;}
#line 2387 "javaa.tab.c"
    break;

  case 34:
#line 526 "javaa.y"
      {(yyval.intval)=0;}
#line 2393 "javaa.tab.c"
    break;

  case 35:
#line 531 "javaa.y"
        {(yyval.intval) = 0x0040;}
#line 2399 "javaa.tab.c"
    break;

  case 36:
#line 532 "javaa.y"
      {(yyval.intval)=0;}
#line 2405 "javaa.tab.c"
    break;

  case 37:
#line 537 "javaa.y"
        {(yyval.intval) = 2;}
#line 2411 "javaa.tab.c"
    break;

  case 38:
#line 539 "javaa.y"
        {(yyval.intval) = 6;}
#line 2417 "javaa.tab.c"
    break;

  case 39:
#line 541 "javaa.y"
        {(yyval.intval) = 4;}
#line 2423 "javaa.tab.c"
    break;

  case 40:
#line 543 "javaa.y"
        {(yyval.intval) = 1;}
#line 2429 "javaa.tab.c"
    break;

  case 41:
#line 544 "javaa.y"
      {(yyval.intval)=0;}
#line 2435 "javaa.tab.c"
    break;

  case 42:
#line 549 "javaa.y"
        {break;}
#line 2441 "javaa.tab.c"
    break;

  case 43:
#line 550 "javaa.y"
        {break;}
#line 2447 "javaa.tab.c"
    break;

  case 44:
#line 555 "javaa.y"
        { AddToInterfaceList((yyvsp[0].string));}
#line 2453 "javaa.tab.c"
    break;

  case 45:
#line 557 "javaa.y"
        { AddToInterfaceList((yyvsp[0].string));}
#line 2459 "javaa.tab.c"
    break;

  case 47:
#line 562 "javaa.y"
      {break;}
#line 2465 "javaa.tab.c"
    break;

  case 48:
#line 566 "javaa.y"
        {NewField((yyvsp[-4].intval)|(yyvsp[-3].intval), (yyvsp[-1].string), (yyvsp[-2].string), (yyvsp[0].argtype));}
#line 2471 "javaa.tab.c"
    break;

  case 49:
#line 571 "javaa.y"
             {(yyval.argtype).type = INTCONSTANT;
              (yyval.argtype).intval = (yyvsp[0].intval);
             }
#line 2479 "javaa.tab.c"
    break;

  case 50:
#line 575 "javaa.y"
             {(yyval.argtype).type = FLOATCONSTANT;
              (yyval.argtype).floatval = (yyvsp[0].floatval);
             }
#line 2487 "javaa.tab.c"
    break;

  case 51:
#line 579 "javaa.y"
             {(yyval.argtype).type = LONGCONSTANT;
              (yyval.argtype).longval = (yyvsp[0].longval);
             }
#line 2495 "javaa.tab.c"
    break;

  case 52:
#line 583 "javaa.y"
             {(yyval.argtype).type = DOUBLECONSTANT;
              (yyval.argtype).doubleval = (yyvsp[0].doubleval);
             }
#line 2503 "javaa.tab.c"
    break;

  case 53:
#line 587 "javaa.y"
             {(yyval.argtype).type = 0;
             }
#line 2510 "javaa.tab.c"
    break;

  case 54:
#line 593 "javaa.y"
        { (yyval.intval) = (yyvsp[-3].intval) | (yyvsp[-2].intval) | (yyvsp[-1].intval) | (yyvsp[0].intval) ;}
#line 2516 "javaa.tab.c"
    break;

  case 56:
#line 598 "javaa.y"
      {break;}
#line 2522 "javaa.tab.c"
    break;

  case 57:
#line 602 "javaa.y"
                                                   {NewNewMethod((yyvsp[-1].intval)|(yyvsp[0].intval));}
#line 2528 "javaa.tab.c"
    break;

  case 58:
#line 607 "javaa.y"
      {
        char* tmpstr;
        /*message("Calling NewMethod.");*/
        tmpstr = ConsStrings("(",ConsStrings((yyvsp[-5].string),ConsStrings(")",(yyvsp[-8].string))));
        /*message(tmpstr);*/
        NewMethod((yyvsp[-7].string), tmpstr, (yyvsp[-1].intval), (yyvsp[0].intval)); }
#line 2539 "javaa.tab.c"
    break;

  case 59:
#line 619 "javaa.y"
        {EndMethod();}
#line 2545 "javaa.tab.c"
    break;

  case 60:
#line 624 "javaa.y"
        {break;}
#line 2551 "javaa.tab.c"
    break;

  case 61:
#line 625 "javaa.y"
        {break;}
#line 2557 "javaa.tab.c"
    break;

  case 62:
#line 630 "javaa.y"
        {AddToThrowsList((yyvsp[0].string));}
#line 2563 "javaa.tab.c"
    break;

  case 63:
#line 632 "javaa.y"
        {AddToThrowsList((yyvsp[0].string));}
#line 2569 "javaa.tab.c"
    break;

  case 64:
#line 637 "javaa.y"
        {(yyval.intval) = (yyvsp[0].intval);}
#line 2575 "javaa.tab.c"
    break;

  case 65:
#line 638 "javaa.y"
        {(yyval.intval) = -1;}
#line 2581 "javaa.tab.c"
    break;

  case 66:
#line 643 "javaa.y"
        { (yyval.string) = (yyvsp[0].string); }
#line 2587 "javaa.tab.c"
    break;

  case 67:
#line 645 "javaa.y"
            { char* tempstring;
              tempstring = (char *) malloc(sizeof(char) * (strlen("B")+1));
              strcpy(tempstring,"V");
              (yyval.string) = tempstring;
        }
#line 2597 "javaa.tab.c"
    break;

  case 68:
#line 654 "javaa.y"
        { (yyval.string) = (yyvsp[0].string);}
#line 2603 "javaa.tab.c"
    break;

  case 69:
#line 655 "javaa.y"
      {(yyval.string) = NULL;}
#line 2609 "javaa.tab.c"
    break;

  case 70:
#line 659 "javaa.y"
        {
          /*message("in arguments with comma.");*/
          (yyval.string) = ConsStrings((yyvsp[-2].string),(yyvsp[0].string));
        }
#line 2618 "javaa.tab.c"
    break;

  case 71:
#line 664 "javaa.y"
        { (yyval.string) = (yyvsp[0].string); /*message("in arguments");*/}
#line 2624 "javaa.tab.c"
    break;

  case 72:
#line 670 "javaa.y"
        { (yyval.string) = (yyvsp[0].string);}
#line 2630 "javaa.tab.c"
    break;

  case 73:
#line 671 "javaa.y"
      {(yyval.string) = NULL;}
#line 2636 "javaa.tab.c"
    break;

  case 74:
#line 675 "javaa.y"
        {
          /*message("in arguments with comma.");*/
          (yyval.string) = ConsStrings((yyvsp[-2].string),(yyvsp[0].string));
        }
#line 2645 "javaa.tab.c"
    break;

  case 75:
#line 680 "javaa.y"
        { (yyval.string) = (yyvsp[0].string); /*message("in arguments");*/}
#line 2651 "javaa.tab.c"
    break;

  case 76:
#line 683 "javaa.y"
        { (yyval.string) = (yyvsp[0].string);
          /*message("calling IncrementLocalVarSlot");*/
          IncrementLocalVarSlot((yyvsp[0].string)); /*message("in methodargument");*/}
#line 2659 "javaa.tab.c"
    break;

  case 77:
#line 687 "javaa.y"
        { (yyval.string) = (yyvsp[-1].string); NewLocalVar((yyvsp[0].string), (yyvsp[-1].string));/*message("in methodargument");*/}
#line 2665 "javaa.tab.c"
    break;

  case 78:
#line 692 "javaa.y"
        {
          /*message("In type.");*/
          (yyval.string) = ConsStrings((yyvsp[0].string),(yyvsp[-1].string));
        }
#line 2674 "javaa.tab.c"
    break;

  case 79:
#line 699 "javaa.y"
        { (yyval.string) = ConsStrings("B",""); }
#line 2680 "javaa.tab.c"
    break;

  case 80:
#line 701 "javaa.y"
        { (yyval.string) = ConsStrings("C",""); }
#line 2686 "javaa.tab.c"
    break;

  case 81:
#line 703 "javaa.y"
        { (yyval.string) = ConsStrings("D",""); }
#line 2692 "javaa.tab.c"
    break;

  case 82:
#line 705 "javaa.y"
        { (yyval.string) = ConsStrings("F",""); }
#line 2698 "javaa.tab.c"
    break;

  case 83:
#line 707 "javaa.y"
        { (yyval.string) = ConsStrings("I",""); }
#line 2704 "javaa.tab.c"
    break;

  case 84:
#line 709 "javaa.y"
        { (yyval.string) = ConsStrings("J",""); }
#line 2710 "javaa.tab.c"
    break;

  case 85:
#line 711 "javaa.y"
        { (yyval.string) = ConsStrings("S",""); }
#line 2716 "javaa.tab.c"
    break;

  case 86:
#line 713 "javaa.y"
        { (yyval.string) = ConsStrings("Z",""); }
#line 2722 "javaa.tab.c"
    break;

  case 87:
#line 715 "javaa.y"
        {
         (yyval.string) = ConsStrings("L", ConsStrings((yyvsp[0].string),";"));
         /*message($$);*/
         /*message("Got classname.");*/}
#line 2731 "javaa.tab.c"
    break;

  case 88:
#line 723 "javaa.y"
        {(yyval.string) = ConsStrings("[",(yyvsp[0].string));}
#line 2737 "javaa.tab.c"
    break;

  case 89:
#line 724 "javaa.y"
      {(yyval.string)=NULL;}
#line 2743 "javaa.tab.c"
    break;

  case 90:
#line 730 "javaa.y"
        { (yyval.intval) = (yyvsp[-4].intval) | (yyvsp[-3].intval) | (yyvsp[-2].intval) | (yyvsp[-1].intval) | (yyvsp[0].intval);}
#line 2749 "javaa.tab.c"
    break;

  case 91:
#line 734 "javaa.y"
        {NewLocalVar((yyvsp[0].string), (yyvsp[-1].string));}
#line 2755 "javaa.tab.c"
    break;

  case 92:
#line 739 "javaa.y"
        {(yyval.string) = (yyvsp[0].string);}
#line 2761 "javaa.tab.c"
    break;

  case 93:
#line 741 "javaa.y"
        {(yyval.string) = ConsStrings("[",ConsStrings((yyvsp[0].string),(yyvsp[-3].string)));}
#line 2767 "javaa.tab.c"
    break;

  case 94:
#line 746 "javaa.y"
            {break;}
#line 2773 "javaa.tab.c"
    break;

  case 95:
#line 750 "javaa.y"
            {break;}
#line 2779 "javaa.tab.c"
    break;

  case 96:
#line 752 "javaa.y"
            {break;}
#line 2785 "javaa.tab.c"
    break;

  case 97:
#line 756 "javaa.y"
            {break;}
#line 2791 "javaa.tab.c"
    break;

  case 98:
#line 758 "javaa.y"
            {break;}
#line 2797 "javaa.tab.c"
    break;

  case 99:
#line 760 "javaa.y"
            {break;}
#line 2803 "javaa.tab.c"
    break;

  case 100:
#line 762 "javaa.y"
            {break;}
#line 2809 "javaa.tab.c"
    break;

  case 101:
#line 766 "javaa.y"
        {DefineLabel((yyvsp[0].string));}
#line 2815 "javaa.tab.c"
    break;

  case 102:
#line 770 "javaa.y"
                {GenNoArgCode((yyvsp[0].intval));
        }
#line 2822 "javaa.tab.c"
    break;

  case 103:
#line 773 "javaa.y"
        {GenOneArgCode((yyvsp[-1].intval), (yyvsp[0].argtype));
        }
#line 2829 "javaa.tab.c"
    break;

  case 104:
#line 776 "javaa.y"
        {GenFieldArgCode((yyvsp[-2].intval), (yyvsp[0].classfieldmethodstruct).classname, (yyvsp[0].classfieldmethodstruct).fieldmethodname, (yyvsp[-1].string));
        }
#line 2836 "javaa.tab.c"
    break;

  case 105:
#line 780 "javaa.y"
        {GenMethodArgCode((yyvsp[-5].intval), (yyvsp[-3].classfieldmethodstruct).classname, (yyvsp[-3].classfieldmethodstruct).fieldmethodname,
                     ConsStrings("(",ConsStrings((yyvsp[-1].string),
                   ConsStrings(")",(yyvsp[-4].string)))));
        }
#line 2845 "javaa.tab.c"
    break;

  case 106:
#line 785 "javaa.y"
        {GenClassArgCode((yyvsp[-1].intval), (yyvsp[0].string));
        }
#line 2852 "javaa.tab.c"
    break;

  case 107:
#line 788 "javaa.y"
        {GenClassArgCode((yyvsp[-1].Rk).terminal, (yyvsp[0].string));
        }
#line 2859 "javaa.tab.c"
    break;

  case 108:
#line 792 "javaa.y"
        {GenINVOKEINTERFACECode((yyvsp[-6].Rk).terminal, (yyvsp[-4].classfieldmethodstruct).classname,
                 (yyvsp[-4].classfieldmethodstruct).fieldmethodname,
                     ConsStrings("(",ConsStrings((yyvsp[-2].string),
                   ConsStrings(")",(yyvsp[-5].string)))), (yyvsp[0].intval));
        }
#line 2869 "javaa.tab.c"
    break;

  case 109:
#line 798 "javaa.y"
        {GenLabelArgCode((yyvsp[-1].intval), (yyvsp[0].string));
        }
#line 2876 "javaa.tab.c"
    break;

  case 110:
#line 801 "javaa.y"
        {GenLocalVarArgCode((yyvsp[-1].intval),(yyvsp[0].intval));
        }
#line 2883 "javaa.tab.c"
    break;

  case 111:
#line 804 "javaa.y"
        {GenIINCCode((yyvsp[-2].Rk).terminal,(yyvsp[-1].intval),(yyvsp[0].intval));
        }
#line 2890 "javaa.tab.c"
    break;

  case 112:
#line 807 "javaa.y"
        {GenLOOKUPSWITCHCode((yyvsp[-5].Rk).terminal,(yyvsp[-3].string),(yyvsp[-1].lookuplistptr));
        }
#line 2897 "javaa.tab.c"
    break;

  case 113:
#line 811 "javaa.y"
        {GenTABLESWITCHCode((yyvsp[-8].Rk).terminal,(yyvsp[-7].intval),(yyvsp[-5].intval),(yyvsp[-3].string),(yyvsp[-1].tablelistptr));
        }
#line 2904 "javaa.tab.c"
    break;

  case 114:
#line 814 "javaa.y"
        {GenMULTIANEWARRAYCode((yyvsp[-2].Rk).terminal,(yyvsp[-1].string),(yyvsp[0].intval));
        }
#line 2911 "javaa.tab.c"
    break;

  case 115:
#line 817 "javaa.y"
        {GenNEWARRAYCode((yyvsp[-1].Rk).terminal,(yyvsp[0].intval));
        }
#line 2918 "javaa.tab.c"
    break;

  case 116:
#line 822 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2924 "javaa.tab.c"
    break;

  case 117:
#line 824 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2930 "javaa.tab.c"
    break;

  case 118:
#line 826 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2936 "javaa.tab.c"
    break;

  case 119:
#line 828 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2942 "javaa.tab.c"
    break;

  case 120:
#line 830 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2948 "javaa.tab.c"
    break;

  case 121:
#line 832 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2954 "javaa.tab.c"
    break;

  case 122:
#line 834 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2960 "javaa.tab.c"
    break;

  case 123:
#line 836 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2966 "javaa.tab.c"
    break;

  case 124:
#line 838 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2972 "javaa.tab.c"
    break;

  case 125:
#line 840 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2978 "javaa.tab.c"
    break;

  case 126:
#line 842 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2984 "javaa.tab.c"
    break;

  case 127:
#line 844 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2990 "javaa.tab.c"
    break;

  case 128:
#line 846 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 2996 "javaa.tab.c"
    break;

  case 129:
#line 848 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3002 "javaa.tab.c"
    break;

  case 130:
#line 850 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3008 "javaa.tab.c"
    break;

  case 131:
#line 852 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3014 "javaa.tab.c"
    break;

  case 132:
#line 854 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3020 "javaa.tab.c"
    break;

  case 133:
#line 856 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3026 "javaa.tab.c"
    break;

  case 134:
#line 858 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3032 "javaa.tab.c"
    break;

  case 135:
#line 860 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3038 "javaa.tab.c"
    break;

  case 136:
#line 862 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3044 "javaa.tab.c"
    break;

  case 137:
#line 864 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3050 "javaa.tab.c"
    break;

  case 138:
#line 866 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3056 "javaa.tab.c"
    break;

  case 139:
#line 868 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3062 "javaa.tab.c"
    break;

  case 140:
#line 870 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3068 "javaa.tab.c"
    break;

  case 141:
#line 872 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3074 "javaa.tab.c"
    break;

  case 142:
#line 874 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3080 "javaa.tab.c"
    break;

  case 143:
#line 876 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3086 "javaa.tab.c"
    break;

  case 144:
#line 878 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3092 "javaa.tab.c"
    break;

  case 145:
#line 880 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3098 "javaa.tab.c"
    break;

  case 146:
#line 882 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3104 "javaa.tab.c"
    break;

  case 147:
#line 884 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3110 "javaa.tab.c"
    break;

  case 148:
#line 886 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3116 "javaa.tab.c"
    break;

  case 149:
#line 888 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3122 "javaa.tab.c"
    break;

  case 150:
#line 890 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3128 "javaa.tab.c"
    break;

  case 151:
#line 892 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3134 "javaa.tab.c"
    break;

  case 152:
#line 894 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3140 "javaa.tab.c"
    break;

  case 153:
#line 896 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3146 "javaa.tab.c"
    break;

  case 154:
#line 898 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3152 "javaa.tab.c"
    break;

  case 155:
#line 900 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3158 "javaa.tab.c"
    break;

  case 156:
#line 902 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3164 "javaa.tab.c"
    break;

  case 157:
#line 904 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3170 "javaa.tab.c"
    break;

  case 158:
#line 906 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3176 "javaa.tab.c"
    break;

  case 159:
#line 908 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3182 "javaa.tab.c"
    break;

  case 160:
#line 910 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3188 "javaa.tab.c"
    break;

  case 161:
#line 912 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3194 "javaa.tab.c"
    break;

  case 162:
#line 914 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3200 "javaa.tab.c"
    break;

  case 163:
#line 916 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3206 "javaa.tab.c"
    break;

  case 164:
#line 918 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3212 "javaa.tab.c"
    break;

  case 165:
#line 920 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3218 "javaa.tab.c"
    break;

  case 166:
#line 922 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3224 "javaa.tab.c"
    break;

  case 167:
#line 924 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3230 "javaa.tab.c"
    break;

  case 168:
#line 926 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3236 "javaa.tab.c"
    break;

  case 169:
#line 928 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3242 "javaa.tab.c"
    break;

  case 170:
#line 930 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3248 "javaa.tab.c"
    break;

  case 171:
#line 932 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3254 "javaa.tab.c"
    break;

  case 172:
#line 934 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3260 "javaa.tab.c"
    break;

  case 173:
#line 936 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3266 "javaa.tab.c"
    break;

  case 174:
#line 938 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3272 "javaa.tab.c"
    break;

  case 175:
#line 940 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3278 "javaa.tab.c"
    break;

  case 176:
#line 942 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3284 "javaa.tab.c"
    break;

  case 177:
#line 944 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3290 "javaa.tab.c"
    break;

  case 178:
#line 946 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3296 "javaa.tab.c"
    break;

  case 179:
#line 948 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3302 "javaa.tab.c"
    break;

  case 180:
#line 950 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3308 "javaa.tab.c"
    break;

  case 181:
#line 952 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3314 "javaa.tab.c"
    break;

  case 182:
#line 954 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3320 "javaa.tab.c"
    break;

  case 183:
#line 956 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3326 "javaa.tab.c"
    break;

  case 184:
#line 958 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3332 "javaa.tab.c"
    break;

  case 185:
#line 960 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3338 "javaa.tab.c"
    break;

  case 186:
#line 962 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3344 "javaa.tab.c"
    break;

  case 187:
#line 964 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3350 "javaa.tab.c"
    break;

  case 188:
#line 966 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3356 "javaa.tab.c"
    break;

  case 189:
#line 968 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3362 "javaa.tab.c"
    break;

  case 190:
#line 970 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3368 "javaa.tab.c"
    break;

  case 191:
#line 972 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3374 "javaa.tab.c"
    break;

  case 192:
#line 974 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3380 "javaa.tab.c"
    break;

  case 193:
#line 976 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3386 "javaa.tab.c"
    break;

  case 194:
#line 978 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3392 "javaa.tab.c"
    break;

  case 195:
#line 980 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3398 "javaa.tab.c"
    break;

  case 196:
#line 982 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3404 "javaa.tab.c"
    break;

  case 197:
#line 984 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3410 "javaa.tab.c"
    break;

  case 198:
#line 986 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3416 "javaa.tab.c"
    break;

  case 199:
#line 988 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3422 "javaa.tab.c"
    break;

  case 200:
#line 990 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3428 "javaa.tab.c"
    break;

  case 201:
#line 992 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3434 "javaa.tab.c"
    break;

  case 202:
#line 994 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3440 "javaa.tab.c"
    break;

  case 203:
#line 996 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3446 "javaa.tab.c"
    break;

  case 204:
#line 998 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3452 "javaa.tab.c"
    break;

  case 205:
#line 1000 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3458 "javaa.tab.c"
    break;

  case 206:
#line 1002 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3464 "javaa.tab.c"
    break;

  case 207:
#line 1004 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3470 "javaa.tab.c"
    break;

  case 208:
#line 1006 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3476 "javaa.tab.c"
    break;

  case 209:
#line 1008 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3482 "javaa.tab.c"
    break;

  case 210:
#line 1010 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3488 "javaa.tab.c"
    break;

  case 211:
#line 1012 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3494 "javaa.tab.c"
    break;

  case 212:
#line 1014 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3500 "javaa.tab.c"
    break;

  case 213:
#line 1016 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3506 "javaa.tab.c"
    break;

  case 214:
#line 1018 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3512 "javaa.tab.c"
    break;

  case 215:
#line 1020 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3518 "javaa.tab.c"
    break;

  case 216:
#line 1022 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3524 "javaa.tab.c"
    break;

  case 217:
#line 1024 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3530 "javaa.tab.c"
    break;

  case 218:
#line 1026 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3536 "javaa.tab.c"
    break;

  case 219:
#line 1028 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3542 "javaa.tab.c"
    break;

  case 220:
#line 1030 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3548 "javaa.tab.c"
    break;

  case 221:
#line 1032 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3554 "javaa.tab.c"
    break;

  case 222:
#line 1034 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3560 "javaa.tab.c"
    break;

  case 223:
#line 1036 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3566 "javaa.tab.c"
    break;

  case 224:
#line 1038 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3572 "javaa.tab.c"
    break;

  case 225:
#line 1040 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3578 "javaa.tab.c"
    break;

  case 226:
#line 1042 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3584 "javaa.tab.c"
    break;

  case 227:
#line 1044 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3590 "javaa.tab.c"
    break;

  case 228:
#line 1046 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3596 "javaa.tab.c"
    break;

  case 229:
#line 1048 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3602 "javaa.tab.c"
    break;

  case 230:
#line 1050 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3608 "javaa.tab.c"
    break;

  case 231:
#line 1052 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3614 "javaa.tab.c"
    break;

  case 232:
#line 1054 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3620 "javaa.tab.c"
    break;

  case 233:
#line 1056 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3626 "javaa.tab.c"
    break;

  case 234:
#line 1058 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3632 "javaa.tab.c"
    break;

  case 235:
#line 1060 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3638 "javaa.tab.c"
    break;

  case 236:
#line 1062 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3644 "javaa.tab.c"
    break;

  case 237:
#line 1064 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3650 "javaa.tab.c"
    break;

  case 238:
#line 1066 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3656 "javaa.tab.c"
    break;

  case 239:
#line 1068 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3662 "javaa.tab.c"
    break;

  case 240:
#line 1070 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3668 "javaa.tab.c"
    break;

  case 241:
#line 1072 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3674 "javaa.tab.c"
    break;

  case 242:
#line 1074 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3680 "javaa.tab.c"
    break;

  case 243:
#line 1076 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3686 "javaa.tab.c"
    break;

  case 244:
#line 1078 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3692 "javaa.tab.c"
    break;

  case 245:
#line 1080 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3698 "javaa.tab.c"
    break;

  case 246:
#line 1082 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3704 "javaa.tab.c"
    break;

  case 247:
#line 1084 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3710 "javaa.tab.c"
    break;

  case 248:
#line 1086 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3716 "javaa.tab.c"
    break;

  case 249:
#line 1088 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3722 "javaa.tab.c"
    break;

  case 250:
#line 1090 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3728 "javaa.tab.c"
    break;

  case 251:
#line 1092 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3734 "javaa.tab.c"
    break;

  case 252:
#line 1094 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3740 "javaa.tab.c"
    break;

  case 253:
#line 1096 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3746 "javaa.tab.c"
    break;

  case 254:
#line 1098 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3752 "javaa.tab.c"
    break;

  case 255:
#line 1100 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3758 "javaa.tab.c"
    break;

  case 256:
#line 1102 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3764 "javaa.tab.c"
    break;

  case 257:
#line 1104 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3770 "javaa.tab.c"
    break;

  case 258:
#line 1106 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3776 "javaa.tab.c"
    break;

  case 259:
#line 1108 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3782 "javaa.tab.c"
    break;

  case 260:
#line 1110 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3788 "javaa.tab.c"
    break;

  case 261:
#line 1112 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3794 "javaa.tab.c"
    break;

  case 262:
#line 1114 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3800 "javaa.tab.c"
    break;

  case 263:
#line 1116 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3806 "javaa.tab.c"
    break;

  case 264:
#line 1120 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3812 "javaa.tab.c"
    break;

  case 265:
#line 1122 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3818 "javaa.tab.c"
    break;

  case 266:
#line 1124 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3824 "javaa.tab.c"
    break;

  case 267:
#line 1126 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3830 "javaa.tab.c"
    break;

  case 268:
#line 1128 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3836 "javaa.tab.c"
    break;

  case 269:
#line 1134 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3842 "javaa.tab.c"
    break;

  case 270:
#line 1136 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3848 "javaa.tab.c"
    break;

  case 271:
#line 1138 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3854 "javaa.tab.c"
    break;

  case 272:
#line 1143 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3860 "javaa.tab.c"
    break;

  case 273:
#line 1145 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3866 "javaa.tab.c"
    break;

  case 274:
#line 1147 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3872 "javaa.tab.c"
    break;

  case 275:
#line 1149 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3878 "javaa.tab.c"
    break;

  case 276:
#line 1154 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3884 "javaa.tab.c"
    break;

  case 277:
#line 1156 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3890 "javaa.tab.c"
    break;

  case 278:
#line 1158 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3896 "javaa.tab.c"
    break;

  case 279:
#line 1162 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3902 "javaa.tab.c"
    break;

  case 280:
#line 1164 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3908 "javaa.tab.c"
    break;

  case 281:
#line 1166 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3914 "javaa.tab.c"
    break;

  case 282:
#line 1168 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3920 "javaa.tab.c"
    break;

  case 283:
#line 1170 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3926 "javaa.tab.c"
    break;

  case 284:
#line 1172 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3932 "javaa.tab.c"
    break;

  case 285:
#line 1174 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3938 "javaa.tab.c"
    break;

  case 286:
#line 1176 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3944 "javaa.tab.c"
    break;

  case 287:
#line 1178 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3950 "javaa.tab.c"
    break;

  case 288:
#line 1180 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3956 "javaa.tab.c"
    break;

  case 289:
#line 1182 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3962 "javaa.tab.c"
    break;

  case 290:
#line 1184 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3968 "javaa.tab.c"
    break;

  case 291:
#line 1186 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3974 "javaa.tab.c"
    break;

  case 292:
#line 1188 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3980 "javaa.tab.c"
    break;

  case 293:
#line 1190 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3986 "javaa.tab.c"
    break;

  case 294:
#line 1192 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3992 "javaa.tab.c"
    break;

  case 295:
#line 1194 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 3998 "javaa.tab.c"
    break;

  case 296:
#line 1196 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4004 "javaa.tab.c"
    break;

  case 297:
#line 1198 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4010 "javaa.tab.c"
    break;

  case 298:
#line 1200 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4016 "javaa.tab.c"
    break;

  case 299:
#line 1205 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4022 "javaa.tab.c"
    break;

  case 300:
#line 1207 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4028 "javaa.tab.c"
    break;

  case 301:
#line 1209 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4034 "javaa.tab.c"
    break;

  case 302:
#line 1211 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4040 "javaa.tab.c"
    break;

  case 303:
#line 1213 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4046 "javaa.tab.c"
    break;

  case 304:
#line 1215 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4052 "javaa.tab.c"
    break;

  case 305:
#line 1217 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4058 "javaa.tab.c"
    break;

  case 306:
#line 1219 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4064 "javaa.tab.c"
    break;

  case 307:
#line 1221 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4070 "javaa.tab.c"
    break;

  case 308:
#line 1223 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4076 "javaa.tab.c"
    break;

  case 309:
#line 1225 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4082 "javaa.tab.c"
    break;

  case 310:
#line 1227 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4088 "javaa.tab.c"
    break;

  case 311:
#line 1229 "javaa.y"
        {(yyval.intval)=(yyvsp[0].Rk).terminal;}
#line 4094 "javaa.tab.c"
    break;

  case 312:
#line 1233 "javaa.y"
        { (yyval.intval) = (yyvsp[0].intval);}
#line 4100 "javaa.tab.c"
    break;

  case 313:
#line 1235 "javaa.y"
        { (yyval.intval) = GetLocalVar((yyvsp[0].string));}
#line 4106 "javaa.tab.c"
    break;

  case 314:
#line 1239 "javaa.y"
        { (yyval.lookuplistptr) = AddToLookupList((yyvsp[0].lookuplistptr),(yyvsp[-3].intval),(yyvsp[-1].string)); }
#line 4112 "javaa.tab.c"
    break;

  case 315:
#line 1240 "javaa.y"
        {(yyval.lookuplistptr) = NULL;}
#line 4118 "javaa.tab.c"
    break;

  case 316:
#line 1244 "javaa.y"
        { (yyval.tablelistptr) = AddToTableList((yyvsp[0].tablelistptr),(yyvsp[-1].string)); }
#line 4124 "javaa.tab.c"
    break;

  case 317:
#line 1245 "javaa.y"
        {(yyval.tablelistptr) = NULL;}
#line 4130 "javaa.tab.c"
    break;

  case 318:
#line 1250 "javaa.y"
        { (yyval.intval) = 4;}
#line 4136 "javaa.tab.c"
    break;

  case 319:
#line 1252 "javaa.y"
        { (yyval.intval) = 5;}
#line 4142 "javaa.tab.c"
    break;

  case 320:
#line 1254 "javaa.y"
        { (yyval.intval) = 6;}
#line 4148 "javaa.tab.c"
    break;

  case 321:
#line 1256 "javaa.y"
        { (yyval.intval) = 7;}
#line 4154 "javaa.tab.c"
    break;

  case 322:
#line 1258 "javaa.y"
        { (yyval.intval) = 8;}
#line 4160 "javaa.tab.c"
    break;

  case 323:
#line 1260 "javaa.y"
        { (yyval.intval) = 9;}
#line 4166 "javaa.tab.c"
    break;

  case 324:
#line 1262 "javaa.y"
        { (yyval.intval) = 10;}
#line 4172 "javaa.tab.c"
    break;

  case 325:
#line 1264 "javaa.y"
        { (yyval.intval) = 11;}
#line 4178 "javaa.tab.c"
    break;

  case 326:
#line 1268 "javaa.y"
             {(yyval.argtype).type = IDENTIFIER;
              (yyval.argtype).stringval = (yyvsp[0].string);
             }
#line 4186 "javaa.tab.c"
    break;

  case 327:
#line 1272 "javaa.y"
             {(yyval.argtype).type = INTCONSTANT;
              (yyval.argtype).intval = (yyvsp[0].intval);
             }
#line 4194 "javaa.tab.c"
    break;

  case 328:
#line 1276 "javaa.y"
             {(yyval.argtype).type = LONGCONSTANT;
              (yyval.argtype).longval = (yyvsp[0].longval);
             }
#line 4202 "javaa.tab.c"
    break;

  case 329:
#line 1280 "javaa.y"
             {(yyval.argtype).type = STRING_LITERAL;
              (yyval.argtype).stringval = (yyvsp[0].string);
             }
#line 4210 "javaa.tab.c"
    break;

  case 330:
#line 1284 "javaa.y"
             {(yyval.argtype).type = FLOATCONSTANT;
              (yyval.argtype).floatval = (yyvsp[0].floatval);
          /*message("got a float constant.");*/
             }
#line 4219 "javaa.tab.c"
    break;

  case 331:
#line 1289 "javaa.y"
             {(yyval.argtype).type = DOUBLECONSTANT;
              (yyval.argtype).doubleval = (yyvsp[0].doubleval);
          /*message("got a double constant.");*/
             }
#line 4228 "javaa.tab.c"
    break;

  case 332:
#line 1297 "javaa.y"
        { break; }
#line 4234 "javaa.tab.c"
    break;

  case 333:
#line 1298 "javaa.y"
      {break;}
#line 4240 "javaa.tab.c"
    break;

  case 334:
#line 1302 "javaa.y"
        { AddToExceptionList((yyvsp[-3].string),(yyvsp[-2].string),(yyvsp[-1].string),(yyvsp[0].string)); }
#line 4246 "javaa.tab.c"
    break;

  case 335:
#line 1304 "javaa.y"
        {
          if ((yyvsp[0].intval) != 0) oops("Must have a class name or 0 here.");
              AddToExceptionList((yyvsp[-3].string),(yyvsp[-2].string),(yyvsp[-1].string),NULL);
        }
#line 4255 "javaa.tab.c"
    break;

  case 336:
#line 1308 "javaa.y"
        {break;}
#line 4261 "javaa.tab.c"
    break;

  case 337:
#line 1313 "javaa.y"
        { break; }
#line 4267 "javaa.tab.c"
    break;

  case 338:
#line 1314 "javaa.y"
      {break;}
#line 4273 "javaa.tab.c"
    break;

  case 339:
#line 1318 "javaa.y"
        { AddToLineNumberList((yyvsp[-1].string),(yyvsp[0].intval)); }
#line 4279 "javaa.tab.c"
    break;

  case 340:
#line 1319 "javaa.y"
        {break;}
#line 4285 "javaa.tab.c"
    break;

  case 341:
#line 1324 "javaa.y"
        { break; }
#line 4291 "javaa.tab.c"
    break;

  case 342:
#line 1325 "javaa.y"
      {break;}
#line 4297 "javaa.tab.c"
    break;

  case 343:
#line 1330 "javaa.y"
        { AddToUserLocalVarList((yyvsp[-4].string),(yyvsp[-3].string),(yyvsp[-2].string),(yyvsp[-1].string),(yyvsp[0].intval)); }
#line 4303 "javaa.tab.c"
    break;

  case 344:
#line 1331 "javaa.y"
        {break;}
#line 4309 "javaa.tab.c"
    break;

  case 345:
#line 1336 "javaa.y"
        { SetSourceFile((yyvsp[0].string)); }
#line 4315 "javaa.tab.c"
    break;

  case 346:
#line 1337 "javaa.y"
        {break;}
#line 4321 "javaa.tab.c"
    break;


#line 4325 "javaa.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1339 "javaa.y"

