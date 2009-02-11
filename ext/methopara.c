#include <ruby/ruby.h>

#define GetCoreDataFromValue(obj, type, ptr) do { \
    ptr = (type*)DATA_PTR(obj); \
} while (0)

#define GetISeqPtr(obj, ptr) \
  GetCoreDataFromValue(obj, rb_iseq_t, ptr)

typedef struct rb_proc_struct rb_proc_t;

typedef struct RNode {
    unsigned long flags;
    char *nd_file;
    union {
	      struct RNode *node;
	      ID id;
	      VALUE value;
	      VALUE (*cfunc)(ANYARGS);
	      ID *tbl;
    } u1;
    union {
        struct RNode *node;
        ID id;
        long argc;
        VALUE value;
    } u2;
    union {
        struct RNode *node;
        ID id;
        long state;
        struct global_entry *entry;
        long cnt;
        VALUE value;
    } u3;
} NODE;

typedef struct rb_iseq_struct {
    /***************/
    /* static data */
    /***************/

    VALUE type;          /* instruction sequence type */
    VALUE name;	         /* String: iseq name */
    VALUE filename;      /* file information where this sequence from */
    VALUE *iseq;         /* iseq (insn number and openrads) */
    VALUE *iseq_encoded; /* encoded iseq */
    unsigned long iseq_size;
    VALUE mark_ary;	/* Array: includes operands which should be GC marked */
    VALUE coverage;     /* coverage array */

    /* insn info, must be freed */
    struct iseq_insn_info_entry *insn_info_table;
    unsigned long insn_info_size;

    ID *local_table;		/* must free */
    int local_table_size;

    /* method, class frame: sizeof(vars) + 1, block frame: sizeof(vars) */
    int local_size;

    /**
     * argument information
     *
     *  def m(a1, a2, ..., aM,                    # mandatory
     *        b1=(...), b2=(...), ..., bN=(...),  # optinal
     *        *c,                                 # rest
     *        d1, d2, ..., dO,                    # post
     *        &e)                                 # block
     * =>
     *
     *  argc           = M
     *  arg_rest       = M+N+1 // or -1 if no rest arg
     *  arg_opts       = N
     *  arg_opts_tbl   = [ (N entries) ]
     *  arg_post_len   = O // 0 if no post arguments
     *  arg_post_start = M+N+2
     *  arg_block      = M+N + 1 + O + 1 // -1 if no block arg
     *  arg_simple     = 0 if not simple arguments.
     *                 = 1 if no opt, rest, post, block.
     *                 = 2 if ambiguos block parameter ({|a|}).
     *  arg_size       = argument size.
     */

    int argc;
    int arg_simple;
    int arg_rest;
    int arg_block;
    int arg_opts;
    int arg_post_len;
    int arg_post_start;
    int arg_size;
    VALUE *arg_opt_table;

    int stack_max; /* for stack overflow check */

    /* catch table */
    struct iseq_catch_table_entry *catch_table;
    int catch_table_size;

    /* for child iseq */
    struct rb_iseq_struct *parent_iseq;
    struct rb_iseq_struct *local_iseq;

    /****************/
    /* dynamic data */
    /****************/

    VALUE self;
    VALUE orig;			/* non-NULL if its data have origin */

    /* block inlining */
    /*
     * NODE *node;
     * void *special_block_builder;
     * void *cached_special_block_builder;
     * VALUE cached_special_block;
     */

    /* klass/module nest information stack (cref) */
    NODE *cref_stack;
    VALUE klass;

    /* misc */
    ID defined_method_id;	/* for define_method */

    /* used at compile time */
    struct iseq_compile_data *compile_data;
} rb_iseq_t;

struct METHOD {
    VALUE oclass;		/* class that holds the method */
    VALUE rclass;		/* class of the receiver */
    VALUE recv;
    ID id, oid;
    NODE *body;
};

enum node_type {
    NODE_METHOD,
#define NODE_METHOD      NODE_METHOD
    NODE_FBODY,
#define NODE_FBODY       NODE_FBODY
    NODE_CFUNC,
#define NODE_CFUNC       NODE_CFUNC
    NODE_SCOPE,
#define NODE_SCOPE       NODE_SCOPE
    NODE_BLOCK,
#define NODE_BLOCK       NODE_BLOCK
    NODE_IF,
#define NODE_IF          NODE_IF
    NODE_CASE,
#define NODE_CASE        NODE_CASE
    NODE_WHEN,
#define NODE_WHEN        NODE_WHEN
    NODE_OPT_N,
#define NODE_OPT_N       NODE_OPT_N
    NODE_WHILE,
#define NODE_WHILE       NODE_WHILE
    NODE_UNTIL,
#define NODE_UNTIL       NODE_UNTIL
    NODE_ITER,
#define NODE_ITER        NODE_ITER
    NODE_FOR,
#define NODE_FOR         NODE_FOR
    NODE_BREAK,
#define NODE_BREAK       NODE_BREAK
    NODE_NEXT,
#define NODE_NEXT        NODE_NEXT
    NODE_REDO,
#define NODE_REDO        NODE_REDO
    NODE_RETRY,
#define NODE_RETRY       NODE_RETRY
    NODE_BEGIN,
#define NODE_BEGIN       NODE_BEGIN
    NODE_RESCUE,
#define NODE_RESCUE      NODE_RESCUE
    NODE_RESBODY,
#define NODE_RESBODY     NODE_RESBODY
    NODE_ENSURE,
#define NODE_ENSURE      NODE_ENSURE
    NODE_AND,
#define NODE_AND         NODE_AND
    NODE_OR,
#define NODE_OR          NODE_OR
    NODE_MASGN,
#define NODE_MASGN       NODE_MASGN
    NODE_LASGN,
#define NODE_LASGN       NODE_LASGN
    NODE_DASGN,
#define NODE_DASGN       NODE_DASGN
    NODE_DASGN_CURR,
#define NODE_DASGN_CURR  NODE_DASGN_CURR
    NODE_GASGN,
#define NODE_GASGN       NODE_GASGN
    NODE_IASGN,
#define NODE_IASGN       NODE_IASGN
    NODE_IASGN2,
#define NODE_IASGN2      NODE_IASGN2
    NODE_CDECL,
#define NODE_CDECL       NODE_CDECL
    NODE_CVASGN,
#define NODE_CVASGN      NODE_CVASGN
    NODE_CVDECL,
#define NODE_CVDECL      NODE_CVDECL
    NODE_OP_ASGN1,
#define NODE_OP_ASGN1    NODE_OP_ASGN1
    NODE_OP_ASGN2,
#define NODE_OP_ASGN2    NODE_OP_ASGN2
    NODE_OP_ASGN_AND,
#define NODE_OP_ASGN_AND NODE_OP_ASGN_AND
    NODE_OP_ASGN_OR,
#define NODE_OP_ASGN_OR  NODE_OP_ASGN_OR
    NODE_CALL,
#define NODE_CALL        NODE_CALL
    NODE_FCALL,
#define NODE_FCALL       NODE_FCALL
    NODE_VCALL,
#define NODE_VCALL       NODE_VCALL
    NODE_SUPER,
#define NODE_SUPER       NODE_SUPER
    NODE_ZSUPER,
#define NODE_ZSUPER      NODE_ZSUPER
    NODE_ARRAY,
#define NODE_ARRAY       NODE_ARRAY
    NODE_ZARRAY,
#define NODE_ZARRAY      NODE_ZARRAY
    NODE_VALUES,
#define NODE_VALUES      NODE_VALUES
    NODE_HASH,
#define NODE_HASH        NODE_HASH
    NODE_RETURN,
#define NODE_RETURN      NODE_RETURN
    NODE_YIELD,
#define NODE_YIELD       NODE_YIELD
    NODE_LVAR,
#define NODE_LVAR        NODE_LVAR
    NODE_DVAR,
#define NODE_DVAR        NODE_DVAR
    NODE_GVAR,
#define NODE_GVAR        NODE_GVAR
    NODE_IVAR,
#define NODE_IVAR        NODE_IVAR
    NODE_CONST,
#define NODE_CONST       NODE_CONST
    NODE_CVAR,
#define NODE_CVAR        NODE_CVAR
    NODE_NTH_REF,
#define NODE_NTH_REF     NODE_NTH_REF
    NODE_BACK_REF,
#define NODE_BACK_REF    NODE_BACK_REF
    NODE_MATCH,
#define NODE_MATCH       NODE_MATCH
    NODE_MATCH2,
#define NODE_MATCH2      NODE_MATCH2
    NODE_MATCH3,
#define NODE_MATCH3      NODE_MATCH3
    NODE_LIT,
#define NODE_LIT         NODE_LIT
    NODE_STR,
#define NODE_STR         NODE_STR
    NODE_DSTR,
#define NODE_DSTR        NODE_DSTR
    NODE_XSTR,
#define NODE_XSTR        NODE_XSTR
    NODE_DXSTR,
#define NODE_DXSTR       NODE_DXSTR
    NODE_EVSTR,
#define NODE_EVSTR       NODE_EVSTR
    NODE_DREGX,
#define NODE_DREGX       NODE_DREGX
    NODE_DREGX_ONCE,
#define NODE_DREGX_ONCE  NODE_DREGX_ONCE
    NODE_ARGS,
#define NODE_ARGS        NODE_ARGS
    NODE_ARGS_AUX,
#define NODE_ARGS_AUX    NODE_ARGS_AUX
    NODE_OPT_ARG,
#define NODE_OPT_ARG     NODE_OPT_ARG
    NODE_POSTARG,
#define NODE_POSTARG     NODE_POSTARG
    NODE_ARGSCAT,
#define NODE_ARGSCAT     NODE_ARGSCAT
    NODE_ARGSPUSH,
#define NODE_ARGSPUSH    NODE_ARGSPUSH
    NODE_SPLAT,
#define NODE_SPLAT       NODE_SPLAT
    NODE_TO_ARY,
#define NODE_TO_ARY      NODE_TO_ARY
    NODE_BLOCK_ARG,
#define NODE_BLOCK_ARG   NODE_BLOCK_ARG
    NODE_BLOCK_PASS,
#define NODE_BLOCK_PASS  NODE_BLOCK_PASS
    NODE_DEFN,
#define NODE_DEFN        NODE_DEFN
    NODE_DEFS,
#define NODE_DEFS        NODE_DEFS
    NODE_ALIAS,
#define NODE_ALIAS       NODE_ALIAS
    NODE_VALIAS,
#define NODE_VALIAS      NODE_VALIAS
    NODE_UNDEF,
#define NODE_UNDEF       NODE_UNDEF
    NODE_CLASS,
#define NODE_CLASS       NODE_CLASS
    NODE_MODULE,
#define NODE_MODULE      NODE_MODULE
    NODE_SCLASS,
#define NODE_SCLASS      NODE_SCLASS
    NODE_COLON2,
#define NODE_COLON2      NODE_COLON2
    NODE_COLON3,
#define NODE_COLON3      NODE_COLON3
    NODE_DOT2,
#define NODE_DOT2        NODE_DOT2
    NODE_DOT3,
#define NODE_DOT3        NODE_DOT3
    NODE_FLIP2,
#define NODE_FLIP2       NODE_FLIP2
    NODE_FLIP3,
#define NODE_FLIP3       NODE_FLIP3
    NODE_ATTRSET,
#define NODE_ATTRSET     NODE_ATTRSET
    NODE_SELF,
#define NODE_SELF        NODE_SELF
    NODE_NIL,
#define NODE_NIL         NODE_NIL
    NODE_TRUE,
#define NODE_TRUE        NODE_TRUE
    NODE_FALSE,
#define NODE_FALSE       NODE_FALSE
    NODE_ERRINFO,
#define NODE_ERRINFO     NODE_ERRINFO
    NODE_DEFINED,
#define NODE_DEFINED     NODE_DEFINED
    NODE_POSTEXE,
#define NODE_POSTEXE     NODE_POSTEXE
    NODE_ALLOCA,
#define NODE_ALLOCA      NODE_ALLOCA
    NODE_BMETHOD,
#define NODE_BMETHOD     NODE_BMETHOD
    NODE_MEMO,
#define NODE_MEMO        NODE_MEMO
    NODE_IFUNC,
#define NODE_IFUNC       NODE_IFUNC
    NODE_DSYM,
#define NODE_DSYM        NODE_DSYM
    NODE_ATTRASGN,
#define NODE_ATTRASGN    NODE_ATTRASGN
    NODE_PRELUDE,
#define NODE_PRELUDE     NODE_PRELUDE
    NODE_LAMBDA,
#define NODE_LAMBDA      NODE_LAMBDA
    NODE_OPTBLOCK,
#define NODE_OPTBLOCK    NODE_OPTBLOCK
    NODE_LAST
#define NODE_LAST        NODE_LAST
};

#define RUBY_VM_METHOD_NODE NODE_METHOD
#define nd_body  u2.node

#define NODE_TYPESHIFT 8
#define NODE_TYPEMASK  (((VALUE)0x7f)<<NODE_TYPESHIFT)
#define RNODE(obj)  (R_CAST(RNode)(obj))
#define nd_type(n) ((int) (((RNODE(n))->flags & NODE_TYPEMASK)>>NODE_TYPESHIFT))
#define RUBY_VM_IFUNC_P(ptr)        (BUILTIN_TYPE(ptr) == T_NODE)
#define RUBY_VM_NORMAL_ISEQ_P(ptr) \
  (ptr && !RUBY_VM_IFUNC_P(ptr))

static VALUE
rb_iseq_parameters(const rb_iseq_t *iseq, int is_proc)
{
    int i, r, s;
    VALUE a, args = rb_ary_new2(iseq->arg_size);
    ID req, opt, rest, block;
#define PARAM_TYPE(type) rb_ary_push(a = rb_ary_new2(2), ID2SYM(type))
#define PARAM_ID(i) iseq->local_table[i]
#define PARAM(i, type) (		      \
	PARAM_TYPE(type),		      \
	rb_id2name(PARAM_ID(i)) ?	      \
	rb_ary_push(a, ID2SYM(PARAM_ID(i))) : \
	a)

    CONST_ID(req, "req");
    CONST_ID(opt, "opt");
    if (is_proc) {
        for (i = 0; i < iseq->argc; i++) {
            PARAM_TYPE(opt);
            rb_ary_push(a, rb_id2name(PARAM_ID(i)) ?
                ID2SYM(PARAM_ID(i)) : Qnil);
            rb_ary_push(a, Qnil);
            rb_ary_push(args, a);
        }
    }
    else {
        for (i = 0; i < iseq->argc; i++) {
            rb_ary_push(args, PARAM(i, req));
        }
    }
    r = iseq->arg_rest != -1 ? iseq->arg_rest :
        iseq->arg_post_len > 0 ? iseq->arg_post_start :
        iseq->arg_block != -1 ? iseq->arg_block :
        iseq->arg_size;
    for (s = i; i < r; i++) {
        PARAM_TYPE(opt);
        if (rb_id2name(PARAM_ID(i))) {
            rb_ary_push(a, ID2SYM(PARAM_ID(i)));
        }
        rb_ary_push(args, a);
    }
    if (iseq->arg_rest != -1) {
        CONST_ID(rest, "rest");
        rb_ary_push(args, PARAM(iseq->arg_rest, rest));
    }
    r = iseq->arg_post_start + iseq->arg_post_len;
    if (is_proc) {
        for (i = iseq->arg_post_start; i < r; i++) {
            PARAM_TYPE(opt);
            rb_ary_push(a, rb_id2name(PARAM_ID(i)) ?
                ID2SYM(PARAM_ID(i)) : Qnil);
            rb_ary_push(a, Qnil);
            rb_ary_push(args, a);
        }
    }
    else {
        for (i = iseq->arg_post_start; i < r; i++) {
            rb_ary_push(args, PARAM(i, req));
        }
    }
    if (iseq->arg_block != -1) {
        CONST_ID(block, "block");
        rb_ary_push(args, PARAM(iseq->arg_block, block));
    }
    return args;
}

static rb_iseq_t *
get_method_iseq(VALUE method)
{
    struct METHOD *data;
    NODE *body;
    rb_iseq_t *iseq;

    Data_Get_Struct(method, struct METHOD, data);
    body = data->body;
    switch (nd_type(body)) {
    case NODE_BMETHOD:
        rb_notimplement();
    case RUBY_VM_METHOD_NODE:
        GetISeqPtr((VALUE)body->nd_body, iseq);
        if (RUBY_VM_NORMAL_ISEQ_P(iseq)) break;
    default:
        return 0;
    }
    return iseq;
}

static VALUE
unnamed_parameters(int arity)
{
    VALUE a, param = rb_ary_new2((arity < 0) ? -arity : arity);
    int n = (arity < 0) ? ~arity : arity;
    ID req, rest;
    CONST_ID(req, "req");
    a = rb_ary_new3(1, ID2SYM(req));
    OBJ_FREEZE(a);
    for (; n; --n) {
	      rb_ary_push(param, a);
    }
    if (arity < 0) {
	      CONST_ID(rest, "rest");
	      rb_ary_store(param, ~arity, rb_ary_new3(1, ID2SYM(rest)));
    }
    return param;
}

/*
 * call-seq:
 *    meth.parameters  => array
 *
 * returns the parameter information of this method
 */

static VALUE
rb_method_parameters(VALUE method)
{
    rb_iseq_t *iseq = get_method_iseq(method);
    if (!iseq) {
        return unnamed_parameters(
            FIX2INT(rb_funcall(method, rb_intern("arity"), 0)));
    }
    return rb_iseq_parameters(iseq, 0);
}

void
Init_methopara(void)
{
    rb_define_method(rb_cMethod, "parameters", rb_method_parameters, 0);
    rb_define_method(rb_cUnboundMethod, "parameters", rb_method_parameters, 0);
}
