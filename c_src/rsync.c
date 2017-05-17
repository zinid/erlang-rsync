#include <stdio.h>
#include <librsync.h>
#include <erl_nif.h>
#include <string.h>

#define RS_JOB_BLOCKSIZE (1 << 16)
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
//#define RS_DEFAULT_STRONG_LEN	8


typedef enum {
  SIGNATURE,
  LOADSIG,
  DELTA,
  PATCH
} job_t;

typedef struct {
  rs_job_t *job;
  char *out;
  char *in;
  size_t out_size;
  size_t in_size;
  rs_signature_t *sig;
  ErlNifBinary bin;
  job_t type;
} state_t;

static ErlNifResourceType *state_r = NULL;

static char *am_error(rs_result err)
{
  switch (err) {
  case RS_DONE: return "done";
  case RS_BLOCKED: return "blocked";
  case RS_RUNNING: return "running";
  case RS_TEST_SKIPPED: return "test_skipped";
  case RS_IO_ERROR: return "io_error";
  case RS_SYNTAX_ERROR: return "syntax_error";
  case RS_MEM_ERROR: return "mem_error";
  case RS_INPUT_ENDED: return "input_ended";
  case RS_BAD_MAGIC: return "bad_magic";
  case RS_UNIMPLEMENTED: return "unimplemented";
  case RS_CORRUPT: return "corrupt";
  case RS_INTERNAL_ERROR: return "internal_error";
  case RS_PARAM_ERROR: return "param_error";
  default:
    return "unexplained_problem";
  }
}

static state_t *init_state()
{
  state_t *state = enif_alloc_resource(state_r, sizeof(state_t));
  memset(state, 0, sizeof(state_t));
  return state;
}

static void destroy_state(ErlNifEnv *env, void *data)
{
  state_t *state = (state_t *) data;
  if (state) {
    if (state->job) rs_job_free(state->job);
    if (state->out) free(state->out);
    if (state->in) free(state->in);
    if (state->sig) rs_free_sumset(state->sig);
    if (state->bin.data) enif_release_binary(&state->bin);
    memset(state, 0, sizeof(state_t));
  }
}

static void trace_callback(rs_loglevel level, char const *msg)
{
  /* shut up librsync tracer */
}

static rs_result read_callback(state_t *state, rs_long_t pos, size_t *len, void **buf)
{
  ErlNifBinary bin = state->bin;
  if (pos >= bin.size)
    return RS_INPUT_ENDED;

  if (pos + *len > bin.size)
    *len = bin.size - pos;

  *buf = bin.data + pos;
  return RS_DONE;
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  state_r = enif_open_resource_type(env, NULL, "state_r",
				    destroy_state, flags, NULL);
  //rs_trace_set_level(RS_LOG_DEBUG);
  rs_trace_to((rs_trace_fn_t *) &trace_callback);
  return 0;
}

static ERL_NIF_TERM format_error(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
  unsigned len;
  int res;

  if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1))
    return enif_make_badarg(env);

  char *atom = malloc(len+1);
  if (!enif_get_atom(env, argv[0], atom, len+1, ERL_NIF_LATIN1)) {
    free(atom);
    return enif_make_badarg(env);
  }

  if (!strcmp("done", atom)) res = RS_DONE;
  else if (!strcmp("blocked", atom)) res = RS_BLOCKED;
  else if (!strcmp("running", atom)) res = RS_RUNNING;
  else if (!strcmp("test_skipped", atom)) res = RS_TEST_SKIPPED;
  else if (!strcmp("io_error", atom)) res = RS_IO_ERROR;
  else if (!strcmp("syntax_error", atom)) res = RS_SYNTAX_ERROR;
  else if (!strcmp("mem_error", atom)) res = RS_MEM_ERROR;
  else if (!strcmp("input_ended", atom)) res = RS_INPUT_ENDED;
  else if (!strcmp("bad_magic", atom)) res = RS_BAD_MAGIC;
  else if (!strcmp("unimplemented", atom)) res = RS_UNIMPLEMENTED;
  else if (!strcmp("corrupt", atom)) res = RS_CORRUPT;
  else if (!strcmp("internal_error", atom)) res = RS_INTERNAL_ERROR;
  else if (!strcmp("param_error", atom)) res = RS_PARAM_ERROR;
  else res = -1;

  free(atom);

  return enif_make_string(env, rs_strerror(res), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM mk_error(ErlNifEnv* env, rs_result res)
{
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
			  enif_make_atom(env, am_error(res)));
}

static ERL_NIF_TERM sig_init(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
  state_t *state = init_state();

  state->job = rs_sig_begin(RS_DEFAULT_BLOCK_LEN, RS_MAX_STRONG_SUM_LENGTH, RS_BLAKE2_SIG_MAGIC);
  state->type = SIGNATURE;
  ERL_NIF_TERM result = enif_make_resource(env, state);
  enif_release_resource(state);
  return result;
}

static ERL_NIF_TERM loadsig_init(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
  state_t *state = init_state();
  state->job = rs_loadsig_begin(&state->sig);
  state->type = LOADSIG;
  ERL_NIF_TERM result = enif_make_resource(env, state);
  enif_release_resource(state);
  return result;
}

static ERL_NIF_TERM delta_init(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;
  rs_result res;

  if (!enif_get_resource(env, argv[0], state_r, (void *) &state))
    return enif_make_badarg(env);

  if (state->type != LOADSIG)
    return enif_make_badarg(env);

  res = rs_build_hash_table(state->sig);
  if (res != RS_DONE)
    return mk_error(env, res);
  if (state->job) rs_job_free(state->job);
  state->job = rs_delta_begin(state->sig);
  state->in_size = 0;
  state->out_size = 0;
  state->type = DELTA;
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM patch_init(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  state_t *state = init_state();

  if (!enif_inspect_iolist_as_binary(env, argv[0], &state->bin))
    return enif_make_badarg(env);

  enif_realloc_binary(&state->bin, state->bin.size);
  state->job = rs_patch_begin((rs_copy_cb *) &read_callback, state);
  state->type = PATCH;
  ERL_NIF_TERM result = enif_make_resource(env, state);
  enif_release_resource(state);
  return result;
}

static ERL_NIF_TERM mk_output(ErlNifEnv* env, state_t *state)
{
  ErlNifBinary output;

  if (state->type == LOADSIG) {
    return enif_make_atom(env, "ok");
  } else {
    enif_alloc_binary(state->out_size, &output);
    memcpy(output.data, state->out, state->out_size);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
			    enif_make_binary(env, &output));
  }
}

static ERL_NIF_TERM do_job(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;
  ErlNifBinary input;
  size_t block_size;
  int is_eof;
  size_t input_size;
  char *input_data;
  rs_result res;

  if (!enif_get_resource(env, argv[0], state_r, (void *) &state))
    return enif_make_badarg(env);

  if (argc == 1) {
    is_eof = 1;
    input_size = 0;
    input_data = NULL;
  } else {
    if (!enif_inspect_iolist_as_binary(env, argv[1], &input))
      return enif_make_badarg(env);
    is_eof = 0;
    input_size = input.size;
    input_data = (char *) input.data;
  }

  block_size = MAX(state->in_size + input_size, RS_JOB_BLOCKSIZE);
  if (input_size) {
    state->in = realloc(state->in, state->in_size + input_size);
    memcpy(state->in + state->in_size, input_data, input_size);
  }
  state->in_size += input_size;
  state->out_size = 0;

  while (1) {
    state->out = realloc(state->out, state->out_size + block_size);
    rs_buffers_t buf = {.next_in = state->in,
			.avail_in = state->in_size,
			.eof_in = is_eof,
			.next_out = state->out + state->out_size,
			.avail_out = block_size};
    res = rs_job_iter(state->job, &buf);
    state->in_size = buf.avail_in;
    state->out_size += block_size - buf.avail_out;
    if (res == RS_BLOCKED) {
      if (buf.avail_in > 0) {
	state->in = realloc(state->in, buf.avail_in);
	memcpy(state->in, buf.next_in, buf.avail_in);
      }
      if (!is_eof)
	return mk_output(env, state);
    } else if (res == RS_DONE) {
      return mk_output(env, state);
    } else {
      return mk_error(env, res);
    }
  }
}

static ERL_NIF_TERM job_iter(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
  return do_job(env, argc, argv);
}

static ERL_NIF_TERM job_done(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
  return do_job(env, argc, argv);
}

static ErlNifFunc nif_funcs[] =
  {
    {"sig_init", 0, sig_init},
    {"loadsig_init", 0, loadsig_init},
    {"delta_init", 1, delta_init},
    {"patch_init", 1, patch_init},
    {"format_error_nif", 1, format_error},
    {"job_iter", 2, job_iter},
    {"job_done", 1, job_done}
  };

ERL_NIF_INIT(rsync, nif_funcs, load, NULL, NULL, NULL)
