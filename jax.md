---
category: framework
name: JAX
filename: learnjax.py
contributors:
    - ["Christopher Jellen", "https://github.com/CDJellen"]
---

JAX is a Python library for composable function transformations over immutable
arrays, compiled to CPUs, GPUs, and TPUs via OpenXLA.

NumPy's primitive is an eager mutation of a host buffer. JAX's primitive is a
pure function over immutable device arrays: operations execute eagerly by
default, or trace into a closed intermediate representation (`jaxpr`) that can
be compiled (`jit`), differentiated (`grad`), vectorized (`vmap`), and sharded
across device meshes (`sharding`)—in any combination.

```python
import dataclasses
from functools import partial
import numpy as np

import jax
import jax.numpy as jnp
from jax import lax
from jax.sharding import Mesh, NamedSharding, PartitionSpec as P

####################################################
# 1. Immutable Arrays & Accelerator Semantics
####################################################

# jax.numpy mirrors numpy, but returns immutable jax.Array values backed by
# accelerator memory (CPU, GPU, or TPU).
x = jnp.array([1.0, 2.0, 3.0])
W = jnp.ones((3, 3))

# Accelerator silicon favors 32-bit and 16-bit throughput; JAX truncates 64-bit
# Python literals to float32/int32 unless jax_enable_x64 is set at startup.
x.dtype      # => dtype('float32')
x.shape      # => (3,)
x.devices()  # => {CpuDevice(id=0)} (or CudaDevice / TpuDevice)

# Standard NumPy reductions, broadcasting, and einsum work identically.
y = jnp.dot(W, x)                # => Array([6., 6., 6.], dtype=float32)
z = jnp.einsum("ij,j->i", W, x)  # => Array([6., 6., 6.], dtype=float32)

# On GPUs/TPUs, float32 matmuls default to bfloat16/TensorFloat-32 tensor cores.
# Request full IEEE-754 float32 accumulation per op (or globally via config):
y_exact = jnp.dot(W, x, precision=lax.Precision.HIGHEST)

# Dispatch is asynchronous: Python returns a future-like Array immediately
# while the device queue executes. Synchronize only when timing or on host.
z.block_until_ready()

# Arrays are immutable: in-place mutation aliases buffers, breaking reverse-mode
# autodiff and XLA kernel fusion.
try:
    x[0] = 99.0
except TypeError:
    pass  # TypeError: JAX arrays are immutable

# Express updates functionally via .at[]. Inside @jax.jit, XLA compiles these
# into in-place hardware writes whenever the input buffer is no longer live.
x_set = x.at[0].set(99.0)     # => Array([99.,  2.,  3.], dtype=float32)
x_add = x.at[1:].add(10.0)    # => Array([ 1., 12., 13.], dtype=float32)
x                             # => Array([ 1.,  2.,  3.], dtype=float32)

# Device kernels cannot raise synchronous Python IndexErrors without stalling
# the accelerator queue. Out-of-bounds reads clamp; out-of-bounds writes drop:
x[100]                                     # => Array(3., dtype=float32)
x.at[100].set(99.0)                        # => Array([1., 2., 3.], dtype=f32)
x.at[100].get(mode="fill", fill_value=0.)  # => Array(0., dtype=float32)


####################################################
# 2. Tracing & JIT Compilation (jax.jit, jaxpr)
####################################################

# A pure function: outputs depend solely on inputs; no globals or I/O mutated.
def selu(x, alpha=1.67, lmbda=1.05):
    return lmbda * jnp.where(x > 0, x, alpha * jnp.exp(x) - alpha)

# JAX does not parse Python ASTs. It traces functions with abstract Tracers
# (ShapedArray(shape, dtype)) to record primitive ops into a typed DAG (jaxpr).
jaxpr_ir = jax.make_jaxpr(selu)(x)

# jax.jit lowers the jaxpr to XLA, fusing elementwise chains into a single
# accelerator kernel that keeps intermediates in SRAM/registers.
fast_selu = jax.jit(selu)
out1 = fast_selu(x)  # 1st call: traces Python, compiles XLA, dispatches
out2 = fast_selu(x)  # 2nd call: skips Python, dispatches cached XLA kernel

# Three corollaries follow directly from tracing with ShapedArray(shape, dtype):

# Corollary A: Side effects run at trace time (once per unique shape & dtype),
# never on cached runtime calls. Dynamic shapes trigger recompilation storms.
trace_count = 0

@jax.jit
def traced_fn(v):
    global trace_count
    trace_count += 1  # Python side effect: recorded only during tracing!
    # For runtime inspection from XLA, use: jax.debug.print("v={}", v)
    return v * 2.0

_ = traced_fn(jnp.ones(3))  # trace_count => 1 (compiled for float32[3])
_ = traced_fn(jnp.zeros(3)) # trace_count => 1 (cache hit: same shape & dtype)
_ = traced_fn(jnp.ones(4))  # trace_count => 2 (new shape float32[4] recompiles)

# Corollary B: Python `if` cannot branch on traced array values, because a
# Tracer carries only shape and dtype (bool[]), not a concrete runtime boolean.
@jax.jit
def bad_branch(v):
    if v > 0:  # Raises TracerBoolConversionError!
        return v
    return -v

try:
    bad_branch(jnp.asarray(1.0))
except jax.errors.TracerBoolConversionError:
    pass

# Branch on runtime values by tracing both paths via lax.cond (or jnp.where):
@jax.jit
def runtime_branch(v):
    return lax.cond(v > 0, lambda t: t, lambda t: -t, v)

runtime_branch(jnp.asarray(-3.0))  # => Array(3., dtype=float32)

# Branch on compile-time hyperparameters via static_argnames:
@partial(jax.jit, static_argnames=("degree",))
def poly(v, degree):
    return sum(v ** p for p in range(degree + 1))  # Unrolled at trace time

poly(x, degree=2)  # => Array([ 3.,  7., 13.], dtype=float32)

# Corollary C: Output shapes inside jit must be static. Data-dependent boolean
# slicing (v[v > 0]) has an unknown runtime length and fails under jit:
try:
    jax.jit(lambda v: v[v > 0])(x)
except jax.errors.NonConcreteBooleanIndexError:
    pass

# Preserve static shapes by masking instead of filtering:
masked = jax.jit(lambda v: jnp.where(v > 0, v, 0.0))(x)


####################################################
# 3. Automatic Differentiation (grad, jvp, vjp)
####################################################

# Autodiff transforms the jaxpr. jax.grad differentiates scalar-valued
# functions with respect to positional argument 0 by default.
def cubic(v):
    return jnp.sum(v ** 3)  # f(v) = sum(v_i^3) => df/dv_i = 3 * v_i^2

jax.grad(cubic)(x)  # => Array([ 3., 12., 27.], dtype=float32)

# Compute primal value, auxiliary data (has_aux=True), and gradients at once:
def loss_with_metrics(w, v):
    pred = jnp.dot(w, v)
    loss = pred ** 2
    return loss, {"pred": pred}

(loss_val, aux), dw = jax.value_and_grad(loss_with_metrics, has_aux=True)(x, x)
# loss_val => Array(196., dtype=float32), aux["pred"] => Array(14., dtype=f32)
# dw       => Array([28., 56., 84.], dtype=float32)

# Differentiate w.r.t. multiple positional arguments via argnums:
du, dv = jax.grad(jnp.dot, argnums=(0, 1))(x, jnp.array([4.0, 5.0, 6.0]))
# du => Array([4., 5., 6.], dtype=float32)
# dv => Array([1., 2., 3.], dtype=float32)

# Because grad returns a pure function, higher-order derivatives compose freely:
f_scalar = lambda t: t ** 3
d3_f = jax.grad(jax.grad(jax.grad(f_scalar)))
d3_f(2.0)  # => Array(6., dtype=float32)

# All autodiff reduces to two linear operators on f: R^n -> R^m (Jacobian J):

# 1. Forward-mode JVP (pushforward): (x, v) -> (f(x), J @ v).
#    One pass per input tangent v; efficient for tall Jacobians (n <= m).
primal, tangent = jax.jvp(cubic, (x,), (jnp.ones_like(x),))
# tangent => Array(42., dtype=float32) (i.e., 3 + 12 + 27)

# 2. Reverse-mode VJP (pullback): x -> (f(x), u -> J^T @ u).
#    One pass per output cotangent u; ideal for wide Jacobians (m << n).
primal, vjp_fn = jax.vjp(cubic, x)
(cotangent,) = vjp_fn(1.0)  # => Array([ 3., 12., 27.], dtype=float32)

# Full Jacobians and Hessians build directly on JVP (jacfwd) and VJP (jacrev):
J_fwd = jax.jacfwd(jnp.sin)(x)       # 3x3 diagonal matrix: diag(cos(x))
J_rev = jax.jacrev(jnp.sin)(x)       # Identical value, computed row-by-row
H = jax.hessian(cubic)(x)            # 3x3 diagonal: diag([6., 12., 18.])

# Detach subexpressions from reverse-mode AD (e.g., bootstrap targets):
def td_loss(q_pred, reward, q_next):
    target = reward + 0.99 * lax.stop_gradient(q_next)
    return (q_pred - target) ** 2

dq_pred, dq_next = jax.grad(td_loss, argnums=(0, 2))(2.0, 1.0, 5.0)
# dq_pred => Array(-7.9, dtype=float32), dq_next => Array(0., dtype=float32)


####################################################
# 4. Auto-Vectorization (jax.vmap)
####################################################

# Write the physics of a single datum; let vmap supply the batch dimension.
def matvec(W, v):
    return jnp.dot(W, v)  # (D_out, D_in) @ (D_in,) -> (D_out,)

batch_x = jnp.ones((8, 3))  # 8 vectors of length 3

# in_axes=(None, 0): leave W unbatched, map over axis 0 of batch_x.
batched_matvec = jax.vmap(matvec, in_axes=(None, 0), out_axes=0)
batched_matvec(W, batch_x).shape  # => (8, 3)

# Nest vmap for all-pairs operations without broadcasting or reshape bugs:
sq_dist = lambda a, b: jnp.sum((a - b) ** 2)
pairwise_sq_dist = jax.vmap(jax.vmap(sq_dist, (None, 0)), (0, None))
pairwise_sq_dist(batch_x, batch_x).shape  # => (8, 8)

# Compose vmap with grad for per-example gradients (e.g., differential privacy):
sample_loss = lambda w, xi, yi: (jnp.dot(w, xi) - yi) ** 2
per_sample_grads = jax.vmap(jax.grad(sample_loss), in_axes=(None, 0, 0))
per_sample_grads(x, batch_x, jnp.zeros(8)).shape  # => (8, 3)


####################################################
# 5. Explicit PRNGs (jax.random)
####################################################

# Global mutable RNG state (numpy.random) is an impure side effect that
# serializes execution and couples results to evaluation order. JAX uses a
# counter-based PRNG (Threefry): a key is an immutable value passed explicitly.
key = jax.random.key(42)

# Feeding the same key to two calls produces identical bitstreams:
r1 = jax.random.normal(key, shape=(3,))
r2 = jax.random.normal(key, shape=(3,))
jnp.array_equal(r1, r2)  # => Array(True, dtype=bool)

# Never reuse a key. Split deterministically into fresh subkeys before use:
key, subkey1, subkey2 = jax.random.split(key, num=3)
draw1 = jax.random.normal(subkey1, shape=(3,))
draw2 = jax.random.uniform(subkey2, shape=(3,))

# Order-independent parallel streams: split N keys and vmap across them.
batch_keys = jax.random.split(key, num=8)
batch_noise = jax.vmap(lambda k: jax.random.normal(k, (3,)))(batch_keys)
batch_noise.shape  # => (8, 3)


####################################################
# 6. Structured Control Flow & Memory (jax.lax)
####################################################

# Python `for` loops inside @jax.jit unroll at trace time, inflating the jaxpr
# and compilation time by O(T). lax.scan compiles a stateful recurrence
# (carry, x_t) -> (carry, y_t) into a single XLA loop with O(1) graph size.
def ema_step(carry, x_t):
    next_carry = 0.9 * carry + 0.1 * x_t
    return next_carry, next_carry  # (new_carry, emitted_slice)

final_state, trajectory = lax.scan(ema_step, jnp.array(0.0), jnp.arange(1., 6.))
# trajectory => Array([0.1, 0.29, 0.561, 0.9049, 1.31441], dtype=float32)

# Bound reverse-mode AD memory on deep compositions or scan steps via
# activation checkpointing (@jax.checkpoint recomputes forward activations
# during the backward pass instead of storing all intermediates in HBM):
@jax.checkpoint
def residual_block(v):
    return v + jax.nn.gelu(v)

jax.grad(lambda v: jnp.sum(residual_block(v)))(x)

# Fixed-bound compiled loop when intermediate outputs are not needed:
sum_1_to_10 = lax.fori_loop(1, 11, lambda i, acc: acc + i, 0)  # => Array(55)

# Data-dependent while_loop (forward-mode JVP only; reverse-mode AD requires a
# statically bounded trip count to allocate residual tapes):
halved = lax.while_loop(lambda v: v > 1.0, lambda v: v * 0.5, 16.0)  # => 1.0


####################################################
# 7. PyTrees: Functional State Containers (jax.tree)
####################################################

# Models and optimizers are nested dicts, tuples, and lists—not flat argument
# lists. JAX generalizes arrays to PyTrees: any nested container of leaf arrays.
params = {
    "encoder": {"w": jnp.ones((4, 3)), "b": jnp.zeros((4,))},
    "head": {"w": jnp.ones((1, 4)), "b": jnp.zeros((1,))},
}

# Deconstruct a PyTree into its flat leaf list and static container structure:
leaves = jax.tree.leaves(params)                # List of 4 jax.Array leaves
treedef = jax.tree.structure(params)            # Static PyTreeDef topology
reconstructed = jax.tree.unflatten(treedef, leaves)

# Every transformation (jit, grad, vmap) natively accepts and returns PyTrees.
# jax.tree.map applies a function leaf-wise across one or more isomorphic trees:
decayed = jax.tree.map(lambda p: 0.99 * p, params)

# Register custom immutable dataclasses as first-class PyTree nodes:
@jax.tree_util.register_dataclass
@dataclasses.dataclass(frozen=True)
class LinearLayer:
    weight: jax.Array
    bias: jax.Array
    name: str = dataclasses.field(metadata=dict(static=True))  # Static metadata

layer = LinearLayer(weight=jnp.eye(3), bias=jnp.zeros(3), name="proj")
len(jax.tree.leaves(layer))  # => 2 (weight and bias; `name` is static)


####################################################
# 8. SPMD Parallelism & Sharding (jax.sharding)
####################################################

# JAX separates global tensor math from physical topology via a device Mesh.
devices = np.array(jax.devices())  # e.g., 1 CPU locally, or N GPUs/TPUs
mesh = Mesh(devices, axis_names=("data",))

# PartitionSpec("data", None) shards axis 0 across "data" and replicates axis 1:
data_sharding = NamedSharding(mesh, P("data", None))
replicated = NamedSharding(mesh, P(None, None))

global_batch = jax.device_put(jnp.ones((len(devices) * 4, 3)), data_sharding)
global_weights = jax.device_put(W, replicated)

# Mode 1 (Automatic GSPMD): write global array math inside @jax.jit; XLA
# partitions the computation and inserts collectives (AllReduce, AllGather).
@jax.jit
def spmd_forward(batch, weights):
    return lax.with_sharding_constraint(batch @ weights, data_sharding)

sharded_out = spmd_forward(global_batch, global_weights)

# Mode 2 (Explicit shard_map): write per-device local math and issue explicit
# collective operations (lax.psum, lax.all_gather, lax.pmean) across mesh axes:
@jax.shard_map(mesh=mesh, in_specs=P("data", None), out_specs=P("data", None))
def normalize_across_mesh(local_batch):
    local_sum = jnp.sum(local_batch)
    global_sum = lax.psum(local_sum, axis_name="data")
    return local_batch / global_sum

normalized = normalize_across_mesh(global_batch)


####################################################
# 9. Synthesis: A Complete MLP in 25 Lines
####################################################

# Unifying the mental model: pure functions + explicit PRNGs + PyTrees +
# vmap + value_and_grad + jit with in-place buffer donation.
def init_mlp(key, sizes):
    keys = jax.random.split(key, len(sizes) - 1)
    return [
        {
            "w": jax.random.normal(k, (m, n)) * jnp.sqrt(2.0 / m),
            "b": jnp.zeros((n,)),
        }
        for k, m, n in zip(keys, sizes[:-1], sizes[1:])
    ]

def forward_single(params, x):
    for layer in params[:-1]:
        x = jax.nn.relu(x @ layer["w"] + layer["b"])
    return x @ params[-1]["w"] + params[-1]["b"]

forward_batch = jax.vmap(forward_single, in_axes=(None, 0))

def mse_loss(params, x_batch, y_batch):
    preds = forward_batch(params, x_batch)
    return jnp.mean((preds - y_batch) ** 2)

# donate_argnums=(0,) tells XLA to reuse the old `params` buffers in-place for
# `new_params`, achieving zero-allocation updates despite immutable semantics.
@partial(jax.jit, donate_argnums=(0,))
def train_step(params, x_batch, y_batch, lr=0.05):
    loss, grads = jax.value_and_grad(mse_loss)(params, x_batch, y_batch)
    new_params = jax.tree.map(lambda p, g: p - lr * g, params, grads)
    return new_params, loss

k_init, k_x = jax.random.split(jax.random.key(0))
mlp_params = init_mlp(k_init, [3, 16, 1])
X = jax.random.normal(k_x, (32, 3))
Y = jnp.sum(X ** 2, axis=-1, keepdims=True)

mlp_params, initial_loss = train_step(mlp_params, X, Y)
for _ in range(50):
    mlp_params, final_loss = train_step(mlp_params, X, Y)

assert final_loss < initial_loss  # Verified: compiled, batched, differentiated
```

## Further Reading

* [JAX Documentation & Tutorials][1]
* [Thinking in JAX][2]
* [The Autodiff Cookbook][3]
* [Equinox][4] & [Flax NNX][5]: Neural network libraries built on JAX PyTrees
* [Optax][6]: Composable gradient transformations for JAX

[1]: https://docs.jax.dev/
[2]: https://docs.jax.dev/en/latest/notebooks/thinking_in_jax.html
[3]: https://docs.jax.dev/en/latest/notebooks/autodiff_cookbook.html
[4]: https://docs.kidger.site/equinox/
[5]: https://flax.readthedocs.io/
[6]: https://optax.readthedocs.io/
