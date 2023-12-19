-- https://www.reedbeta.com/blog/quick-and-easy-gpu-random-numbers-in-d3d11/
def rand (rng_state : i64) : f32 =
    let rng_state = u32.i64 rng_state
    let rng_state = rng_state ^ (rng_state << 13)
    let rng_state = rng_state ^ (rng_state >> 17)
    let rng_state = rng_state ^ (rng_state << 5)
    let rng_state = (f32.u32 (rng_state % 1000) / 1000f32)
    in rng_state

type angle = f32
type slime = {x: f32, y: f32, theta: angle}
type slimes[ns] = [ns]slime
type signals [m][n] = [n][m]f32
type state [ns][m][n] = (slimes[ns], signals[m][n])
type params = {eps: f32, r: f32, inertia: f32, decay: f32, drop_rate: f32, diffuse: f32}

def iter 'a n (f : a -> a) a : a = foldl (\a _ -> f a) a (iota n)

def diffuse [m][n] v (d : [n][m]f32) : [][]f32 =
  let f = (1-v)/8
  --let v1 = (1-v)/4
  in map (\r -> map (\c ->
    v * d[r][c] +
    f * (d[(r-1)%n][c] + d[(r+1)%n][c] + d[r][(c-1)%m] + d[r][(c+1)%m] +
         1.0 * (d[(r-1)%n][(c-1)%m] + d[(r+1)%n][(c-1)%m] + d[(r-1)%n][(c+1)%m] + d[(r+1)%n][(c+1)%m])
    )
  ) (iota m)) (iota n)

-- python issue?
def to (n : i64) (f : f32) = i64.f32 ((1 + (f % 1)) % 1 * f32.i64 n)

def signal_limit : f32 = 2

def step_slime_theta (s : slime) (dtheta : angle) (r : f32) : slime =
  let theta = s.theta + dtheta in
  { x = s.x + r * f32.cos theta,
    y = s.y + r * f32.sin theta,
    theta }

def slime_round m n (s : slime) : (i64, i64) = (to n s.y, to m s.x)

def sig_lookup [m][n] (sig : signals[m][n]) (s : slime) =
  let (row, col) = slime_round m n s
  in sig[row, col]

def pseudorandom (s : slime) =
  let i = i64.f32 (s.x * 100000)
  let j = i64.f32 (s.y * 100000)
  in rand ((i << 12) * j)
def slime_try (p : params) (sig : signals[][]) (s : slime) =
  let eps = p.eps let r = p.r let thr = p.inertia
  let s0 = step_slime_theta s 0 r
  let s1 = step_slime_theta s eps r
  let s2 = step_slime_theta s (-eps) r
  let v0 = sig_lookup sig s0
  let v1 = sig_lookup sig s1
  let v2 = sig_lookup sig s2
  let c01 = v0 + thr > v1
  let c02 = v0 + thr > v2
  let c10 = not c01
  let c20 = not c02 in
    if c01 && c02 then s0
    else if c10 && c02 then s1
    else if c20 && c01 then s2
    else if pseudorandom s > 0.5
    --else if (rand (i64.f32 (1000 * (s.x*s.x + s.y*s.y)))) > 0.5
     then s1
     else s2
  --else step_slime_theta s (rand (i64.f32 (1000 * (s.x+s.y)))) r
  --if v0 + thr > v1
  --then if v0 + thr > v2 then s0
  --     else s2
  --else if v1 > v2 then if v0 + thr > v2 then s1
  --     -- random step
  --     else step_slime_theta s (rand (i64.f32 (1000 * (s.x+s.y)))) r
  --     else s2

def update [ns][m][n] (p : params) ((slimes,signals) : *state[ns][m][n]) : *state[ns][m][n] =
  let is = map (slime_round m n) slimes
  let slimes = map (slime_try p signals) slimes
  let drop_rate = p.drop_rate * (f32.i64 (m*n))
  let signals = reduce_by_index_2d signals (+) 0 is (replicate ns drop_rate)
  let signals = diffuse p.diffuse signals
  let signals = map (map (\v -> f32.min (v * p.decay) signal_limit)) signals
  in (slimes, signals)

def gen_rand k : []slime =
  map (\i -> {x = rand i, y = rand (i << 13), theta = rand(i << 26)}) (iota k)

def gen_slimes_grid m n : []slime = tabulate_2d m n (\col row ->
  { x = (f32.i64 row) / (f32.i64 m),
    y = (f32.i64 col) / (f32.i64 n),
    theta = rand (col*n+row) }) |> flatten

def gen_circle k : []slime =
  map (\s ->
    let x = s.x - 0.5
    let y = s.y - 0.5
    let r = 2.5 * f32.sqrt (x*x + y*y)
    in {x = x / r + 0.5, y = y/r + 0.5, theta = 0}
  ) (gen_rand k)

def gen_circle' k : []slime =
  tabulate k (\i ->
    let theta = 6.28 * rand i
    let phi = 6.28 * rand (i << 12)
    let r = 0.10
    in {x = 0.5 + r * f32.cos theta, y = 0.5 + r * f32.sin theta, theta = phi})

def gen_disk k : []slime =
  tabulate k (\i ->
    let theta = 6.28 * rand i
    --let phi = f32.pi + theta
    let phi = 6.28 * rand (i << 12)
    let r = 0.5 * f32.sqrt (rand (i << 18))
    in {x = 0.5 + r * f32.cos theta, y = 0.5 + r * f32.sin theta, theta = phi})

def init (ns : i64) m n =
  let slimes = gen_disk ns
  let sig : signals[][] = tabulate_2d n m (\_ _ -> 0)
  in (slimes, sig)

def run p (ns : i64) m n k =
  let (slimes, signals) = init ns m n
  let history = replicate k signals
  let state = (slimes, signals)
  let (history, _) = loop (history, state) = copy (history, state) for i < k do
    let next = update p (copy state)
    in (history with [i] = state.1, next)
  in history

def max_signal (s : [][][]f32) : f32 = f32.maximum (flatten_3d s)

entry main eps r inertia drop_rate decay diffuse ns m n k =
  let frames = run {eps, r, inertia, decay, drop_rate, diffuse} ns m n k
  let max = max_signal frames
  let frames = map (\f -> map (\r -> map (\v -> u8.f32 (255 * v / max)) r) f) frames
  --let frames = map (\f ->
  --  let max = f32.maximum (flatten f)
  --  in map (\r -> map (\v -> u8.f32 (255 * v / max)) r) f) frames
  in frames