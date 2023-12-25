-- https://www.reedbeta.com/blog/quick-and-easy-gpu-random-numbers-in-d3d11/
def rand (rng_state : i64) : f32 =
    let rng_state = u32.i64 rng_state
    let rng_state = rng_state ^ (rng_state << 13)
    let rng_state = rng_state ^ (rng_state >> 17)
    let rng_state = rng_state ^ (rng_state << 5)
    let rng_state = (f32.u32 (rng_state % 1000) / 1000f32)
    in rng_state

-- maybe useful in the repl to iterate `update`
def iter 'a n (f : a -> a) a : a = foldl (\a _ -> f a) a (iota n)

type color = {r: f32, g: f32, b: f32}
def color_map2 (op : f32 -> f32 -> f32) (a : color) (b : color) : color =
  {r = a.r `op` b.r, g = a.g `op` b.g, b = a.b `op` b.b}
def color_add (a : color) (b : color) : color = {r = a.r + b.r, g = a.g + b.g, b = a.b + b.b}
def color_scale l (c : color) : color = {r = c.r * l, g = c.g * l, b = c.b * l }
def color_min : color -> color -> color = color_map2 f32.min
def color_max : color -> color -> color = color_map2 f32.max
def color_div : color -> color -> color = color_map2 (/)
def color_zero : color = {r=0, g=0, b=0}

type angle = f32
type slime = {x: f32, y: f32, theta: angle, color: color}
type slimes[ns] = [ns]slime
type signals [m][n] = [n][m]color
type state [ns][m][n] =
  { slimes: slimes[ns]
  , signals: signals[m][n] }
type params = {eps: f32, step: f32, inertia: f32, decay: f32, drop_rate: f32, diffuse: f32}

--def sig_lim : f32 = 5
def signal_limit : color = {r = 30, g = 30, b = 30}

def diffuse [m][n] v (d : [n][m]f32) : [][]f32 =
  let f = (1-v)/8
  in map (\r -> map (\c ->
    v * d[r][c] +
    f * (d[(r-1)%n][c] + d[(r+1)%n][c] + d[r][(c-1)%m] + d[r][(c+1)%m] +
         1.0 * (d[(r-1)%n][(c-1)%m] + d[(r+1)%n][(c-1)%m] + d[(r-1)%n][(c+1)%m] + d[(r+1)%n][(c+1)%m])
    )
  ) (iota m)) (iota n)

def diffuse_color [m][n] v (d : [n][m]color) : [][]color =
  let f = (1-v)/8
  in map (\r -> map (\c ->
     v `color_scale ` d[r][c] `color_add`
    (f `color_scale ` (d[(r-1)%n][c] `color_add` d[(r+1)%n][c] `color_add` d[r][(c-1)%m] `color_add` d[r][(c+1)%m] `color_add`
                      (d[(r-1)%n][(c-1)%m] `color_add` d[(r+1)%n][(c-1)%m] `color_add` d[(r-1)%n][(c+1)%m] `color_add` d[(r+1)%n][(c+1)%m])))
  ) (iota m)) (iota n)

-- python issue?
def to (n : i64) (f : f32) = i64.f32 ((1 + (f % 1)) % 1 * f32.i64 n)

def slime_step (s : slime) (dtheta : angle) (r : f32) : slime =
  let theta = s.theta + dtheta in
  { x = s.x + r * f32.cos theta,
    y = s.y + r * f32.sin theta,
    theta, color = s.color }

def slime_round m n (s : slime) : (i64, i64) = (to n s.y, to m s.x)

def color_sum (c : color) = c.r + c.g + c.b

def sig_lookup [m][n] (sig : signals[m][n]) (s : slime) =
  let (row, col) = slime_round m n s
  in sig[row, col]

-- random value per slime state
-- simpler combinations of x and y lead to visual regularities; not sure if this is sufficient *shrug*
def pseudorandom (s : slime) =
  let i = i64.f32 (s.x * 100000)
  let j = i64.f32 (s.y * 100000)
  in rand ((i << 12) * j)

def slime_try (p : params) (sig : signals[][]) (s : slime) =
  let eps = p.eps let r = p.step let thr = p.inertia
  let s0 = slime_step s 0 r
  let s1 = slime_step s eps r
  let s2 = slime_step s (-eps) r
  let v0 = color_sum (sig_lookup sig s0)
  let v1 = color_sum (sig_lookup sig s1)
  let v2 = color_sum (sig_lookup sig s2)
  let c01 = v0 + thr > v1
  let c02 = v0 + thr > v2
  let c10 = not c01
  let c20 = not c02 in
  -- L low M medium H high:
    -- LHL -> forward
    if c01 && c02 then s0
    -- HML -> left
    else if c10 && c02 then s1
    -- LMH -> right
    else if c20 && c01 then s2
    -- HLH -> random turn
    else if pseudorandom s > 0.5
     then s1
     else s2

--def sig_add [m][n] (a : signals[m][n]) (b : signals[m][n]) =
--  let x = map2 (+) (flatten a) (flatten b)
--  in unflatten x

def update [ns][m][n] (p : params) (state : *state[ns][m][n]) : *state[ns][m][n] =
  let {slimes, signals} = state
  let drop_rate = p.drop_rate * (f32.i64 (m*n))
  let is = map (slime_round m n) slimes
  let values = map (\s -> drop_rate `color_scale` s.color) slimes
  let slimes = map (slime_try p signals) slimes
  let signals = reduce_by_index_2d signals color_add color_zero is values
  let signals = diffuse_color p.diffuse signals
  let signals = map (map (color_scale p.decay)) signals
  let signals = map (map (color_min signal_limit)) signals
  in {slimes, signals}

def run signals slimes p k =
  let history = replicate k signals
  let state = {slimes, signals}
  let (history, _) = loop (history, state) = copy (history, state) for i < k do
    let next = update p (copy state)
    in (history with [i] = state.signals, next)
  in history

-- disk, uniform density
def gen_disk signals k : []slime =
  tabulate k (\i ->
    let theta = 6.28 * rand i
    let phi = 6.28 * rand (i << 12)
    let r = 0.5 * f32.sqrt (rand (i << 18))
    let s = {
        x = 0.5 + r * f32.cos theta, y = 0.5 + r * f32.sin theta,
        theta = phi, color = color_zero }
    in {x = s.x, y = s.y, theta = s.theta, color = sig_lookup signals s})

def gen_rect signals k : []slime =
  tabulate k (\i ->
    let x = rand i
    let y = rand (i << 6)
    let phi = 6.28 * rand (i << 12)
    let s = { x, y, theta = phi, color = color_zero }
    in {x = s.x, y = s.y, theta = s.theta, color = sig_lookup signals s})

def color_transpose r g b : [][]color =
  map3 (map3 (\r g b -> {r=r*r,g=g*g,b=b*b})) r g b

entry main r g b eps step inertia drop_rate decay diffuse ns k =
  let signals = (color_transpose r g b)
  let frames = run signals (gen_rect signals ns) {eps, step, inertia, decay, drop_rate, diffuse} k
  let max = reduce color_max color_zero (flatten_3d frames)
  let frames = map (map (map (`color_div` max))) frames
  let r = map (map (map (\v -> u8.f32 (255 * f32.sqrt v.r)))) frames
  let g = map (map (map (\v -> u8.f32 (255 * f32.sqrt v.g)))) frames
  let b = map (map (map (\v -> u8.f32 (255 * f32.sqrt v.b)))) frames
  in (r, g, b)