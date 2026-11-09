module vec3 = {
  type t = {x: f32, y: f32, z: f32}
  def map f (v: t) =
    {x = f v.x, y = f v.y, z = f v.z}

  def map2 f (a: t) (b: t) =
    {x = f a.x b.x, y = f a.y b.y, z = f a.z b.z}

  def (+) = map2 (f32.+)
  def (-) = map2 (f32.-)
  def (*) = map2 (f32.*)
  def (/) = map2 (f32./)

  def dot (a: t) (b: t) = f32.(a.x * b.x + a.y * b.y + a.z * b.z)
  def scale (s: f32) = map (f32.* s)
  def quadrance v = dot v v
  def norm = quadrance >-> f32.sqrt
  def normalize (v: t) = scale (1.0 f32./ norm v) v                      
  def cross ({x = ax, y = ay, z = az}: t)
            ({x = bx, y = by, z = bz}: t) : t =
    f32.({x = ay * bz - az * by, y = az * bx - ax * bz, z = ax * by - ay * bx})
         
  def mk x y z = {x, y, z}
  def unmkA ({x, y, z}: t) = [x,y,z]
  def unmkT ({x, y, z}: t) = (x,y,z)
  def unmk {x, y, z} = {x, y, z}
}                           

def clamp01 x = f32.min (f32.max x 0) 1
                          
type Material = {
    color: vec3.t,
    roughness: f32,
}

def fresnel_schlick (cos_theta: f32) (f0: vec3.t) : vec3.t =
  let c = f32.max 0.0 (f32.min 1.0 cos_theta)
  let f = (1.0 - c) ** 5.0
  in f0 vec3.+ vec3.scale f ((vec3.mk 1.0 1.0 1.0) vec3.- f0)

def ggx_distribution (n: vec3.t) (h: vec3.t) (roughness: f32) : f32 =
  let r = f32.max 0.045 (f32.min 1.0 roughness)
  let a = r * r
  let a2 = a * a
  let nh = f32.max 0.0 (vec3.dot n h)
  let nh2 = nh * nh
  let denom = nh2 * (a2 - 1.0) + 1.0

  in a2 / f32.max 1.0e-7 (3.14159265 * denom * denom)

def geometry_schlick_ggx (nv: f32) (roughness: f32) : f32 =
  let r = f32.max 0.045 (f32.min 1.0 roughness)
  let k = ((r + 1.0) * (r + 1.0)) / 8.0
  in nv / (nv * (1.0 - k) + k)

def geometry_smith (n: vec3.t) (v: vec3.t) (l: vec3.t) (roughness: f32) : f32 =
  let nv = f32.max 0.0 (vec3.dot n v)
  let nl = f32.max 0.0 (vec3.dot n l)
  in geometry_schlick_ggx nv roughness *
     geometry_schlick_ggx nl roughness

def brdf (mat: Material) (n: vec3.t) (v: vec3.t) (l: vec3.t) : vec3.t =
  let nv0 = vec3.dot n v
  let nl0 = vec3.dot n l
  in if nv0 <= 0.0 || nl0 <= 0.0 then vec3.mk 0.0 0.0 0.0 else
     let nv = f32.max 0.0001 nv0
     let nl = f32.max 0.0001 nl0
     let h = vec3.normalize (v vec3.+ l)
     let vh = f32.max 0.0 (vec3.dot v h)
     let f0 = vec3.mk 0.04 0.04 0.04
     let d = ggx_distribution n h mat.roughness
     let g = geometry_smith n v l mat.roughness
     let f = fresnel_schlick vh f0
     let spec = vec3.scale (d * g / (4.0 * nv * nl)) f
     let kd = (vec3.mk 1.0 1.0 1.0) vec3.- f
     let dif = vec3.scale (1.0 / 3.14159265) (kd vec3.* mat.color)
     in dif vec3.+ spec
                   
type ObjectField = {
    dist: f32,
    mat: Material,
}

type^ SDF = vec3.t -> ObjectField

def grad f x = vjp f x 1f32
                   
def sdf_normal (sdf: SDF) (p: vec3.t): vec3.t =
  vec3.normalize (grad (\x -> (sdf x).dist) p)

type Hit = {
    hit: bool,
    mat: Material,
    pos: vec3.t,
    dir: vec3.t,
}

def ballMaterial: Material = {
    color = vec3.mk 0.2 0.1 0.8,
    roughness = 0.5,
}

def redMaterial: Material = {
    color = vec3.mk 0.8 0.1 0.2,
    roughness = 0.9,
}

def union (a: SDF) (b: SDF) : SDF =
  \p ->
    let da = a p
    let db = b p
    in
      if da.dist < db.dist then da else db

def sphere x y z l m =
  \p -> { dist = (vec3.norm (p vec3.- (vec3.mk x y z))) - l, mat = m }

def repeat_sdf_xz sdf =
  \p ->
    let (x, y, z) = vec3.unmkT p
    let x' = x - 1.0 - 2.0 * f32.floor (x / 2.0)
    let z' = z - 1.0 - 2.0 * f32.floor (z / 2.0)
    in sdf (vec3.mk x' y z')

def y_plane y m =
  \p ->
    let (_, y', _) = vec3.unmkT p
    in { dist = y' - y, mat = m }
    
def world: SDF = union (y_plane 0 redMaterial) (repeat_sdf_xz (sphere 0 1 0 1 ballMaterial))
                                                              
def raymarch (sdf: SDF) (pos: vec3.t) (dir: vec3.t) : Hit =
  let (p,h,_) = loop (p, h, i) = (pos, false, 0)
                while i < 200 && !h do
                let t = (sdf p).dist
                let s = vec3.scale t dir
                in (p vec3.+ s, t < 0.001, i+1)
  in {hit=h, mat=(sdf p).mat, pos=p, dir=dir}

type Camera = {
    origin: vec3.t,
    target: vec3.t,
    up: vec3.t,
    fov: f32,
}

def camera: Camera = {
    origin = vec3.mk 0 15 10,
    target = vec3.mk 0 1 0,
    up = vec3.mk 0 1 0,
    fov = 1.04719755,
}

def cameraRay (cam: Camera) x y : vec3.t =
    let forward = vec3.normalize (cam.target vec3.- cam.origin)
    let right = vec3.normalize (vec3.cross forward cam.up)
    let up = vec3.cross right forward
    let scale = f32.tan (cam.fov / 2.0)
    in vec3.normalize (
         forward vec3.+
         vec3.scale (x * scale) right vec3.+
         vec3.scale (y * scale) up)

    
def near_surface (n: vec3.t) (p: vec3.t) = (vec3.scale 0.002 n vec3.+ p)
def sun_direction = vec3.normalize (vec3.mk 2.0 1.0 1.0)

def sky (dir: vec3.t) : vec3.t =
  let d = vec3.normalize dir
  let t = f32.max 0.0 (f32.min 1.0 (0.5 * (d.y + 1.0)))
  let horizon = vec3.mk 8.0 8.5 10.0
  let zenith  = vec3.mk 1.0 2.5 6.0
  let base = vec3.scale (1.0 - t) horizon vec3.+ vec3.scale t zenith
  let sunDot = f32.max 0.0 (vec3.dot d sun_direction)

  let sunCore = sunDot ** 512.0
  let sunGlow = sunDot ** 32.0
  let sunColor  = vec3.mk 20.0 16.0 10.0
  let glowColor = vec3.mk 3.0 2.0 1.0

  in base vec3.+ vec3.scale sunCore sunColor vec3.+ vec3.scale sunGlow glowColor
    
def refl d n = d vec3.- vec3.scale (2.0 * vec3.dot d n) n

def incoming_sky (sdf: SDF) (origin: vec3.t) (dir: vec3.t) : vec3.t =
  let blocker = raymarch sdf origin dir
  in if blocker.hit then vec3.mk 0.0 0.0 0.0 else sky dir

def renderPixel (cam: Camera) x y : vec3.t =
  let dir = cameraRay cam x y
  let hit = raymarch world cam.origin dir
  in if !hit.hit then sky dir else
       let normal = sdf_normal world hit.pos
       let surf = near_surface normal hit.pos
       let view = vec3.scale (-1.0) dir
       let light = sun_direction
       let nl = f32.max 0.0 (vec3.dot normal light)
       let light_radiance = incoming_sky world surf light
       let sun = vec3.scale nl (brdf hit.mat normal view light vec3.* light_radiance)
       let ref_dir = vec3.normalize (refl dir normal)
       let nr = f32.max 0.0 (vec3.dot normal ref_dir)
       let ref_radiance = incoming_sky world surf ref_dir
       let reflected = vec3.scale nr (brdf hit.mat normal view ref_dir vec3.* ref_radiance)
       in sun vec3.+ reflected

def srgb_to_rec2020 (c: vec3.t) : vec3.t =
  let (r, g, b) = vec3.unmkT c
  in vec3.mk
    (0.6274038959 * r + 0.3292830384 * g + 0.0433130657 * b)
    (0.0690972894 * r + 0.9195403951 * g + 0.0113623156 * b)
    (0.0163914389 * r + 0.0880133079 * g + 0.8955952532 * b)
    
def rec2020_to_srgb (c: vec3.t) : vec3.t =
  let (r, g, b) = vec3.unmkT c
  in vec3.mk
    ( 1.6604910021 * r - 0.5876411388 * g - 0.0728498633 * b)
    (-0.1245504745 * r + 1.1328998971 * g - 0.0083494226 * b)
    (-0.0181507634 * r - 0.1005788980 * g + 1.1187296614 * b)

def agx_inset (c: vec3.t) : vec3.t =
  let (r, g, b) = vec3.unmkT c
  in vec3.mk
    (0.8566271533 * r + 0.0951212405 * g + 0.0482516061 * b)
    (0.1373189729 * r + 0.7612419906 * g + 0.1014390365 * b)
    (0.1118982130 * r + 0.0767994186 * g + 0.8113023684 * b)

def agx_outset (c: vec3.t) : vec3.t =
  let (r, g, b) = vec3.unmkT c
  in vec3.mk
    ( 1.1271005818 * r - 0.1106066431 * g - 0.0164939387 * b)
    (-0.1413297635 * r + 1.1578237022 * g - 0.0164939387 * b)
    (-0.1413297635 * r - 0.1106066431 * g + 1.2519364066 * b)

def agx_contrast (x: f32) : f32 =
  let x2 = x * x
  let x4 = x2 * x2
  let x6 = x4 * x2

  in -17.86 * x6 * x
     + 78.01 * x6
     - 126.7 * x4 * x
     + 92.06 * x4
     - 28.72 * x2 * x
     + 4.361 * x2
     - 0.1718 * x
     + 0.002857

def agx_linear (hdr: vec3.t) : vec3.t =
  let c = vec3.map (f32.max 0.0) hdr
  let c = srgb_to_rec2020 c
  let c = agx_inset c
  let c = vec3.map (\x -> f32.log2 (f32.max x 1.0e-10)) c
  let min_ev = -12.47393
  let max_ev = 4.026069
  let c = vec3.map (\x -> clamp01 ((x - min_ev) / (max_ev - min_ev))) c
  let c = vec3.map agx_contrast c
  let c = agx_outset c
  let c = vec3.map (\x -> f32.max 0.0 x ** 2.2) c
  let c = rec2020_to_srgb c
  in vec3.map clamp01 c

def srgb_encode (x: f32) : f32 =
  let c = f32.max 0.0 x
  in if c <= 0.0031308 then 12.92 * c else 1.055 * (c ** (1.0 / 2.4)) - 0.055

def agx (hdr: vec3.t) : vec3.t =
  vec3.map srgb_encode (agx_linear hdr)

entry main h w =
  tabulate_2d h w
              (\j i -> let x = -1 + f32.i64 i * (2    / f32.i64 w)
                       let y =  1 + f32.i64 j * ((-2) / f32.i64 h)
                       let out = agx (vec3.scale 0.1 (renderPixel camera x y))
                       in map (\x -> u8.f32 (255*clamp01 x)) (vec3.unmkA out))

                           
