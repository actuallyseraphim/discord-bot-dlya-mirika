import "vec3"
import "common"

type Material = {
    color: vec3.t,
    roughness: f32,
}

def mat_interpolate (t: f32) (a: Material) (b: Material) : Material =
  let t = clamp01 t
  let color = a.color vec3.+ vec3.scale t (b.color vec3.- a.color)

  let roughness = a.roughness + (b.roughness - a.roughness) * t

  in { color = color
     , roughness = roughness}  

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
