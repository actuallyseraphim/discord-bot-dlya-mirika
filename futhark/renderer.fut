import "vec3"
import "raymarching"
import "world"
import "material"
       
type Camera = {
    origin: vec3.t,
    target: vec3.t,
    up: vec3.t,
    fov: f32,
}

def camera: Camera = {
    origin = vec3.mk 2 3 5,
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
    
def incoming_sky (sdf: SDF.t) (origin: vec3.t) (dir: vec3.t) : vec3.t =
  let blocker = raymarch sdf origin dir
  in if blocker.hit then vec3.mk 0.02 0.01 0.1 else sky dir

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
       let ref_dir = vec3.normalize (vec3.refl dir normal)
       let nr = f32.max 0.0 (vec3.dot normal ref_dir)
       let ref_radiance = incoming_sky world surf ref_dir
       let reflected = vec3.scale nr (brdf hit.mat normal view ref_dir vec3.* ref_radiance)
       in sun vec3.+ reflected
