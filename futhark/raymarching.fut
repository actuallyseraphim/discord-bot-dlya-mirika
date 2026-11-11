import "vec3"
import "material"
open import "sdf"

def grad f x = vjp f x 1f32

def sdf_normal (sdf: SDF.t) (p: vec3.t): vec3.t =
  vec3.normalize (grad (\x -> (sdf x).dist) p)

type Hit = {
    hit: bool,
    mat: Material,
    pos: vec3.t,
    dir: vec3.t,
}

def raymarch (sdf: SDF.t) (pos: vec3.t) (dir: vec3.t) : Hit =
  let omega = 0.5
  let (p,h,_,_,_,_) = loop (p, h, d, r_prev, r, i) = (pos, false, 1, 0, 0, 0)
                        while i < 200 && !h do
                        let d = r+omega*r*(d-r_prev+r)/(d+r_prev-r)
                        let r_next = (sdf (p vec3.+ vec3.scale d dir)).dist
                        let (d, r_next) = if d > r + r_next then (r, (sdf (p vec3.+ vec3.scale r dir)).dist) else (d, r_next)
                        let s = vec3.scale d dir
                        in (p vec3.+ s, r_next < 0.001, d, r, r_next, i+1)
  in {hit=h, mat=(sdf p).mat, pos=p, dir=dir}
