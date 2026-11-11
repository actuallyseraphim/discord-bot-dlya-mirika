import "vec3"
import "sdf"
import "material"
       
def houseWallMaterial: Material =
  { color = vec3.mk 0.8 0.65 0.45
  , roughness = 0.9 }

def roofMaterial: Material =
  { color = vec3.mk 0.5 0.08 0.08
  , roughness = 0.8 }

def doorMaterial: Material =
  { color = vec3.mk 0.3 0.12 0.05
  , roughness = 0.9 }

def windowMaterial: Material =
  { color = vec3.mk 0.1 0.35 0.8
  , roughness = 0.2 }

def groundMaterial: Material =
  { color = vec3.mk 0.2 0.5 0.15
  , roughness = 1.0 }
                
def box_dist
    (x: f32) (y: f32) (z: f32)
    (hx: f32) (hy: f32) (hz: f32) : f32 =
  let qx = f32.abs x - hx
  let qy = f32.abs y - hy
  let qz = f32.abs z - hz

  let ox = f32.max qx 0.0
  let oy = f32.max qy 0.0
  let oz = f32.max qz 0.0

  let outside = f32.sqrt (ox*ox + oy*oy + oz*oz)
  let inside = f32.min (f32.max qx (f32.max qy qz)) 0.0
  in
  outside + inside

def box cx cy cz hx hy hz m =
  \p ->
    let (x, y, z) = vec3.unmkT p
    in { dist = box_dist (x-cx) (y-cy) (z-cz) hx hy hz
       , mat = m }
    
def sphere l m =
  \p -> { dist = vec3.norm p - l, mat = m }

def translate sdf x y z : SDF.t =
  \p -> sdf (p vec3.- (vec3.mk x y z))

def scale (sdf : SDF.t) x y z : SDF.t =
  \p -> let s = sdf (p vec3./ (vec3.mk x y z))
        in { dist=s.dist * (f32.min (f32.min x y) z), mat = s.mat }
                    
    
def sdTriPrism hx hy m : SDF.t =
  \p ->
    let k = f32.sqrt 3.0
    let hx = hx*0.5*k
    let p = vec3.mk ((f32.abs p.x / hx) - 1) (p.y / hx + 1 / k) p.z
    let p = if p.x+k*p.y > 0 then vec3.mk ((p.x-k*p.y)/2) ((-k*p.x-p.y)/2) p.z else p
    let p = vec3.mk (p.x - f32.min (f32.max p.x (-2)) 0) p.y p.z
    let d1 = f32.sqrt(p.x*p.x+p.y*p.y)*f32.sgn(-p.y)*hx
    let d2 = f32.abs(p.z)-hy
    let md1 = f32.max 0 d1
    let md2 = f32.max 0 d2
    in {dist=(f32.sqrt (md1*md1+md2*md2)) + (f32.min (f32.max d1 d2) 0), mat=m}
                                       
def house: SDF.t =
  let body =
    box 0.0 0.9 0.0
        1.2 0.9 1.0
        houseWallMaterial

  let roof = translate (scale (sdTriPrism 1 1.1 roofMaterial) 2 0.5 1) 0 2.0 0 

  let door =
    box 0.0 0.625 1.01
        0.30 0.625 0.01
        doorMaterial

  let left_window =
    box (-0.75) 1.25 1.01
        0.25 0.25 0.01
        windowMaterial

  let right_window =
    box 0.75 1.25 1.01
        0.25 0.25 0.01
        windowMaterial

  in
  (SDF.union
   (SDF.union
    (SDF.smooth_union 0.1
     body roof) door)
  (SDF.union left_window right_window))
  
def y_plane y m =
  \p ->
    let (_, y', _) = vec3.unmkT p
    in { dist = y' - y, mat = m }

def world: SDF.t = 
  SDF.smooth_union 0.1
  (y_plane 0.0 groundMaterial)
  house
