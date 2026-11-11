import "material"
import "vec3"

type ObjectField = {
    dist: f32,
    mat: Material,
  }

def objField (dist: f32) (mat: Material) : ObjectField =
  { dist = dist, mat = mat }
  
module SDF = {
  type^ t = vec3.t -> ObjectField

  def union (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      in if da.dist < db.dist then da else db

  def intersection (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      in
      if da.dist > db.dist then da else db

  def subtraction (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      let d = -db.dist
      in if da.dist > d then da
         else { dist = d, mat = db.mat }

  def smooth_union (k: f32) (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      let h = f32.max 0.0 (f32.min 1.0 (0.5 + 0.5 * (db.dist - da.dist) / k))
      let d =
        db.dist * (1.0 - h)
        + da.dist * h
        - k * h * (1.0 - h)
      let mat = mat_interpolate h db.mat da.mat
      in { dist = d, mat = mat }
      
  def smooth_intersection (k: f32) (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      let h = f32.max 0.0 (f32.min 1.0 (0.5 + 0.5 * (da.dist - db.dist) / k))
      let d =
        db.dist * (1.0 - h)
        + da.dist * h
        + k * h * (1.0 - h)
      let mat = mat_interpolate h db.mat da.mat
      in { dist = d, mat = mat }

  def smooth_subtraction (k: f32) (a: t) (b: t) : t =
    \p ->
      let da = a p
      let db = b p
      let h = f32.max 0.0 (f32.min 1.0 (0.5 + 0.5 * (da.dist + db.dist) / k))
      let d =
        -db.dist * (1.0 - h)
        + da.dist * h
        + k * h * (1.0 - h)
      let mat = mat_interpolate h db.mat da.mat
      in { dist = d, mat = mat }
}
