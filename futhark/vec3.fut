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
  def refl d n = d - scale (2.0 f32.* dot d n) n
         
  def mk x y z = {x, y, z}
  def unmkA ({x, y, z}: t) = [x,y,z]
  def unmkT ({x, y, z}: t) = (x,y,z)
  def unmk {x, y, z} = {x, y, z}
}                           
