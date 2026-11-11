import "vec3"
import "common"

module tonemapping = {
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
}
