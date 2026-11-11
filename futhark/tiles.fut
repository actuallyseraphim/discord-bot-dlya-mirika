import "vec3"
import "common"                                 
import "tonemapping"
import "renderer"
       
entry main h w =
  tabulate_2d h w
              (\j i -> let x = -1 + f32.i64 i * (2    / f32.i64 w)
                       let y =  1 + f32.i64 j * ((-2) / f32.i64 h)
                       let out = tonemapping.agx (vec3.scale 0.1 (renderPixel camera x y))
                       in map (\x -> u8.f32 (255*clamp01 x)) (vec3.unmkA out))

                           
