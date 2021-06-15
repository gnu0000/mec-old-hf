#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"

camera 
   {
	location  <-1.05,  .7, .5>
	direction <0, 0, 3>
	up        <0, 1, 0>
	right     <1, 0, 0>
	look_at   <1, .4, .5>
   }       

#declare oo = height_field {tga "a.tga"}

#declare ooo = union
   {
   object {oo}
   object {oo translate <1, 0, 0>}
   object {oo translate <-1, 0, 0>}

   scale <1, .1, 1>
   translate <0, .5, 0>

	texture 
      { 
      MathB

//		pigment {color Gray70}
//		finish { diffuse 1 ambient 0.1 }
		}
   }

light_source {< 31, 52, -20> color White}
light_source {< -51, 80, 70> color Gray90}

object 
   {
	ooo
   }


