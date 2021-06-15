#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"

camera 
   {
	direction       <0, 0, 2.4>
	up              <0, 1, 0>
	right           <4/3, 0, 0>
	location        <.35,  1.2, -1.5>
	look_at         <.5,  .1, .5>

//	location        <.1, .02, .1>
//	look_at         <1,  .17, 1>
   }       

#declare oo = height_field {tga "sample1.tga" scale <1.02, 1.02, 1.02>}


///* yellowish light */
//object { light_source { <3, 2, -1>  color red 1 green .75 blue .5
//  area_light <0.2, 0, 0> <0,0,-2.5> 2, 7
//  adaptive 1
//  jitter
//  } 
//}
//
///* reddish light */
//object { light_source { <-3, -3, -.7> color red .7 green .28 blue .112
//    area_light <.5, -.5, 0> <0, 0, -1> 2, 10
//    adaptive 1
//    jitter
//   }
//}
//
//
///* sky background fill light */
//object { light_source { <-2, -2, -10>  color red .03 green .084 blue .12
//  area_light <4, 0, 0> <0, 4, 0> 3, 3
//  adaptive 1
//  jitter
//  } 
//}
//
///* light up the sky itself */
//light_source { <1,-1,1> color White
//		spotlight
//		point_at <1,1,-10>
//		radius 100 falloff 180
//	}
//
//// big light in the sky 
//
//object { sphere { <30,20,-10> 2 }
//	 texture { pigment { White }
//		   finish { ambient 2 } 
//		 }
//	} 
light_source {< 1, 6, -4> color White }

object {
	sphere  { <0, 0, 0>, 100               /* sky sphere */
		  texture { 
			    pigment { gradient z
			    color_map { 
			       [ 0.0 color red 0 green 0 blue .6 ]
			       [ 0.4 color red .05 green .05 blue .55 ]
			       [ 0.5 color red .1 green .1 blue .5 ]
			       [ 0.7 color red .1 green .1 blue .35 ]
			       [ 1.0 color Black ]

				       } /* end color_map */
			    } /* end pigment */
			   scale <200, 200, 200>
			  }  /* end texture */
		} /* end sphere */
	translate <0, 0, 0>
	} /* end object */


#declare op = union
   {
   object {oo}
   object {oo translate <1, 0, 0>}
   object {oo translate <0, 0, 1>}
   cylinder { <0, 0, 0> <0, 1, 0> .005 texture {MathR}}
   cylinder { <1, 0, 0> <1, 1, 0> .005 texture {MathR}}
   cylinder { <0, 0, 1> <0, 1, 1> .005 texture {MathR}}
   cylinder { <1, 0, 1> <1, 1, 1> .005 texture {MathR}}
   }

object 
   {
	op
	scale     < 1, 0.25, 1 >            // reduce height (was ys=0.20)
	rotate    < 0, 0, 0 >              // flip up from xz plane to xy plane
	translate < 0, 0, 0 >
	texture 
      { 
		pigment {color White}
		finish { diffuse 1 ambient 0.02 }
		}
    } /* end object */

