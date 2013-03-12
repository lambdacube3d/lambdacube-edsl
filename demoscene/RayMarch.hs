module RayMarch (fxRayMarch) where

import LC_API
import Utility
import Swizzling

{-
iGlobalTime
iResolution.xy
-}
-- port of: https://www.shadertoy.com/view/MsfGzr
fxRayMarch :: Exp F Float -> Exp F V2F -> Exp Obj (Image N1 V4F)
fxRayMarch iGlobalTime iResolution = renderScreen frag
  where
{-
float tunnel(vec3 p)
{
	return cos(p.x)+cos(p.y*1.5)+cos(p.z)+cos(p.y*20.)*.05;
}
-}
    tunnel :: Exp F V3F -> Exp F Float
    tunnel p = cos' x @+ cos' (y @* floatF 1.5) @+ cos' z @+ cos' (y @* floatF 20) @* floatF 0.05
      where
        V3 x y z = unpack' p

{-
float ribbon(vec3 p)
{
	return length(max(abs(p-vec3(cos(p.z*1.5)*.3,-.5+cos(p.z)*.2,.0))-vec3(.125,.02,iGlobalTime+3.),vec3(.0)));
}
-}
    ribbon :: Exp F V3F -> Exp F Float
    ribbon p = length' $ max' (abs' (p @- v1) @- v2) (Const zero' :: Exp F V3F)
      where
        V3 x y z = unpack' p
        v1 = pack' $ V3 (cos' (z @* floatF 1.5) @* floatF 0.3) (floatF (-0.5) @+ cos' z @* floatF 0.2) (floatF 0)
        v2 = pack' $ V3 (floatF 0.125) (floatF 0.02) (iGlobalTime @+ floatF 3)

{-
float scene(vec3 p)
{
	return min(tunnel(p),ribbon(p));
}
-}
    scene :: Exp F V3F -> Exp F Float
    scene p = min' (ribbon p) (tunnel p)

{-
vec3 getNormal(vec3 p)
{
	vec3 eps=vec3(.1,0,0);
	return normalize(vec3(scene(p+eps.xyy),scene(p+eps.yxy),scene(p+eps.yyx)));
}
-}
    getNormal :: Exp F V3F -> Exp F V3F
    getNormal p = normalize' $ pack' $ V3 (scene $ p @+ xyy) (scene $ p @+ yxy) (scene $ p @+ yyx)
      where
        xyy = Const (V3 0.1 0 0) :: Exp F V3F
        yxy = Const (V3 0 0.1 0) :: Exp F V3F
        yyx = Const (V3 0 0 0.1) :: Exp F V3F

{-
void main(void)
{
	vec2 v = -1.0 + 2.0 * gl_FragCoord.xy / iResolution.xy;
	v.x *= iResolution.x/iResolution.y;
 
	vec4 color = vec4(0.0);
	vec3 org   = vec3(sin(iGlobalTime)*.5,cos(iGlobalTime*.5)*.25+.25,iGlobalTime);
	vec3 dir   = normalize(vec3(v.x*1.6,v.y,1.0));
	vec3 p     = org,pp;
	float d    = .0;

	//First raymarching
	for(int i=0;i<64;i++)
	{
	  	d = scene(p);
		p += d*dir;
	}
	pp = p;
	float f=length(p-org)*0.02;

	//Second raymarching (reflection)
	dir=reflect(dir,getNormal(p));
	p+=dir;
	for(int i=0;i<32;i++)
	{
		d = scene(p);
	 	p += d*dir;
	}
	color = max(dot(getNormal(p),vec3(.1,.1,.0)), .0) + vec4(.3,cos(iGlobalTime*.5)*.5+.5,sin(iGlobalTime*.5)*.5+.5,1.)*min(length(p-org)*.04, 1.);

	//Ribbon Color
	if(tunnel(pp)>ribbon(pp))
		color = mix(color, vec4(cos(iGlobalTime*.3)*.5+.5,cos(iGlobalTime*.2)*.5+.5,sin(iGlobalTime*.3)*.5+.5,1.),.3);

	//Final Color
	vec4 fcolor = ((color+vec4(f))+(1.-min(pp.y+1.9,1.))*vec4(1.,.8,.7,1.))*min(iGlobalTime*.5,1.);
	gl_FragColor = vec4(fcolor.xyz,1.0);
}
-}
    iter :: GPU a => Exp F Float -> (Exp F a -> Exp F a) -> Exp F a -> Exp F a
    iter n fn s0 = Loop st lc sr (tup2 (floatF 0,s0))
      where
        st is = tup2 (i @+ floatF 1, fn s)
          where
            (i,s) = untup2 is
        lc is = i @< n
          where
            (i,_) = untup2 is
        sr is = s
          where
            (_,s) = untup2 is
        
    frag _ = FragmentOut $ (pack' $ V4 r g b (floatF 1)) :. ZT
      where
        -- v, org, dir, p, pp
        V4 fx fy _ _    = unpack' fragCoord'
        V2 w h          = unpack' iResolution
        v               = pack' $ V2 ((floatF (-1) @+ floatF 2 @* fx @/ w) @* (w @/ h)) (floatF (-1) @+ floatF 2 @* fy @/ h)
        org             = pack' $ V3 (sin' iGlobalTime @* floatF 0.5) (cos' (iGlobalTime @* floatF 0.5) @* floatF 0.25 @+ floatF 0.25) iGlobalTime
        V2 vx vy        = unpack' v
        dir             = normalize' $ pack' $ V3 (vx @* floatF 1.6) vy (floatF 1)
        -- First raymarching
        pp              = iter (floatF 64) (\a -> a @+ dir @* scene a) org
        f               = length' $ (pp @- org) @* floatF 0.02
        -- Second raymarching (reflection)
        dir2            = reflect' dir (getNormal pp)
        p               = iter (floatF 32) (\a -> a @+ dir @* scene a) (pp @+ dir2)
        color           = pack' (V4 (floatF 0.3) (cos' (iGlobalTime @* floatF 0.5) @* floatF 0.5 @+ floatF 0.5) (sin' (iGlobalTime @* floatF 0.5) @* floatF 0.5 @+ floatF 0.5) (floatF 1))
                          @* min' (length' (p @- org) @* floatF 0.04) (floatF 1)
                          @+ max' (dot' (getNormal p) (Const $ V3 0.1 0.1 0 :: Exp F V3F)) (floatF 0)
        -- TODO: Ribbon Color
        rcolor          = mix'  color
                                (pack' $ V4 (cos' (iGlobalTime @* floatF 0.3) @* floatF 0.5 @+ floatF 0.5)
                                            (cos' (iGlobalTime @* floatF 0.2) @* floatF 0.5 @+ floatF 0.5)
                                            (sin' (iGlobalTime @* floatF 0.3) @* floatF 0.5 @+ floatF 0.5)
                                            (floatF 1)
                                )
                                (floatF 0.3)
        color2          = Cond (tunnel pp @> ribbon pp) rcolor color
        fcolor          = ((color2 @+ f) @+ (Const $ V4 1 0.8 0.7 1 :: Exp F V4F) @* (floatF 1 @- min' (y_ pp @+ floatF 1.9) (floatF 1)))
                          @* min' (iGlobalTime @* floatF 0.5) (floatF 1)
        V4 r g b _      = unpack' fcolor
