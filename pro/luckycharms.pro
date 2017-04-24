pro luckycharms,hearts=hearts,horseshoes=horseshoes,moons=moons $
                ,clovers=clovers,stars=stars;,balloons=balloons

if not keyword_set(hearts) and $
   not keyword_set(horseshoes) and $
   not keyword_set(moons) and $
   not keyword_set(clovers) and $
   not keyword_set(stars) then $
      print,'LUCKYCHARMS> Use /Hearts /Horseshoes /Moons /Clovers /Stars'

;This is the long awaited Luckycharms plot symbol set for IDL
;Created by Jim Davenport, Nov 2007
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."

;hearts
if keyword_set(hearts) then begin
    x=[0,.5,.4,.3,.1,0,-.1,-.3,-.4,-.5,0]*3
    y=[0,.7,.9,1,.9,.6,.9,1,.9,.7,0]*3
usersym,x,y,/fill
endif

;horseshoes
if keyword_set(horseshoes) then begin
    x=[0, .33,.5,.7,.7,.33,.2,.2,.33,.33,0,-.33,-.33,-.2,-.2,-.33,-.7,-.7$
    ,-.5,-.33,0]*2
    y=[-1,-.9,-.75,-.33,1,1,1,.5,.5,0,-.2,0,.5,.5,1,1,1,-.33,-.75,-.9,-1]*2
usersym,x,y,/fill
endif

;moons
if keyword_set(moons) then begin
    x=[.2,0,-.2,-.4,-.2,0,.2 , -.2,-.5,-.66,-.8,-.66,-.5,-.2,.2]*3
    y=[1,.8,.55,0,-.55,-.8,-1, -.9,-.7,-.55,0,.55,.7,.9,1]*3
usersym,x,y,/fill
endif

;clovers
if keyword_set(clovers) then begin
    x=.5*[0,1,1.25,1.5,2.4,2.9,3,3.8,4.5,5,4.5,4,1,$
          4,4.5,5,4.5,3.8,3,2.9,2.4,1.5,1.25,1,0,$
          -1,-1.25,-1.5,-2.4,-2.9,-3,-3.8,-4.5,-5,-4.5,-4,-1,$
          -4,-4.5,-5,-4.5,-3.8,-3,-2.9,-2.4,-1.5,-1.25,-1,0]
    y=.5*[1,4,4.5,5,4.7,4,3,2.9,2.5,1.7,1,.7,0,$
          -.7,-1,-1.7,-2.5,-2.9,-3,-4,-4.7,-5,-4.5,-4,-1,$
          -4,-4.5,-5,-4.7,-4,-3,-2.9,-2.5,-1.7,-1,-.7,0,$
          .7,1,1.7,2.5,2.9,3,4,4.7,5,4.5,4,1]
 usersym,x,y,/fill
endif

;stars
if keyword_set(stars) then begin
    x=[0,.2,1., .3,.6, 0.,-.6,-.3,-1,-.2,0]*3
    y=[1,.2,.2,-.2,-1,-.6,-1,-.2,.2,.2,1]*3
usersym,x,y,/fill
endif


return
end
