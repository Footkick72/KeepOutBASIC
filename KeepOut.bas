1 hgr
2 HCOLOR = 3
3 PI = 3.141592653589793238462643383279
4 xscreen = 280
5 yscreen = 192
6 e = rnd(-1298472)
7 csize = 2
8 rem size of a cell, in meters

30 dim triangles(12*1000)
40 ntriangles = 0
50 dim rectangles(16*1000)
60 nrectangles = 0
61 rem to draw a triangle, add the relevant points to triangles -
62 rem x1,y1,z1,x2,y2,z2,x3,y3,z3 - and increment ntriangles
63 rem drawing a rectangle is the analagous with appropriate variables
64 rem the arrays have space for 1000 at once

70 xcamera = 0
80 ycamera = -csize/2
90 zcamera = 0
100 yrot = 0
110 FOV = 120
111 rem choosing to ignore x and z rotation as they are not needed for this
112 rem rotation of 0 is in the +z direction; angles are 0-2π
113 rem FOV is linear and represents how many px a meter is

114 dim c1(2), c2(2), frontier(60), nfrontier(60), ahead(2), sideways(2), f1(2), f2(2), f3(2)
115 rem used for cell wall drawing

118 dim snakes(5*3)

120 dim map(30*30)
121 for i=0 to 30*30-1
122 map(i)=0
123 rem represents the map; 0 = wall, 1 = clear

125 dim goal(2)
130 difficulty = 0.8

140 xcamera = 0
150 ycamera = -csize/2
160 zcamera = 0
170 yrot = 0
180 gosub 4000
190 hiresmode = 1
194 hgr
195 goto 340
200 rem GAME LOOP
210 keystroke = peek(-16384)
220 if keystroke < 128 then goto 200
230 poke -16368,0

240 keystroke = keystroke - 128 + 32
250 if keystroke == asc("w") then znext = zcamera - csize*cos(yrot): xnext = xcamera - csize*sin(yrot)
260 if keystroke == asc("a") then znext = zcamera - csize*sin(yrot): xnext = xcamera + csize*cos(yrot)
270 if keystroke == asc("s") then znext = zcamera + csize*cos(yrot): xnext = xcamera + csize*sin(yrot)
280 if keystroke == asc("d") then znext = zcamera + csize*sin(yrot): xnext = xcamera - csize*cos(yrot)
290 if keystroke == asc("q") then yrot = yrot - PI/2
300 if keystroke == asc("e") then yrot = yrot + PI/2
310 if xnext > 0 or znext > 0 then goto 340
320 if map(-int(xnext+0.1)/csize*30-int(znext+0.1)/csize) >< 0 then zcamera = int(znext+0.1): xcamera = int(xnext+0.1)
330 if goal(0) == -xcamera/csize and goal(1) == -zcamera/csize then goto 1000

340 if hiresmode == 0 then poke 230,32: goto 360
350 if hiresmode == 1 then poke 230,64: goto 360
360 call 62450
370 gosub 3000
380 gosub 5900
390 if hiresmode == 0 then hiresmode = 1: POKE -16300,0: goto 410
400 if hiresmode == 1 then hiresmode = 0: POKE -16299,0: goto 410
410 goto 200

999 rem YOU WIN! Now what do we do?
1000 text: home
1010 difficulty = difficulty + 0.01
1020 if difficulty > 0.99 then difficulty = 0.99
1030 print "YOU WIN!"
1040 input "Would You Like to Play Again? Y/N    "; r$
1060 if asc(r$) == asc("y") or asc(r$) == asc("Y") then goto 180
1070 print "Come Again!"
1080 end

3000 rem subroutine that draws a level
3010 depth = 0
3020 ahead(0) = int(sin(yrot)+0.1): ahead(1) = int(cos(yrot)+0.1)
3030 sideways(0) = int(cos(yrot)+0.1): sideways(1) = -int(sin(yrot)+0.1)
3040 frontier(30) = 1

3050 depth = depth + 1
3060 for i=0 to 59
3070 if frontier(i) == 0 then goto 3200

3080 xgrid = -xcamera/csize: zgrid = -zcamera/csize: x=xgrid: y=zgrid: gosub 3310
3090 xdist = xgrid + ahead(0)*depth: zdist = zgrid + ahead(1)*depth
3100 ishift = i-30
3110 f1(0) = xdist+(ishift+1)*sideways(0): f1(1) = zdist+(ishift+1)*sideways(1)
3120 f2(0) = xdist+(ishift)*sideways(0): f2(1) = zdist+(ishift)*sideways(1)
3130 f3(0) = xdist+(ishift-1)*sideways(0): f3(1) = zdist+(ishift-1)*sideways(1)

3140 if f1(0) < 0 or f1(0) > 29 or f1(1) < 0 or f1(1) > 29 then goto 3160
3150 if map(f1(0)*30 + f1(1)) >< 0 and nfrontier(i-1) == 0 then nfrontier(i-1)=1: x = f1(0): y = f1(1): gosub 3310

3160 if f2(0) < 0 or f2(0) > 29 or f2(1) < 0 or f2(1) > 29 then goto 3180
3170 if map(f2(0)*30 + f2(1)) >< 0 and nfrontier(i) == 0 then nfrontier(i)=1: x = f2(0): y = f2(1): gosub 3310

3180 if f3(0) < 0 or f3(0) > 29 or f3(1) < 0 or f3(1) > 29 then goto 3200
3190 if map(f3(0)*30 + f3(1)) >< 0 and nfrontier(i+1) == 0 then nfrontier(i+1)=1: x = f3(0): y = f3(1): gosub 3310
3200 next i

3210 f = 0
3220 for i=0 to 59
3230 frontier(i) = nfrontier(i)
3240 nfrontier(i) = 0
3270 if frontier(i) == 1 then f=1
3280 next i

3290 if f == 1 then goto 3050
3300 return

3310 rem subroutine that draws a cell with all boundaries
3320 rem takes input as x and y for cell coordinates
3330 if map(x*30+y) == 0 then return
3334 HCOLOR = 3
3335 if goal(0) = x and goal(1) = y then HCOLOR = 5

3340 if x+1 == 30 then c1(0)=x: c1(1)=y: c2(0)=x+1: c2(1)=y: gosub 3500: goto 3360
3350 if map((x+1)*30+y) == 0 then c1(0)=x: c1(1)=y: c2(0)=x+1: c2(1)=y: gosub 3500

3360 if x-1 == -1 then c1(0)=x: c1(1)=y: c2(0)=x-1: c2(1)=y: gosub 3500: goto 3380
3370 if map((x-1)*30+y) == 0 then c1(0)=x: c1(1)=y: c2(0)=x-1: c2(1)=y: gosub 3500

3380 if y+1 == 30 then c1(0)=x: c1(1)=y: c2(0)=x: c2(1)=y+1: gosub 3500: goto 3400
3390 if map(x*30+(y+1)) == 0 then c1(0)=x: c1(1)=y: c2(0)=x: c2(1)=y+1: gosub 3500

3400 if y-1 == -1 then c1(0)=x: c1(1)=y: c2(0)=x: c2(1)=y-1: gosub 3500: goto 3420
3410 if map(x*30+(y-1)) == 0 then c1(0)=x: c1(1)=y: c2(0)=x: c2(1)=y-1: gosub 3500

3420 return

3500 rem subroutine that draws a wall between at the boundary
3510 rem of 2 cells - c1 and c2, xy pairs
3520 xmid = (c1(0) + c2(0))/2
3530 zmid = (c1(1) + c2(1))/2
3540 if c1(1) == c2(1) then goto 3750

3550 rectangles(12*nrectangles+0) = xmid*csize + csize/2
3560 rectangles(12*nrectangles+1) = csize
3570 rectangles(12*nrectangles+2) = zmid*csize

3580 rectangles(12*nrectangles+3) = xmid*csize - csize/2
3590 rectangles(12*nrectangles+4) = csize
3600 rectangles(12*nrectangles+5) = zmid*csize

3610 rectangles(12*nrectangles+6) = xmid*csize - csize/2
3620 rectangles(12*nrectangles+7) = 0
3630 rectangles(12*nrectangles+8) = zmid*csize

3640 rectangles(12*nrectangles+9) = xmid*csize + csize/2
3650 rectangles(12*nrectangles+10) = 0
3660 rectangles(12*nrectangles+11) = zmid*csize

3700 goto 3900

3750 rectangles(12*nrectangles+0) = xmid*csize
3760 rectangles(12*nrectangles+1) = csize
3770 rectangles(12*nrectangles+2) = zmid*csize + csize/2

3780 rectangles(12*nrectangles+3) = xmid*csize
3790 rectangles(12*nrectangles+4) = csize
3800 rectangles(12*nrectangles+5) = zmid*csize - csize/2

3810 rectangles(12*nrectangles+6) = xmid*csize
3820 rectangles(12*nrectangles+7) = 0
3830 rectangles(12*nrectangles+8) = zmid*csize - csize/2

3840 rectangles(12*nrectangles+9) = xmid*csize
3850 rectangles(12*nrectangles+10) = 0
3860 rectangles(12*nrectangles+11) = zmid*csize + csize/2

3900 nrectangles = nrectangles + 1
3910 return

4000 rem subroutine that generates a level
4010 rem operates on list map
4020 rem takes difficulty as input (0 - 1)
4040 snakes(0) = 0
4050 snakes(1) = 0
4060 snakes(2) = 0
4070 snakes(3) = -1: snakes(4) = -1: snakes(5) = -1: snakes(6) = -1: snakes(7) = -1: snakes(8) = -1: snakes(9) = -1: snakes(10) = -1: snakes(11) = -1: snakes(12) = -1: snakes(13) = -1: snakes(13) = -1: snakes(14) = -1
4080 for s=0 to 4
4090 if snakes(s*3) == -1 and snakes(s*3+1) == -1 and snakes(s*3+2) == -1 then goto 4400

4100 if rnd(1) < 0.2 then snakes(s*3+2) = int(4*rnd(1))
4120 if snakes(s*3+2) == 0 and snakes(s*3) > 0 then snakes(s*3) = snakes(s*3) - 1: goto 4160
4130 if snakes(s*3+2) == 1 and snakes(s*3) < 29 then snakes(s*3) = snakes(s*3) + 1: goto 4160
4140 if snakes(s*3+2) == 2 and snakes(s*3+1) > 0 then snakes(s*3+1) = snakes(s*3+1) - 1: goto 4160
4150 if snakes(s*3+2) == 3 and snakes(s*3+1) < 29 then snakes(s*3+1) = snakes(s*3+1) + 1: goto 4160

4160 map(snakes(s*3)*30 + snakes(s*3+1)) = 1

4230 if rnd(1) < difficulty * 0.2 then goto 4260
4240 if rnd(1) < (1 - difficulty) * 0.2 then goal(0)=snakes(s*3): goal(1)=snakes(s*3+1): snakes(s*3) = -1: snakes(s*3+1) = -1: snakes(s*3+2) = -1
4250 goto 4400

4259 rem splitting code
4260 for ns=0 to 4
4270 if snakes(ns*3) >< -1 and snakes(ns*3+1) >< -1 and snakes(ns*3+2) >< -1 then next s
4280 snakes(ns*3) = snakes(s*3)
4290 snakes(ns*3+1) = snakes(ns*3+1)
4300 snakes(ns*3+2) = int(4*rnd(1))

4400 next s

4410 for s=0 to 4
4420 if snakes(s*3) >< -1 and snakes(s*3+1) >< -1 and snakes(s*3+2) >< -1 then goto 4080
4430 next s

4500 return

5900 rem subprotocol to draw all existing objects in the frame
5910 rem does not compute occlusion
5920 gosub 5990
5930 gosub 6590
5940 nrectangles = 0: ntriangles = 0
5950 return

5990 if ntriangles == 0 then return
6000 for objecti=0 to ntriangles-1 step 1
6010 x1 = triangles(9 * objecti + 0)
6020 y1 = triangles(9 * objecti + 1)
6030 z1 = triangles(9 * objecti + 2)
6040 x2 = triangles(9 * objecti + 3)
6050 y2 = triangles(9 * objecti + 4)
6060 z2 = triangles(9 * objecti + 5)
6070 x3 = triangles(9 * objecti + 6)
6080 y3 = triangles(9 * objecti + 7)
6090 z3 = triangles(9 * objecti + 8)

6100 tx = x1
6110 ty = y1
6120 tz = z1
6130 gosub 8100
6140 x1 = tx
6150 y1 = ty
6160 z1 = tz

6200 tx = x2
6210 ty = y2
6220 tz = z2
6230 gosub 8100
6240 x2 = tx
6250 y2 = ty
6260 z2 = tz

6300 tx = x3
6310 ty = y3
6320 tz = z3
6330 gosub 8100
6340 x3 = tx
6350 y3 = ty
6360 z3 = tz

6400 x1 = x1 * FOV / z1 + xscreen/2
6410 y1 = y1 * FOV / z1 + yscreen/2
6420 x2 = x2 * FOV / z2 + xscreen/2
6430 y2 = y2 * FOV / z2 + yscreen/2
6440 x3 = x3 * FOV / z3 + xscreen/2
6450 y3 = y3 * FOV / z3 + yscreen/2

6500 sx = x1: sy = y1: ex = x2: ey = y2: gosub 8000
6510 sx = x2: sy = y2: ex = x3: ey = y3: gosub 8000
6520 sx = x1: sy = y1: ex = x3: ey = y3: gosub 8000

6530 next objecti
6540 return

6590 if nrectangles == 0 then return
6600 for objecti=0 to nrectangles-1 step 1
6610 x1 = rectangles(12 * objecti + 0)
6620 y1 = rectangles(12 * objecti + 1)
6630 z1 = rectangles(12 * objecti + 2)
6640 x2 = rectangles(12 * objecti + 3)
6650 y2 = rectangles(12 * objecti + 4)
6660 z2 = rectangles(12 * objecti + 5)
6670 x3 = rectangles(12 * objecti + 6)
6680 y3 = rectangles(12 * objecti + 7)
6690 z3 = rectangles(12 * objecti + 8)
6700 x4 = rectangles(12 * objecti + 9)
6710 y4 = rectangles(12 * objecti + 10)
6720 z4 = rectangles(12 * objecti + 11)

6800 tx = x1
6810 ty = y1
6820 tz = z1
6830 gosub 8100
6840 x1 = tx
6850 y1 = ty
6860 z1 = tz

6900 tx = x2
6910 ty = y2
6920 tz = z2
6930 gosub 8100
6940 x2 = tx
6950 y2 = ty
6960 z2 = tz

7000 tx = x3
7010 ty = y3
7020 tz = z3
7030 gosub 8100
7040 x3 = tx
7050 y3 = ty
7060 z3 = tz

7100 tx = x4
7110 ty = y4
7120 tz = z4
7130 gosub 8100
7140 x4 = tx
7150 y4 = ty
7160 z4 = tz

7200 x1 = x1 * FOV / z1 + xscreen/2
7210 y1 = y1 * FOV / z1 + yscreen/2
7220 x2 = x2 * FOV / z2 + xscreen/2
7230 y2 = y2 * FOV / z2 + yscreen/2
7240 x3 = x3 * FOV / z3 + xscreen/2
7250 y3 = y3 * FOV / z3 + yscreen/2
7260 x4 = x4 * FOV / z4 + xscreen/2
7270 y4 = y4 * FOV / z4 + yscreen/2

7340 sx = x1: sy = y1: ex = x2: ey = y2: gosub 8000
7440 sx = x2: sy = y2: ex = x3: ey = y3: gosub 8000
7540 sx = x3: sy = y3: ex = x4: ey = y4: gosub 8000
7640 sx = x1: sy = y1: ex = x4: ey = y4: gosub 8000

7700 next objecti
7710 return

7998 rem subprotocol to draw a line clipped to screen
7999 rem takes input as sx, sy, ex, ey (start end x y)
8000 r = 0
8010 if (sx < 0 and ex < 0) or (sx > xscreen and ex > xscreen) or (sy < 0 and ey < 0) or (sy > yscreen and ey > yscreen) then return
8020 if sx < 0 then t = sx: sx = 0: sy = (abs(t-0)/abs(t-ex))*ey + (1-abs(t-0)/abs(t-ex))*sy
8030 if sy < 0 then t = sy: sy = 0: sx = (abs(t-0)/abs(t-ey))*ex + (1-abs(t-0)/abs(t-ey))*sx
8040 if sx >= xscreen then t = sx: sx = xscreen-1: sy = (abs(t-xscreen+1)/abs(t-ex))*ey + (1-abs(t-xscreen+1)/abs(t-ex))*sy
8050 if sy >= yscreen then t = sy: sy = yscreen-1: sx = (abs(t-yscreen+1)/abs(t-ey))*ex + (1-abs(t-yscreen+1)/abs(t-ey))*sx
8060 if r = 0 then r = 1: tx = sx: ty = sy: sx = ex: sy = ey: ex = tx: ey = ty: goto 8010
8070 hplot sx, sy to ex, ey

8098 rem subprotocol to transform x,y,z coordinates into camera space for drawing
8099 rem takes input as tx, ty, tz (transform x y z)
8100 tx = tx + xcamera
8110 tz = tz + zcamera
8120 ty = ty + ycamera
8130 ox = tx
8140 oz = tz
8150 tx = ox * cos(yrot) - oz * sin(yrot)
8160 tz = ox * sin(yrot) + oz * cos(yrot)
8165 if tz <= 0.05 then tz = 0.0001
8170 return
