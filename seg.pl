:- module(
  seg, [
    seg/1,
    segArgs/2,
    segStart/2, segStartX/2, segStartY/2,
    segEnd/2, segEndX/2, segEndY/2,
    segStartEnd/3,
    segEtiqs/2,
    segThickness/2,
    segHV/5,
    segTop/3,
    segBottom/3,
    segLeft/3,
    segRight/3,
    segHeight/3,
    segWidth/3
  ]).

:- use_module(library(clpBNR)).

segArgs(seg(Start, End, Etiqs, Thickness), [Start, End, Etiqs, Thickness]).

segStart(Seg, Start) :-
  segArgs(Seg, [Start | _]).

segStartX(Seg, X) :-
  segStart(Seg, point(X, _)).

segStartY(Seg, Y) :-
  segStart(Seg, point(_, Y)).

segEnd(Seg, End) :-
  segArgs(Seg, [_, End | _]).

segEndX(Seg, X) :-
  segEnd(Seg, point(X, _)).

segEndY(Seg, Y) :-
  segEnd(Seg, point(_, Y)).

segStartEnd(Seg, Start, End) :-
  segArgs(Seg, [Start, End | _]).

segEtiqs(Seg, Etiqs) :-
  segArgs(Seg, [_, _, Etiqs | _]).

segThickness(Seg, Thickness) :-
  segArgs(Seg, [_, _, _, Thickness]).

segInterpolate(Seg, Coeff, point(X, Y)) :-
  segStart(Seg, point(StartX, StartY)),
  segEnd(Seg, point(EndX, EndY)),
  {
    X == StartX * (1 - Coeff) + EndX * Coeff,
    Y == StartY * (1 - Coeff) + EndY * Coeff
  }.

segVec(Seg, vec(X, Y)) :-
  segStart(Seg, point(StartX, StartY)),
  segEnd(Seg, point(EndX, EndY)),
  {
    X == EndX - StartX,
    Y == EndY - StartY
  }.

vecLength(vec(X, Y), L) :-
  { L == sqrt(X ** 2 + Y ** 2) }.

vecNormalize(vec(X, Y), vec(XOut, YOut)) :-
  vecLength(vec(X, Y), L),
  {
    XOut == X / L,
    YOut == Y / L
  },
  vecLength(vec(XOut, YOut), 1).

vecRotate(vec(X, Y), Angle, vec(XOut, YOut)) :-
  {
    XOut == X * cos(Angle) - Y * sin(Angle),
    YOut == X * sin(Angle) + Y * cos(Angle),
    X == XOut * cos(-Angle) - YOut * sin(-Angle),
    Y == XOut * sin(-Angle) + YOut * cos(-Angle)
  }.

segNormal(Seg, Normal) :-
  segVec(Seg, Vec),
  vecNormalize(Vec, VecNorm),
  vecRotate(VecNorm, -pi/2, Normal).

segCoeffOffset(Seg, Coeff, point(X, Y), point(XOut, YOut)) :-
  segNormal(Seg, vec(XNorm, YNorm)),
  segThickness(Seg, Thickness),
  {
    Coeff2 == (Coeff - 0.5) * Thickness,
    XOut == X + XNorm * Coeff2,
    YOut == Y + YNorm * Coeff2
  }.

segHDirCoeff(left, 0).
segHDirCoeff(mid, 0.5).
segHDirCoeff(right, 1).
segVDirCoeff(top, 0).
segVDirCoeff(mid, 0.5).
segVDirCoeff(bottom, 1).

segHV(Dir, H, V, Seg, Point) :-
  segHDirCoeff(H, HCoeff),
  segVDirCoeff(V, VCoeff),
  segHVCoeff(Dir, HCoeff, VCoeff, Seg, Point).
segHVCoeff(v, H, V, Seg, Point) :-
  segInterpolate(Seg, V, VPoint),
  segCoeffOffset(Seg, H, VPoint, Point).
segHVCoeff(h, H, V, Seg, Point) :-
  segInterpolate(Seg, H, HPoint),
  segCoeffOffset(Seg, 1-V, HPoint, Point).

segTop(v, Seg, Top) :-
  segStartY(Seg, Top).
segBottom(v, Seg, Bottom) :-
  segEndY(Seg, Bottom).
segLeft(v, Seg, Left) :-
  segStartX(Seg, Left).
segRight(v, Seg, Right) :-
  segEndX(Seg, Right).

segHeight(v, Seg, Height) :-
  segTop(v, Seg, Top),
  segBottom(v, Seg, Bottom),
  { Height == Bottom - Top }.
segWidth(v, Seg, Width) :-
  segLeft(v, Seg, Left),
  segRight(v, Seg, Right),
  { Width == Right - Left }.

seg(Seg) :-
  segArgs(Seg, [point(X1, Y1), point(X2, Y2), _, Thickness]),
  [X1, Y1, X2, Y2, Thickness]::real,
  { (X1 =< X2) or (Y1 =< Y2) }.

:- begin_tests(seg).

seg1(Seg) :-
  { Thickness == sqrt(2) },
  segArgs(Seg, [point(0, 0), point(5, 5), [], Thickness]).

test('segInterpolate_0') :-
  seg1(Seg),
  segInterpolate(Seg, 0, P),
  segStart(Seg, P).
test('segInterpolate_0.5') :-
  seg1(Seg),
  segInterpolate(Seg, 0.5, point(X, Y)),
  {
    X == 2.5,
    Y == 2.5
  }.
test('segInterpolate_1') :-
  seg1(Seg),
  segInterpolate(Seg, 1, P),
  segEnd(Seg, P).
test('segVec') :-
  seg1(Seg),
  segVec(Seg, vec(5, 5)).
test('vecLength') :-
  seg1(Seg),
  segVec(Seg, Vec),
  vecLength(Vec, L), 
  { L == sqrt(25 + 25) }.
test('vecNormalize') :-
  seg1(Seg),
  segVec(Seg, Vec),
  vecNormalize(Vec, VecNorm),
  vecLength(VecNorm, 1).
test_vecRotate_Gen(0, 5, 5).
test_vecRotate_Gen(pi/4, 0, sqrt(50)).
test_vecRotate_Gen(-pi/2, 5, -5).
test_vecRotate_Gen(R, _, _) :-
  length(L, 100),
  maplist(random, L),
  maplist([X, Y]>>(Y is ((X * 2) - 1) * pi), L, Rs),
  member(R, Rs).
test('vecRotate', [forall(test_vecRotate_Gen(R, X, Y))]) :-
  seg1(Seg),
  segVec(Seg, Vec),
  vecRotate(Vec, R, vec(X, Y)).
test('segNormal') :-
  seg1(Seg),
  segNormal(Seg, vec(X, Y)),
  {
    L == 1 / sqrt(2),
    X == L,
    Y == -L
  }.
test('segCoeffOffset') :-
  seg1(Seg),
  segCoeffOffset(Seg, 0, point(0, 0), point(-0.5, 0.5)).

test_segHV_Gen(h , left  , bottom , -0.5 , 0.5).
test_segHV_Gen(h , left  , mid    , 0    , 0).
test_segHV_Gen(h , left  , top    , 0.5  , -0.5).
test_segHV_Gen(h , mid   , bottom , 2    , 3).
test_segHV_Gen(h , mid   , mid    , 2.5  , 2.5).
test_segHV_Gen(h , mid   , top    , 3    , 2).
test_segHV_Gen(h , right , bottom , 4.5  , 5.5).
test_segHV_Gen(h , right , mid    , 5    , 5).
test_segHV_Gen(h , right , top    , 5.5  , 4.5).
test_segHV_Gen(v , left  , bottom , 4.5  , 5.5).
test_segHV_Gen(v , left  , mid    , 2    , 3).
test_segHV_Gen(v , left  , top    , -0.5 , 0.5).
test_segHV_Gen(v , mid   , bottom , 5    , 5).
test_segHV_Gen(v , mid   , mid    , 2.5  , 2.5).
test_segHV_Gen(v , mid   , top    , 0    , 0).
test_segHV_Gen(v , right , bottom , 5.5  , 4.5).
test_segHV_Gen(v , right , mid    , 3    , 2).
test_segHV_Gen(v , right , top    , 0.5  , -0.5).
test('segHV_v_left_top', [forall(test_segHV_Gen(Dir, H, V, X, Y))]) :-
  seg1(Seg),
  segHV(Dir, H, V, Seg, point(X, Y)).



:- end_tests(seg).
