:- module(
  seg, [
    segStart/2, segStartX/2, segStartY/2,
    segEnd/2, segEndX/2, segEndY/2,
    segStartEnd/3,
    segThickness/2,
    segCorner/4
  ]).

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

segThickness(Seg, Thickness) :-
  segArgs(Seg, [_, _, _, Thickness]).

segCornerOp(v, left, -).
segCornerOp(v, right, +).
segCornerOp(v, top, segStart).
segCornerOp(v, bottom, segEnd).
segCornerOp(h, left, segStart).
segCornerOp(h, right, segEnd).
segCornerOp(h, top, -).
segCornerOp(h, bottom, +).

segCornerApply(v, Seg, Thickness, Op, Getter, point(XOffset, Y)) :-
  call(Getter, Seg, point(X, Y)),
  Expr =..[Op, X, (Thickness / 2.0)],
  [XOffset, X, Thickness]::real,
  {XOffset == Expr}.
segCornerApply(h, Seg, Thickness, Getter, Op, point(X, YOffset)) :-
  call(Getter, Seg, point(X, Y)),
  Expr =..[Op, Y, (Thickness / 2.0)],
  [YOffset, Y, Thickness]::real,
  {YOffset == Expr}.

segCorner(Dir, H-V, Seg, Corner) :-
  segCornerOp(Dir, H, HOp),
  segCornerOp(Dir, V, VOp),
  segThickness(Seg, Thickness),
  segCornerApply(Dir, Seg, Thickness, HOp, VOp, Corner).