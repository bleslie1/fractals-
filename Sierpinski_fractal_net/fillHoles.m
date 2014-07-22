(* ::Package:: *)

BeginPackage["fillHoles`",{"sierpinskifractalnet2D`"}]
fillHolesMethod::usage="dynamic module for fill holes method"

Begin["`Private`"]

a={-2/Sqrt[(3)],0,-(1/(2Sqrt[6]))};
b={1/Sqrt[(3)],-1,-(1/(2Sqrt[6]))};
c={1/Sqrt[(3)],1,-(1/(2Sqrt[6]))};
(*1st Iteration*)
d={-1/(2*Sqrt[(3)]),-1/2,-(1/(2Sqrt[6]))};
e={1/Sqrt[(3)],0,-(1/(2Sqrt[6]))};
f={-1/(2*Sqrt[(3)]),1/2,-(1/(2Sqrt[6]))};
(*2nd Iteration*)
(*Edge:u_0\[Rule]u_1*)
g={-5/(4*Sqrt[(3)]),-1/4,-(1/(2Sqrt[6]))};
h={1/(4*Sqrt[(3)]),-3/4,-(1/(2Sqrt[6]))};
(*Edge:u_1\[Rule]u_2*)
i={1/Sqrt[(3)],-1/2,-(1/(2Sqrt[6]))};
j={1/Sqrt[(3)],1/2,-(1/(2Sqrt[6]))};
(*Edge:u_2\[Rule]u_0*)
k={1/(4*Sqrt[(3)]),3/4,-(1/(2Sqrt[6]))};
l={-5/(4*Sqrt[(3)]),1/4,-(1/(2Sqrt[6]))};
(*Edge:3 center-x,-y,y*)
m={-1/(2*Sqrt[(3)]),0,-(1/(2Sqrt[6]))};
(*n={1/(4*Sqrt[(3)]),-1/4,-(1/(2Sqrt[6]))};*)
o={1/(4*Sqrt[(3)]),1/4,-(1/(2Sqrt[6]))};
pattern[{Polygon[pts_]}]:={Texture[ExampleData[{"Texture","HexagonalHoles"}]],Polygon[pts,VertexTextureCoordinates->pts]}

u0={-2/Sqrt[3],0,-(1/(2Sqrt[6]))};
u1={1/Sqrt[3],-1,-(1/(2Sqrt[6]))};
u2={1/Sqrt[3],1,-(1/(2Sqrt[6]))};
u3=(1/3)(u0+u1+u2);
perpendicular1=Cross[u1-u0,u2-u0];
(*****************************************************)
simplex[theta_,t_, contraction_,{t0_,t1_,t2_},{u0_,u1_,u2_}]:=
Module[{t3=1/3*(t0+t1+t2),
perpendicular=Cross[t1-t0,t2-t0],
fractalNetOuter={{PointSize[Small],Black,Point[{u0,u1,u2}]},{Triangle[u0,u1,u2]}},
fractalINNER={{PointSize[Small],Black,Point[{u0,u1,u2}]},{Polygon[{u0,u1,u2}-{0,0,0.001}]}}},
{(*Graphics3D[fractalNetOuter],(*{Red,Arrow[Tube[{u3,perpendicular/3}]]},*)*)
Graphics3D[GeometricTransformation[fractalINNER,AffineTransform[{RotationMatrix[theta,perpendicular]*contraction,t*(t3+{0,0,0.002})/2}]]]}]
(*****************************************************)
F0={{{-2/Sqrt[3],0,-(1/(2Sqrt[6]))},{1/Sqrt[3],-1,-(1/(2Sqrt[6]))},{1/Sqrt[3],1,-(1/(2Sqrt[6]))}}};
(*****************************************************)
F[n_]:=If[n==0,F0,recursivelyCreateChildren2D[1,n,F1NetOne][[All,1]]]
(*****************************************************)

makeFrame[theta_,t_,contraction_,n_]:=Show[
(*****************************************************)
Graphics3D[{Red,Arrowheads[Medium],Arrow[Tube[{u3,perpendicular1/10}],.1]}],
(*****************************************************)
Graphics3D[
	(*VERTEX LABELS*)
				{Style[Text["\!\(\*SubscriptBox[\(u\), \(0\)]\)",u0+.05],Large],
				Style[Text["\!\(\*SubscriptBox[\(u\), \(1\)]\)",u1+.05],Large],
				Style[Text["\!\(\*SubscriptBox[\(u\), \(2\)]\)",u2+.05],Large],
				Style[Text["\!\(\*SubscriptBox[\(u\), \(3\)]\)",u3+.05],Large]}],
Graphics3D[pattern[{Polygon[{d,e,f}]}]],
(*****************************************************)
Graphics3D[Map[Polygon,F[n+1][[1;;(3*4^(n))]]]],
Flatten[Map[Apply[simplex, {theta, t, contraction, F0[[1]],#}]&,F[n]]],
			Boxed->False,ImageSize->{600,600},ViewPoint->{0.18,-1,0.542},ViewAngle->0.69115,PlotRange->1.5,Background->White,Lighting->"Neutral",
Axes->True,AxesOrigin->{0,0,0},AxesStyle->Directive[Dotted,Italic,Black,Thin]]
(*****************************************************)
fillHolesMethod=DynamicModule[{theta=0,t=0,contraction=1},Column[{Panel[Grid[{{"n",SetterBar[Dynamic[n],{0,1,2,3}]},{"angle",Slider[Dynamic[theta],{0,Pi}]},{"translate",Slider[Dynamic[t],{0,1}]},{"contract",Slider[Dynamic[contraction],{1,1/2}]}},Alignment->Left],ImageSize-> {600,125},Background->White],
Dynamic[makeFrame[theta,t,contraction,n]]}]];
(*Manipulate[makeFrame[theta,t,contraction,n],{{theta,0,"\[Theta]"},0,Pi},{{t,0,"translate"},0,1},{{contraction,1,"contract"},1,1/2},{{n,0,"iteration"},{0,1,2,3}}];*)

End[ ]
EndPackage[ ]
