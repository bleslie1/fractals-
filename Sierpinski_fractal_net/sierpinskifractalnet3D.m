(* ::Package:: *)

BeginPackage["sierpinskifractalnet3D`",{"sierpinskifractalnet2D`"}]
fractalNetModule3D::usage="fractalNetModule3D: Displays folding between 2D and 3D fractal net."
Begin["`Private`"]
(*******************************************************************)
(*Begin: 3D Unfolding Code*)(*******************************************************************)(*******************************************************************)
(*Documentation: applyRotationStack*)applyRotationStack::usage="applyRotationStack: Applies a rotation transform to a 3D triangle, using a stack of rotations.";
(*Function: applyRotationStack*)
applyRotationStack[point_,rotations_]:=Fold[(RotationTransform@@#2)[#1]&,point,rotations];
(*******************************************************************)
(*******************************************************************)
(*Documentation: drawFolded3D*)
drawFolded3D::usage="drawFolded3D: Unfolds and displays a tetrahedron with extra rotation information,using a stack of rotation information,i.e.multiple roatations. Each side of the tetrahedron has it's color.";
(*Function: drawFolded3D*)
drawFolded3D[{{pBLeft_,pBRight_,PBUp_,PTop_},{rotationsBase_,rotations1_,rotations2_,rotations3_}}]:=Graphics3D@{Opacity[1],EdgeForm[{Thickness[Medium],Directive[Black]}],{{Opacity[1],Darker[Green,.6],Specularity[White,50],Polygon[applyRotationStack[{pBLeft,pBRight,PBUp},rotationsBase]]},(*Yellow*){Opacity[1],Darker[Pink,.6],Specularity[White,50],Polygon[applyRotationStack[{pBLeft,pBRight,PTop},rotations1]]},(*Red*){Opacity[1],Darker[Blue,.6],Specularity[White,50],Polygon[applyRotationStack[{pBLeft,PBUp,PTop},rotations2]]},(*Blue*)(*{White,Specularity[GrayLevel[1],200]}*){Opacity[1],Darker[Yellow,.6],Specularity[White,50],Polygon[applyRotationStack[{pBRight,PBUp,PTop},rotations3]]}}}
(*******************************************************************)
(*******************************************************************)
(*Documentation: showFolded3D*)
showFolded3D::usage="showFolded3D: Unfolds and displays a list of tetrahedrons with extra rotation information.";
(*Function: showFolded3D*)
showFolded3D[plist_]:=Show[Map[drawFolded3D,plist]]
(*******************************************************************)
(*******************************************************************)
(*Documentation: midpoint*)
midpoint::usage="midpoint: Computes midpoints a midpoint between two points to be used in the IFS method for generating the fractal.";
(*Function: midpoint*)
midpoint[x_,y_]:=(x+y)/2
(*******************************************************************)
(*******************************************************************)
(*In the 2D code we use only unit Triangle, In 3D code the unit traingle is used as the Base points of the unit Tetrahedron.*)
(*Points on Tetrahedron S0*)
b=N[{-(1/(2 Sqrt[3])),-(1/2),-(1/(2 Sqrt[6]))}];
c=N[{1/Sqrt[3],0,-(1/(2 Sqrt[6]))}];
a=N[{-(1/(2 Sqrt[3])),1/2,-(1/(2 Sqrt[6]))}];
d=N[{0,0,Sqrt[2/3]-1/(2 Sqrt[6])}];
(*******************************************************************)
(*******************************************************************)
(*Midpoints on Tetrehderon,for Subscript[S,1]*)
ab=midpoint[a,b];
ac=midpoint[a,c];
ad=midpoint[a,d];
bc=midpoint[b,c];
bd=midpoint[b,d];
cd=midpoint[c,d];
(*******************************************************************)
(*******************************************************************)
(*Documentation: foldedAngle*)
foldedAngle::usage="foldedAngle: Computes an stores the angle variable to be used in the folds.";
(*Function: foldedAngle*)
foldedAngle=N[Pi-ArcSec[3]];
(*******************************************************************)
(*******************************************************************)
(*Documentation: foldedAngle*)
tetrahedronF0original::usage="tetrahedronF0original: Unit Tetrahdron that unfolds to the first net, takes a list of thetas to unfold at various iterations.";
(*Function: tetrahedronF0original*)
tetrahedronF0original[theta_]:=Module[{angleToChange=theta[[1]]},{{a,b,c,d},{{{0,b-a,a}},{{angleToChange,b-a,b}},{{angleToChange,a-c,c}},{{angleToChange,c-b,c}}}}];
(*******************************************************************)
(*******************************************************************)
(*Documentation: tetrahedronF0new*)
tetrahedronF0new::usage="tetrahedronF0original: Unit tetrahedron that unfolds to the second net.";
(*Function: tetrahedronF0new*)
tetrahedronF0new[theta_]:=Module[{angleToChange=theta[[1]]},{{a,b,c,d},{{{0,b-a,a}},{{angleToChange,b-a,b}},{{angleToChange,d-a,a},{angleToChange,b-a,b}},{{angleToChange,c-b,b}}}}];
(*******************************************************************)
(*******************************************************************)
(*Documentation: computeChildren3D*)
computeChildren3D::usage="computeChildren3D: Takes a tetrahedron and splits it using the IFS, modified by taking into account the roatation method,  where the rotation method is dependent upon the iteration.
\[IndentingNewLine]rotationsBase: the roatation stack for the bottom/base/Gray side of the the current tetrahedron.
\[IndentingNewLine]rotations1: the roatation stack of the yellow side of the current tetreahedron.
\[IndentingNewLine]rotations2: the roatation stack of the red side of the current tetrahedron.
\[IndentingNewLine]rotation3: the roatation stack of the blue side of the current tetreahedron.";
(*Function: computeChildren3D*)
computeChildren3D[{{pBLeft_,pBRight_,pBUp_,pTop_},{rotationsBase_,rotations1_,rotations2_,rotations3_}},theta_,iteration_]:=Module[{pLRMid=midpoint[pBLeft,pBRight],pLUMid=midpoint[pBLeft,pBUp],pRUMid=midpoint[pBRight,pBUp],pLTMid=midpoint[pBLeft,pTop],pRTMid=midpoint[pBRight,pTop],pUTMid=midpoint[pBUp,pTop],angleToChange=-theta[[iteration+2]]},{{{pLTMid,pRTMid,pUTMid,pTop},{Prepend[rotations2,{-angleToChange,pUTMid-pLTMid,pLTMid}],rotations1,rotations2,rotations3}},(*Top Tetrahedron*){{pBLeft,pLRMid,pLUMid,pLTMid},{rotationsBase,rotations1,rotations2,Prepend[rotations1,{angleToChange,pLTMid-pLRMid,pLTMid}]}},(*Left Tetrahedron*){{pLRMid,pBRight,pRUMid,pRTMid},{rotationsBase,rotations1,Prepend[rotationsBase,{-angleToChange,pLTMid-pUTMid,pRUMid}],rotations3}},(*"Middle" Tetrahedron*){{pLUMid,pRUMid,pBUp,pUTMid},{rotationsBase,Prepend[rotations3,{-angleToChange,pUTMid-pRUMid,pUTMid}],rotations2,rotations3}} (*Right Tetrahedron*)}]
(*******************************************************************)
(*******************************************************************)
(*Documentation: recursivelyCreateChildren3D*)
recursivelyCreateChildren3D::usage="computeChildren3D: Takes an argument,iteration,maxIterations,start,theta.iteration:determines the iteration.maxIterations:determines when to stop,calls function till interation greater the maxIterations.Where theta is a list of angles to use for subrotations dependent upon the iteration";
(*Function: recursivelyCreateChildren3D*)
recursivelyCreateChildren3D[iteration_,maxIterations_,start_,theta_]:=Which[maxIterations==0,start,iteration>=maxIterations,start,True,recursivelyCreateChildren3D[iteration+1,maxIterations,Flatten[Map[computeChildren3D[#,theta,iteration]&,start],1],theta]]
(*******************************************************************)
(*******************************************************************)
(*Documentation: foldedSierpinski*)
foldedSierpinski::usage="computeChildren3D: set intial conditions for recursivelyCreateChildren3D.";
(*Function: foldedSierpinski*)
foldedSierpinski[iteration_,theta_,net_]:=
If[net,
If[iteration==0,{tetrahedronF0new[theta]},
recursivelyCreateChildren3D[0,iteration,{tetrahedronF0new[theta]},theta]],
If[iteration==0,{tetrahedronF0original[theta]},
recursivelyCreateChildren3D[0,iteration,{tetrahedronF0original[theta]},theta]]]
(*******************************************************************)
(*******************************************************************)
(*Documentation: FoldingModule1*)
FoldingModule1::usage="FoldingModule1: Folding module with sub-folding capablites.";
(*Function: FoldingModule1*)
FoldingModule1=DynamicModule[{n=0,theta=Table[foldedAngle,{i,5}],net=False},Column[{Checkbox[Dynamic[net]],
SetterBar[Dynamic[n],{0,1,2,3}],Dynamic[Column[Table[With[{i=i},Slider[Dynamic[theta[[i]]],{foldedAngle,0}]],{i,n+1}]]],
Dynamic[Show[showFolded3D[foldedSierpinski[n,theta,net]],Axes->True,AxesOrigin->{0,0,-(1/(2 Sqrt[6]))},AxesStyle->Directive[Dotted,Italic,Black],Ticks->{{-1,1},{-1,1},{-1,1}},AxesLabel->{"x","y","z"},PlotRange->1.5,FaceGrids->{{0,-1,0},{-1,0,0},{0,0,-1}},FaceGridsStyle->Directive[Thickness[.001],LightGray,Dotted],Boxed->False,Axes->True,AxesStyle->Thickness[.001],Background->White,Lighting->"Neutral"]]}]];
(*******************************************************************)
(*******************************************************************)
(*Documentation: FoldingModule2*)
FoldingModule2::usage="FoldingModule2: Folding module that folds all iterations.";
(*Function: FoldingModule2*)
FoldingModule2=DynamicModule[{n=0,alphatheta=0,net=False},Column[{Checkbox[Dynamic[net]],SetterBar[Dynamic[n],{0,1,2,3}],Slider[Dynamic[alphatheta],{0,foldedAngle}],
Dynamic[Show[showFolded3D[foldedSierpinski[n,Table[foldedAngle-alphatheta,{i,5}],net]],Axes->True,AxesOrigin->{0,0,-(1/(2 Sqrt[6]))},AxesStyle->Directive[Dotted,Italic,Black],Ticks->{{-1,1},{-1,1},{-1,1}},AxesLabel->{"x","y","z"},PlotRange->1.5,FaceGrids->{{0,-1,0},{-1,0,0},{0,0,-1}},FaceGridsStyle->Directive[Thickness[.001],LightGray,Dotted],Boxed->False,Axes->True,AxesStyle->Thickness[.001],Background->Black,Lighting->"Neutral"]]}]];
(*******************************************************************)
(*******************************************************************)
(*End: 3D Unfolding Code*)(*******************************************************************)


(*Vertex Coordinates for Net 1*)
net1ptsS0={(*Perimeter Vertices Net1*)
(*Subscript[d, 1]*){-2/Sqrt[3],0,-1/(2Sqrt[6])},
(*Subscript[d, 2]*){1/Sqrt[3],-1,-1/(2Sqrt[6])},
(*Subscript[d, 3]*){1/Sqrt[3],1,-1/(2Sqrt[6])},
(*Midpoint Vertices Subscript[S, 0]*)
(*a=PBleft*){-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},
(*b=PBup*){-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},
(*c=PBright*){1/Sqrt[3],0,-1/(2Sqrt[6])}};

net1ptsS1={(*Perimeter Vertices Net1*)
(*Subscript[d, 1]*){-2/Sqrt[3],0,-1/(2Sqrt[6])},
(*Subscript[d, 2]*){1/Sqrt[3],-1,-1/(2Sqrt[6])},
(*Subscript[d, 3]*){1/Sqrt[3],1,-1/(2Sqrt[6])},
(*Midpoint Vertices Subscript[S, 0]*)
(*a=PBleft*){-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},
(*b=PBup*){-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},
(*c=PBright*){1/Sqrt[3],0,-1/(2Sqrt[6])}};


(*Vertex Coordinates for Net 2*)
net2pts={(*Perimeter Vertices Net2*)
(*1*){-2/Sqrt[3],0,-1/(2Sqrt[6])},
(*2*){1/Sqrt[3],-1,-1/(2Sqrt[6])},
(*3*){-2/Sqrt[3],1,-1/(2Sqrt[6])},
(*Midpoint Vertices Subscript[S, 0]*)
(*4*){-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},
(*5*){-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},
(*6*){1/Sqrt[3],0,-1/(2Sqrt[6])}};
pts3D={{0,0,Sqrt[2/3]-1/(2Sqrt[6])},
(*4*){-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},
(*5*){-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},
(*6*){1/Sqrt[3],0,-1/(2Sqrt[6])}};
(*Labels*)
labelsNetOneS0={"\!\(\*SubscriptBox[\(x\), \(00\)]\)","\!\(\*SubscriptBox[\(x\), \(01\)]\)","\!\(\*SubscriptBox[\(x\), \(02\)]\)","\!\(\*SubscriptBox[\(x\), \(03\)]\)","\!\(\*SubscriptBox[\(x\), \(04\)]\)","\!\(\*SubscriptBox[\(x\), \(05\)]\)"};
labelsNetTwoS0={"\!\(\*SubscriptBox[\(x\), \(0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 3\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 2\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 1\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 3\)]\)"};
(*Labels*)
labelsNetOneS1={Text[Style["\!\(\*SubscriptBox[\(u\), \(0\)]\)",Large,Bold]],Text[Style["\!\(\*SubscriptBox[\(u\), \(1\)]\)",Large,Bold]],Text[Style["\!\(\*SubscriptBox[\(u\), \(2\)]\)",Large,Bold]],"\!\(\*SubscriptBox[\(x\), \(01\)]\)","\!\(\*SubscriptBox[\(x\), \(02\)]\)","\!\(\*SubscriptBox[\(x\), \(12\)]\)","\!\(\*SubscriptBox[\(u\), \(0\)]\)","\!\(\*SubscriptBox[\(u\), \(0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 2\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 1\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 3\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 0\)]\)","\!\(\*SubscriptBox[\(x\), \(0, 2\)]\)"};
(*Labels*)
labels3D={"\!\(\*SubscriptBox[\(u\), \(3\)]\)","\!\(\*SubscriptBox[\(u\), \(1\)]\)","\!\(\*SubscriptBox[\(u\), \(2\)]\)","\!\(\*SubscriptBox[\(u\), \(0\)]\)"};
(*Labels*)
offsets2DNetOne=-{{-1,1},{-1,-1},{1,1},{-1,-1},{-1,1},{1,-1}};
offsets2DNetTwo=-{{-1,-1},{-1,-1},{1,1},{-1,-1},{1,1},{1,1}};
offsets2DNetOneS1=-{{-1,-1},{-1,-1},{1,1},{-1,-1},{1,1},{1,1},{-1,-1},{-1,-1},{1,1},{-1,-1},{1,1},{1,1},{-1,-1},{-1,-1},{1,1}};
offsets3D={{-1,-1},{1,1},{1,-1},{-1,-1}};
(*Labeled Vertex Coordinates for Net 1, Subscript[S, 0]*)
labeledNet1PtsS0=Graphics3D[{PointSize[Large],Point@net1ptsS0,
MapIndexed[Style[Text[First[labelsNetOneS0[[#2]]],#1,First[offsets2DNetOne[[#2]]]],Black,20]&,net1ptsS0]},Boxed->False,Axes->False];
(*Labeled Vertex Coordinates for Net 1, Subscript[S, 1]*)
labeledNet1PtsS1=Graphics3D[{PointSize[Large],Point@net1ptsS0,
MapIndexed[Style[Text[First[labelsNetOneS1[[#2]]],#1,First[offsets2DNetOneS1[[#2]]]],Black,20]&,net1ptsS1]},Boxed->False,Axes->False];
(*Labeled Vertex Coordinates for Net 2*)
labeledNet2pts=Graphics3D[{PointSize[Large],Point@net2pts,
MapIndexed[Style[Text[First[labelsNetTwoS0[[#2]]],#1,First[offsets2DNetTwo[[#2]]]],Black,20]&,net2pts]},Boxed->False,Axes->False];
labeled3DPts=Graphics3D[{PointSize[Large],Point@pts3D,
MapIndexed[Style[Text[First[labels3D[[#2]]],#1,First[offsets3D[[#2]]]],Black,20]&,pts3D]},Boxed->False,Axes->False];

fractalNetModule3D=DynamicModule[{alphatheta=0},Column[{Panel[Grid[{{"n",SetterBar[Dynamic[n],{0,1,2,3}]},{"net",Checkbox[Dynamic[net]]},{"Fold",Slider[Dynamic[alphatheta],{0,foldedAngle}]},{" "}},Frame->All,Spacings->{0,1},Alignment->Left],ImageSize-> {600,125},Background->White],
Dynamic[Show[If[alphatheta>foldedAngle-10^-6,labeled3DPts,If[net,labeledNet2pts,labeledNet1PtsS1]],
showFolded3D[foldedSierpinski[n,Table[foldedAngle-alphatheta,{i,5}],net]],generateSierpinskiNet[n+1,net],
Boxed->False,ImageSize->{600,600},ViewPoint->{0.18,-1,0.542},ViewAngle->0.69115,PlotRange->1.5,Background->White,Lighting->"Neutral",
Axes->True,AxesOrigin->{0,0,0},AxesStyle->Directive[Dotted,Italic,Black,Thin]]]}]];

End[ ]
EndPackage[ ]
