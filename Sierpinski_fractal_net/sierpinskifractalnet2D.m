(* ::Package:: *)

BeginPackage["sierpinskifractalnet2D`"]

fractalNetModule2D::usage="fractalNetModule2D: Displays 2D fractal nets."
recursivelyCreateChildren2D::usage="recursivelyCreateChildren2D: Takes the input iteration, to determine the starting iteration. Takes the input maxIterations to determine the iteration you want to call. Takes the input start to determine the starting point. Then uses this information to determine how many times to run the computeChildren for finding the midpoints to determine the sub-triangles."
F1NetOne::usage="F1: is the first iteration of the fractal. Needed for the recursivelyCreateChildren function."
F1NetTwo::usage="F1: is the first iteration of the fractal. Needed for the recursivelyCreateChildren function."
showFolded2D::usage="showFolded2D: Places a unit triangle wrapper around the around the points and line, beacuse the above method  draws everything as dotted lines.";
generateSierpinskiNet::usage="generateSierpinskiNet: Sets intial conditions for recursivelyCreateChildren2D to be used in manipulate."

Begin["`Private`"]
(*******************************************************************)
(*Begin: 3D Unfolding Code*)(*******************************************************************)(*******************************************************************)
(*Documentation: colorFunction*)colorFunction::usage="colorFunction: Uses a switch statement to switch between colors for a given iterantion.";
(*Function: colorFunction*)
colorFunction[n_]:=Switch[n,1,Red,2,Red,3,Red,4,Red,5,Red,6,Red,7,Red,8,Red]
(*******************************************************************)(*******************************************************************)
(*Documentation: cutColor*)cutColor::usage="cutColor: Creates the color for the cuts using a If stament to detrmine the iteration and wether or not it's fold or cut.";
(*Function: cutColor*)
cutColor[{cut_,iteration_}]:=If[cut,colorFunction[iteration],Directive[Thin,Black,Dotted]]
(*******************************************************************)(*******************************************************************)
(*Documentation: drawFolded2D*)drawFolded2D::usage="drawFolded2D: CDraws the points and the lines using three points(p) and three lines(c) (this is where it draws 2 sets of lines).";
(*Function: drawFolded2D*)
drawCutLine[{point1_, point2_, color_}]:=Graphics3D@{Thick, cutColor[color],Line@{point1,point2}}
drawFolded2D[folded_]:=Map[drawCutLine,MapThread[List,{RotateLeft[folded[[1]]],folded[[1]], Reverse[folded[[2]]]}]]
(*******************************************************************)
(*******************************************************************)
(*Documentation: F0TriangleNetOne*)
(*In the 2D code we use only unit Triangle, In 3D code the unit traingle is used as the Base points of the unit Tetrahedron.*)F0TriangleNetOne::usage="F0TriangleNetOne: is the unit triangle's coordinate, for the first case of the net. Needed for the computeChildren function.";
F0TriangleNetOne={{{-2/Sqrt[3],0,-1/(2Sqrt[6])},{1/Sqrt[3],-1,-1/(2Sqrt[6])},{1/Sqrt[3],1,-1/(2Sqrt[6])}},{{False,0},{False,0},{False,0}}};
F0TriangleNetTwo={{{-2/Sqrt[3],0,-1/(2Sqrt[6])},{1/Sqrt[3],-1,-1/(2Sqrt[6])},{1/Sqrt[3],0,-1/(2Sqrt[6])},{-2/Sqrt[3],1,-1/(2Sqrt[6])}},{{False,0},{False,0},{False,0},{False,0}}};
(*******************************************************************)
(*******************************************************************)
(*Documentation: cutColor*)
(*Function: showFolded2D*)
showFolded2D[folded_,net_]:=Show[Flatten[Map[drawFolded2D,folded]],
If[net,Graphics3D@{Thickness[Medium],Black,Line@Append[F0TriangleNetTwo[[1]],F0TriangleNetTwo[[1]][[1]]]},Graphics3D@{Thickness[Medium],Black,Line@Append[F0TriangleNetOne[[1]],F0TriangleNetOne[[1]][[1]]]}], Boxed->False]

(*******************************************************************)(*******************************************************************)
(*Documentation: F1*)
(*Function: F1*)
F1NetOne={{{{-(2/Sqrt[3]),0,-(1/(2 Sqrt[6]))},{-(1/(2 Sqrt[3])),-(1/2),-(1/(2 Sqrt[6]))},{-(1/(2 Sqrt[3])),1/2,-(1/(2 Sqrt[6]))}},{{False,1},{False,1},{False,1}}},{{{-(1/(2 Sqrt[3])),-(1/2),-(1/(2 Sqrt[6]))},{1/Sqrt[3],-1,-(1/(2 Sqrt[6]))},{1/Sqrt[3],0,-(1/(2 Sqrt[6]))}},{{False,1},{False,1},{False,1}}},{{{-(1/(2 Sqrt[3])),1/2,-(1/(2 Sqrt[6]))},{1/Sqrt[3],0,-(1/(2 Sqrt[6]))},{1/Sqrt[3],1,-(1/(2 Sqrt[6]))}},{{False,1},{False,1},{False,1}}},{{{1/Sqrt[3],0,-(1/(2 Sqrt[6]))},{-(1/(2 Sqrt[3])),1/2,-(1/(2 Sqrt[6]))},{-(1/(2 Sqrt[3])),-(1/2),-(1/(2 Sqrt[6]))}},{{False,1},{False,1},{False,1}}}};
(*******************************************************************)
(*******************************************************************)
(*Documentation: F1*)
(*Function: F1*)
F1NetTwo={{{{-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},{-2/Sqrt[3],1,-1/(2Sqrt[6])},{-2/Sqrt[3],0,-1/(2Sqrt[6])}},{{False,1},{False,1},{False,1}}},
{{{-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},{-2/Sqrt[3],0,-1/(2Sqrt[6])},{-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])}},{{False,1},{False,1},{False,1}}},
{{{1/Sqrt[3],0,-1/(2Sqrt[6])},{-1/(2Sqrt[3]),1/2,-1/(2Sqrt[6])},{-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])}},{{False,1},{False,1},{False,1}}},
{{{1/Sqrt[3],-1,-1/(2Sqrt[6])},{-1/(2Sqrt[3]),-1/2,-1/(2Sqrt[6])},{1/Sqrt[3],0,-1/(2Sqrt[6])}},{{False,1},{False,1},{False,1}}}};

(*******************************************************************)(*******************************************************************)
(*Documentation: computeChildren2D*)computeChildren2D::usage="computeChildren2D: Computes or caculates the midpoints to find the sub triangles or children by taking the points and the lines using three points(p) and three lines(c), divieds by 1/2 to find the midpoints and determines which midpoints to iterate using the True function and the define piramative iteratation.";
(*Function: computeChildren2D*)
computeChildren2D[{{pLeft_,pRight_,pUp_},{cLeft_,cRight_,cDown_}},iteration_]:=Module[{pDMid=(pLeft+pRight)/2,pLMid=(pLeft+pUp)/2,pRMid=(pRight+pUp)/2},{{{pLeft,pDMid,pLMid},{cLeft,{True,iteration},cDown}},
{{pDMid,pRight,pRMid},{{True,iteration},cRight,cDown}},
{{pLMid,pRMid,pUp},{cLeft,cRight,cDown}},
{{pRMid,pLMid,pDMid},{{True,iteration},{True,iteration},{False,iteration}}}}]
(*******************************************************************)(*******************************************************************)
(*Documentation: recursivelyCreateChildren2D*)
(*Function: recursivelyCreateChildren2D*)
recursivelyCreateChildren2D[iteration_,maxIterations_,start_]:=Which[(*maxIterations\[Equal]0,F0Triangle,*)
maxIterations==1,start,
iteration >=  maxIterations,start,
True,recursivelyCreateChildren2D[iteration+1,maxIterations,Flatten[Map[computeChildren2D[#,iteration]&,start],1]]]
(*******************************************************************)(*******************************************************************)
(*Documentation: generateSierpinskiNet*)generateSierpinskiNet::usage="generateSierpinskiNet: Sets intial conditions for recursivelyCreateChildren2D to be used in manipulate.";
(*Function: generateSierpinskiNet*)
generateSierpinskiNet[iteration_,net_]:=If[iteration==0,If[net,showFolded2D[{F0TriangleNetTwo},net],showFolded2D[{F0TriangleNetOne},net]],
                                                       If[net,showFolded2D[recursivelyCreateChildren2D[1,iteration,F1NetTwo],net],
                                                              showFolded2D[recursivelyCreateChildren2D[1,iteration,F1NetOne],net]]]
(*******************************************************************)(*******************************************************************)
(*Documentation: fractalNetModule2D*)
(*Function: generateSierpinskiNet*)
fractalNetModule2D=Manipulate[generateSierpinskiNet[n,net],{{n,0,"n"},{0,1,2,3,4}},{{net,False,"net"},{False, True}}];
End[ ]
EndPackage[ ]
