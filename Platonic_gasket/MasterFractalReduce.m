(* ::Package:: *)

BeginPackage["MasterFractalReduce`Private`"]
TetrahedronFractalReduce::usage = "TetrahedronFractalReduce[n] gives the n iteration of the Seirpenski Gasket."
HexahedronFractalReduce::usage = "HexahedronFractalReduce[nhexa] gives the ..."
OctahedronFractalReduce::usage = "OcathedronFractalReduce[nocta] gives the nth iteration of the Sierpinski Cheese."
IcosahedronFractalReduce::usage = "IcosahedronFractalReduce[nicosa] gives the ..."
DodecahedronFractalReduce::usage = "DodecahedronFractalReduce[ndodeca] gives the nth ..."

(**Tetrahedron Code**)
t1=PolyhedronData["Tetrahedron","VertexCoordinates"];
tetvec[{x_,y_,z_},{coordsx_,coordsy_,coordsz_}]:=({x,y,z}-{coordsx,coordsy,coordsz})/2;
Thread[tetvec[{x,y,z},t1],List,-1];
grosslogicalexpressiontet=PolyhedronData["Tetrahedron","RegionFunction"][x,y,z] &&Not[Apply[Or,Map[Apply[PolyhedronData["Tetrahedron","RegionFunction"],#]&,Thread[tetvec[{x,y,z},t1],List,-1]]]];
lessgrosslogicalexpressiontet[x_,y_,z_]=FullSimplify[LogicalExpand[Resolve[grosslogicalexpressiontet]]];
Clear[TetrahedronFractalDrilling];
TetrahedronFractalDrilling[0,distance_,{xcoords_,ycoords_,zcoords_}]=False;
TetrahedronFractalDrilling[1,distance_,{xcoords_,ycoords_,zcoords_}]=lessgrosslogicalexpressiontet[(x-xcoords)/distance,(y-ycoords)/distance,(z-zcoords)/distance];
TetrahedronFractalDrilling[n_,distance_,{xcoords_,ycoords_,zcoords_}]:=TetrahedronFractalDrilling[n,distance,{xcoords,ycoords,zcoords}]=TetrahedronFractalDrilling[n-1,distance,{xcoords,ycoords,zcoords}]||TetrahedronFractalDrilling[n-1,distance/2,{xcoords,ycoords,zcoords}+t1[[1]]*distance/2]||TetrahedronFractalDrilling[n-1,distance/2,{xcoords,ycoords,zcoords}+t1[[2]]*distance/2]||TetrahedronFractalDrilling[n-1,distance/2,{xcoords,ycoords,zcoords}+t1[[3]]*distance/2]||TetrahedronFractalDrilling[n-1,distance/2,{xcoords,ycoords,zcoords}+t1[[4]]*distance/2];
Clear[TetrahedronFractalReduce]
TetrahedronFractalReduce[0]=PolyhedronData["Tetrahedron","RegionFunction"][x,y,z];
TetrahedronFractalReduce[n_]:=TetrahedronFractalReduce[n]=PolyhedronData["Tetrahedron","RegionFunction"][x,y,z] - TetrahedronFractalDrilling[n,1,{0,0,0}];

(**Hexahedron Code**)
h1=Select[Join[Join[Join[PolyhedronData["Cube","VertexCoordinates"],Map[#+{0,0,1/2}&,PolyhedronData["Cube","VertexCoordinates"]]],Map[#+{0,1/2,0}&,PolyhedronData["Cube","VertexCoordinates"]]],Map[#+{1/2,0,0}&,PolyhedronData["Cube","VertexCoordinates"]]],Apply[PolyhedronData["Cube","RegionFunction"],#]&];
hexvec[{xhexa_,yhexa_,zhexa_},{coordsxhexa_,coordsyhexa_,coordszhexa_}]:=3*({xhexa,yhexa,zhexa}-(1-1/3)*{coordsxhexa,coordsyhexa,coordszhexa});
grosslogicalexpressionhex=PolyhedronData["Cube","RegionFunction"][xhexa,yhexa,zhexa] &&Not[Apply[Or,Map[Apply[PolyhedronData["Cube","RegionFunction"],#]&,Thread[hexvec[{xhexa,yhexa,zhexa},h1],List,-1]]]];
lessgrosslogicalexpressionhex[xhexa_,yhexa_,zhexa_]=FullSimplify[grosslogicalexpressionhex];
Clear[HexahedronFractalDrilling];
HexahedronFractalDrilling[0,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]=False;
HexahedronFractalDrilling[1,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]=lessgrosslogicalexpressionhex[(xhexa-xcoordshexa)/distancehexa,(yhexa-ycoordshexa)/distancehexa,(zhexa-zcoordshexa)/distancehexa];
HexahedronFractalDrilling[nhexa_,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]:=HexahedronFractalDrilling[nhexa,distancehexa,{xcoordshexa,ycoordshexa,zcoordshexa}]=HexahedronFractalDrilling[nhexa-1,distancehexa,{xcoordshexa,ycoordshexa,zcoordshexa}] ||Apply[Or,Map[HexahedronFractalDrilling[nhexa-1,distancehexa*1/3,#]&,Map[distancehexa*(1-1/3)*#+{xcoordshexa,ycoordshexa,zcoordshexa}&,h1]]];
Clear[HexahedronFractalReduce]
HexahedronFractalReduce[0]=PolyhedronData["Cube","RegionFunction"][xhexa,yhexa,zhexa];
HexahedronFractalReduce[nhexa_]:=HexahedronFractalReduce[nhexa]=PolyhedronData["Cube","RegionFunction"][xhexa,yhexa,zhexa] &&Not[HexahedronFractalDrilling[nhexa,1,{0,0,0}]];
Clear[HexahedronComplimentDrilling];
HexahedronComplimentDrilling[0,depthhexa_,mindepthhexa_,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]=False;
HexahedronComplimentDrilling[1,depthhexa_,mindepthhexa_,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]=If[depthhexa+1 >= mindepthhexa,lessgrosslogicalexpressionhex[(xhexa-xcoordshexa)/distancehexa,(yhexa-ycoordshexa)/distancehexa,(zhexa-zcoordshexa)/distancehexa],False];
HexahedronComplimentDrilling[nhexa_,depthhexa_,mindepthhexa_,distancehexa_,{xcoordshexa_,ycoordshexa_,zcoordshexa_}]:=HexahedronComplimentDrilling[nhexa,depthhexa,mindepthhexa,distancehexa,{xcoordshexa,ycoordshexa,zcoordshexa}]=HexahedronComplimentDrilling[nhexa-1,depthhexa,mindepthhexa,distancehexa,{xcoordshexa,ycoordshexa,zcoordshexa}] ||Apply[Or,Map[HexahedronComplimentDrilling[nhexa-1,depthhexa+1,mindepthhexa,distancehexa*1/3,#]&,Map[distancehexa*(1-1/3)*#+{xcoordshexa,ycoordshexa,zcoordshexa}&,h1]]];

(**Octahedron Code**)
o1=PolyhedronData["Octahedron","VertexCoordinates"];
octvec[{octax_,octay_,octaz_},{coordsxocta_,coordsyocta_,coordszocta_}]:=2*{octax,octay,octaz}-{coordsxocta,coordsyocta,coordszocta};
Thread[octvec[{octax,octay,octaz},o1],List,-1];
grosslogicalexpressionoct=PolyhedronData["Octahedron","RegionFunction"][octax,octay,octaz] &&Not[Apply[Or,Map[Apply[PolyhedronData["Octahedron","RegionFunction"],#]&,Thread[octvec[{octax,octay,octaz},o1],List,-1]]]];
lessgrosslogicalexpressionoct[octax_,octay_,octaz_]=FullSimplify[Resolve[grosslogicalexpressionoct]];
Clear[OctahedronFractalDrilling];
OctahedronFractalDrilling[0,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]=False;
OctahedronFractalDrilling[1,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]=lessgrosslogicalexpressionoct[(octax-xcoordsocta)/distanceocta,(octay-ycoordsocta)/distanceocta,(octaz-zcoordsocta)/distanceocta];
OctahedronFractalDrilling[nocta_,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]:=OctahedronFractalDrilling[nocta,distanceocta,{xcoordsocta,ycoordsocta,zcoordsocta}]=OctahedronFractalDrilling[nocta-1,distanceocta,{xcoordsocta,ycoordsocta,zcoordsocta}]||OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[1]]*distanceocta/2]||OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[2]]*distanceocta/2]||OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[3]]*distanceocta/2]||OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[4]]*distanceocta/2]||
OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[5]]*distanceocta/2]||OctahedronFractalDrilling[nocta-1,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[6]]*distanceocta/2];
Clear[OctahedronFractalReduce]
OctahedronFractalReduce[0]=PolyhedronData["Octahedron","RegionFunction"][octax,octay,octaz];
OctahedronFractalReduce[nocta_]:=OctahedronFractalReduce[nocta]=PolyhedronData["Octahedron","RegionFunction"][octax,octay,octaz] &&Not[OctahedronFractalDrilling[nocta,1,{0,0,0}]];
Clear[OctahedronComplimentDrilling];
OctahedronComplimentDrilling[0,depthocta_,mindepthocta_,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]=False;
OctahedronComplimentDrilling[1,depthocta_,mindepthocta_,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]=If[depthocta+1 >= mindepthocta,lessgrosslogicalexpressionoct[(octax-xcoordsocta)/distanceocta,(octay-ycoordsocta)/distanceocta,(octaz-zcoordsocta)/distanceocta],False];
OctahedronComplimentDrilling[nocta_,depthocta_,mindepthocta_,distanceocta_,{xcoordsocta_,ycoordsocta_,zcoordsocta_}]:=OctahedronComplimentDrilling[nocta,depthocta,mindepthocta,distanceocta,{xcoordsocta,ycoordsocta,zcoordsocta}]=OctahedronComplimentDrilling[nocta-1,depthocta,mindepthocta,distanceocta,{xcoordsocta,ycoordsocta,zcoordsocta}]||OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[1]]*distanceocta/2]||OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[2]]*distanceocta/2]||OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[3]]*distanceocta/2]||OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[4]]*distanceocta/2]||
OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[5]]*distanceocta/2]||OctahedronComplimentDrilling[nocta-1,depthocta+1,mindepthocta,distanceocta/2,{xcoordsocta,ycoordsocta,zcoordsocta}+o1[[6]]*distanceocta/2];

(**Icosahedron Code**)
grosslogicalexpressionicosa=PolyhedronData["Icosahedron","RegionFunction"][xicosa,yicosa,zicosa] &&Not[Apply[Or,Map[Apply[PolyhedronData["Icosahedron","RegionFunction"],#]&,Thread[icosavec[{xicosa,yicosa,zicosa},i1],List,-1]]]];
i1=PolyhedronData["Icosahedron","VertexCoordinates"];
icosavec[{xicosa_,yicosa_,zicosa_},{coordsxicosa_,coordsyicosa_,coordszicosa_}]:=(1.+GoldenRatio)*{xicosa,yicosa,zicosa}-{coordsxicosa,coordsyicosa,coordszicosa}*(1.+GoldenRatio)/GoldenRatio;
Thread[icosavec[{xicosa,yicosa,zicosa},i1],List,-1];
lessgrosslogicalexpressionicosa[xicosa_,yicosa_,zicosa_]=grosslogicalexpressionicosa;
Clear[IcosahedronFractalDrilling];
IcosahedronFractalDrilling[0,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=False;
IcosahedronFractalDrilling[1,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=lessgrosslogicalexpressionicosa[(xicosa-xcoordsicosa)/distanceicosa,(yicosa-ycoordsicosa)/distanceicosa,(zicosa-zcoordsicosa)/distanceicosa];
IcosahedronFractalDrilling[nicosa_,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]:=IcosahedronFractalDrilling[nicosa,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]=IcosahedronFractalDrilling[nicosa-1,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[1]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[2]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[3]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[4]]*distanceicosa/2]||
IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[5]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[6]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[7]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[8]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[9]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[10]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[11]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[12]]*distanceicosa/2];
Clear[IcosahedronFractalDrilling];
IcosahedronFractalDrilling[0,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=False;
IcosahedronFractalDrilling[1,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=lessgrosslogicalexpressionicosa[(xicosa-xcoordsicosa)/distanceicosa,(yicosa-ycoordsicosa)/distanceicosa,(zicosa-zcoordsicosa)/distanceicosa];
IcosahedronFractalDrilling[nicosa_,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]:=IcosahedronFractalDrilling[nicosa,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]=IcosahedronFractalDrilling[nicosa-1,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[1]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[2]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[3]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[4]]*distanceicosa/2]||
IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[5]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[6]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[7]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[8]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[9]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[10]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[11]]*distanceicosa/2]||IcosahedronFractalDrilling[nicosa-1,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[12]]*distanceicosa/2];
Clear[IcosahedronFractalReduce]
IcosahedronFractalReduce[0]=PolyhedronData["Icosahedron","RegionFunction"][xicosa,yicosa,zicosa] ;
IcosahedronFractalReduce[nicosa_]:=IcosahedronFractalReduce[nicosa]=PolyhedronData["Icosahedron","RegionFunction"][xicosa,yicosa,zicosa] &&Not[IcosahedronFractalDrilling[nicosa,1,{0,0,0}]];
Clear[IcosahedronComplimentDrilling];
IcosahedronComplimentDrilling[0,depthicosa_,mindepthicosa_,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=False;
IcosahedronComplimentDrilling[1,depthicosa_,mindepthicosa_,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]=If[depthicosa+1 >= mindepthicosa,lessgrosslogicalexpressionicosa[(xicosa-xcoordsicosa)/distanceicosa,(yicosa-ycoordsicosa)/distanceicosa,(zicosa-zcoordsicosa)/distanceicosa],False];
IcosahedronComplimentDrilling[nicosa_,depthicosa_,mindepthicosa_,distanceicosa_,{xcoordsicosa_,ycoordsicosa_,zcoordsicosa_}]:=IcosahedronComplimentDrilling[nicosa,depthicosa,mindepthicosa,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]=IcosahedronComplimentDrilling[nicosa-1,depthicosa,mindepthicosa,distanceicosa,{xcoordsicosa,ycoordsicosa,zcoordsicosa}]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[1]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[2]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[3]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[4]]*distanceicosa/2]||
IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[5]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[6]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[7]]*distanceicosa/2]||
IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[8]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[9]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[10]]*distanceicosa/2]||IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[11]]*distanceicosa/2]||
IcosahedronComplimentDrilling[nicosa-1,depthicosa+1,mindepthicosa,distanceicosa/2,{xcoordsicosa,ycoordsicosa,zcoordsicosa}+i1[[12]]*distanceicosa/2];

(**Dodecahedron Code**)
d1=PolyhedronData["Dodecahedron","VertexCoordinates"];
dodecavec[{xdodeca_,ydodeca_,zdodeca_},{coordsxdodeca_,coordsydodeca_,coordszdodeca_}]:=(2.+GoldenRatio)*{xdodeca,ydodeca,zdodeca}-{coordsxdodeca,coordsydodeca,coordszdodeca}*(2.+GoldenRatio)/1.4;
Thread[dodecavec[{xdodeca,ydodeca,zdodeca},d1],List,-1];
grosslogicalexpressiondodeca=PolyhedronData["Dodecahedron","RegionFunction"][xdodeca,ydodeca,zdodeca] &&Not[Apply[Or,Map[Apply[PolyhedronData["Dodecahedron","RegionFunction"],#]&,Thread[dodecavec[{xdodeca,ydodeca,zdodeca},d1],List,-1]]]];
lessgrosslogicalexpressiondodeca[xdodeca_,ydodeca_,zdodeca_]=grosslogicalexpressiondodeca;
Clear[DodecahedronFractalDrilling];
DodecahedronFractalDrilling[0,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]=False;
DodecahedronFractalDrilling[1,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]=lessgrosslogicalexpressiondodeca[(xdodeca-xcoordsdodeca)/distancedodeca,(ydodeca-ycoordsdodeca)/distancedodeca,(zdodeca-zcoordsdodeca)/distancedodeca];
DodecahedronFractalDrilling[ndodeca_,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]:=DodecahedronFractalDrilling[ndodeca,distancedodeca,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}]=DodecahedronFractalDrilling[ndodeca-1,distancedodeca,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[1]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[2]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[3]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[4]]*distancedodeca/2]||
DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[5]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[6]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[7]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[8]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[9]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[10]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[11]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[12]]*distancedodeca/2]||
DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[13]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[14]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[15]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[16]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[17]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[18]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[19]]*distancedodeca/2]||DodecahedronFractalDrilling[ndodeca-1,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[20]]*distancedodeca/2];
Clear[DodecahedronFractalReduce]
DodecahedronFractalReduce[0]=PolyhedronData["Dodecahedron","RegionFunction"][xdodeca,ydodeca,zdodeca] ;
DodecahedronFractalReduce[ndodeca_]:=DodecahedronFractalReduce[ndodeca]=PolyhedronData["Dodecahedron","RegionFunction"][xdodeca,ydodeca,zdodeca] &&Not[DodecahedronFractalDrilling[ndodeca,1,{0,0,0}]];
Clear[DodecahedronComplimentDrilling];
DodecahedronComplimentDrilling[0,depthdodeca_,mindepthdodeca_,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]=False;
DodecahedronComplimentDrilling[1,depthdodeca_,mindepthdodeca_,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]=If[depthdodeca+1 >= mindepthdodeca,lessgrosslogicalexpressiondodeca[(xdodeca-xcoordsdodeca)/distancedodeca,(ydodeca-ycoordsdodeca)/distancedodeca,(zdodeca-zcoordsdodeca)/distancedodeca],False];
DodecahedronComplimentDrilling[ndodeca_,depthdodeca_,mindepthdodeca_,distancedodeca_,{xcoordsdodeca_,ycoordsdodeca_,zcoordsdodeca_}]:=DodecahedronComplimentDrilling[ndodeca,depthdodeca,mindepthdodeca,distancedodeca,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}]=DodecahedronComplimentDrilling[ndodeca-1,depthdodeca,mindepthdodeca,distancedodeca,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[1]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[2]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[3]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[4]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[5]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[6]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[7]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[8]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[9]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[10]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[11]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[12]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[13]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[14]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[15]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[16]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[17]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[18]]*distancedodeca/2]||DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[19]]*distancedodeca/2]||
DodecahedronComplimentDrilling[ndodeca-1,depthdodeca+1,mindepthdodeca,distancedodeca/2,{xcoordsdodeca,ycoordsdodeca,zcoordsdodeca}+d1[[20]]*distancedodeca/2];

EndPackage[];






