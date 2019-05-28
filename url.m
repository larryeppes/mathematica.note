(* ::Package:: *)

(*\:4eceurl\:4e2d\:7684\:6e90\:7801\:4e2d\:63d0\:53d6\:6240\:6709\:4ee5host\:5f00\:5934\:7684url\:5217\:8868*)
FetchURLList[url_,host_]:=Select[Import[url,"Hyperlinks"],StringStartsQ[#,host]&];



