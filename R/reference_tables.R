
rock.table <- read.table(textConnection(
  'id name
  1 "sedimentary"
  2 "volcaniclastic"
  3 "covered"
  '), header=TRUE, stringsAsFactors=FALSE
)

litho.table <- read.table(textConnection(
  'id name def_col
  1 "claystone" "mistyrose1"
  2 "mudstone" "snow3"
  3 "shale" "gray40"
  4 "siltstone" "grey90" 
  5 "sandstone" "yellow1"
  6 "conglomerate" "gold1"
  7 "breccia" "goldenrod1"
  8 "limestone" "skyblue1"
  9 "dolostone" "deepskyblue2"
  10 "marl" "cadetblue4"
  11 "chalk" "lightblue3"
  12 "diatomite" "beige"
  13 "coal" "black"
  14 "gypsum" "lightyellow2"
  15 "halite" "paleturquoise1"
  16 "chert" "khaki3"
  17 "glauconite" "darkolivegreen3"
  18 "limonite" "lavenderblush3"
  19 "siderite" "indianred1"
  20 "phosphorite" "sandybrown"
  21 "tuff" "lightcoral"
  22 "lapillistone" "mistyrose2"
  23 "agglomerate" "pink4"
  24 "bentonite" "moccasin"
  '), header=TRUE, stringsAsFactors=FALSE
)

gs.table <- read.table(textConnection(
  'id name xAxis
1 "clay" 0.19
2 "clay / silt" 0.26
3 "silt" 0.33
4 "silt / very fine sand" 0.365
5 "very fine sand" 0.40
6 "very fine / fine sand" 0.435
7 "fine sand" 0.47
8 "fine / medium sand" 0.505
9 "medium sand" 0.54
10 "medium / coarse sand" 0.575
11 "coarse sand" 0.61
12 "coarse / very coarse sand" 0.645
13 "very coarse sand" 0.68
14 "very coarse / granule" 0.715
15 "granule" 0.75
16 "granule / pebble" 0.785
17 "pebble" 0.82
18 "pebble / cobble" 0.855
19 "cobble" 0.89
20 "cobble / boulder" 0.925
21 "boulder" 0.96
22 "mudstone" 0.26
23 "wackestone" 0.40
24 "packstone" 0.54
25 "grainstone" 0.68
26 "floatstone" 0.40
27 "rudstone" 0.54
28 "boundstone" 0.82
29 "framestone" 0.82
30 "bindstone" 0.82
31 "bafflestone" 0.82
32 "crystalline" 0.96
33 "fine ash" 0.33
34 "medium ash" 0.54
35 "coarse ash" 0.61
36 "very coarse ash" 0.68 
37 "fine lapilli" 0.75
38 "medium lapilli" 0.805
39 "coarse lapilli" 0.82
40 "fine blocks and bombs" 0.89
41 "coarse blocks and bombs" 0.96
'), header=TRUE, stringsAsFactors=FALSE
)

stain.table <- read.table(textConnection(
  'id name color
  1 "weak" "grey94"
  2 "moderate weak" "grey71"
  3 "moderate" "grey49"
  4 "moderate strong" "grey26"
  5 "strong" "grey0"
  '), header=TRUE, stringsAsFactors=FALSE
)

biotur.table <- read.table(textConnection(
  'id name percent
  1 "sparse" 0.166
  2 "low" 0.333
  3 "moderate" 0.5
  4 "high" 0.666
  5 "intense" 0.83
  6 "complete" 1
  '), header=TRUE, stringsAsFactors=FALSE
)

sed.struc.table <- read.table(textConnection(
'id name file_name
1 "massive" "sd_s1r"
2 "planar lamination" "sd_s2r"
3 "horizontal bedding" "sd_s3r"
4 "normal grading" "sd_s4r"
5 "inverse grading" "sd_s5r"
6 "cross bedding" "sd_s6r"
7 "festoon cross bedding" "sd_s7r"
8 "herringbone cross bedding" "sd_s8r"
9 "hummocky cross bedding" "sd_s9r"
10 "planar cross bedding" "sd_s10r"
11 "through cross bedding" "sd_s11r"
12 "swaley cross bedding" "sd_s12r"
13 "ripple lamination" "sd_s13r"
14 "climbing ripples" "sd_s14r"
15 "current ripple cross lamination" "sd_s15r"
16 "wave ripple cross lamination" "sd_s16r"
17 "flaser lamination" "sd_s17r"
18 "heterolithic lamination" "sd_s18r"
19 "lenticular lamination" "sd_s19r"
20 "wavy lamination" "sd_s20r"
21 "ball and pillow" "sd_s21r"
22 "convolute bedding" "sd_s22r"
23 "dish and pillar" "sd_s23r"
24 "evaporite molds" "sd_s24r"
25 "flame structures" "sd_s25r"
26 "flute casts" "sd_s26r"
27 "groove cast" "sd_s27r"
28 "hardground" "sd_s28r"
29 "load cast" "sd_s29r"
30 "mudcracks" "sd_s30r"
31 "pseudo nodules" "sd_s31r"
32 "tool marks" "sd_s32r"
33 "rain prints" "sd_s33r"
34 "slump structures" "sd_s34r"
35 "syneresis cracks" "sd_s35r"
36 "mottling" "sd_s36r"
37 "peds" "sd_s37r"
38 "rooting" "sd_s38r"
39 "slickensides" "sd_s39r"
'), header=TRUE, stringsAsFactors=FALSE
)

fossil.table <- read.table(textConnection(
  'id name file_name
  1 "macrofossils" "f_s1"
  2 "invertebrates" "f_s2"
  3 "annelids" "f_s3"
  4 "arthropods" "f_s4"
  5 "arachnids" "f_s5"
  6 "crustaceans" "f_s6"
  7 "insects" "f_s7"
  8 "trilobites" "f_s8"
  9 "brachiopods" "f_s9"
  10 "bryozoans" "f_s10"
  11 "cnidarians" "f_s11"
  12 "corals" "f_s12"
  13 "stromatoporoids" "f_s13"
  14 "echinoderms" "f_s14"
  15 "crinoids" "f_s15"
  16 "echinoids" "f_s16"
  17 "graptolites" "f_s17"
  18 "mollusks" "f_s18"
  19 "cephalopods" "f_s19"
  20 "ammonoids" "f_s20"
  21 "belemnoids" "f_s21"
  22 "nautiloids" "f_s22"
  23 "gastropods" "f_s23"
  24 "bivalves" "f_s24"
  25 "sponges" "f_s25"
  26 "vertebrates" "f_s26"
  27 "amphibians" "f_s27"
  28 "fish" "f_s28"
  29 "mammals" "f_s29"
  30 "reptiles" "f_s30"
  31 "plants" "f_s31"
  32 "leaves" "f_s32"
  33 "roots" "f_s33"
  34 "wood" "f_s34"
  35 "algae" "f_s35"
  36 "conifers" "f_s36"
  37 "ferns" "f_s37"
  38 "flowering plants" "f_s38"
  39 "stromatolites" "f_s39"
  40 "fungi" "f_s40"
  41 "microfossils" "f_s41"
  42 "conodonts" "f_s42"
  43 "diatoms" "f_s43"
  44 "foraminifera" "f_s44"
  45 "larger foraminifera" "f_s45"
  46 "benthonic foraminifera" "f_s46"
  47 "planktonic foraminifera" "f_s47"
  48 "nannofossils" "f_s48"
  49 "ostracodes" "f_s49"
  50 "palynomorphs" "f_s50"
  51 "acritarchs" "f_s51"
  52 "chitinozoans" "f_s52"
  53 "dinoflagellates" "f_s53"
  54 "pollen spores" "f_s54"
  55 "radiolarians" "f_s55"
  56 "silicoflagellates" "f_s56"
  57 "spicules" "f_s57"
  '), header=TRUE, stringsAsFactors=FALSE
)

#1 "trace fossils" "f_s41"
#2 "burrows" "f_s42"
#3 "coprolites" "f_s43"
#4 "tracks" "f_s44"

others.table <- read.table(textConnection(
  'id name file_name
1 "concretions" "other_s1r"
2 "nodules" "other_s2r"
3 "intraclasts" "other_s3r"
4 "stylolites" "other_s4r"
5 "calcite" "other_s5r"
6 "feldspar" "other_s6r"
7 "glauconite" "other_s7r"
8 "gypsum" "other_s8r"
9 "kaolinite" "other_s9r"
10 "mica" "other_s10r"
11 "olivine" "other_s11r"
12 "organic matter" "other_s12r"
13 "pyrite" "other_s13r"
14 "pyroxene" "other_s14r"
15 "siderita" "other_s15r"
16 "sulphur" "other_s16r"
'), header=TRUE, stringsAsFactors=FALSE
)

samples.table <- read.table(textConnection(
  'id name pch color
  1 "rock sample" 15 "chocolate4"
  2 "fossil sample" 16 "springgreen4"
  3 "foraminifera" 17 "deepskyblue3"
  4 "geochemistry" 13 "slateblue3"
  5 "palynology" 8  "darkblue"
  6 "petrography" 9 "tomato3"
  7 "petrophysics" 7 "black"
  8 "XRD" 11 "orangered1"
  '), header=TRUE, stringsAsFactors=FALSE
)

width.table <- read.table(textConnection(
  'id name arg value
  1 "w.oma" "w.oma" 1.5
  2 "w.int" "w.int" 0.5
  3 "w.chronostrat" "d.chronostrat" 1
  4 "w.lithostrat" "d.lithostrat" 1
  5 "w.ncore" "d.ncore" 0.75
  6 "w.oil.stain" "d.oil.stain" .55
  7 "w.bioturbation" "d.bioturbation" 2
  8 "w.barscale" "d.barscale" 1.25
  9 "w.GR.log" "d.GR.log" 3.25
  10 "w.fossils" "d.fossils" 1.5
  11 "w.tracefossils" "d.tracefossils" 1.5
  12 "w.sed.structures" "d.sed.structures" 1.5
  13 "w.other.sym" "d.other.sym" 1.5
  14 "w.struct.data" "d.struct.data" 1
  15 "w.bed.number" "d.bed.number" 0.75
  16 "w.lithology" "d.lithology" 5.08
  17 "w.samples" "d.samples" 0.75
  '), header=TRUE, stringsAsFactors=FALSE
)

litho.unit.table <- read.table(textConnection(
  'id name
  1 "group"
  2 "formation"
  3 "member"
  '), header=TRUE, stringsAsFactors=FALSE
)

gts.table <- read.table(textConnection(
'ID name
1 "super_eonothem"
2 "eonothem"
3 "erathem"
4 "system"
5 "series"
6 "series_subseries"
7 "stage"
'), header=TRUE, stringsAsFactors=FALSE)

headerLines <- read.table(textConnection(
  'x0 x1 y0 y1 lwd colour
  0 0.96 0 0 1.1 "red"
  0 0.96 0.37 0.37 0.8 "black"
  0 0.96 0.54 0.54 0.8 "black"
  0 0.96 0.89 0.89 0.2 "black"
  0 0 -0.15 1 1.1 "black"
  0.19 0.19 -0.02 0.03 0.5 "red"
  0.33 0.33 -0.12 0.03 0.9 "red"
  0.40 0.40 -0.02 0.04 0.5 "red"
  0.47 0.47 -0.02 0.05 0.5 "red"
  0.54 0.54 -0.02 0.06 0.5 "red"
  0.61 0.61 -0.02 0.07 0.5 "red"
  0.68 0.68 -0.12 0.08 0.9 "red"
  0.75 0.75 -0.02 0.09 0.5 "red"
  0.82 0.82 -0.02 0.10 0.5 "red"
  0.89 0.89 -0.02 0.11 0.5 "red"
  0.96 0.96 -0.02 0.13 0.5 "red"
  0.33 0.33 0.37 0.54 0.5 "black"
  0.68 0.68 0.37 0.76 0.5 "black"
  0.26 0.26 0.54 0.76 0.5 "black"
  0.40 0.40 0.54 0.76 0.5 "black"
  0.54 0.54 0.54 0.76 0.5 "black"
  0.82 0.82 0.54 0.76 0.5 "black"
  0.96 0.96 0.13 1 0.5 "black"
  0.33 0.33 0.86 0.9 0.5 "black"
  0.54 0.54 0.86 0.9 0.5 "black"
  0.61 0.61 0.86 0.9 0.5 "black"
  0.75 0.75 0.86 0.9 0.5 "black"
  0.785 0.785 0.86 0.9 0.5 "black"
  0.89 0.89 0.86 0.9 0.5 "black"
  0.68 0.68 0.71 1 0.5 "black"
  0.82 0.82 0.71 1 0.5 "black"
  '), header=TRUE, stringsAsFactors=FALSE
)

headerText <- read.table(textConnection(
  'label x y just rot cex font
  "Mudstone" 0.01 0.45 "left" 0 1 2
  "Sandstone" 0.34 0.45 "left" 0 1 2
  "Conglom" 0.69 0.45 "left" 0 0.9 2
  "0.004" 0.19 0.05 "left" 90 0.9 1
  "0.062" 0.33 0.05 "left" 90 0.9 1
  "0.125" 0.40 0.06 "left" 90 0.9 1
  "0.25" 0.47 0.07 "left" 90 0.9 1
  "0.5" 0.54 0.08 "left" 90 0.9 1
  "1" 0.61 0.09 "left" 90 0.9 1
  "2" 0.68 0.10 "left" 90 0.9 1
  "4" 0.75 0.11 "left" 90 0.9 1
  "64" 0.82 0.12 "left" 90 0.9 1
  "256" 0.89 0.13 "left" 90 0.9 1
  "clay" 0.09 -0.049 "left" 0 0.9 1
  "silt" 0.245 -0.049 "left" 0 0.9 1
  "vf" 0.35 -0.049 "left" 0 0.9 1
  "f" 0.445 -0.049 "left" 0 0.9 1
  "m" 0.495 -0.049 "left" 0 0.9 1
  "c" 0.58 -0.049 "left" 0 0.9 1
  "vc" 0.62 -0.049 "left" 0 0.9 1
  "gr" 0.71 -0.049 "left" 0 0.9 1
  "pe" 0.78 -0.049 "left" 0 0.9 1
  "co" 0.85 -0.049 "left" 0 0.9 1
  "bo" 0.92 -0.049 "left" 0 0.9 1
  "Mdst" 0.13 0.65 "center" 0 0.8 2
  "Wkst" 0.33 0.65 "center" 0 0.8 2
  "Pkst" 0.47 0.65 "center" 0 0.8 2
  "Grst" 0.61 0.65 "center" 0 0.8 2
  "Bdst" 0.69 0.65 "left" 0 0.8 2
  "Crys" 0.83 0.65 "left" 0 0.8 2
  "Tuff" 0.34 0.95 "center" 0 1 2
  "Lap" 0.75 0.95 "center" 0 1 2
  "Agg" 0.89 0.95 "center" 0 1 2
  "f" 0.3 0.84 "center" 0 0.9 1
  "m" 0.51 0.84 "center" 0 0.9 1
  "c" 0.58 0.84 "center" 0 0.9 1
  "vc" 0.645 0.84 "center" 0 0.8 1
  "f" 0.715 0.84 "center" 0 0.9 1
  "m" 0.758 0.83 "center" 0 0.9 1
  "c" 0.8 0.84 "center" 0 0.9 1
  "f" 0.86 0.84 "center" 0 0.9 1
  "c" 0.9375 0.84 "center" 0 0.9 1
  '), header=TRUE, stringsAsFactors=FALSE
)

chrono.table <- read.table(textConnection(
'"ID" "chrono_unit" "name" "Beginning" "End" "color" "source"
1 "stage" "aalenian" 174.1 170.3 "#8DE0F0" "ICS_2018"
2 "stage" "aeronian" 440.8 438.5 "#AEE4D6" "ICS_2018"
3 "stage" "albian" 113 100.5 "#CEE7B1" "ICS_2018"
4 "stage" "anisian" 247.2 242 "#BC90C7" "ICS_2018"
5 "stage" "aptian" 125 113 "#C1E2A8" "ICS_2018"
6 "stage" "aquitanian" 23.03 20.44 "#FFEE64" "ICS_2018"
7 "eonothem" "archean" 4000 2500 "#E72582" "ICS_2018"
8 "stage" "artinskian" 290.1 283.5 "#EA9181" "ICS_2018"
9 "stage" "asselian" 298.9 295 "#EB7D6B" "ICS_2018"
10 "stage" "bajocian" 170.3 168.3 "#9CE3F1" "ICS_2018"
11 "stage" "barremian" 129.4 125 "#B4DE9F" "ICS_2018"
12 "stage" "bartonian" 41.2 37.8 "#FFC8A5" "ICS_2018"
13 "stage" "bashkirian" 323.2 315.2 "#93CCCC" "ICS_2018"
14 "stage" "bathonian" 168.3 166.1 "#AAE7F2" "ICS_2018"
15 "stage" "berriasian" 145 139.8 "#8ED184" "ICS_2018"
16 "stage" "burdigalian" 20.44 15.97 "#FFEF6E" "ICS_2018"
17 "stage" "calabrian" 1.8 0.781 "#FFF0CA" "ICS_2018"
18 "stage" "callovian" 166.1 163.5 "#B9EBF2" "ICS_2018"
19 "system" "calymmian" 1600 1400 "#FFC694" "ICS_2018"
20 "system" "cambrian" 541 485.4 "#83AF77" "ICS_2018"
21 "series" "cambrianseries2" 521 509 "#99C797" "ICS_2018"
22 "stage" "cambrianstage10" 489.5 485.4 "#E6F3D7" "ICS_2018"
23 "stage" "cambrianstage2" 529 521 "#A7C29C" "ICS_2018"
24 "stage" "cambrianstage3" 521 514 "#A6CBA0" "ICS_2018"
25 "stage" "cambrianstage4" 514 509 "#B3CFA9" "ICS_2018"
26 "stage" "campanian" 83.6 72.1 "#EBEC9E" "ICS_2018"
27 "stage" "capitanian" 265.1 259.1 "#FFAA99" "ICS_2018"
28 "system" "carboniferous" 358.9 298.9 "#5FB6B5" "ICS_2018"
29 "stage" "carnian" 237 227 "#C8AFD7" "ICS_2018"
30 "stage" "cenomanian" 100.5 93.9 "#B8DA7A" "ICS_2018"
31 "erathem" "cenozoic" 66 0 "#F6EB3B" "ICS_2018"
32 "stage" "changhsingian" 254.14 251.902 "#FFCAC0" "ICS_2018"
33 "stage" "chattian" 27.82 23.03 "#FFE5BC" "ICS_2018"
34 "series" "cisuralian" 298.9 272.95 "#F8745F" "ICS_2018"
35 "stage" "coniacian" 89.8 86.3 "#D2E38C" "ICS_2018"
36 "system" "cretaceous" 145 66 "#82CC73" "ICS_2018"
37 "system" "cryogenian" 720 635 "#FFCD7C" "ICS_2018"
38 "stage" "danian" 66 61.6 "#FFBB7E" "ICS_2018"
39 "stage" "dapingian" 470 467.3 "#5DCBB1" "ICS_2018"
40 "stage" "darriwilian" 467.3 458.4 "#6ACFBA" "ICS_2018"
41 "system" "devonian" 419.2 358.9 "#D59C58" "ICS_2018"
42 "stage" "drumian" 504.5 500.5 "#BFDCB6" "ICS_2018"
43 "system" "ectasian" 1400 1200 "#FFD0A1" "ICS_2018"
44 "system" "ediacaran" 635 541 "#FFD789" "ICS_2018"
45 "stage" "eifelian" 393.3 387.7 "#F9D592" "ICS_2018"
46 "stage" "emsian" 407.6 393.3 "#EBD193" "ICS_2018"
47 "erathem" "eoarchean" 4000 3600 "#E42F99" "ICS_2018"
48 "series" "eocene" 56 33.9 "#FFBC87" "ICS_2018"
49 "stage" "famennian" 372.2 358.9 "#F4EDD3" "ICS_2018"
50 "stage" "floian" 477.7 470 "#34C1A8" "ICS_2018"
51 "stage" "fortunian" 541 529 "#9ABF93" "ICS_2018"
52 "stage" "frasnian" 382.7 372.2 "#F6EBC0" "ICS_2018"
53 "series" "furongian" 497 485.4 "#B2E0B1" "ICS_2018"
54 "stage" "gelasian" 2.58 1.8 "#FFEEB7" "ICS_2018"
55 "stage" "givetian" 387.7 382.7 "#F8DFA0" "ICS_2018"
56 "stage" "gorstian" 427.4 425.6 "#C8EDE9" "ICS_2018"
57 "stage" "greenlandian" 0.0117 0.008276 "#FDF5F2" "ICS_2018"
58 "series" "guadalupian" 272.95 259.1 "#FF8B75" "ICS_2018"
59 "stage" "guzhangian" 500.5 497 "#CCE0BF" "ICS_2018"
60 "stage" "gzhelian" 303.7 298.9 "#C9DAD6" "ICS_2018"
61 "eonothem" "hadean" 4567 4000 "#B12A7E" "ICS_2018"
62 "stage" "hauterivian" 132.9 129.4 "#A7D996" "ICS_2018"
63 "stage" "hettangian" 201.3 199.3 "#37C4F0" "ICS_2018"
64 "stage" "hirnantian" 445.2 443.8 "#A2DEC3" "ICS_2018"
65 "series" "holocene" 0.0117 0 "#FFF3E6" "ICS_2018"
66 "stage" "homerian" 430.5 427.4 "#C9ECE0" "ICS_2018"
67 "stage" "induan" 251.902 251.2 "#A566B3" "ICS_2018"
68 "stage" "jiangshanian" 494 489.5 "#D9EECE" "ICS_2018"
69 "system" "jurassic" 201.3 145 "#1AC4EC" "ICS_2018"
70 "stage" "kasimovian" 307 303.7 "#BBD7D6" "ICS_2018"
71 "stage" "katian" 453 445.2 "#93DAC3" "ICS_2018"
72 "stage" "kimmeridgian" 157.3 152.1 "#C6EFFC" "ICS_2018"
73 "stage" "kungurian" 283.5 272.95 "#E99B8D" "ICS_2018"
74 "stage" "ladinian" 242 237 "#C99CCD" "ICS_2018"
75 "stage" "langhian" 15.97 13.82 "#FFF078" "ICS_2018"
76 "series" "llandovery" 443.8 433.4 "#0FB58F" "ICS_2018"
77 "stage" "lochkovian" 419.2 410.8 "#ECBE7A" "ICS_2018"
78 "series" "lopingian" 259.1 251.902 "#FFB5A6" "ICS_2018"
79 "series" "lower cretaceous" 145 100.5 "#8FD07B" "ICS_2018"
80 "series" "lower devonian" 419.2 393.3 "#EDB46E" "ICS_2018"
81 "series" "lower jurassic" 201.3 174.1 "#28C1F0" "ICS_2018"
82 "series-subseries" "lowermississippian" 358.9 346.7 "#80B88B" "ICS_2018"
83 "series" "lowerordovician" 485.4 470 "#0FB58F" "ICS_2018"
84 "series sub-series" "lowerpennsylvanian" 323.2 315.2 "#85C9CB" "ICS_2018"
85 "series" "lowertriassic" 251.902 247.2 "#995BAE" "ICS_2018"
86 "stage" "ludfordian" 425.6 423 "#D6F1EA" "ICS_2018"
87 "series" "ludlow" 427.4 423 "#BBE8E0" "ICS_2018"
88 "stage" "lutetian" 47.8 41.2 "#FFBD98" "ICS_2018"
89 "stage" "maastrichtian" 72.1 66 "#F9F1A8" "ICS_2018"
90 "stage" "meghalayan" 0.0042 0 "#FFF4F0" "ICS_2018"
91 "erathem" "mesoarchean" 3200 2800 "#FD86B8" "ICS_2018"
92 "erathem" "mesoproterozoic" 1600 1000 "#FFBB7E" "ICS_2018"
93 "erathem" "mesozoic" 251.902 66 "#6CC0DB" "ICS_2018"
94 "stage" "messinian" 7.246 5.333 "#FFF395" "ICS_2018"
95 "series" "miaolingian" 509 497 "#A6D3A4" "ICS_2018"
96 "series" "middle devonian" 393.3 382.7 "#F9CB86" "ICS_2018"
97 "series" "middle jurassic" 174.1 163.5 "#71D9EF" "ICS_2018"
98 "series-subseries" "middlemississippian" 346.7 330.9 "#9BBE8B" "ICS_2018"
99 "series" "middleordovician" 470 458.4 "#44C3A0" "ICS_2018"
100 "series-subseries" "middlepennsylvanian" 315.2 307 "#A1CFCC" "ICS_2018"
101 "stage" "middlepleistocene" 0.781 0.126 "#FFF1D3" "ICS_2018"
102 "series" "middletriassic" 247.2 237 "#B084C2" "ICS_2018"
103 "series" "miocene" 23.03 5.333 "#FFEB3D" "ICS_2018"
104 "series" "mississippian" 358.9 323.2 "#67A385" "ICS_2018"
105 "stage" "moscovian" 315.2 307 "#AFD3CC" "ICS_2018"
106 "erathem" "neoarchean" 2800 2500 "#FEAFCB" "ICS_2018"
107 "system" "neogene" 23.03 2.58 "#FFDB4B" "ICS_2018"
108 "erathem" "neoproterozoic" 1000 541 "#FFB964" "ICS_2018"
109 "stage" "norian" 227 208.5 "#D5BBDD" "ICS_2018"
110 "stage" "northgrippian" 0.008276 0.0042 "#FEF6F3" "ICS_2018"
111 "stage" "olenekian" 251.2 247.2 "#B171B8" "ICS_2018"
112 "series" "oligocene" 33.9 23.03 "#FFC694" "ICS_2018"
113 "system" "ordovician" 485.4 443.8 "#00AF90" "ICS_2018"
114 "system" "orosirian" 2050 1800 "#FE85A8" "ICS_2018"
115 "stage" "oxfordian" 163.5 157.3 "#B7ECFC" "ICS_2018"
116 "stage" "paibian" 497 494 "#CCE9C4" "ICS_2018"
117 "erathem" "paleoarchean" 3600 3200 "#FC67B1" "ICS_2018"
118 "series" "paleocene" 66 56 "#FFB17B" "ICS_2018"
119 "system" "paleogene" 66 23.03 "#FFA76F" "ICS_2018"
120 "erathem" "paleoproterozoic" 2500 1600 "#FF6485" "ICS_2018"
121 "erathem" "paleozoic" 541 251.902 "#A3BC99" "ICS_2018"
122 "series" "pennsylvanian" 323.2 298.9 "#93CCCC" "ICS_2018"
123 "system" "permian" 298.9 251.902 "#F95F44" "ICS_2018"
124 "eonothem" "phanerozoic" 541 0 "#A1D4E2" "ICS_2018"
125 "stage" "piacenzian" 3.6 2.58 "#FFF9CF" "ICS_2018"
126 "series" "pleistocene" 2.58 0.0117 "#FFEFC0" "ICS_2018"
127 "stage" "pliensbachian" 190.8 182.7 "#70D1F3" "ICS_2018"
128 "series" "pliocene" 5.333 2.58 "#FFF6B2" "ICS_2018"
129 "stage" "pragian" 410.8 407.6 "#ECC886" "ICS_2018"
130 "super-eonothem" "precambrian" 4567 541 "#FF6485" "ICS_2018"
131 "stage" "priabonian" 37.8 33.9 "#FFD2B3" "ICS_2018"
132 "series" "pridoli" 423 419.2 "#E4F5EB" "ICS_2018"
133 "stage" "pridoli-null" 423 419.2 "#E4F5EB" "ICS_2018"
134 "eonothem" "proterozoic" 2500 541 "#EA5B70" "ICS_2018"
135 "system" "quaternary" 2.58 0 "#FFF49E" "ICS_2018"
136 "stage" "rhaetian" 208.5 201.3 "#E3C8E2" "ICS_2018"
137 "stage" "rhuddanian" 443.8 440.8 "#A0DFCC" "ICS_2018"
138 "system" "rhyacian" 2300 2050 "#FF7A9C" "ICS_2018"
139 "stage" "roadian" 272.95 268.8 "#FF9581" "ICS_2018"
140 "stage" "rupelian" 33.9 27.82 "#FFDBAE" "ICS_2018"
141 "stage" "sakmarian" 295 290.1 "#EA8776" "ICS_2018"
142 "stage" "sandbian" 458.4 453 "#88D5B1" "ICS_2018"
143 "stage" "santonian" 86.3 83.6 "#DFE895" "ICS_2018"
144 "stage" "selandian" 61.6 59.2 "#FFC482" "ICS_2018"
145 "stage" "serpukhovian" 330.9 323.2 "#C4C78A" "ICS_2018"
146 "stage" "serravallian" 13.82 11.63 "#FFF181" "ICS_2018"
147 "stage" "sheinwoodian" 433.4 430.5 "#BCE7D6" "ICS_2018"
148 "system" "siderian" 2500 2300 "#FF6F90" "ICS_2018"
149 "system" "silurian" 443.8 419.2 "#AFE3CD" "ICS_2018"
150 "stage" "sinemurian" 199.3 190.8 "#54CBF1" "ICS_2018"
151 "system" "statherian" 1800 1600 "#FE90B5" "ICS_2018"
152 "system" "stenian" 1200 1000 "#FFDBAE" "ICS_2018"
153 "stage" "telychian" 438.5 433.4 "#BBE8E0" "ICS_2018"
154 "series" "terreneuvian" 541 521 "#8EBB8B" "ICS_2018"
155 "stage" "thanetian" 59.2 56 "#FFC58B" "ICS_2018"
156 "stage" "tithonian" 152.1 145 "#D4F3FD" "ICS_2018"
157 "stage" "toarcian" 182.7 174.1 "#8DD8F4" "ICS_2018"
158 "system" "tonian" 1000 720 "#FFC370" "ICS_2018"
159 "stage" "tortonian" 11.63 7.246 "#FFF28B" "ICS_2018"
160 "stage" "tournaisian" 358.9 346.7 "#8EBB8B" "ICS_2018"
161 "stage" "tremadocian" 485.4 477.7 "#28BDA0" "ICS_2018"
162 "system" "triassic" 251.902 201.3 "#824FA8" "ICS_2018"
163 "stage" "turonian" 93.9 89.8 "#C5DF83" "ICS_2018"
164 "series" "upper cretaceous" 100.5 66 "#ABD671" "ICS_2018"
165 "series" "upperdevonian" 382.7 358.9 "#F6E1B2" "ICS_2018"
166 "series" "upper jurassic" 163.5 145 "#A9E8FB" "ICS_2018"
167 "series sub-series" "upper mississippian" 330.9 323.2 "#B6C48A" "ICS_2018"
168 "series" "upperordovician" 458.4 443.8 "#7AD2B1" "ICS_2018"
169 "series sub-series" "upper pennsylvanian" 307 298.9 "#BCD6CD" "ICS_2018"
170 "stage" "upper pleistocene" 0.126 0.0117 "#FFF2DD" "ICS_2018"
171 "series" "upper triassic" 237 201.3 "#BBA3D1" "ICS_2018"
172 "stage" "valanginian" 139.8 132.9 "#9AD58D" "ICS_2018"
173 "stage" "visean" 346.7 330.9 "#A9C18B" "ICS_2018"
174 "series" "wenlock" 433.4 427.4 "#AEE4D6" "ICS_2018"
175 "stage" "wordian" 268.8 265.1 "#FFA08D" "ICS_2018"
176 "stage" "wuchiapingian" 259.1 254.14 "#FFC0B3" "ICS_2018"
177 "stage" "wuliuan" 509 504.5 "#B2D7AD" "ICS_2018"
178 "stage" "ypresian" 56 47.8 "#FFB38C" "ICS_2018"
179 "stage" "zanclean" 5.333 3.6 "#FFF8C5" "ICS_2018"
'), header=TRUE, stringsAsFactors=FALSE)

munsell.table <- read.table(textConnection(
'id munsell_code munsell_hex
"1" "5R 8/2" "#D8C6C4"
"2" "5R 7/4" "#D1A5A1"
"3" "5R 6/2" "#A6908F"
"4" "5R 6/6" "#C68480"
"5" "5R 5/4" "#9D6F6D"
"6" "5R 4/2" "#745B5A"
"7" "5R 4/6" "#904E4D"
"8" "5R 3/4" "#6A3B3C"
"9" "5R 2/2" "#422C2E"
"10" "5R 2/6" "#581D28"
"11" "10R 8/2" "#DAC6C0"
"12" "10R 7/4" "#D3A597"
"13" "10R 6/2" "#A7908A"
"14" "10R 6/6" "#C68570"
"15" "10R 5/4" "#9D7064"
"16" "10R 4/2" "#745C56"
"17" "10R 4/6" "#90503F"
"18" "10R 3/4" "#693C34"
"19" "10R 2/2" "#422C2A"
"20" "5YR 8/4" "#ECC2A4"
"21" "5YR 7/2" "#C1AB9E"
"22" "5YR 6/4" "#B58D72"
"23" "5YR 5/2" "#8C776A"
"24" "5YR 5/6" "#A66E46"
"25" "5YR 4/4" "#805840"
"26" "5YR 3/2" "#594439"
"27" "5YR 3/4" "#65402B"
"28" "5YR 2/2" "#402D25"
"29" "10YR 8/2" "#D8C8B1"
"30" "10YR 8/6" "#EFC382"
"31" "10YR 7/4" "#C9AB81"
"32" "10YR 6/2" "#A3937E"
"33" "10YR 6/6" "#B88E51"
"34" "10YR 5/4" "#947650"
"35" "10YR 4/2" "#6F5F4C"
"36" "10YR 2/2" "#3D2F21"
"37" "5Y 8/4" "#D9CA93"
"38" "5Y 7/2" "#B8AF94"
"39" "5Y 7/6" "#C4AF5F"
"40" "5Y 6/4" "#A49562"
"41" "5Y 5/2" "#837B62"
"42" "5Y 5/6" "#90792C"
"43" "5Y 4/4" "#706031"
"44" "5Y 3/2" "#504834"
"45" "10Y 8/2" "#CECCAC"
"46" "10Y 7/4" "#B5B279"
"47" "10Y 6/2" "#98977B"
"48" "10Y 6/6" "#9C9944"
"49" "10Y 5/4" "#807D49"
"50" "10Y 4/2" "#62634B"
"51" "5GY 7/2" "#ACB396"
"52" "5GY 7/4" "#A8B680"
"53" "5GY 5/2" "#777E67"
"54" "5GY 3/2" "#454B39"
"55" "10GY 7/2" "#A1B59F"
"56" "10GY 6/4" "#7B9E77"
"57" "10GY 5/2" "#6F806E"
"58" "10GY 4/4" "#496948"
"59" "10GY 3/2" "#3E4C3F"
"60" "5G 7/2" "#9BB6A7"
"61" "5G 7/4" "#87BBA0"
"62" "5G 6/6" "#55A480"
"63" "5G 5/2" "#6A8074"
"64" "5G 5/6" "#3B8968"
"65" "5G 3/2" "#3A4D44"
"66" "10G 8/2" "#B2D1C6"
"67" "10G 6/2" "#809B92"
"68" "10G 4/2" "#4F665F"
"69" "5BG 7/2" "#98B6B1"
"70" "5BG 6/6" "#40A49C"
"71" "5BG 5/2" "#66807D"
"72" "5BG 4/6" "#2F6B67"
"73" "5BG 3/2" "#364D4B"
"74" "5B 8/2" "#B8CED3"
"75" "5B 7/6" "#69BBCF"
"76" "5B 6/2" "#83999F"
"77" "5B 5/6" "#2E859C"
"78" "5PB 7/2" "#A9B0BB"
"79" "5PB 5/2" "#757B88"
"80" "5PB 3/2" "#414958"
"81" "5P 6/2" "#9A929F"
"82" "5P 4/2" "#685E6D"
"83" "5P 2/2" "#382E3D"
"84" "5RP 8/2" "#D5C7CB"
"85" "5RP 6/2" "#A29096"
"86" "5RP 4/2" "#715C63"
"87" "5RP 2/2" "#402C36"
"88" "5B 9/1" "#DCE8E6"
"89" "5G 8/1" "#C0CEC4"
"90" "5GY 8/1" "#C9CCBA"
"91" "5Y 8/1" "#D2CDBD"
"92" "5YR 8/1" "#D5C8BE"
"93" "5B 7/1" "#A8B5B4"
"94" "5YR 6/1" "#9B908A"
"95" "5Y 6/1" "#979386"
"96" "5G 6/1" "#8B968D"
"97" "5GY 6/1" "#95978A"
"98" "5B 5/1" "#6E787A"
"99" "5G 4/1" "#59665C"
"100" "5GY 4/1" "#5C6154"
"101" "5Y 4/1" "#645F55"
"102" "5YR 4/1" "#695E57"
"103" "5G 2/1" "#2B332D"
"104" "5GY 2/1" "#30332D"
"105" "5Y 2/1" "#332F2A"
"106" "5YR 2/1" "#3B312E"
"107" "N9" "#E5E5E5"
"108" "N8" "#CACACA"
"109" "N7" "#AFAFAF"
"110" "N6" "#959595"
"111" "N5" "#7B7B7B"
"112" "N4" "#616161"
"113" "N3" "#484848"
"114" "N2" "#313131"
"115" "N1" "#1D1D1D"
'), header=TRUE, stringsAsFactors=FALSE)
